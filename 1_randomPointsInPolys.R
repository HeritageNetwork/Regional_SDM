# File: 1_randomPointsInPolys.r
# Purpose: GRTS sampling of EDM polygons to create spatially balanced random points

library(spsurvey)
library(RSQLite)
library(sp)
library(rgdal)

####
# Assumptions
# - there is one shapefile for each element in the working directory listed below
# - the shapefile is named with the species code that is used in the lookup table
#   in the sqlite database to link to other element information (full name, common name, etc.)
#   e.g. glypmuhl.shp
# - the polygon shapefile has at least these fields EO_ID, SCIEN_NAME, COMMONNAME, ERACCURACY

#######
######
## these are the lines you need to change

### This is the directory that has your species polygon data. One shapefile for each species   
setwd("G:/RegionalSDM/inputs/species/glypmuhl/polygon_data")

### This is the directory you want the output data (random point shapefile) written to

outdir <- "G:/RegionalSDM/inputs/species/glypmuhl/point_data"

### This is the full path and name of the information-tracking database
db_file <- "F:/_Howard/git/Regional_SDM/SDM_lookupAndTracking.sqlite"

db <- dbConnect(SQLite(),dbname=db_file)

## should not need to change anything else below here
#######
#######

#get a list of what's in the directory
fileList <- dir( pattern = ".shp$")

#loop through all species (having only one is ok)
for (fileName in fileList){
	shpName <- strsplit(fileName,"\\.")[[1]][[1]]
	sppCode <- shpName

	shapef <- readOGR(fileName, layer = shpName, pointDropZ = TRUE)
	#check for proper column names
	shpColNms <- names(shapef@data)
	desiredCols <- c("EO_ID", "SCIEN_NAME", "COMMONNAME", "ERACCURACY")
	if("FALSE" %in% c(desiredCols %in% shpColNms)) {
		stop("at least one column is missing or incorrectly named")
		}

	#pare down columns
	colList <- c(grep("^EO_ID$",names(shapef@data)),
		grep("SCIEN_NAME",names(shapef@data)),
		grep("COMMONNAME",names(shapef@data)),
		grep("ERACCURACY",names(shapef@data)))
	shapef@data <- shapef@data[,colList]

	#get projection info for later
	projInfo <- shp_expl@proj4string
	
	#explode multi-part polys
	shp_expl <- disaggregate(shapef)

	#add some columns (explode id and area)
	shp_expl@data <- cbind(shp_expl@data, 
		EXPL_ID = rownames(shp_expl@data), 
		AREAM2 = sapply(slot(shp_expl, "polygons"), slot, "area"))
			
	#write out the exploded polygon set
	nm.PyFile <- paste(sppCode, "_expl", sep = "")
	writeOGR(shp_expl, dsn = ., layer = nm.PyFile, driver="ESRI Shapefile", overwrite_layer=TRUE)
	  
    #name of random points output shapefile; add path to (now input) polygon file
    nm.RanPtFile <- paste(outdir,"/", sppCode, "_RanPts", sep = "")
	nm.PyFile <- paste(outdir,"/", sppCode, "_expl", sep = "")
	
	#tell the console what's up
	print(paste("Beginning on ", 
		sppCode, ", ", grep(fileName, fileList) , " of ", length(fileList), sep = ""))

	# Optional: send all the coming GRTS info messages to a file, so it's easier to 
	# track progress on the terminal.
	# sinkName <- paste(outdir, "/", "RunMsgs_",Sys.Date(), ".txt",sep="")
	# sink(file = sinkName, append=TRUE, type="output")

	###############################
	#####     Placing random points within each sample unit (polygon/EO)
	#####
	###############################
      
	#get the attribute table from above 
	att.pt <- shp_expl@data

	# just in case convert to upper
	names(att.pt) <- toupper(names(att.pt))
	
	#add another copy of the expl_ID field - the original becomes 'mdcaty' in 
	#the final output
	att.pt$EXPL_ID2 <- att.pt$EXPL_ID

	#calculate Number of points for each poly, stick into new field
	att.pt$PolySampNum <- round(400*((2/(1+exp(-(att.pt[,"AREAM2"]/900+1)*0.004)))-1))
	#make a new field for the design, providing a stratum name
	att.pt <- cbind(att.pt, "panelNum" = paste("poly_",att.pt$EXPL_ID, sep=""))

	##
	# modify point counts based on representational accuracy (RA)
	# polygons with higher RA get higher probability of sampling (more points in dataset)
	# polygons with lower RA get lower probability of sampling (fewer points on per-area basis)
	# this is, in effect, weighting the sampling within the modeling effort - we are simply 
	# doing it here a-priori
	##
	
	#assign values to eraccuracy
	raVals <- c("very high", "high", "medium", "low", "very low")
	att.pt$ERACCURACY <- tolower(att.pt$ERACCURACY)
	att.pt$ERACCURACY <- factor(att.pt$ERACCURACY, levels = raVals)
	####################### these values should be discussed ###################################
	### right now, the numbers are treated as mulitpliers, so very high gets 2X the number of
	### points, high get 1.5X and very low gets 1/2 the number of points
	ERA_wgt <- c("very high" = 2, "high" = 1.5, "medium" = 1, "low" = 0.75, "very low" = 0.5)
	
	att.pt$ERAWT <- unname(ERA_wgt[att.pt$ERACCURACY])
	att.pt$PSampNum <- att.pt$ERAWT * att.pt$PolySampNum
	
	#create the vector for indicating how many points to put in each polygon, 
	#then each value in the vector needs to be attributed to the sampling unit 
	#(either EO_ID or Shape_ID)
	sampNums <- c(att.pt[,"PSampNum"])
	names(sampNums) <- att.pt[,"EXPL_ID"]
	
	# sample MUST be larger than 1 for any single polygon use OVER to increase 
	# sample sizes in these. To handle this, create a vector that contains 
	# 2 when sample size = 1, otherwise 0
	overAmt <- ifelse(sampNums == 1,2,0)

	#initialize the design list and the names vector so 
	#they are available in the for loop
	SampDesign <- vector("list",length(sampNums))
	namesVec <- vector("list", length(sampNums))

	# Build the sampling design, as required by GRTS
	# this is a list 'SampDesign' with internal lists: 'panel', 'seltype', and 
	# 'over' for each entry of SampDesign
	for (i in 1:length(sampNums)){
		#build a vector of names to apply after the for loop
		namesVec[i] <- paste("poly_",i,sep="")
		#initialize the internal list
		SampDesign[[i]] <- vector("list",3)
		#populate the internal list
		SampDesign[[i]] <- list(panel=c(FirstSamp=sampNums[[i]]),
				seltype="Equal", 
				over=overAmt[[i]])
	}
	#apply that names vector
	names(SampDesign) <- namesVec

	# if you want to check out this design, 
	# a couple of exploratory commands here, commented out
	# list the names of one of the sub lists
	#names(SampDesign[[1]])
	# show something about the structure of the list
	#summary(SampDesign)  ##str(SampDesign) is even more thorough

	# Create the GRTS survey design
	grtsResult <- grts(design=SampDesign,
				 src.frame="shapefile", #source of the frame
				 in.shape=nm.PyFile,    #name of input shapefile no extension
				 att.frame=att.pt,      #attributes associated with elements in the frame
				 type.frame="area",     #type of frame:"finite", "linear", "area"
				 stratum="panelNum",	#stratum field in att.pt
				 mdcaty="EXPL_ID",		#categories for random pt probabilities
				 DesignID= sppCode,  	#name for the design, used to create a site ID
				 prj=nm.PyFile,
				 out.shape=nm.RanPtFile)

	ranPts <- as(grtsResult, "SpatialPointsDataFrame")
	# projection info didn't stick, apply from what we grabbed earlier
	ranPts@proj4string <- projInfo
	# remove extranneous fields, write it out
	fullName <- paste(nm.RanPtFile,".shp",sep="")
	colsToKeep <- c("stratum","EO_ID","SCIEN_NAME","ERACCURACY")
	ranPts <- ranPts[,colsToKeep]
	writeOGR(ranPts, dsn = fullName, layer = nm.RanPtFile, 
				driver="ESRI Shapefile", overwrite_layer=TRUE)

	###############################
	#####     Write out various stats and data to the database
	#####
	###############################

	# prep the data
	OutPut <- data.frame(SciName = paste(att.pt[1,"SCIEN_NAME"]),
		CommName=paste(att.pt[1,"COMMONNAME"]),
		ElemCode=sppCode,
		RandomPtFile=nm.RanPtFile,
		date = paste(Sys.Date()),
		time = format(Sys.time(), "%X"),
		Loc_Use=""
		)

	#Write the data to the SQLite database
	dbWriteTable(db,"tblPrepStats",OutPut,append=TRUE)
	###############################

	#stop the sink
	# sink()
	#tell the console what's up
	print(paste("Finished with ", sppCode, sep=""))
#close the for loop
}

#Close Connection to the SQL DB
dbDisconnect(db)

