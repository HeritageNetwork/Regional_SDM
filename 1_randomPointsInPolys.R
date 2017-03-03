# File: 1_randomPointsInPolys.r
# Purpose: GRTS sampling of EDM polygons to create spatially balanced random points
# these are the random presence points being created here, from polygon presence data.

library(spsurvey)
library(RSQLite)
library(rgdal)

####
# Assumptions
# - there is one shapefile for each element in the working directory listed below
# - the shapefile is named with the species code that is used in the lookup table
#   e.g. glypmuhl.shp
# - There is lookup data in the sqlite database to link to other element information (full name, common name, etc.)
# - the polygon shapefile has at least these fields EO_ID_ST, SNAME, SCOMNAME, RA

####
#### modify paths ----
## these are the lines you need to change

### This is the folder that has your species polygon data. 
polydir <- "K:/Reg5Modeling_Project/inputs/species/glypmuhl/polygon_data"
setwd(polydir)

### This is the directory you want the output data (random point shapefile) written to
outdir <- "K:/Reg5Modeling_Project/inputs/species/glypmuhl/point_data"

### This is the full path and name of the information-tracking database
db_file <- "K:/Reg5Modeling_Project/databases/SDM_lookupAndTracking.sqlite"
db <- dbConnect(SQLite(),dbname=db_file)

#get a list of what's in the directory
fileList <- dir( pattern = ".shp$")
fileList
#look at the output and choose which shapefile you want to run
#enter its location in the list (first = 1, second = 2, etc)
n <- 1

## should not need to change anything else below here
####
####

# load data, QC ----
fileName <- fileList[[n]]
shpName <- strsplit(fileName,"\\.")[[1]][[1]]
sppCode <- shpName

shapef <- readOGR(fileName, layer = shpName) #Z-dimension discarded msg is OK
#check for proper column names. If no error in next three lines, then good to go
shpColNms <- names(shapef@data)
desiredCols <- c("EO_ID_ST", "SNAME", "SCOMNAME", "RA")
if("FALSE" %in% c(desiredCols %in% shpColNms)) {
	  stop("at least one column is missing or incorrectly named")
  } else {
    print("Required columns are present")
  }

#pare down columns
shapef@data <- shapef@data[,desiredCols]

#get projection info for later
projInfo <- shapef@proj4string

# explode multi-part polys ----
shp_expl <- disaggregate(shapef)

#add some columns (explode id and area)
shp_expl@data <- cbind(shp_expl@data, 
	EXPL_ID = rownames(shp_expl@data), 
	AREAM2 = sapply(slot(shp_expl, "polygons"), slot, "area"))
		
#write out the exploded polygon set
nm.PyFile <- paste(sppCode, "_expl", sep = "")

# projection info doesn't stick, apply from what we grabbed earlier
shp_expl@proj4string <- projInfo
writeOGR(shp_expl, dsn = ".", layer = nm.PyFile, driver="ESRI Shapefile", overwrite_layer=TRUE)
  
#name of random points output shapefile; add path to (now input) polygon file
nm.RanPtFile <- paste(outdir,"/", sppCode, "_RanPts", sep = "")
nm.PyFile <- paste(polydir,"/", sppCode, "_expl", sep = "")

###############################
####  Placing random points within each sample unit (polygon/EO) ----
####
###############################
    
#get the attribute table from above 
att.pt <- shp_expl@data

# just in case convert to upper
names(att.pt) <- toupper(names(att.pt))

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
att.pt$RA <- tolower(att.pt$RA)
att.pt$RA <- factor(att.pt$RA, levels = raVals)

#### these values should be discussed 
### right now, the numbers are treated as mulitpliers, so very high gets 2X the number of
### points, high get 1.5X and very low gets 1/2 the number of points
ERA_wgt <- c("very high" = 2, "high" = 1.5, "medium" = 1, "low" = 0.75, "very low" = 0.5)

att.pt$ERAWT <- unname(ERA_wgt[att.pt$RA])
att.pt$PSampNum <- att.pt$ERAWT * att.pt$PolySampNum

# create the vector for indicating how many points to put in each polygon, 
# then each value in the vector needs to be attributed to the sampling unit 
# (either EO_ID or Shape_ID)
sampNums <- c(att.pt[,"PSampNum"])
names(sampNums) <- att.pt[,"EXPL_ID"]

# sample MUST be larger than 1 for any single polygon use OVER to increase 
# sample sizes in these. To handle this, create a vector that contains 
# 2 when sample size = 1, otherwise 0
overAmt <- ifelse(sampNums == 1,2,0)

# initialize the design list and the names vector so 
# they are available in the for loop
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

# Create the GRTS survey design [might take a while]
grtsResult <- grts(design=SampDesign,
			 src.frame="shapefile", #source of the frame
			 in.shape=nm.PyFile,    #name of input shapefile no extension
			 att.frame=att.pt,      #attributes associated with elements in the frame
			 type.frame="area",     #type of frame:"finite", "linear", "area"
			 stratum="panelNum",	#stratum field in att.pt
			 mdcaty="EXPL_ID",		#categories for random pt probabilities
			 DesignID= sppCode,  	#name for the design, used to create a site ID
			 prj=nm.PyFile,
			 out.shape=nm.RanPtFile,
			 shapefile=FALSE)

ranPts <- as(grtsResult, "SpatialPointsDataFrame")
# projection info doesn't stick, apply from what we grabbed earlier
ranPts@proj4string <- projInfo
# remove extranneous fields, write it out
fullName <- paste(nm.RanPtFile,".shp",sep="")
colsToKeep <- c("stratum", desiredCols)
ranPts <- ranPts[,colsToKeep]
writeOGR(ranPts, dsn = fullName, layer = nm.RanPtFile, 
			driver="ESRI Shapefile", overwrite_layer=TRUE)

###############################
#####     Write out various stats and data to the database ----
#####
###############################

# prep the data
OutPut <- data.frame(SciName = paste(att.pt[1,"SNAME"]),
	CommName=paste(att.pt[1,"SCOMNAME"]),
	ElemCode=sppCode,
	RandomPtFile=nm.RanPtFile,
	date = paste(Sys.Date()),
	time = format(Sys.time(), "%X"),
	Loc_Use=""
	)

#Write the data to the SQLite database
dbWriteTable(db,"tblPrepStats",OutPut,append=TRUE)

## clean up ----
#Close Connection to the SQL DB
dbDisconnect(db)
# remove all objects before moving on to the next script
rm(list=ls())
