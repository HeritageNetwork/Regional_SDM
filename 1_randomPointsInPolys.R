# File: randomPointsInPolys.r
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
# - the polygon shapefile has these fields EO_ID, SCIEN_NAME, COMMONNAME

######
## these are the lines you need to change

### This is the directory that has your species polygon data. One shapefile for each species   
#setwd("G:/SDM_test/ElementData")
setwd("~/Documents/SDM/GIS/ElementData")

### This is the full path and name of the information-tracking database
#db_file<-"F:/_Howard/git/Regional_SDM/SDM_lookupAndTracking.sqlite"
db_file<-"~/Documents/SDM/Regional_SDM/SDM_lookupAndTracking.sqlite"
db<-dbConnect(SQLite(),dbname=db_file)

## should not need to change anything else
#######



#get a list of what's in the directory
fileList <- dir( pattern = ".shp$")

#loop through all species (having only one is ok)
for (fileName in fileList){
  shpName <- strsplit(fileName,"\\.")[[1]][[1]]
  sppCode <- shpName

	shapef <- readOGR(fileName, layer = shpName)
	#explode multi-part polys
	shp_expl <- disaggregate(shapef)
	#strip some columns
	colList <- c(grep("^EO_ID$",names(shp_expl@data)),
		   grep("SCIEN_NAME",names(shp_expl@data)),
		   grep("COMMONNAME",names(shp_expl@data)))
	shp_expl@data <- shp_expl@data[,colList]

		#add some columns (explode id and area)
	shp_expl@data <- cbind(shp_expl@data, 
			EXPL_ID = rownames(shp_expl@data), 
			AREAM2 = sapply(slot(shp_expl, "polygons"), slot, "area"))
			
	
      #write out the exploded polygon set
    nm.PyFile <- paste(sppCode, "_expl", sep = "")
  	writeOGR(shp_expl, dsn = ".", layer = nm.PyFile, driver="ESRI Shapefile", overwrite_layer=TRUE)
	  

    #name of output shapefile for random points within polygons
    nm.RanPtFile <- paste(sppCode, "_RanPts", sep = "")

      #tell the console what's up
      print(paste("Beginning on ", 
                   sppCode, ", ", grep(fileName, fileList) , " of ", length(fileList), sep = ""))
      #send all the coming GRTS info messages to a file, so it's easier to 
      #track progress on the terminal
      # sinkName <- paste("RunMsgs_",Sys.Date(), ".txt",sep="")
      # sink(file = sinkName, append=TRUE, type="output")

      ###############################
      #####     Placing random points within each sample unit (polygon/EO)
      #####
      ###############################
      
      #get the attribute table from above 
      #att.pt <- read.dbf(nm.PtFile)
	  att.pt <- shp_expl@data
	  
		#just in case convert to upper
		#names(att.pt) <- toupper(names(att.pt))	
      #need to clean up colnames some more (remove extranneous from above),
      #find the locations of the varying named columms (straight grep).
      # colList <- c(grep("^EO_ID$",names(att.pt)),
                   # grep("SCIEN_NAME",names(att.pt)),
                   # grep("COMMONNAME",names(att.pt)),
                   # grep("EXPL_ID",names(att.pt)),                                      
                   # grep("PY_AREAM2",names(att.pt)))
      #do the extract (colList is a list of column numbers)
      # att.pt <- att.pt[,colList]
      #rename
      # names(att.pt) <- c("EO_ID", "SCI_NAME",
      #                    "COMMNAME", "expl_ID", "AreaM2")
      #order the dataframe by expl_ID (=the way polygon shapefile is ordered).
          # Note that now that it is sorted the same way as the polygon shapefile, 
          # we'll use this attribute table for the final output, rather than the 
          # attribute table of the polygon shapefile. This, in effect provides the 
          # 'join' so that extra attributes from the above GRTS run can be joined 
          # to the output from this GRTS run.
      # att.pt <- att.pt[order(att.pt$expl_ID),]
      #add another copy of the expl_ID field - the original becomes 'mdcaty' in 
      #the final output
      att.pt$EXPL_ID2 <- att.pt$EXPL_ID

      #### here would be a good place (I think) to add a name field and attribute with elemname
#### use "cbind" e.g. cbind(att.pt, elemCode = elemName)
      #calculate Number of points for each poly
      #calc values into a new field
      att.pt$PolySampNum <- round(400*((2/(1+exp(-(att.pt[,"AREAM2"]/900+1)*0.004)))-1))
      #make a new field for the design, providing a stratum name
      att.pt <- cbind(att.pt, "panelNum" = paste("poly_",att.pt$EXPL_ID, sep=""))

      #create the vector for indicating how many points to put in each polygon, 
      #then each value in the vector needs to be attributed to the sampling unit 
      #(either EO_ID or Shape_ID)
      sampNums <- c(att.pt[,"PolySampNum"])
      names(sampNums) <- att.pt[,"EXPL_ID"]
      # sample MUST be larger than 1 for any single polygon use OVER to increase 
      # sample sizes in these. To handle this, create a vector that contains 
      # 2 when sample size = 1, otherwise 0
      overAmt <- ifelse(sampNums == 1,2,0)

      #initialize the design list and the names vector so 
      #they are available in the for loop
      SampDesign <- vector("list",length(sampNums))
      namesVec <- vector("list", length(sampNums))

      # whew!  build the sampling design, as required by GRTS
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

      #if you want to check out this design, 
      #a couple of exploratory commands here, commented out
        #list the names of one of the sub lists
      #names(SampDesign[[1]])
        #show something about the structure of the list
      #summary(SampDesign)  ##str(SampDesign) is even more thorough

      # Create the GRTS survey design
      Unequalsites <- grts(design=SampDesign,
                     src.frame="shapefile", #source of the frame
                     in.shape=nm.PyFile,    #name of input shapefile no extension
                     att.frame=att.pt,      #a data frame of attributes associated with elements in the frame
                     type.frame="area",     #type of frame:"finite", "linear", "area"
                     stratum="panelNum",
                     mdcaty="EXPL_ID",
                     DesignID= sppCode,  #name for the design, which is used to create a site ID for each site.
                     shapefile=TRUE,
                     prj=nm.PyFile,
                     out.shape=nm.RanPtFile)

## GRTS just wrote the shapefile here. So now would be the time to open it up again and delete columns
	  
	  ###############################
      #####     Write out various stats and data to the database
      #####
      ###############################

      # prep the data
      OutPut <- data.frame(SciName = paste(att.pt[1,"SCI_NAME"]),
                   CommName=paste(att.pt[1,"COMMNAME"]),
                   ElemCode=sppCode,
                   RandomPtFile=nm.RanPtFile,
                   date = paste(Sys.Date()),
                   time = format(Sys.time(), "%X"),
                   Loc_Use=""
                   )

      #write the data to the database
      #sqlSave(Cn.MDB.out, OutPut, tablename = "tblPrepStats", 
      #                            append = TRUE, 
      #                            rownames = FALSE)

      #Write the data to the SQLite database
      dbWriteTable(db,"tblPrepStats",OutPut,append=TRUE)
      ###############################
      
      #stop the sink
      # sink()
      #tell the console what's up
      print(paste("Finished with ", sppCode, sep=""))
#close the for loop
}

# close the connection to the Access DB
#close(Cn.MDB.out)

#Close Connection to the SQL DB
dbDisconnect(db)

