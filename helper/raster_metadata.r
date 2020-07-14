# create raster metadata

# to later wrap into a function, the things needed that vary:
# 1. model_run_name 
# 2. algo (what algorithm is this)
# 3. ensemble_stat (what statistic does this ensemble represent?)
# either 2 or 3 would have data, the other would be NA
# everythig else can get drawn from the database(s)?

## TODO: store contact info for models in the DB so it can get 
## queried and populated here instead of hard-coding it. 


library(XML)

library(here)
library(raster)
library(sf)

# build everything in lists, then export
# setup base structure ----
m <- vector("list", 1)
names(m) <- "metadata"

m[[1]] <- vector("list", 8)
names(m[[1]]) <- c("Identification_Information", "Data_Quality_Information",
                  "Spatial_Data_Organization_Information", "Spatial_Reference_Information",
                  "Entity_and_Attribute_Information", "Distribution_Information",
                  "Metadata_Reference_Information", "esri")

m[[1]]$Identification_Information <- vector("list",13)
names(m[[1]]$Identification_Information) <- c("Citation", 
                                                 "Description", 
                                                 "Status", 
                                                 "Time_Period_of_Content", 
                                                 "Spatial_Domain", 
                                                 "Keywords", 
                                                 "Access_Constraints", 
                                                 "Use_Constraints", 
                                                 "Point_of_Contact", 
                                                 "Data_Set_Credit", 
                                                 "Security_Information", 
                                                 "Native_Data_Set_Environment", 
                                                 "Cross_Reference")

# citation section ----

# get data from databases
db <- dbConnect(SQLite(),dbname=nm_db_file)
sql <- paste0("SELECT * ",
                  "FROM lkpAlgorithms ",
                  "WHERE shortCode ='", algo , "';")
algo.dat <- dbGetQuery(db, sql)

cutecode <- strsplit(model_run_name, split = "_")[[1]][[1]]
sql <- paste0("SELECT * ",
              "FROM lkpSpecies ",
              "WHERE sp_code ='", cutecode , "';")
spp.dat <- dbGetQuery(db, sql)

sql <- paste0("SELECT * ",
              "FROM tblModelResults ",
              "WHERE model_run_name ='", model_run_name , "';")
model.run.dat <- dbGetQuery(db, sql)

model.date <- strsplit(model_run_name, split = "_")[[1]][[2]]
algos <- strsplit(model.run.dat$algorithms, split = ", ")[[1]]
dbDisconnect(db)
rm(db)

cit_i <- as.list(c("Citation_Information" = NA))
cit_i[[1]] <- vector("list", 6)
names(cit_i[[1]]) <- c("Originator",
                      "Publication_Date",
                      "Title",
                      "Publication_Information",
                      "Other_Citation_Details",
                      "Online_Linkage")

cit_i[[1]]$Originator <- "NatureServe"
cit_i[[1]]$Publication_Date <- model.date
cit_i[[1]]$Title <- paste0("Predicted habitat suitability for ", 
                           spp.dat$common_name, "(", spp.dat$scientific_name,")", 
                           " based on the ", 
                           algo.dat$fullName, 
                           " algorithm.")
cit_i[[1]]$Publication_Information <- as.list(c(
    "Publication_Place" = " Arlington, VA",
    "Publisher" = "NatureServe"
    ))
cit_i[[1]]$Other_Citation_Details <- "This model was created collaboratively with Natural Heritage New Mexico and the New York Natural Heritage Program."
cit_i[[1]]$Online_Linkage <- NA

# Citation:
#   Citation_Information:
#   Originator: BLM REQUIRED – U.S. Department of Interior, Bureau of Land Management (BLM) – <Optional: Append additional originating office information following coma. Example: “U.S. Department of Interior, Bureau of Land Management (BLM), Arizona State Office.”>
#   Publication_Date: BLM REQUIRED – <Formatted as YYYYMMDD, YYYY-MM-DD, YYYY-MM, YYYY. Must specify a date here, no text descriptions allowed.>
#   Title: BLM REQUIRED – <The title should include minimum information of what, where, and when of the dataset.  Refer to the Naming Convention for Citation Title for Geospatial Metadata provided in WO IM 2013-044.>
#   Publication_Information:
#     Publication_Place: BLM RECOMMENDED
#     Publisher: BLM REQUIRED – Bureau of Land Management
#   Other_Citation_Details: BLM RECOMMENDED – <Enter any extra information here that the user should know about the origin or publication of the data. These would be details that are otherwise not covered by other available subcategories of the Citation section.>
#   Online_Linkage: BLM REQUIRED, If Applicable –<Only for data published to the public – may be required by some publishing systems, and managed by other publishing systems. Direct URL to dataset. Example: https://www.blm.gov/... >
## in xml ##
# <citation>
#   <citeinfo>
#     <origin>BLM REQUIRED - U.S. Department of Interior, Bureau of Land Management (BLM) - &lt;Optional: Append additional originating office information following coma. Example: "U.S. Department of Interior, Bureau of Land Management (BLM), Arizona State Office."&gt;</origin>
#     <pubdate>BLM REQUIRED - &lt;Formatted as YYYYMMDD, YYYY-MM-DD, YYYY-MM, YYYY. Must specify a date here, no text descriptions allowed.&gt;</pubdate>
#     <title>BLM REQUIRED - &lt;The title should include minimum information of what, where, and when of the dataset.  Refer to the Naming Convention for Citation Title for Geospatial Metadata provided in WO IM 2013-044.&gt;</title>
#     <pubinfo>
#       <publish>BLM REQUIRED - Bureau of Land Management</publish>
#     </pubinfo>
#     <onlink>BLM REQUIRED, If Applicable - &lt;Only for data published to the public - may be required by some publishing systems, and managed by other publishing systems. Direct URL to dataset. Example: https://www.blm.gov/... &gt;</onlink>
#   </citeinfo>
# </citation>


# description section ----

des_i <- as.list(c("Description" = NA))
des_i[[1]] <- vector("list", 2)
names(des_i[[1]]) <- c("Abstract",
                       "Purpose")
des_i[[1]]$Abstract <- paste0("This habitat suitability model (HSM) displays ",
                             "the probability (0-1) of a location having similar ",
                             "environmental conditions in comparison to known presence locations. ",
                             "It is one of ", length(algos), " individual models created for this ",
                             "species, to be combined into an ensemble model.")
  
des_i[[1]]$Purpose <- paste0("This habitat suitability model (HSM) was developed ",
                            "to support survey and discovery, conservation action, ", 
                            " and management decisionmaking for this species.")

#   Description:
#   Abstract: BLM REQUIRED – <A brief narrative summary of the data set. Describe in general terms what this data set portrays and the type of data (attributes) associated with it. Example: "This theme contains polygons of Big Horn Sheep distribution with attributes for season of use, key value, occupancy, and others (see Entity/Attribute section)".>
#   Purpose: BLM REQUIRED – <A summary of the intentions with which the data set was developed. Why was the data set created? What business functions does this data set support? This [POINT, LINE, POLYGON] dataset was created to [EXPLANATION OF PURPOSE OF THIS DATASET]. This is not the correct location for any processing steps; those belong in the process section of the metadata.>
#   Sample: "This data set was developed to support land-use planning and site-specific project planning", or “This point dataset was created to represent the 591 National Natural Landmark (NNL) sites for mapping and visualization purposes. These data were created from the centroids of the NNL Boundary dataset.”

# status section ----

stat_i <- as.list(c("Status" = NA))
stat_i[[1]] <- vector("list", 2)
names(stat_i[[1]]) <- c("Progress",
                        "Maintenance_and_Update_Frequency")
stat_i[[1]]$Progress <- paste0("This product is complete for its intended use. ",
                               "If additional locations for this species are discovered, ",
                               "it should be modeled anew.")
stat_i[[1]]$Maintenance_and_Update_Frequency <- 
            paste0("This model should be re-run if surveys reveal additional information ",
                    "about the distribution of this species or if additional relevant ",
                    "predictor variables are developed.")
# Status:
#   Progress: BLM REQUIRED
#   Maintenance_and_Update_Frequency: BLM REQUIRED
# <status>
#   <progress>BLM REQUIRED</progress>
#   <update>BLM REQUIRED</update>
# </status>

# time period of content ----

tpoc <- as.list(c("Time_Period_of_Content" = NA))
tpoc[[1]] <- vector("list",2)
names(tpoc[[1]]) <- c("Time_Period_Information",
                      "Currentness_Reference")

tpoc[[1]]$Time_Period_Information <- vector("list",2)
names(tpoc[[1]]$Time_Period_Information) <- c("Single_Date", "Calendar_Date")

tpoc[[1]]$Time_Period_Information$Single_Date <- model.date
tpoc[[1]]$Time_Period_Information$Calendar_Date <- model.date
tpoc[[1]]$Currentness_Reference <- "Period of data compilation and model development."

# Time_Period_of_Content:
#   Time_Period_Information:
#   Single_Date/Time:
#   Calendar_Date: BLM REQUIRED – <Can use single date or multiple dates or range of dates. Formatted as YYYYMMDD, YYYY-MM-DD, YYYY-MM, YYYY. >
#   Currentness_Reference: BLM REQUIRED
# <timeperd>
#   <timeinfo>
#     <sngdate>
#       <caldate>BLM REQUIRED - &lt;Can use single date or multiple dates or range of dates. Formatted as YYYYMMDD, YYYY-MM-DD, YYYY-MM, YYYY. &gt;</caldate>
#     </sngdate>
#   </timeinfo>
#   <current>BLM REQUIRED</current>
# </timeperd>

# spatial domain ----

# use study area instead of output raster (faster?)
studyArea <- st_read(here("_data","species",  spp.dat$sp_code, "inputs", "model_input",
                          paste0(model_run_name, "_studyArea.gpkg")))
latlon_bb <- st_bbox(st_transform(studyArea, 4326))

# predictionFileName <- paste0(model.run.dat$model_run_name, "_", algo, ".tif")
# fullPath <- here("_data","species", spp.dat$sp_code, "outputs","model_predictions",predictionFileName)
# ras <- raster(fullPath)
# extent(ras)

spdom <- as.list(c("Spatial_Domain" = NA))
spdom[[1]] <- as.list(c("Bounding_Coordinates" = NA))
spdom[[1]][[1]] <- vector("list",4)
names(spdom[[1]][[1]]) <- c("West_Bounding_Coordinate", 
                            "East_Bounding_Coordinate",
                            "North_Bounding_Coordinate",
                            "South_Bounding_Coordinate")

spdom[[1]][[1]]$West_Bounding_Coordinate <- latlon_bb$xmin
spdom[[1]][[1]]$East_Bounding_Coordinate <- latlon_bb$xmax
spdom[[1]][[1]]$North_Bounding_Coordinate <- latlon_bb$ymax
spdom[[1]][[1]]$South_Bounding_Coordinate <- latlon_bb$ymin

# Spatial_Domain:
#   Bounding_Coordinates: 
#   West_Bounding_Coordinate: BLM REQUIRED
# East_Bounding_Coordinate: BLM REQUIRED
# North_Bounding_Coordinate: BLM REQUIRED
# South_Bounding_Coordinate: BLM REQUIRED
# <spdom>
#   <bounding>
#     <westbc>BLM REQUIRED</westbc>
#     <eastbc>BLM REQUIRED</eastbc>
#     <northbc>BLM REQUIRED</northbc>
#     <southbc>BLM REQUIRED</southbc>
# </bounding>
# </spdom>

# keywords ----
kw <- as.list(c("Keywords" = NA))
kw[[1]] <- vector("list",4)
names(kw[[1]]) <- c("Theme", 
                        "Theme",
                        "Theme",
                        "Place")
kw[[1]][[1]]$Theme_Keyword_Thesaurus <- "ISO 19115 Topic Category"
kw[[1]][[1]]$Theme_Keyword <- "environment"
kw[[1]][[2]] <- vector("list",3)
names(kw[[1]][[2]]) <- c("Theme_Keyword_Thesaurus", "Theme_Keyword", "Theme_Keyword")
kw[[1]][[2]]$Theme_Keyword_Thesaurus <- "BLM-THEME"
kw[[1]][[2]][[2]] <- "Geospatial"
kw[[1]][[2]][[3]] <- "Endangered"

kw[[1]][[3]] <- vector("list",4)
names(kw[[1]][[3]]) <- c("Theme_Keyword_Thesaurus", "Theme_Keyword", "Theme_Keyword", "Theme_Keyword")
kw[[1]][[3]]$Theme_Keyword_Thesaurus <- "None"
kw[[1]][[3]][[2]] <- "Habitat Suitability Model"
kw[[1]][[3]][[3]] <- "Species Distribution Model"
kw[[1]][[3]][[4]] <- spp.dat$scientific_name


# get the states that intersect with the range
studyArea <- st_read(here("_data","species",  spp.dat$sp_code, "inputs", "model_input",
                     paste0(model_run_name, "_studyArea.gpkg")))
usStates <- st_read(here("_data","other_spatial","feature","US_States.shp"))
intersectingStates = usStates$NAME[st_intersects(usStates, studyArea, sparse = FALSE)]
rm(studyArea, usStates)

kw[[1]]$Place <- vector("list", length(intersectingStates)+1)
names(kw[[1]]$Place) <- c("Place_Keyword_Thesaurus", rep("Place_Keyword", length(intersectingStates)))
kw[[1]]$Place[[1]] <- "BLM-STATE"
for(i in 1:length(intersectingStates)){
  kw[[1]]$Place[[i+1]] <- as.character(intersectingStates[[i]])
}

# Theme:
#   Theme_Keyword_Thesaurus: BLM REQUIRED – ISO 19115 Topic Category
#   Theme_Keyword: BLM REQUIRED – <At least 1 Keyword from ISO THESAURUS to describe and identify these data. Each keyword is on a separate line. Spelling and case is important for these keywords, please use exact values.  ISO 19115 Topic Category numeric codes are being phased out, and it is recommended to discontinue including them in metadata. >
# Theme:
#   Theme_Keyword_Thesaurus: BLM REQUIRED – BLM-THEME
#   Theme_Keyword: BLM REQUIRED – <At least 1 Keyword from "BLM-THEME" THESAURUS provided in WO IM 2013-182.>
# Theme:
#   Theme_Keyword_Thesaurus: BLM RECOMMENDED – <Program or Project required Keyword Thesaurus. Example: Rapid Ecoregional Assessment Initiative Keyword List.>
#   Theme_Keyword: BLM RECOMMENDED – <Can be from a project keyword thesaurus or a free-form list of keywords such as acronyms or other related terms.  This will allow a project to predefine at least some additional keywords or if none are defined to allow for a list of user identified keywords.>
# Theme:
#   Theme_Keyword_Thesaurus: BLM RECOMMENDED – <Specify “None”, or other formal keyword thesaurus reference.>
#   Theme_Keyword: BLM REQUIRED, If Applicable – <Any other user defined Keywords, appropriate BLM or other Acronyms not previously used in one of the other thesauri.>
# Place:
#   Place_Keyword_Thesaurus: BLM REQUIRED – BLM-STATE
#   Place_Keyword: BLM REQUIRED – < At least 1 keyword from BLM-STATE Thesaurus. >
  
# Access_Constraints ----

accc <- as.list(c("Access_Constraints" = paste0(
  "These are BLM data which might contain sensitive information, and may not be ",
  "releasable under the Privacy Act.  Access to these records is limited to ",
  "Authorized Personnel Only."
)))
  
#Access_Constraints: BLM REQUIRED – <Use appropriate BLM standardized access constraints provided in WO IM 2014-029, see the directives attachment Standardized Disclaimer Statements for BLM for specifics.>
## tgh: see https://www.blm.gov/policy/im-2014-029

# Use constraints ----

usec <- as.list(c("Use_Constraints" = paste0(
  "NON-PUBLIC. BLM INTERNAL USE ONLY. NOT FOR DISTRIBUTION. The information contained ",
  "in these data is dynamic and maychange over time. The data are not better than the ",  
  "sources from which they were derived, and both scale and accuracy might vary ", 
  "across the dataset. These data might not have the accuracy, resolution, completeness, timeliness, ",
  "or other characteristics appropriate for applications that potential users of the data ",
  "may contemplate. The User is encouraged to carefully consider the content of the metadata ",
  "file associated with these data.  These data are neither legal documents nor land surveys, ",
  "and must not be used as such. Official records may be referenced at most BLM offices. Please ",
  "report any errors in the data to the BLM office from which it was obtained.  The BLM should be ",
  "cited as the data source in any products derived from these data. Any Users wishing to modify ", 
  "the data are obligated to describe within the process history section of the metadata the types ",
  "of modifications they have performed. The User specifically agrees not to misrepresent the data, ",
  "nor to imply that changes made were approved or endorsed by BLM. This data may be updated by the ",
  "BLM without notification.  By using these data you hereby agree to these conditions."
)))


#(Optional paragraph for information specific to the dataset, for example:  These data were digitized and edited from best available sources at scales ranging from1:24,000to 1:500,000.  These data were developed in support of the Western Sahara Resource Management Plan (RMP).  Users are cautioned that these data should only be considered a supplement to the RMP, and not a substitute for the RMP.  These data do not eliminate the need for onsite survey and evaluation of specific sites.  The User should ensure that they have the latest version of these data.)

#Use_Constraints: BLM REQUIRED – <Use appropriate BLM standardized use constraints provided in WO IM 2014-029, see the directives attachment Standardized Disclaimer Statements for BLM for specifics.>
#<Please use BLM standardized Use Constraints provided in WO IM 2014-029.>
## tgh: see https://www.blm.gov/policy/im-2014-029 and link therein to the pdf 

# Point_of_Contact ----

poc <- as.list(c("Point_of_Contact" = NA))
poc[[1]] <- as.list(c("Contact_Info" = vector("list",4)))
names(poc[[1]]) <- c("Contact_Organization_Primary", 
                     "Contact_Position",
                     "Contact_Address",
                     "Contact_Electronic_Mail_Address")
  
poc[[1]]$Contact_Organization_Primary <- vector("list",2)
names(poc[[1]]$Contact_Organization_Primary) <- c("Contact_Organization","Contact_Person")
poc[[1]]$Contact_Organization_Primary$Contact_Organization <- "NatureServe"
poc[[1]]$Contact_Organization_Primary$Contact_Person <- "Patrick McIntyre, PhD "
poc[[1]]$Contact_Position <- "Senior Ecologist, NatureServe"
poc[[1]]$Contact_Address <- vector("list",6)
names(poc[[1]]$Contact_Address) <- c("Address_Type","Address","City","State_or_Province","Postal_Code","Country")
poc[[1]]$Contact_Address$Address_Type <- "Mailing Address"
poc[[1]]$Contact_Address$Address <- "1680 38th St. Suite 120"
poc[[1]]$Contact_Address$City <- "Boulder"
poc[[1]]$Contact_Address$State_or_Province <- "CO"
poc[[1]]$Contact_Address$Postal_Code <- "80301"
poc[[1]]$Contact_Address$Country <- "US"

poc[[1]]$Contact_Electronic_Mail_Address <- "Patrick_McIntyre@natureserve.org"


# Point_of_Contact:
#   Contact_Information:
#   Contact_Organization_Primary: [OR Contact_Person_Primary:]
      # Contact_Organization: BLM REQUIRED
      # Contact_Person: BLM RECOMMENDED
      # OR
      # Contact_Person_Primary: [OR Contact_Organization_Primary:]
      # Contact_Organization: BLM REQUIRED
      # Contact_Person: BLM REQUIRED
# 
# Contact_Position: BLM REQUIRED
# Contact_Address:
#   Address_Type: BLM REQUIRED
#   Address: BLM REQUIRED
#   City: BLM REQUIRED
#   State_or_Province: BLM REQUIRED – <Use postal 2 character code (CA, VA, etc.)>
#   Postal_Code: BLM REQUIRED – <either the 5 or 10 alphanumeric-character code (99999-9999)>
#   Country: BLM REQUIRED
# Contact_Voice_Telephone: BLM RECOMMENDED
# Contact_TDD/TTY_Telephone: BLM RECOMMENDED
# Contact_Facsimile_Telephone: BLM RECOMMENDED
# Contact_Electronic_Mail_Address: BLM REQUIRED
# Hours_of_Service: BLM RECOMMENDED
# Contact_Instructions: BLM RECOMMENDED

# <ptcontac>
#   <cntinfo>
#     <cntorgp>
#       <cntorg>BLM REQUIRED</cntorg>
#     </cntorgp>
#     <cntpos>BLM REQUIRED</cntpos>
#     <cntaddr>
#       <addrtype>BLM REQUIRED</addrtype>
#       <address>BLM REQUIRED</address>
#       <city>BLM REQUIRED</city>
#       <state>BLM REQUIRED - &lt;Use postal 2 character code (CA, VA, etc.)&gt;</state>
#       <postal>BLM REQUIRED - &lt;either the 5 or 10 alphanumeric-character code (99999-9999)&gt;</postal>
#       <country>BLM REQUIRED</country>
#     </cntaddr>
#     <cntemail>BLM REQUIRED</cntemail>
#   </cntinfo>
# </ptcontac>



# Data_Set_Credit ----

dsc <- as.list(c("Data_Set_Credit" = paste0(
  "This data set was developed by NatureServe and the Natural Heritage Network"
)))

# Data_Set_Credit: BLM RECOMMENDED – <Expect most cases will be listed as BLM Admin State.>


# Security_Information ----

seci <- as.list(c("Security_Information" = NA))
seci[[1]] <- vector("list", 3)
names(seci[[1]]) <- c("Security_Classification_System",
                      "Security_Classification", 
                      "Security_Handling_Description")
seci[[1]]$Security_Classification_System <- NA
seci[[1]]$Security_Classification <- NA
seci[[1]]$Security_Handling_Description <- NA
                  
# Security_Information:
#   Security_Classification_System: BLM REQUIRED, If Applicable
#   Security_Classification: BLM REQUIRED, If Applicable
#   Security_Handling_Description: BLM REQUIRED, If Applicable

# Native_Data_Set_Environment ----

sysi <- Sys.info()

ndse <- as.list(c("Native_Data_Set_Environment" = paste0(
        sysi["sysname"]," ",
        sysi["release"]," ",
        sysi["version"], ". ",
        model.run.dat$r_version
)))
rm(sysi)
#Native_Data_Set_Environment: BLM RECOMMENDED – <A description of the processing environment in which the data set resides. For BLM this is should focus on the software used to in the processing such as the name and version of the software.> Example of what Arc 10 Metadata puts in the metadata by default:  “Microsoft Windows 7 Version 6.1 (Build 7601) Service Pack 1; ESRI ArcGIS 10.0.5.4400”.

# Cross_Reference ----

cref <- as.list(c("Cross_Reference" = NA))
cref[[1]] <- vector("list", 4)
names(cref[[1]]) <- c("Citation_Information", 
                      "Publication_Information",
                      "Other_Citation_Details",
                      "Online_Linkage")
cref[[1]]$Citation_Information <- vector("list", 3)
names(cref[[1]]$Citation_Information) <- c("Originator",
                                           "Publication_Date",
                                           "Title")
cref[[1]]$Citation_Information$Originator <- vector("list", 11)
cref[[1]]$Citation_Information$Originator[[1]] <- "Helen R Sofaer"
cref[[1]]$Citation_Information$Originator[[2]] <- "Catherine S Jarnevich"
cref[[1]]$Citation_Information$Originator[[3]] <- "Ian S Pearse"
cref[[1]]$Citation_Information$Originator[[4]] <- "Regan L Smyth"
cref[[1]]$Citation_Information$Originator[[5]] <- "Stephanie Auer"
cref[[1]]$Citation_Information$Originator[[6]] <- "Gericke L Cook"
cref[[1]]$Citation_Information$Originator[[7]] <- "Thomas C Edwards, Jr"
cref[[1]]$Citation_Information$Originator[[8]] <- "Gerald F Guala"
cref[[1]]$Citation_Information$Originator[[9]] <- "Timothy G Howard"
cref[[1]]$Citation_Information$Originator[[10]] <- "Jeffrey T Morisette"
cref[[1]]$Citation_Information$Originator[[11]] <- "Healy Hamilton"
cref[[1]]$Citation_Information$Publication_Date <- "2019"
cref[[1]]$Citation_Information$Title <- "The development and delivery of species distribution models to inform decision-making"
cref[[1]]$Publication_Information <- vector("list", 2)
names(cref[[1]]$Publication_Information) <- c("Publication_Place","Publisher")
cref[[1]]$Publication_Information$Publication_Place <- "Bioscience"
cref[[1]]$Publication_Information$Publisher <- "Oxford Academic Press"
cref[[1]]$Other_Citation_Details <- "Volume 69, page 544."
cref[[1]]$Online_Linkage <- "https://academic.oup.com/bioscience/article/69/7/544/5505326"

# Cross_Reference:
#   Citation_Information:
#     Originator: BLM RECOMMENDED 
#     Publication_Date: BLM RECOMMENDED
#     Title: BLM RECOMMENDED 
#   Publication_Information:
#     Publication_Place: BLM RECOMMENDED
#     Publisher: BLM RECOMMENDED
#   Other_Citation_Details: BLM RECOMMENDED
#   Online_Linkage: BLM RECOMMENDED


# join 'em up ----
m[[1]]$Identification_Information$Citation <- cit_i
m[[1]]$Identification_Information$Description <- des_i[[1]]
m[[1]]$Identification_Information$Status <- stat_i$Status
m[[1]]$Identification_Information$Time_Period_Information <- tpoc$Time_Period_Information
m[[1]]$Identification_Information$Spatial_Domain <- spdom$Spatial_Domain
m[[1]]$Identification_Information$Keywords <- kw[[1]]
m[[1]]$Identification_Information$Access_Constraints <- accc[[1]]
m[[1]]$Identification_Information$Use_Constraints <- usec[[1]]
m[[1]]$Identification_Information$Point_of_Contact <- poc[[1]]
m[[1]]$Identification_Information$Data_Set_Credit <- dsc[[1]]
m[[1]]$Identification_Information$Security_Information <- seci[[1]]
m[[1]]$Identification_Information$Native_Data_Set_Environment <- ndse[[1]]  
m[[1]]$Identification_Information$Cross_Reference <- cref[[1]]

# data quality information ----
# Data_Quality_Information:
#   Attribute_Accuracy:
#     Attribute_Accuracy_Report: BLM REQUIRED
#   Logical_Consistency_Report: BLM REQUIRED
#   Completeness_Report: BLM REQUIRED
#   Positional_Accuracy:
#     Horizontal_Positional_Accuracy:
#       Horizontal_Positional_Accuracy_Report: BLM REQUIRED 
#     Vertical_Positional_Accuracy:
#       Vertical_Positional_Accuracy_Report: BLM REQUIRED, If Applicable – <Applies to data sets containing a vertical component.>
#   Lineage:
#     Source_Information:
#       Source_Citation:
#         Citation_Information:
#           Originator: BLM RECOMMENDED
#           Publication_Date: BLM RECOMMENDED
#           Title: BLM RECOMMENDED
#           Publication_Information:
#             Publication_Place: BLM RECOMMENDED
#           Publisher: BLM RECOMMENDED
#           Other_Citation_Details: BLM RECOMMENDED
#           Online_Linkage: BLM RECOMMENDED
#           Source_Scale_Denominator: BLM RECOMMENDED
#           Type_of_Source_Media: BLM RECOMMENDED
#           Source_Citation_Abbreviation: BLM RECOMMENDED – <Uniquely identifying abbreviation or number that can be referenced in process step elements – Source Used Abbrev, and Source Produced Abbrev – to specify which of the sources was used or produced in a specific process step.>
#             Source_Contribution: BLM RECOMMENDED – <A brief statement identifying the information contributed by the source data.> 
#             Sample: "Data source provided digital lines for the road transportation network".
#           Source_Time_Period_of_Content:
#             Time_Period_Information:
#             Single_Date/Time:
#             Calendar_Date: BLM RECOMMENDED – <Can use single date or multiple dates or range of dates.>
#           Source_Currentness_Reference: BLM RECOMMENDED 
#     Process_Step:
#       Process_Description: BLM REQUIRED – < Information about a single event. Repeat as often as needed to represent significant processing steps. At a minimum, entries should address Initial Compilation, Data Conversions, Quality–Control Steps, Special Processes, and Updates.>
#       Source_Used_Citation_Abbreviation: BLM RECOMMENDED – < Value specified in Source Citation Abbreviation representing a dataset used as input to the specific process step.>
#       Source_Produced_Citation_Abbreviation: BLM RECOMMENDED – < Value specified in Source Citation Abbreviation representing the dataset produced by the specific process step.>  
#       Process_Date: BLM REQUIRED
#     Process_Contact:
#       Contact_Information:
#       Contact_Organization_Primary: [OR Contact_Person_Primary:]
#       Contact_Organization: BLM REQUIRED
#       Contact_Person: BLM RECOMMENDED
#       OR 
#       Contact_Person_Primary: [OR Contact_Organization_Primary:]
#       Contact_Organization: BLM REQUIRED
#       Contact_Person: BLM REQUIRED
#       
#       Contact_Position: BLM REQUIRED
#       Contact_Address:
#         Address_Type: BLM REQUIRED
#       Address: BLM REQUIRED
#       City: BLM REQUIRED
#       State_or_Province: BLM REQUIRED – <Use postal 2 character code (CA, VA, etc.)>
#         Postal_Code: BLM REQUIRED – <either the 5 or 10 alphanumeric-character code (99999-9999)>
#         Country: BLM REQUIRED
#       Contact_Voice_Telephone: BLM RECOMMENDED
#       Contact_TDD/TTY_Telephone: BLM RECOMMENDED
#       Contact_Facsimile_Telephone: BLM RECOMMENDED
#       Contact_Electronic_Mail_Address: BLM REQUIRED
#       Hours_of_Service: BLM RECOMMENDED
#       Contact_Instructions: BLM RECOMMENDED

# <dataqual>
#   <attracc>
#     <attraccr>BLM REQUIRED</attraccr>
#   </attracc>
#   <logic>BLM REQUIRED</logic>
#   <complete>BLM REQUIRED</complete>
#   <posacc>
#     <horizpa>
#       <horizpar>BLM REQUIRED</horizpar>
#     </horizpa>
#   </posacc>
#   <lineage>
#     <procstep>
#       <procdesc>BLM REQUIRED - &lt; Information about a single event. Repeat as often as needed to represent significant processing steps. At a minimum, entries should address Initial Compilation, Data Conversions, Quality-Control Steps, Special Processes, and Updates.&gt;</procdesc>
#       <procdate>BLM REQUIRED</procdate>
#       <proccont>
#         <cntinfo>
#         <cntorgp>
#           <cntorg>BLM REQUIRED</cntorg>
#         </cntorgp>
#         <cntpos>BLM REQUIRED</cntpos>
#         <cntaddr>
#           <addrtype>BLM REQUIRED</addrtype>
#           <address>BLM REQUIRED</address>
#           <city>BLM REQUIRED</city>
#           <state>BLM REQUIRED - &lt;Use postal 2 character code (CA, VA, etc.)&gt;</state>
#           <postal>BLM REQUIRED - &lt;either the 5 or 10 alphanumeric-character code (99999-9999)&gt;</postal>
#           <country>BLM REQUIRED</country>
#         </cntaddr>
#           <cntemail>BLM REQUIRED</cntemail>
#         </cntinfo>
#       </proccont>
#     </procstep>
#   </lineage>
# </dataqual>


# replaceInList <- function (x, FUN, ...) 
# {
#   if (is.list(x)) {
#     for (i in seq_along(x)) {
#       x[i] <- list(replaceInList(x[[i]], FUN, ...))
#     }
#     x
#   }
#   else FUN(x, ...)
# }
# n <- replaceInList(m, function(x)if(is.null(x))"none" else x)



##' Convert List to XML
##'
##' Can convert list or other object to an xml object using xmlNode
##' @title List to XML
##' @param item 
##' @param tag xml tag
##' @return xmlNode
##' @export
##' @author David LeBauer, Carl Davidson, Rob Kooper, julien colomb
##' see: https://stackoverflow.com/questions/6256064/how-to-create-xml-from-r-objects-e-g-is-there-a-listtoxml-function
listToXml <- function(item, tag) {
  # just a textnode, or empty node with attributes
  if(typeof(item) != 'list') {
    if (length(item) > 1) {
      xml <- xmlNode(tag)
      for (name in names(item)) {
        xmlAttrs(xml)[[name]] <- item[[name]]
      }
      return(xml)
    } else {
      return(xmlNode(tag, item))
    }
  }
  
  # create the node
  if (identical(names(item), c("text", ".attrs"))) {
    # special case a node with text and attributes
    xml <- xmlNode(tag, item[['text']])
  } else {
    # node with child nodes
    xml <- xmlNode(tag)
    for(i in 1:length(item)) {
      if (length (item[[i]]) == 0) {}
      else if (names(item)[i] != ".attrs") {
        print(i)
        if (is.null (names(item[[i]][1])) ){
          print(i)
          for (j in c(1:length (item[[i]]))){
            child <- xmlNode(names(item)[i])
            xmlValue(child) <- item[[i]][j]
            xml <- append.xmlNode(xml,child)
          }
        } else {
          xml <- append.xmlNode(xml, listToXml(item[[i]], names(item)[i]))
        }
        
      }
    }    
  }
  
  # add attributes to node
  attrs <- item[['.attrs']]
  for (name in names(attrs)) {
    xmlAttrs(xml)[[name]] <- attrs[[name]]
  }
  return(xml)
}



testout <- listToXml(m, "test")

saveXML(testout, file="test6.xml")




