# create raster metadata. Intro and load libraries ----

# This was built for a modeling project for US BLM that required FGDC metadata.
# To use it, the primary modifcation you need to make is the contact
# information starting near line 76 and the assignments
# of these contacts scattered throughout. Ideally this 
# would be updated to draw from a DB and assignments based
# on task, not name. 
# TODO: ^^
model_run_name
ensemble_algos
algo <- "xgb"

# to later wrap into a function, the things needed that vary:
# 1. model_run_name 
# 2. algo (what algorithm is this)
# 3. ensemble_stat (what statistic does this ensemble represent?)
# either 2 or 3 would have data, the other would be NA
# everythig else can get drawn from the database(s)?

library(here)
library(XML)

library(RSQLite)
library(DBI)

library(raster)
library(sf)

for(algo in ensemble_algos){
  
  # start by gathering up data from databases ----
  cutecode <- strsplit(model_run_name, split = "_")[[1]][[1]]
  model.date <- strsplit(model_run_name, split = "_")[[1]][[2]]
  
  db <- dbConnect(SQLite(),dbname=nm_db_file)
  sql <- paste0("SELECT * ",
                "FROM lkpAlgorithms ",
                "WHERE shortCode ='", algo , "';")
  algo.dat <- dbGetQuery(db, sql)
  
  sql <- paste0("SELECT * ",
                "FROM lkpSpecies ",
                "WHERE sp_code ='", cutecode , "';")
  spp.dat <- dbGetQuery(db, sql)
  
  sql <- paste0("SELECT * ",
                "FROM tblModelResults ",
                "WHERE model_run_name ='", model_run_name , "';")
  model.run.dat <- dbGetQuery(db, sql)
  algos <- strsplit(model.run.dat$algorithms, split = ", ")[[1]]
  
  sql <- paste0("SELECT * ",
                "FROM tblModelResultsValidationStats ",
                "WHERE model_run_name ='", model_run_name , "' ", 
                "AND algorithm = '", algo, "';")
  valStats.dat <- dbGetQuery(db, sql)
  
  sql <- paste0("SELECT * ",
                "FROM tblModelInputs ",
                "WHERE model_run_name ='", model_run_name , "' ", 
                "AND algorithm = '", algo, "';")
  inputStats.dat <- dbGetQuery(db, sql)
  
  sql <- paste0("SELECT ProgramName, State, DataProvidedDate, EGT_ID ", 
                "FROM lkpDataSources ",
                "INNER JOIN mapDataSourcesToSpp on mapDataSourcesToSpp.DataSourcesID = lkpDataSources.DataSourcesID ", 
                "WHERE mapDataSourcesToSpp.EGT_ID = ", spp.dat$EGT_ID[[1]], ";")
  presenceDatContributors.dat <- dbGetQuery(db, sql)
  
  sql <- paste0("SELECT * from tblModelResultsVarsUsed ",
                "WHERE model_run_name = '", model_run_name, "' ",
                "AND algorithm = '", algo, "' ", 
                "AND inFinalModel = 1;")
  varsUsed.dat <- dbGetQuery(db, sql)
  
  dbDisconnect(db)
  rm(db)
  
  # setup all contacts that will be used ----
  # perhaps the best solution is to grab all this from a 'people' table
  # in a database that is pre-populated and has assigned roles. 
  
  # build the skeleton
  contact_skeleton <- as.list(c("Contact_Information" = NA))
  contact_skeleton$Contact_Information <- vector("list",5)
  names(contact_skeleton$Contact_Information) <- c("Contact_Organization_Primary", 
                                                  "Contact_Position",
                                                  "Contact_Address",
                                                  "Contact_Voice_Telephone",
                                                  "Contact_Electronic_Mail_Address")
  contact_skeleton$Contact_Information$Contact_Organization_Primary <- vector("list",2)
  names(contact_skeleton$Contact_Information$Contact_Organization_Primary) <- c("Contact_Organization","Contact_Person")
  contact_skeleton$Contact_Information$Contact_Address <- vector("list",6)
  names(contact_skeleton$Contact_Information$Contact_Address) <- c("Address_Type","Address","City","State_or_Province","Postal_Code","Country")
  contact_skeleton$Contact_Information$Contact_Address$Address_Type <- "Mailing Address"
  
  # apply skeleton to four contacts that will be used
  all_contacts <- vector("list",4)
  names(all_contacts) <- c("NS_McIntyre","BLM_Redecker","BLM_Davidson","NYNHP_Howard")
  all_contacts$NS_McIntyre <- contact_skeleton
  all_contacts$BLM_Redecker <- contact_skeleton
  all_contacts$BLM_Davidson <- contact_skeleton
  all_contacts$NYNHP_Howard <- contact_skeleton
  
  # now populate each
  all_contacts$NS_McIntyre$Contact_Information$Contact_Organization_Primary$Contact_Organization <- "NatureServe"
  all_contacts$NS_McIntyre$Contact_Information$Contact_Organization_Primary$Contact_Person <- "Patrick McIntyre, PhD"
  all_contacts$NS_McIntyre$Contact_Information$Contact_Position <- "Senior Ecologist, NatureServe"
  all_contacts$NS_McIntyre$Contact_Information$Contact_Address$Address <- "1680 38th St. Suite 120"
  all_contacts$NS_McIntyre$Contact_Information$Contact_Address$City <- "Boulder"
  all_contacts$NS_McIntyre$Contact_Information$Contact_Address$State_or_Province <- "CO"
  all_contacts$NS_McIntyre$Contact_Information$Contact_Address$Postal_Code <- "80301"
  all_contacts$NS_McIntyre$Contact_Information$Contact_Address$Country <- "US"
  all_contacts$NS_McIntyre$Contact_Information$Contact_Voice_Telephone <- "+1-703-797-4812"
  all_contacts$NS_McIntyre$Contact_Information$Contact_Electronic_Mail_Address <- "Patrick_McIntyre@natureserve.org"
  
  all_contacts$BLM_Redecker$Contact_Information$Contact_Organization_Primary$Contact_Organization <- "Bureau of Land Management"
  all_contacts$BLM_Redecker$Contact_Information$Contact_Organization_Primary$Contact_Person <- "Nathan Redecker"
  all_contacts$BLM_Redecker$Contact_Information$Contact_Position <- "Botany and Monitoring Specialist"
  all_contacts$BLM_Redecker$Contact_Information$Contact_Address$Address <- "New Mexico State Office, 301 Dinosaur Trail"
  #all_contacts$BLM_Redecker$Contact_Information$Contact_Address$Address <- "301 Dinosaur Trail"
  all_contacts$BLM_Redecker$Contact_Information$Contact_Address$City <- "Santa Fe"
  all_contacts$BLM_Redecker$Contact_Information$Contact_Address$State_or_Province <- "NM"
  all_contacts$BLM_Redecker$Contact_Information$Contact_Address$Postal_Code <- "87508"
  all_contacts$BLM_Redecker$Contact_Information$Contact_Address$Country <- "US"
  all_contacts$BLM_Redecker$Contact_Information$Contact_Voice_Telephone <- "+1-505-954-2116"
  all_contacts$BLM_Redecker$Contact_Information$Contact_Electronic_Mail_Address <- "nredecker@blm.gov"
  
  all_contacts$BLM_Davidson$Contact_Information$Contact_Organization_Primary$Contact_Organization <- "Bureau of Land Management"
  all_contacts$BLM_Davidson$Contact_Information$Contact_Organization_Primary$Contact_Person <- "Zoe Davidson"
  all_contacts$BLM_Davidson$Contact_Information$Contact_Position <- "Acting Branch Chief of Resources"
  all_contacts$BLM_Davidson$Contact_Information$Contact_Address$Address <- "New Mexico State Office, 301 Dinosaur Trail"
  #all_contacts$BLM_Davidson$Contact_Information$Contact_Address$Address <- "301 Dinosaur Trail"
  all_contacts$BLM_Davidson$Contact_Information$Contact_Address$City <- "Santa Fe"
  all_contacts$BLM_Davidson$Contact_Information$Contact_Address$State_or_Province <- "NM"
  all_contacts$BLM_Davidson$Contact_Information$Contact_Address$Postal_Code <- "87508"
  all_contacts$BLM_Davidson$Contact_Information$Contact_Address$Country <- "US"
  all_contacts$BLM_Davidson$Contact_Information$Contact_Voice_Telephone <- "+1-505-954-2045"
  all_contacts$BLM_Davidson$Contact_Information$Contact_Electronic_Mail_Address <- "zdavidson@blm.gov"
  
  all_contacts$NYNHP_Howard$Contact_Information$Contact_Organization_Primary$Contact_Organization <- "New York Natural Heritage Program - SUNY ESF"
  all_contacts$NYNHP_Howard$Contact_Information$Contact_Organization_Primary$Contact_Person <- "Tim Howard"
  all_contacts$NYNHP_Howard$Contact_Information$Contact_Position <- "Director of Science"
  all_contacts$NYNHP_Howard$Contact_Information$Contact_Address$Address <- "625 Broadway, 5th Floor"
  #all_contacts$NYNHP_Howard$Contact_Information$Contact_Address$Address <- ""
  all_contacts$NYNHP_Howard$Contact_Information$Contact_Address$City <- "Albany"
  all_contacts$NYNHP_Howard$Contact_Information$Contact_Address$State_or_Province <- "NY"
  all_contacts$NYNHP_Howard$Contact_Information$Contact_Address$Postal_Code <- "12233-4757"
  all_contacts$NYNHP_Howard$Contact_Information$Contact_Address$Country <- "US"
  all_contacts$NYNHP_Howard$Contact_Information$Contact_Voice_Telephone <- "+1-518-402-8945"
  all_contacts$NYNHP_Howard$Contact_Information$Contact_Electronic_Mail_Address <- "tghoward@esf.edu"
  
  # setup base structure ----
  m <- vector("list", 7)
  names(m) <- c("Identification_Information", "Data_Quality_Information",
                    "Spatial_Data_Organization_Information", "Spatial_Reference_Information",
                    "Entity_and_Attribute_Information", "Distribution_Information",
                    "Metadata_Reference_Information")
  
  # Identification_Information ----
  m$Identification_Information <- vector("list",13)
  names(m$Identification_Information) <- c("Citation", 
                    "Description", 
                    "Time_Period_of_Content", 
                    "Status", 
                    "Spatial_Domain", 
                    "Keywords", 
                    "Access_Constraints", 
                    "Use_Constraints", 
                    "Point_of_Contact", 
                    "Data_Set_Credit", 
                    "Security_Information", 
                    "Native_Data_Set_Environment", 
                    "Cross_Reference")
  
  # ID_info: citation section ----
  
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
  
  
  # ID_info: description section ----
  
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
  
  # ID_info: status section ----
  
  stat_i <- as.list(c("Status" = NA))
  stat_i[[1]] <- vector("list", 2)
  names(stat_i[[1]]) <- c("Progress",
                          "Maintenance_and_Update_Frequency")
  stat_i[[1]]$Progress <- paste0("Complete")
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
  
  # ID_info: time period of content ----
  
  tpoc <- as.list(c("Time_Period_of_Content" = NA))
  tpoc[[1]] <- vector("list",2)
  names(tpoc[[1]]) <- c("Time_Period_Information",
                        "Currentness_Reference")
  
  tpoc[[1]]$Time_Period_Information <- vector("list",1)
  names(tpoc[[1]]$Time_Period_Information) <- c("Single_Date")
  tpoc[[1]]$Time_Period_Information$Single_Date <- as.list(c("Calendar_Date" =  model.date))
  tpoc[[1]]$Currentness_Reference <- "Period of data compilation and model development."
  
  # Time_Period_of_Content:
  #   Time_Period_Information:
  #     Single_Date/Time:
  #       Calendar_Date: BLM REQUIRED – <Can use single date or multiple dates or range of dates. Formatted as YYYYMMDD, YYYY-MM-DD, YYYY-MM, YYYY. >
  #   Currentness_Reference: BLM REQUIRED
  # <timeperd>
  #   <timeinfo>
  #     <sngdate>
  #       <caldate>BLM REQUIRED - &lt;Can use single date or multiple dates or range of dates. Formatted as YYYYMMDD, YYYY-MM-DD, YYYY-MM, YYYY. &gt;</caldate>
  #     </sngdate>
  #   </timeinfo>
  #   <current>BLM REQUIRED</current>
  # </timeperd>
  
  # ID_info: spatial domain ----
  
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
  
  spdom[[1]][[1]]$West_Bounding_Coordinate <- latlon_bb$xmin[[1]]
  spdom[[1]][[1]]$East_Bounding_Coordinate <- latlon_bb$xmax[[1]]
  spdom[[1]][[1]]$North_Bounding_Coordinate <- latlon_bb$ymax[[1]]
  spdom[[1]][[1]]$South_Bounding_Coordinate <- latlon_bb$ymin[[1]]
  
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
  
  # ID_info: keywords ----
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
    
  # ID_info: Access_Constraints ----
  
  # using #4 from here
  #  https://www.blm.gov/sites/blm.gov/files/uploads/IM2014-029_att1.pdf
  # based on feedback from BLM
  
  accc <- as.list(c("Access_Constraints" =  paste0(
    "NON-BLM DATA-FOR INTERNAL USE ONLY-NOT FOR DISTRIBUTION. These data are available ",
    "to appropriate Bureau of Land Management (BLM) staff, contractors,and partners. ",
    "All other requests for this data will be referred to the source agency. This data ",
    "might contain sensitive information and can be released by the source agency, ",
    "subject to FOIA limitations.")
  ))
  
  # this is #6
  # accc <- as.list(c("Access_Constraints" = paste0(
  #   "These are BLM data which might contain sensitive information, and may not be ",
  #   "releasable under the Privacy Act.  Access to these records is limited to ",
  #   "Authorized Personnel Only."
  # )))
    
  #Access_Constraints: BLM REQUIRED – <Use appropriate BLM standardized access constraints provided in WO IM 2014-029, see the directives attachment Standardized Disclaimer Statements for BLM for specifics.>
  ## tgh: see https://www.blm.gov/policy/im-2014-029
  
  # ID_info: Use constraints ----
  
  #this is #4, as above in access constraints
  usec <- as.list(c("Use_Constraints" = paste0(
    "NON-BLM DATA-USE IS SUBJECT TO DATA SHARING AGREEMENT. All use must conform to the ", 
    "restrictions placed on the User by the Data Sharing Agreement on file in Records Management. ",
    "Users will take all measures to ensure this data is protected from disclosure. ", 
    "Users assume the entire risk associated with use of this data.  The BLM has no liability for ", 
    "unintentional disclosure, or for any use or misuse of this data. Users bear all ", 
    "responsibility in determining the fitness of this data for the intended use. Users must ", 
    "consider the following: time sensitivity of the data; spatial differences in measure, ", 
    "projection, or accuracy; differences in terminology, description, or place names. External ", 
    "source metadata might not be complete, reliable, or observe BLM, Federal Geographic Data ", 
    "Committee or International Standards Organization standards. In the event of highly sensitive data ", 
    "the source organization might have modified the data for release. Users will assume ", 
    "that no attempts have been made to verify, correct, or complete the source metadata except ", 
    "where noted in a process step-the data and metadata are 'as is' from the source. The BLM ", 
    "attempts to ensure that the external data in corporate holdings is current. Users are responsible ", 
    "for verifying the timeliness of data. The BLM is not responsible for maintenance or update, and ", 
    "offers no warranty of data.  Users agree not to misrepresent the data, nor to imply that changes made ", 
    "were approved or endorsed by the BLM. Users will cite this data as source data in any products ", 
    "derived from this data. Users that modify data are obligated to describe in a process step the ", 
    "modifications made. The source organization might update this data without notification to users. ", 
    "Users will accept the data as is, with all faults."
    )))
     
  # this is #6
  # usec <- as.list(c("Use_Constraints" = paste0(
  #   "NON-PUBLIC. BLM INTERNAL USE ONLY. NOT FOR DISTRIBUTION. The information contained ",
  #   "in these data is dynamic and maychange over time. The data are not better than the ",  
  #   "sources from which they were derived, and both scale and accuracy might vary ", 
  #   "across the dataset. These data might not have the accuracy, resolution, completeness, timeliness, ",
  #   "or other characteristics appropriate for applications that potential users of the data ",
  #   "may contemplate. The User is encouraged to carefully consider the content of the metadata ",
  #   "file associated with these data.  These data are neither legal documents nor land surveys, ",
  #   "and must not be used as such. Official records may be referenced at most BLM offices. Please ",
  #   "report any errors in the data to the BLM office from which it was obtained.  The BLM should be ",
  #   "cited as the data source in any products derived from these data. Any Users wishing to modify ", 
  #   "the data are obligated to describe within the process history section of the metadata the types ",
  #   "of modifications they have performed. The User specifically agrees not to misrepresent the data, ",
  #   "nor to imply that changes made were approved or endorsed by BLM. This data may be updated by the ",
  #   "BLM without notification.  By using these data you hereby agree to these conditions."
  # )))
  
  #Use_Constraints: BLM REQUIRED – <Use appropriate BLM standardized use constraints provided in WO IM 2014-029, see the directives attachment Standardized Disclaimer Statements for BLM for specifics.>
  #<Please use BLM standardized Use Constraints provided in WO IM 2014-029.>
  ## tgh: see https://www.blm.gov/policy/im-2014-029 and link therein to the pdf 
  
  # ID_info: Point_of_Contact ----
  
  poc <- as.list(c("Point_of_Contact" = NA))
  poc[[1]] <- as.list(c("Contact_Information" = NA))
  poc[[1]] <- all_contacts$NS_McIntyre$Contact_Information
  
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
  
  
  
  # ID_info: Data_Set_Credit ----
  
  dsc <- as.list(c("Data_Set_Credit" = paste0(
    "This data set was developed by NatureServe and the Natural Heritage Network"
  )))
  
  # Data_Set_Credit: BLM RECOMMENDED – <Expect most cases will be listed as BLM Admin State.>
  
  
  # ID_info: Security_Information ----
  
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
  
  # ID_info: Native_Data_Set_Environment ----
  
  sysi <- Sys.info()
  
  ndse <- as.list(c("Native_Data_Set_Environment" = paste0(
          sysi["sysname"]," ",
          sysi["release"]," ",
          sysi["version"], ". ",
          model.run.dat$r_version
  )))
  rm(sysi)
  #Native_Data_Set_Environment: BLM RECOMMENDED – <A description of the processing environment in which the data set resides. For BLM this is should focus on the software used to in the processing such as the name and version of the software.> Example of what Arc 10 Metadata puts in the metadata by default:  “Microsoft Windows 7 Version 6.1 (Build 7601) Service Pack 1; ESRI ArcGIS 10.0.5.4400”.
  
  # ID_info: Cross_Reference ----
  
  cref <- as.list(c("Cross_Reference" = NA))
  cref[[1]] <- vector("list", 1)
  names(cref[[1]]) <- c("Citation_Information")
  cref[[1]]$Citation_Information <- vector("list", 6)
  names(cref[[1]]$Citation_Information) <- c("Originator",
                                             "Publication_Date",
                                             "Title",
                                             "Publication_Information",
                                             "Other_Citation_Details",
                                             "Online_Linkage")
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
  
  cref[[1]]$Citation_Information$Publication_Information <- vector("list", 2)
  names(cref[[1]]$Citation_Information$Publication_Information) <- c("Publication_Place","Publisher")
  cref[[1]]$Citation_Information$Publication_Information$Publication_Place <- "Bioscience"
  cref[[1]]$Citation_Information$Publication_Information$Publisher <- "Oxford Academic Press"
  cref[[1]]$Citation_Information$Other_Citation_Details <- "Volume 69, page 544."
  cref[[1]]$Citation_Information$Online_Linkage <- "https://academic.oup.com/bioscience/article/69/7/544/5505326"
  
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
  
  
  # Data Quality Information ----
  
  m$Data_Quality_Information <- vector("list",5)
  names(m$Data_Quality_Information) <- c("Attribute_Accuracy", 
                                           "Logical_Consistency_Report", 
                                           "Completeness_Report", 
                                           "Positional_Accuracy", 
                                           "Lineage")
  
  # DQ_info: attribute accuracy ----
  
  atta <- as.list(c("Attribute_Accuracy" = NA))
  atta[[1]] <- as.list(c("Attribute_Accuracy_Report" = paste0(
    "The only attribute associated with this data set is the raster VALUE, ",
    "which represents the probability that the attributed raster cell is similar ",
    "to conditions where this species is known to occur. External validation ", 
    "reports accuracy for this attribute with the True Skill Statistic as ", 
    round(valStats.dat[valStats.dat$metric == "TSS", "metric_mn"], 3), 
    " and the Area Under the ROC Curve (AUC) as ",
    round(valStats.dat[valStats.dat$metric == "AUC", "metric_mn"], 3), 
    "."
  )))
  
  #   Attribute_Accuracy:
  #     Attribute_Accuracy_Report: BLM REQUIRED
  
  #   <attracc>
  #     <attraccr>BLM REQUIRED</attraccr>
  #   </attracc>
  
  # DQ_info: logical consistency ----
  locorep <- as.list(c("Logical_Consistency_Report" = paste0(
    "This data set represents the output of a modeled relationship based on many other input ",
    "rasters. Before processing, all input rasters were snapped to the same projection, ",
    "cell size, and extent, ensuring consistent output. Inputs and outputs use the Albers ", 
    "Equal Area projection to maintain consistency in area representation throughout. This is an ",
    "important attribute for analysis and visualization. Raster values are assigned NULL outside ", 
    "a predefined modeling area to limit erroneous prediction extrapolation."
  )))
  
  #   Logical_Consistency_Report: BLM REQUIRED
  
  #   <logic>BLM REQUIRED</logic>
  
  
  # DQ_info: completeness ----
  corep <- as.list(c("Completeness_Report" = paste0(
    "All raster cells within the modeling area are assigned a VALUE and thus ",
    "the raster represents a complete prediction surface. Care was taken to ",
    "ensure all input rasters were free of NULL cells within the modeling area ",
    "which would have resulted in equivalent NULL cells in this output."
    )))
  
  #   Completeness_Report: BLM REQUIRED
  
  #   <complete>BLM REQUIRED</complete>
  
  # DQ_info: positional accuracy ----
  
  predictionFileName <- paste0(model.run.dat$model_run_name, "_", algo, ".tif")
  fullPath <- here("_data","species", spp.dat$sp_code, "outputs","model_predictions",predictionFileName)
  ras <- raster(fullPath)
  xreso <- xres(ras)
  yreso <- yres(ras)
  
  poac <- as.list(c("Positional_Accuracy" = NA))
  poac[[1]] <- vector("list",2)
  names(poac[[1]]) <- c("Horizontal_Positional_Accuracy","Vertical_Positional_Accuracy")
  
  poac[[1]]$Horizontal_Positional_Accuracy <- as.list(c("Horizontal_Positional_Accuracy_Report" = paste0(
    "Horizontal accuracy is dependent on the accuracy of the inputs used to generate this model ", 
    "and these inputs vary considerably. As such, all boundaries between habitat suitability levels ", 
    "should be considered as approximate on the ground and should be validated with field visitation. ",
    "Raster cell size is ", round(xreso), " meters by ", round(yreso), " meters, indicating positional accuracy ",
    "should be considered to be broader than this cell size." 
    )))
  
  poac[[1]]$Vertical_Positional_Accuracy <- as.list(c("Vertical_Positional_Accuracy_Report" = paste0(
    "There is no explicit vertical component in this output."
    )))
  
  #   Positional_Accuracy:
  #     Horizontal_Positional_Accuracy:
  #       Horizontal_Positional_Accuracy_Report: BLM REQUIRED 
  #     Vertical_Positional_Accuracy:
  #       Vertical_Positional_Accuracy_Report: BLM REQUIRED, If Applicable – <Applies to data sets containing a vertical component.>
  
  #   <posacc>
  #     <horizpa>
  #       <horizpar>BLM REQUIRED</horizpar>
  #     </horizpa>
  #   </posacc>
  
  # DQ_info: lineage ----
  
  lin <- as.list(c("Lineage" = NA))
  lin[[1]] <- vector("list",3) # number of steps, plus one for source_info if needed
  names(lin[[1]]) <- rep("Process_Step", 3)
  
  lin[[1]][[1]] <- vector("list",3)
  names(lin[[1]][[1]]) <- c("Process_Description", "Process_Date", "Process_Contact")
  lin[[1]][[1]]$Process_Description <- paste0("Acquire and clean up input points. ", 
        "We acquired presence locations from these sources: (",
        paste(presenceDatContributors.dat$ProgramName, collapse = ", "), ". ",
        "Grouping (", inputStats.dat$feat_grp_count, " groups) and random selection within ",
        "these groups resulted in ", inputStats.dat$tot_obs_subsamp, " presence observations ", 
        "used as inputs to the modeling process.")
  
  lin[[1]][[1]]$Process_Date <- format(as.Date(inputStats.dat$datetime), "%Y%m%d")
  lin[[1]][[1]]$Process_Contact <- as.list(c("Contact_Information" = NA))
  lin[[1]][[1]]$Process_Contact$Contact_Information <- all_contacts$BLM_Davidson$Contact_Information
  
  lin[[1]][[2]] <- vector("list",3)
  names(lin[[1]][[2]]) <- c("Process_Description", "Process_Date", "Process_Contact")
  lin[[1]][[2]]$Process_Description <- paste0("Model relationship between presence ",
          "observations and the background landscape. We used ", algo.dat$fullName, " ",
          "in the R package ", algo.dat$rPackage, " using ", model.run.dat$r_version, ". ", 
          "A total number of ", nrow(varsUsed.dat), " environmental variables were used ",
          "in this model.")
  lin[[1]][[2]]$Process_Date <- format(as.Date(model.run.dat$model_start_time), "%Y%m%d")
  lin[[1]][[2]]$Process_Contact <- as.list(c("Contact_Information" = NA))
  lin[[1]][[2]]$Process_Contact$Contact_Information <- all_contacts$NYNHP_Howard$Contact_Information
  
  lin[[1]][[3]] <- vector("list",3)
  names(lin[[1]][[3]]) <- c("Process_Description", "Process_Date", "Process_Contact")
  lin[[1]][[3]]$Process_Description <- paste0("Use model to predict probability of ",
          "suitable habitat throughout study area. The final output ranges from ", 
          round(minValue(ras), 1), " to ", round(maxValue(ras), 1), ". ")
  lin[[1]][[3]]$Process_Date <- format(as.Date(model.run.dat$model_start_time), "%Y%m%d")
  lin[[1]][[3]]$Process_Contact <- as.list(c("Contact_Information" = NA))
  lin[[1]][[3]]$Process_Contact$Contact_Information <- all_contacts$NYNHP_Howard$Contact_Information
  
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
  #       Process_Contact:
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
    #         <cntemail>BLM REQUIRED</cntemail>
  #         </cntinfo>
  #       </proccont>
  #     </procstep>
  #   </lineage>
  # </dataqual>
  
  # Spatial_Data_Organization_Information ----
  
  sdoi <-  as.list(c("Spatial_Data_Organization_Information" = NA))
  sdoi[[1]] <- vector("list",2)
  names(sdoi[[1]]) <- c("Direct_Spatial_Reference_Method", "Raster_Object_Information")
  sdoi[[1]]$Direct_Spatial_Reference_Method <- "Raster"
  sdoi[[1]]$Raster_Object_Information <- vector("list",4)
  names(sdoi[[1]]$Raster_Object_Information) <- c("Raster_Object_Type", "Row_Count", "Column_Count", "Vertical_Count")
  sdoi[[1]]$Raster_Object_Information$Raster_Object_Type <- "Grid Cell"
  sdoi[[1]]$Raster_Object_Information$Row_Count <- dim(ras)[[1]]
  sdoi[[1]]$Raster_Object_Information$Column_Count <- dim(ras)[[2]]
  sdoi[[1]]$Raster_Object_Information$Vertical_Count <- dim(ras)[[3]]
  
  # Spatial_Data_Organization_Information:
  #   Direct_Spatial_Reference_Method: BLM RECOMMENDED [“Point”, “Vector”, “Raster”] –  <The metadata needs to be first exported out from the dataset itself in order to automatically populate this information – with the output XML then being able to be further edited from there.>
  #   Indirect_Spatial_Reference: BLM RECOMMENDED – <name of types of geographic features, addressing schemes, or other means through which locations are referenced in the data set.>
    
  
  # Spatial_Reference_Information ----
  ## hardcode it all, especially since R is in the middle of a revamp
  spref <- as.list(c("Spatial_Reference_Information" = NA))
  spref[[1]] <- as.list(c("Horizontal_Coordinate_System_Definition" = NA))
  spref[[1]][[1]] <- vector("list",2)
  names(spref[[1]][[1]]) <- c("Planar", "Geodetic")
  spref[[1]][[1]]$Planar <- vector("list",2)
  names(spref[[1]][[1]]$Planar) <- c("Map_Projection", "Planar_Coordinate_Information")
  spref[[1]][[1]]$Planar$Map_Projection <- vector("list",2)
  names(spref[[1]][[1]]$Planar$Map_Projection) <- c("Map_Projection_Name", "Albers_Conical_Equal_Area")
  spref[[1]][[1]]$Planar$Map_Projection$Map_Projection_Name = "Albers_Conical_Equal_Area"
  spref[[1]][[1]]$Planar$Map_Projection$Albers_Conical_Equal_Area <- vector("list",6)
  names(spref[[1]][[1]]$Planar$Map_Projection$Albers_Conical_Equal_Area) <- c("Standard_Parallel",
                                                         "Standard_Parallel",
                                                         "Longitude_of_Central_Meridian ",
                                                         "Latitude_of_Projection_Origin ",
                                                         "False_Easting ",
                                                         "False_Northing")
  spref[[1]][[1]]$Planar$Map_Projection$Albers_Conical_Equal_Area[[1]] <- 29.5
  spref[[1]][[1]]$Planar$Map_Projection$Albers_Conical_Equal_Area[[2]] <- 45.5
  spref[[1]][[1]]$Planar$Map_Projection$Albers_Conical_Equal_Area[[3]] <- -96
  spref[[1]][[1]]$Planar$Map_Projection$Albers_Conical_Equal_Area[[4]] <- 23
  spref[[1]][[1]]$Planar$Map_Projection$Albers_Conical_Equal_Area[[5]] <- 0.0
  spref[[1]][[1]]$Planar$Map_Projection$Albers_Conical_Equal_Area[[6]] <- 0.0
  
  spref[[1]][[1]]$Planar$Planar_Coordinate_Information <- vector("list",3)
  names(spref[[1]][[1]]$Planar$Planar_Coordinate_Information) <- c("Planar_Coordinate_Encoding_Method",
                                                                   "Coordinate_Representation",
                                                                   "Planar_Distance_Units")
  spref[[1]][[1]]$Planar$Planar_Coordinate_Information$Planar_Coordinate_Encoding_Method <- "coordinate pair"
  spref[[1]][[1]]$Planar$Planar_Coordinate_Information$Coordinate_Representation <- vector("list",2)
  names(spref[[1]][[1]]$Planar$Planar_Coordinate_Information$Coordinate_Representation) <- c(
    "Abscissa_Resolution", 
    "Ordinate_Resolution"
  )
  spref[[1]][[1]]$Planar$Planar_Coordinate_Information$Coordinate_Representation$Abscissa_Resolution <- xreso
  spref[[1]][[1]]$Planar$Planar_Coordinate_Information$Coordinate_Representation$Ordinate_Resolution <- yreso
  spref[[1]][[1]]$Planar$Planar_Coordinate_Information$Planar_Distance_Units <- "Meter"
  
    
  spref[[1]][[1]]$Geodetic <- vector("list",4)
  names(spref[[1]][[1]]$Geodetic) <- c("Horizontal_Datum_Name","Ellipsoid_Name",
                                       "Semi-major_Axis", "Denominator_of_Flattening_Ratio")
  spref[[1]][[1]]$Geodetic$Horizontal_Datum_Name <- "North American Datum of 1983"
  spref[[1]][[1]]$Geodetic$Ellipsoid_Name <- "Geodetic Reference System 1980"
  spref[[1]][[1]]$Geodetic$`Semi-major_Axis` <- 6378137.0
  spref[[1]][[1]]$Geodetic$Denominator_of_Flattening_Ratio <- 298.257222101
  
  # Spatial_Reference_Information:
  #   Horizontal_Coordinate_System_Definition:
  #   Geographic: [OR Planar:]
  #     Latitude_Resolution: BLM RECOMMENDED
  #     Longitude_Resolution: BLM RECOMMENDED
  #     Geographic_Coordinate_Units: BLM REQUIRED, If Applicable - <Specify format of geographic coordinates, including “Decimal Degrees”, “Degrees, minutes, decimal seconds”, or other valid unit name.>
  #   OR
  #  Planar: [OR Geographic:]
  #   Map_Projection: [OR Grid_Coordinate_System:]
  #   Map_Projection_Name: BLM REQUIRED, If Applicable - <Specify full name of map projection, including “Albers Conical Equal Area”, “Alaska Albers Conical Equal Area”, “Lambert Conformal Conic”, “Transverse Mercator”, or other valid map projection name.>
  #   OR
  #  Grid_Coordinate_System: [OR Map_Projection:]
  #   Grid_Coordinate_System_Name: BLM REQUIRED, If Applicable - <Specify "Universal Transverse Mercator", "Universal Polar Stereographic", "State Plane Coordinate System 1927", "State Plane Coordinate System 1983", "ARC Coordinate System", or "other grid system". >
  #     UTM: [OR State_Plane_Coordinate_System:]
  # UTM_Zone_Number: BLM REQUIRED, If Applicable - <Specify numeric zone number, use positive numbers for northern     hemisphere, and negative numbers for southern hemisphere. Do not use ‘N’ or ‘S’ notation. > 
  #   OR
  # State_Plane_Coordinate_System: [OR UTM:]
  # SPCS_Zone_Identifier: BLM REQUIRED, If Applicable - <Specify State Plane Zone identifier.>
  #   
  #   Planar_Coordinate_Information:
  #   Planar_Coordinate_Encoding_Method: BLM RECOMMENDED– <Use either “coordinate pair”, “row and column”, or “distance and bearing”.>
  #   Coordinate_Representation:
  #   Abscissa_Resolution: BLM RECOMMENDED
  # Ordinate_Resolution: BLM RECOMMENDED
  # Planar_Distance_Units: BLM RECOMMENDED
  # Geodetic_Model:
  #   Horizontal_Datum_Name: BLM REQUIRED, If Applicable - <Specify full name of  horizontal datum from “North American Datum of 1983”, “North American Datum of 1927”, or other valid horizontal datum name.>
  #   
  #   Ellipsoid_Name: BLM RECOMMENDED
  # Semi-major_Axis: BLM RECOMMENDED
  # Denominator_of_Flattening_Ratio: BLM RECOMMENDED 
  # Vertical_Coordinate_System_Definition:
  #   Altitude_System_Definition:
  #   Altitude_Datum_Name: BLM RECOMMENDED
  # Altitude_Resolution: BLM RECOMMENDED 
  # Altitude_Distance_Units: BLM RECOMMENDED 
  # Altitude_Encoding_Method: BLM RECOMMENDED 
  # Depth_System_Definition:
  #   Depth_Datum_Name: BLM RECOMMENDED
  # Depth_Resolution: BLM RECOMMENDED
  # Depth_Distance_Units: BLM RECOMMENDED
  # Depth_Encoding_Method: BLM RECOMMENDED
  
  # <spref>
  #   <horizsys>
  #   <geograph>
  #     <geogunit>BLM REQUIRED, If Applicable - &lt;Specify format of geographic coordinates, including "Decimal Degrees", "Degrees, minutes, decimal seconds", or other valid unit name.&gt;</geogunit>
  #   </geograph>
  #     <geodetic>
  #       <horizdn>BLM REQUIRED, If Applicable - &lt;Specify full name of  horizontal datum from "North American Datum of 1983", "North American Datum of 1927", or other valid horizontal datum name.&gt;</horizdn>
  #     </geodetic>
  #   </horizsys>
  # </spref>
    
    
  # Entity_and_Attribute_Information ----
  
  eai <- as.list(c("Entity_and_Attribute_Information" = NA))
  eai[[1]] <- vector("list",2)
  names(eai[[1]]) <- c("Detailed_Description","Overview_Description")
  eai[[1]]$Detailed_Description <- vector("list",2)
  names(eai[[1]]$Detailed_Description) <- c("Entity_Type","Attribute")
  eai[[1]]$Detailed_Description$Entity_Type <- vector("list",3)
  names(eai[[1]]$Detailed_Description$Entity_Type) <- c("Entity_Type_Label", "Entity_Type_Definition", "Entity_Type_Definition_Source")
  eai[[1]]$Detailed_Description$Entity_Type$Entity_Type_Label <- "Raster cells"
  eai[[1]]$Detailed_Description$Entity_Type$Entity_Type_Definition <- "Raster cells sized to match all input predictor data."
  eai[[1]]$Detailed_Description$Entity_Type$Entity_Type_Definition_Source <- "None"
  
  eai[[1]]$Detailed_Description$Attribute <- vector("list",4)
  names(eai[[1]]$Detailed_Description$Attribute) <- c("Attribute_Label", "Attribute_Definition",
                                                      "Attribute_Definition_Source", "Attribute_Domain_Values")
  eai[[1]]$Detailed_Description$Attribute$Attribute_Label <- "VALUE"
  eai[[1]]$Detailed_Description$Attribute$Attribute_Definition <- 
    paste0("Modeled prediction of suitability of raster cell for ", spp.dat$common_name, 
           " (", spp.dat$scientific_name, ") where values closer to 1 represent higher ",
           "suitability.")
  eai[[1]]$Detailed_Description$Attribute$Attribute_Definition_Source <- "Suitability modeling methodology."
  eai[[1]]$Detailed_Description$Attribute$Attribute_Domain_Values <- 
    as.list(c("Range_Domain" = NA))
  eai[[1]]$Detailed_Description$Attribute$Attribute_Domain_Values$Range_Domain <- vector("list",2)
  names(eai[[1]]$Detailed_Description$Attribute$Attribute_Domain_Values$Range_Domain) <-
    c("Range_Domain_Minimum", "Range_Domain_Maximum")
  eai[[1]]$Detailed_Description$Attribute$Attribute_Domain_Values$Range_Domain$Range_Domain_Minimum <- 
    round(minValue(ras), 2)
  eai[[1]]$Detailed_Description$Attribute$Attribute_Domain_Values$Range_Domain$Range_Domain_Maximum <- 
    round(maxValue(ras), 2)  
  
  eai[[1]]$Overview_Description <- vector("list",2)
  names(eai[[1]]$Overview_Description) <- c("Entity_and_Attribute_Overview", "Entity_and_Attribute_Detail_Citation")
  eai[[1]]$Overview_Description$Entity_and_Attribute_Overview <- paste0(
    "This is a raster dataset with a single attribute, the value of each raster cell. ",
    "This value represents habitat suitability for the species being represented, ",
    "based on modeling the relationship between known locations for this species ", 
    "and environmental conditions throughout the modeling area. Values generally ",
    "range from 0-1, where higher values represent higher suitability."
    )
  eai[[1]]$Overview_Description$Entity_and_Attribute_Detail_Citation <- "None"
  
  # Entity_and_Attribute_Information:
  #   Detailed_Description:
  #     Entity_Type:
  #       Entity_Type_Label: BLM REQUIRED – <The name of the entity or is the spatial object(s) being portrayed in this data set.> 
  #         Sample: "Grazing Allotments Polygons" and/or “Arcs”.
  #       Entity_Type_Definition: BLM REQUIRED – <The description of the entity. This is a detailed description of the entity including any information on how to display the entity.> 
  #         Sample: "Grazing allotments as described and mapped in the Allotment Management Plan".
  #       Entity_Type_Definition_Source: BLM RECOMMENDED – <The authority or source of the entity definition.> 
  #         Sample: a reference to BLM Manuals/Handbooks/Data Standards documentation or to standard professional references.
  #     Attribute:
  #       Attribute_Label: BLM REQUIRED, If Applicable – <The name of the attribute of the spatial object(s) being portrayed in this data set. May not apply to certain raster datasets that lack an attribute table.> 
  #         Sample: "Grazing Allotments Polygons Name" or “Grazing Pasture Number”.
  #       Attribute_Definition: BLM REQUIRED, If Applicable – <The description of the attribute including any information on how to display the attribute. May not apply to certain raster datasets that lack an attribute table.>
  #           Sample: "Grazing allotments as described and mapped in the Allotment Management Plan".
  #         Attribute_Definition_Source: BLM RECOMMENDED – <The authority or source of the attribute definition.> 
  #           Example: a reference to BLM Manuals/Handbooks / Data Standards documentation or to standard professional references.
  #       Attribute_Domain_Values:
  #         Codeset_Domain: 
  #         Codeset_Name: BLM REQUIRED, If Applicable 
  #         Codeset_Source: BLM REQUIRED, If Applicable 	
  #         OR
  #         Enumerated_Domain:
  #           Enumerated_Domain_Value: BLM REQUIRED, If Applicable
  #           Enumerated_Domain_Value_Definition: BLM REQUIRED, If Applicable
  #           Enumerated_Domain_Value_Definition_Source: BLM RECOMMENDED
  #         OR
  #         Range_Domain:
  #           Range_Domain_Minimum: BLM REQUIRED, If Applicable
  #           Range_Domain_Maximum: BLM REQUIRED, If Applicable
  #           Attribute_Units_of_Measure: BLM RECOMMENDED
  #           Attribute_Measurement_Resolution: BLM RECOMMENDED
  #         OR
  #         Unrepresentable_Domain: BLM REQUIRED, If Applicable
  # 
  # Overview_Description:
  #   Entity_and_Attribute_Overview: BLM REQUIRED – <Detailed summary of the information contained in a data set. This is another instance where a specific office may choose to extend the standard by including reference to documented data standards.>
  #   Entity_and_Attribute_Detail_Citation: BLM RECOMMENDED – <Reference to the complete description of the entity types, attributes, and attribute values for the data set.>
  #   
  #   
  
  # Distribution Information ----
  
  di <- as.list(c("Distribution_Information" = NA))
  di[[1]] <- vector("list",3)
  names(di[[1]]) <- c("Distributor","Resource_Description",
                      "Distribution_Liability")
  di[[1]]$Distributor <- as.list(c("Contact_Information" = NA))
  di[[1]]$Distributor$Contact_Information <- all_contacts$NS_McIntyre$Contact_Information
  di[[1]]$Resource_Description <- "For internal use only. Contains sensitive information."
  di[[1]]$Distribution_Liability <- "For internal use only. Contains sensitive information."
  
  # Distribution_Information:
  #   Distributor:
  #     Contact_Information:
  #     Contact_Organization_Primary: [OR Contact_Person_Primary:]
  #     Contact_Organization: BLM REQUIRED
  #     Contact_Person: BLM RECOMMENDED
  #     OR 
  #     Contact_Person_Primary: [OR Contact_Organization_Primary:]
  #     Contact_Organization: BLM REQUIRED
  #     Contact_Person: BLM REQUIRED
  #     
  #     Contact_Position: BLM REQUIRED
  #     Contact_Address:
  #       Address_Type: BLM REQUIRED
  #     Address: BLM REQUIRED
  #     City: BLM REQUIRED
  #     State_or_Province: BLM REQUIRED – <Use postal 2 character code (CA, VA, etc.)>
  #       Postal_Code: BLM REQUIRED – <either the 5 or 10 alphanumeric-character code (99999-9999)>
  #       Country: BLM REQUIRED
  #     Contact_Voice_Telephone: BLM RECOMMENDED
  #     Contact_TDD/TTY_Telephone: BLM RECOMMENDED
  #     Contact_Facsimile_Telephone: BLM RECOMMENDED
  #     Contact_Electronic_Mail_Address: BLM REQUIRED
  #     Hours_of_Service: BLM RECOMMENDED
  #     Contact_Instructions: BLM RECOMMENDED
  #   Resource_Description: BLM REQUIRED, If Applicable <Examples (required for Geoportal systems): “Downloadable data” or, “Live     Data and Maps”.>
  #   Distribution_Liability: BLM REQUIRED – <Use BLM standardized Distributor Liability Statements provided in WO IM 2014-029>.
  #   Standard_Order_Process:
  #     Digital_Form:
  #     Digital_Transfer_Information:
  #     Format_Name: BLM REQUIRED, If Applicable
  #     Format_Specification: BLM RECOMMENDED 
  #     File_Decompression_Technique: BLM RECOMMENDED 
  #     Format_Version_Number OR Format Version Date: BLM RECOMMENDED
  #     Transfer_Size: BLM RECOMMENDED 
  #     Digital_Transfer_Option:
  #       Online_Option:
  #       Computer_Contact_Information:
  #       Network_Address:
  #       Network_Resource_Name: BLM REQUIRED, If Applicable –<Only for data published to the public – may be required by some publishing systems, and managed by other publishing systems. Direct URL to dataset. Example: https://www.blm.gov/... >
  #       Access_Instructions: BLM RECOMMENDED
  #     Offline_Option:
  #       Offline_Media: BLM RECOMMENDED
  #     Recording_Capacity:
  #       Recording_Density: BLM RECOMMENDED
  #     Recording_Density_Units: BLM RECOMMENDED
  #     Recording_Format: BLM RECOMMENDED
  #     Compatibility_Information: BLM RECOMMENDED
  #     Fees: BLM RECOMMENDED
  #     Ordering_Instructions: BLM RECOMMENDED
  #     Turnaround: BLM RECOMMENDED
  #     Technical_Prerequisites: BLM RECOMMENDED
  #     Available_Time_Period:
  #       Time_Period_Information:
  #       Single_Date/Time:
  #       Calendar_Date: BLM Recommended – <Populate with date dataset is made available – can be same as publication date; Can alternatively use single date or multiple dates or range of dates if that better describes data availability.>
  # 
  # <distinfo>
  #   <distrib>
  #     <cntinfo>
  #       <cntorgp>
  #         <cntorg>BLM REQUIRED</cntorg>
  #       </cntorgp>
  #       <cntpos>BLM REQUIRED</cntpos>
  #       <cntaddr>
  #         <addrtype>BLM REQUIRED</addrtype>
  #         <address>BLM REQUIRED</address>
  #         <city>BLM REQUIRED</city>
  #         <state>BLM REQUIRED - &lt;Use postal 2 character code (CA, VA, etc.)&gt;</state>
  #         <postal>BLM REQUIRED - &lt;either the 5 or 10 alphanumeric-character code (99999-9999)&gt;</postal>
  #         <country>BLM REQUIRED</country>
  #       </cntaddr>
  #       <cntemail>BLM REQUIRED</cntemail>
  #     </cntinfo>
  #   </distrib>
  #   <resdesc>BLM REQUIRED, If Applicable &lt;Examples (required for Geoportal systems): "Downloadable data" or, "Live     Data and Maps".&gt;</resdesc>
  #   <distliab>BLM REQUIRED - &lt;Use BLM standardized Distributor Liability Statements provided in WO IM 2014-029&gt;.</distliab>
  #   <stdorder>
  #     <digform>
  #       <digtopt>
  #         <onlinopt>
  #           <computer>
  #             <networka>
  #               <networkr>BLM REQUIRED, If Applicable - &lt;Only for data published to the public - may be required by some publishing systems, and managed by other publishing systems. Direct URL to dataset. Example: https://www.blm.gov/... &gt;</networkr>
  #             </networka>
  #           </computer>
  #         </onlinopt>
  #       </digtopt>
  #     </digform>
  #   </stdorder>
  # </distinfo>
  
  
  
  
  # Metadata_Reference_Information ----
  mri <- as.list(c("Metadata_Reference_Information" = NA))
  mri[[1]] <- vector("list",9)
  names(mri[[1]]) <- c("Metadata_Date", "Metadata_Review_Date",
                       "Metadata_Contact", "Metadata_Standard_Name", 
                       "Metadata_Standard_Version", "Metadata_Time_Convention", 
                       "Metadata_Access_Constraints", "Metadata_Use_Constraints", 
                       "Metadata_Security_Information")
  mri[[1]]$Metadata_Date <- format(Sys.Date(), "%Y%m%d")
  mri[[1]]$Metadata_Review_Date <- format(Sys.Date(), "%Y%m%d")
  mri[[1]]$Metadata_Contact <- as.list(c("Contact_Information" = NA))
  mri[[1]]$Metadata_Contact$Contact_Information <- all_contacts$NYNHP_Howard$Contact_Information
  mri[[1]]$Metadata_Standard_Name <- "FGDC Content Standard for Digital Geospatial Metadata"
  mri[[1]]$Metadata_Standard_Version <- "FGDC-STD-001-1998"
  mri[[1]]$Metadata_Time_Convention <- "local time"
  mri[[1]]$Metadata_Access_Constraints <- "None"
  mri[[1]]$Metadata_Use_Constraints <- "None"
  mri[[1]]$Metadata_Security_Information <- vector("list",3)
  names(mri[[1]]$Metadata_Security_Information) <- c("Metadata_Security_Classification_System",
                                                    "Metadata_Security_Classification",
                                                    "Metadata_Security_Handling_Description" )
  mri[[1]]$Metadata_Security_Information$Metadata_Security_Classification_System <- "None"
  mri[[1]]$Metadata_Security_Information$Metadata_Security_Classification <- "Unclassified"
  mri[[1]]$Metadata_Security_Information$Metadata_Security_Handling_Description <- "Metadata can be shared."
  
  # Metadata_Reference_Information:
  #   Metadata_Date: BLM REQUIRED
  #   Metadata_Review_Date: BLM REQUIRED
  #   Metadata_Contact:
  #     Contact_Information:
  #     Contact_Organization_Primary: [OR Contact_Person_Primary:]
  #     Contact_Organization: BLM REQUIRED
  #     Contact_Person: BLM RECOMMENDED
  #     OR 
  #     Contact_Person_Primary: [OR Contact_Organization_Primary:]
  #     Contact_Organization: BLM REQUIRED
  #     Contact_Person: BLM REQUIRED
  #     
  #     Contact_Position: BLM REQUIRED
  #     Contact_Address:
  #       Address_Type: BLM REQUIRED
  #     Address: BLM REQUIRED 
  #     City: BLM REQUIRED 
  #     State_or_Province: BLM REQUIRED – <Use postal 2 character code (CA, VA, etc.)>
  #       Postal_Code: BLM REQUIRED – <either the 5 or 10 alphanumeric-character code (99999-9999)>
  #       Country: BLM REQUIRED
  #     Contact_Voice_Telephone: BLM RECOMMENDED
  #     Contact_TDD/TTY_Telephone: BLM RECOMMENDED
  #     Contact_Facsimile_Telephone: BLM RECOMMENDED
  #     Contact_Electronic_Mail_Address: BLM REQUIRED
  #     Hours_of_Service: BLM RECOMMENDED
  #     Contact_Instructions: BLM RECOMMENDED
  #   Metadata_Standard_Name: BLM REQUIRED – FGDC Content Standard for Digital Geospatial Metadata
  #   Metadata_Standard_Version: BLM REQUIRED – FGDC-STD-001-1998
  #   Metadata_Time_Convention: BLM RECOMMENDED – <If using time elements associated with dates throughout the metadata, specify how that time is being used - either “local time”, “local time with time differential factor”, or “universal time”>
  #   Metadata_Access_Constraints: BLM REQUIRED, If Applicable
  #   Metadata_Use_Constraints: BLM REQUIRED, If Applicable
  #   Metadata_Security_Information:
  #     Metadata_Security_Classification_System: BLM REQUIRED, If Applicable
  #     Metadata_Security_Classification: BLM REQUIRED, If Applicable
  #     Metadata_Security_Handling_Description: BLM REQUIRED, If Applicable
  #   Metadata_Extensions:
  #     Online_Linkage: BLM RECOMMENDED
  #     Profile_Name: BLM RECOMMENDED
  # 
  # <metainfo>
  #   <metd>BLM REQUIRED</metd>
  #   <metrd>BLM REQUIRED</metrd>
  #   <metc>
  #     <cntinfo>
  #       <cntorgp>
  #         <cntorg>BLM REQUIRED</cntorg>
  #       </cntorgp>
  #       <cntpos>BLM REQUIRED</cntpos>
  #       <cntaddr>
  #         <addrtype>BLM REQUIRED</addrtype>
  #         <address>BLM REQUIRED</address>
  #         <city>BLM REQUIRED</city>
  #         <state>BLM REQUIRED - &lt;Use postal 2 character code (CA, VA, etc.)&gt;</state>
  #         <postal>BLM REQUIRED - &lt;either the 5 or 10 alphanumeric-character code (99999-9999)&gt;</postal>
  #         <country>BLM REQUIRED</country>
  #       </cntaddr>
  #       <cntemail>BLM REQUIRED</cntemail>
  #     </cntinfo>
  #   </metc>
  #   <metstdn>BLM REQUIRED - FGDC Content Standard for Digital Geospatial Metadata</metstdn>
  #   <metstdv>BLM REQUIRED - FGDC-STD-001-1998</metstdv>
  # </metainfo>
  
  # Compile all the parts ----
  m$Identification_Information$Citation <- cit_i
  m$Identification_Information$Description <- des_i[[1]]
  m$Identification_Information$Time_Period_of_Content <- tpoc$Time_Period_of_Content
  m$Identification_Information$Status <- stat_i$Status
  m$Identification_Information$Spatial_Domain <- spdom$Spatial_Domain
  m$Identification_Information$Keywords <- kw[[1]]
  m$Identification_Information$Access_Constraints <- accc[[1]]
  m$Identification_Information$Use_Constraints <- usec[[1]]
  m$Identification_Information$Point_of_Contact <- all_contacts$NS_McIntyre
  m$Identification_Information$Data_Set_Credit <- dsc[[1]]
  m$Identification_Information$Security_Information <- seci[[1]]
  m$Identification_Information$Native_Data_Set_Environment <- ndse[[1]]  
  m$Identification_Information$Cross_Reference <- cref[[1]]
  m$Data_Quality_Information$Attribute_Accuracy <- atta$Attribute_Accuracy
  m$Data_Quality_Information$Logical_Consistency_Report <- locorep$Logical_Consistency_Report
  m$Data_Quality_Information$Completeness_Report <- corep$Completeness_Report
  m$Data_Quality_Information$Positional_Accuracy <- poac$Positional_Accuracy
  m$Data_Quality_Information$Lineage <- lin$Lineage
  m$Spatial_Data_Organization_Information <- sdoi$Spatial_Data_Organization_Information
  m$Spatial_Reference_Information <- spref$Spatial_Reference_Information
  m$Entity_and_Attribute_Information <- eai$Entity_and_Attribute_Information
  m$Distribution_Information <- di$Distribution_Information
  m$Metadata_Reference_Information <- mri$Metadata_Reference_Information
  
  
  
  # convert to short codes (!!) ----
  #  lookup table to short code conversion ----
  lkpCodes <- data.frame(matrix(c(
    "idinfo", "Identification_Information", 
    "dataqual", "Data_Quality_Information", 
    "spdoinfo", "Spatial_Data_Organization_Information", 
    "spref", "Spatial_Reference_Information", 
    "eainfo", "Entity_and_Attribute_Information", 
    "distinfo", "Distribution_Information", 
    "metainfo", "Metadata_Reference_Information", 
    "citation", "Citation", 
    "descript", "Description", 
    "status", "Status", 
    "timeperd", "Time_Period_of_Content", 
    "spdom", "Spatial_Domain", 
    "keywords", "Keywords", 
    "accconst", "Access_Constraints", 
    "useconst", "Use_Constraints", 
    "ptcontac", "Point_of_Contact", 
    "datacred", "Data_Set_Credit", 
    "secinfo", "Security_Information", 
    "native", "Native_Data_Set_Environment", 
    "crossref", "Cross_Reference",
    "citeinfo", "Citation_Information", 
    "origin", "Originator", 
    "pubdate", "Publication_Date", 
    "title", "Title", 
    "pubinfo", "Publication_Information", 
    "pubplace", "Publication_Place", 
    "publish", "Publisher", 
    "othercit", "Other_Citation_Details", 
    "onlink", "Online_Linkage",
    "abstract", "Abstract",
    "purpose", "Purpose",
    "progress", "Progress",
    "update", "Maintenance_and_Update_Frequency",
    "timeinfo", "Time_Period_Information",
    "current", "Currentness_Reference",
    "sngdate", "Single_Date", 
    "caldate", "Calendar_Date",
    "bounding", "Bounding_Coordinates",
    "westbc", "West_Bounding_Coordinate", 
    "eastbc", "East_Bounding_Coordinate",
    "northbc", "North_Bounding_Coordinate",
    "southbc", "South_Bounding_Coordinate",
    "theme", "Theme",
    "place", "Place",
    "themekt", "Theme_Keyword_Thesaurus", 
    "themekey", "Theme_Keyword",
    "placekt", "Place_Keyword_Thesaurus", 
    "placekey", "Place_Keyword",
    "cntorgp","Contact_Organization_Primary",
    "cntpos","Contact_Position",
    "cntaddr","Contact_Address",
    "cntvoice", "Contact_Voice_Telephone", 
    "cntemail","Contact_Electronic_Mail_Address",
    "cntorg","Contact_Organization",
    "cntper","Contact_Person",
    "addrtype","Address_Type",
    "address","Address",
    "city","City",
    "state","State_or_Province",
    "postal","Postal_Code",
    "country","Country",
    "secsys", "Security_Classification_System",
    "secclass","Security_Classification", 
    "sechandl","Security_Handling_Description",
    "attracc","Attribute_Accuracy", 
    "logic","Logical_Consistency_Report", 
    "complete", "Completeness_Report", 
    "posacc", "Positional_Accuracy", 
    "lineage", "Lineage",
    "attraccr", "Attribute_Accuracy_Report",
    "horizpa", "Horizontal_Positional_Accuracy",
    "vertacc", "Vertical_Positional_Accuracy",
    "horizpar", "Horizontal_Positional_Accuracy_Report",
    "vertaccr", "Vertical_Positional_Accuracy_Report",
    "procstep", "Process_Step",
    "procdesc", "Process_Description", 
    "procdate", "Process_Date", 
    "proccont", "Process_Contact",
    "direct", "Direct_Spatial_Reference_Method",
    "raster","Raster",
    "rasttype", "Raster_Object_Type", 
    "rowcount", "Row_Count", 
    "colcount", "Column_Count", 
    "vrtcount", "Vertical_Count",
    "horizsys", "Horizontal_Coordinate_System_Definition",
    "planar", "Planar", 
    "geodetic", "Geodetic",
    "mapproj", "Map_Projection", 
    "mapprojn", "Map_Projection_Name", 
    "plance", "Planar_Coordinate_Encoding_Method",
    "coordrep", "Coordinate_Representation",
    "absres", "Abscissa_Resolution", 
    "ordres", "Ordinate_Resolution",
    "planci", "Planar_Coordinate_Information",
    "albers", "Albers_Conical_Equal_Area",
    "stdparll", "Standard_Parallel",
    "longcm", "Longitude_of_Central_Meridian ",
    "latprjo", "Latitude_of_Projection_Origin ",
    "feast", "False_Easting ",
    "fnorth", "False_Northing",
    "plandu", "Planar_Distance_Units",
    "horizdn", "Horizontal_Datum_Name",
    "ellips", "Ellipsoid_Name",
    "semiaxis", "Semi-major_Axis", 
    "denflat", "Denominator_of_Flattening_Ratio",
    "detailed", "Detailed_Description",
    "overview", "Overview_Description",
    "eaover", "Entity_and_Attribute_Overview",
    "eadetcit", "Entity_and_Attribute_Detail_Citation",
    "enttyp", "Entity_Type",
    "attr", "Attribute",
    "enttypl", "Entity_Type_Label", 
    "enttypd", "Entity_Type_Definition",
    "enttypds", "Entity_Type_Definition_Source", 
    "attrlabl", "Attribute_Label", 
    "attrdef", "Attribute_Definition",
    "attrdefs", "Attribute_Definition_Source",
    "attrdomv", "Attribute_Domain_Values",
    "rdom", "Range_Domain",
    "rdommin", "Range_Domain_Minimum", 
    "rdommax", "Range_Domain_Maximum",
    "cntinfo", "Contact_Information",
    "distrib", "Distributor",
    "resdesc", "Resource_Description",
    "distliab", "Distribution_Liability", 
    "stdorder", "Standard_Order_Process",
    "metd", "Metadata_Date", 
    "metrd", "Metadata_Review_Date",
    "metc", "Metadata_Contact", 
    "metstdn", "Metadata_Standard_Name", 
    "metstdv", "Metadata_Standard_Version", 
    "mettc", "Metadata_Time_Convention", 
    "metac", "Metadata_Access_Constraints", 
    "metuc", "Metadata_Use_Constraints", 
    "metsi", "Metadata_Security_Information",
    "metscs", "Metadata_Security_Classification_System", 
    "metsc", "Metadata_Security_Classification", 
    "metshd", "Metadata_Security_Handling_Description", 
    "rastinfo", "Raster_Object_Information"
  ), ncol = 2, byrow = TRUE), stringsAsFactors = FALSE)
  names(lkpCodes) <- c("codenm","fullnm")
  
  # #  conversion (list rename) function ----
  # this version gets mismatch warnings ...
  # listRename <- function(nested_list) {
  #   found <- names(nested_list) %in% lkpCodes$fullnm
  #   names(nested_list)[found] <- lkpCodes[lkpCodes$fullnm %in% names(nested_list)[found],"codenm"]
  #   lapply(nested_list, FUN = function(x){
  #     if (is.list(x)) {
  #       print(names(x))
  #       listRename(x)
  #     } else {
  #       x
  #     }
  #   })
  # }
  # y <- listRename(m)
  
  listRename <- function(nested_list) {
    newnames <- lkpCodes$codenm[match(names(nested_list), lkpCodes$fullnm)]
    names(nested_list)[!is.na(newnames)] <- newnames[!is.na(newnames)]
    lapply(nested_list, FUN = function(x){
      if (is.list(x)) {
        listRename(x)
      } else {
        x
      }
    })
  }
  y <- listRename(m)
  
  # yet another way, using purrr
  # library(purrr)
  # 
  # lkp2 <- lkpCodes$codenm
  # names(lkp2) <- lkpCodes$fullnm
  # 
  # rename <- function(nested_list) {
  #   found <- names(nested_list) %in% names(lkp2)
  #   names(nested_list)[found] <- lkp2[names(nested_list)[found]]
  #   nested_list %>% map(~{
  #     if (is.list(.x)) {
  #       rename(.x)
  #     } else {
  #       .x
  #     }
  #   })
  # }
  # y <- rename(m)
  
  
  # convert to xml  ----
  
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
          #print(i)
          if (is.null (names(item[[i]][1])) ){
            #print(i)
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
  
  # export the file ----
  metadataConverted <- listToXml(y, "metadata")
  fn <- paste0(model.run.dat$model_run_name, "_", algo, ".xml")
  fp <- here("_data","species", spp.dat$sp_code, "outputs","metadata")
  setwd(fp)
  saveXML(metadataConverted, file = fn)

} # this is the end of the algo loop


