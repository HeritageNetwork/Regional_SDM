PRAGMA foreign_keys=OFF;
BEGIN TRANSACTION;
CREATE TABLE `tblModelResultsVarsUsed` (
	`model_run_name`	TEXT,
	`gridName`	TEXT,
	`inFinalModel`	INTEGER,
	`impVal`	REAL,
	FOREIGN KEY(`model_run_name`) REFERENCES `tblModelResults`(`model_run_name`),
	PRIMARY KEY(`model_run_name`,`gridName`)
);
CREATE TABLE `tblModelResultsCutoffs` (
	`model_run_name`	TEXT,
	`ElemCode`	TEXT,
	`dateTime`	TEXT,
	`cutCode`	TEXT,
	`cutValue`	REAL,
	`capturedEOs`	INTEGER,
	`capturedPolys`	INTEGER,
	`capturedPts`	INTEGER,
	PRIMARY KEY(`model_run_name`,`cutCode`),
	FOREIGN KEY(`model_run_name`) REFERENCES `tblModelResults`(`model_run_name`)
);
CREATE TABLE `tblModelInputsHucs` (
	`table_code`	TEXT,
	`huc_06`	INTEGER,
	`huc_08`	INTEGER,
	`huc_10`	INTEGER,
	PRIMARY KEY(`table_code`),
	FOREIGN KEY(`table_code`) REFERENCES `tblModelInputs`(`table_code`)
);
CREATE TABLE `tblModelInputs` (
	`table_code`	TEXT,
	`EGT_ID`	INTEGER,
	`datetime`	TEXT,
	`filename_pres`	TEXT,
	`filename_bkgd`	TEXT,
	`feat_count`	INTEGER,
	`feat_grp_count`	INTEGER,
	`obs_count`	INTEGER,
	`bkgd_count`	INTEGER,
	`range_clip_definitionXXX`	TEXT,
	`range_area_sqkm`	REAL,
	`range_area_huc_count`	INTEGER,
	PRIMARY KEY(`table_code`)
);
CREATE TABLE `mapDataSourcesToSpp` (
	`DataSourcesToSpeciesID`	INTEGER NOT NULL,
	`DataSourcesID`	INTEGER,
	`EGT_ID`	INTEGER,
	PRIMARY KEY(`DataSourcesToSpeciesID`)
);
INSERT INTO mapDataSourcesToSpp VALUES(24,25,101917);
INSERT INTO mapDataSourcesToSpp VALUES(25,26,101917);
INSERT INTO mapDataSourcesToSpp VALUES(26,27,101917);
INSERT INTO mapDataSourcesToSpp VALUES(27,28,101917);
INSERT INTO mapDataSourcesToSpp VALUES(28,26,102063);
INSERT INTO mapDataSourcesToSpp VALUES(29,27,102063);
INSERT INTO mapDataSourcesToSpp VALUES(30,28,102063);
CREATE TABLE `lkpThresholdTypes` (
	`ID`	INTEGER,
	`cutCode`	TEXT,
	`cutFullName`	TEXT,
	`cutDescription`	TEXT,
	`cutCitationShort`	TEXT,
	`cutCitationFull`	TEXT,
	`sortOrder`	INTEGER,
	PRIMARY KEY(`ID`)
);
INSERT INTO lkpThresholdTypes VALUES(1,'MTP','Minimum Training Presence','The highest probability value at which 100% of input presence points remain classified as suitable habitat.',NULL,'Pearson, R. G., C. J. Raxworthy, M. Nakamura, and A. Townsend Peterson. 2007. Predicting species distributions from small numbers of occurrence records: a test case using cryptic geckos in Madagascar. Journal of Biogeography 34:102–117. ',3);
INSERT INTO lkpThresholdTypes VALUES(2,'TenPctile','Tenth percentile of training presence','The probability at which 90% of the input presence points are classified as suitable habitat.',NULL,NULL,6);
INSERT INTO lkpThresholdTypes VALUES(3,'maxSSS','Maximum of sensitivity plus specificity','The probability at which the sum of sensitivity and specificity is maximized.','LiuEtAl2015','Liu, C., G. Newell, and M. White. 2015. On the selection of thresholds for predicting species occurrence with presence‐only data. Ecology and Evolution 6:337–348.',2);
INSERT INTO lkpThresholdTypes VALUES(4,'eqSS','Equal sensitivity and specificity','The probability at which the absolute value of the difference between sensitivity and specificity is minimized.','LiuEtAl2005','Liu, C., P. M. Berry, T. P. Dawson, and R. G. Pearson. 2005. Selecting thresholds of occurrence in the prediction of species distributions. Ecography 28:385–393.',1);
INSERT INTO lkpThresholdTypes VALUES(5,'FMeasPt01','F-measure with alpha set to 0.01','The probability value at which the harmonic mean of precision and recall, with strong weighting towards recall, is maximized.','LiuEtAl2005','Liu, C., P. M. Berry, T. P. Dawson, and R. G. Pearson. 2005. Selecting thresholds of occurrence in the prediction of species distributions. Ecography 28:385–393.',7);
INSERT INTO lkpThresholdTypes VALUES(6,'ROC','ROC plot upper left corner','The point on the ROC curve with the shortest distance to the top-left corner of the ROC plot.','LiuEtAl2005','Liu, C., P. M. Berry, T. P. Dawson, and R. G. Pearson. 2005. Selecting thresholds of occurrence in the prediction of species distributions. Ecography 28:385–393.',8);
INSERT INTO lkpThresholdTypes VALUES(7,'MTPP','Minimum Training Presence by Polygon','The highest probability value at which 100% of input polygons have at least one presence point classified as suitable habitat.',NULL,'Pearson, R. G., C. J. Raxworthy, M. Nakamura, and A. Townsend Peterson. 2007. Predicting species distributions from small numbers of occurrence records: a test case using cryptic geckos in Madagascar. Journal of Biogeography 34:102–117. ',4);
INSERT INTO lkpThresholdTypes VALUES(8,'MTPEO_original','Minimum Training Presence by Element Occurrence','The highest probability value at which 100% of input EOs have at least one presence point classified as suitable habitat.',NULL,'Pearson, R. G., C. J. Raxworthy, M. Nakamura, and A. Townsend Peterson. 2007. Predicting species distributions from small numbers of occurrence records: a test case using cryptic geckos in Madagascar. Journal of Biogeography 34:102–117. ',5);
INSERT INTO lkpThresholdTypes VALUES(9,'MTPEO','Minimum Training Presence by Group','The highest probability value at which 100% of input groups have at least one presence point classified as suitable habitat.',NULL,'Pearson, R. G., C. J. Raxworthy, M. Nakamura, and A. Townsend Peterson. 2007. Predicting species distributions from small numbers of occurrence records: a test case using cryptic geckos in Madagascar. Journal of Biogeography 34:102–117. ',5);
CREATE TABLE `lkpRange` (
	`range_id`	INTEGER PRIMARY KEY AUTOINCREMENT,
	`EGT_ID`	INTEGER,
	`huc10_id`	TEXT,
	`origin`	TEXT,
	`occurrence`	TEXT,
	`version_info`	TEXT,
	`comments`	TEXT
);
INSERT INTO lkpRange VALUES(1,104362,'0902031102','terrestrial_ranges_merged',NULL,'2019-01-07','examples for two hucs');
INSERT INTO lkpRange VALUES(2,106446,'0902031102','terrestrial_ranges_merged',NULL,'2019-01-07',NULL);
INSERT INTO lkpRange VALUES(3,799416,'0902031102','terrestrial_ranges_merged',NULL,'2019-01-07',NULL);
INSERT INTO lkpRange VALUES(4,100473,'0902031102','terrestrial_ranges_merged',NULL,'2019-01-07',NULL);
INSERT INTO lkpRange VALUES(5,102615,'0902031102','terrestrial_ranges_merged',NULL,'2019-01-07',NULL);
INSERT INTO lkpRange VALUES(6,104362,'0902031103','terrestrial_ranges_merged',NULL,'2019-01-07',NULL);
INSERT INTO lkpRange VALUES(7,106446,'0902031103','terrestrial_ranges_merged',NULL,'2019-01-07',NULL);
INSERT INTO lkpRange VALUES(8,799416,'0902031103','terrestrial_ranges_merged',NULL,'2019-01-07',NULL);
INSERT INTO lkpRange VALUES(9,100473,'0902031103','terrestrial_ranges_merged',NULL,'2019-01-07',NULL);
INSERT INTO lkpRange VALUES(10,102615,'0902031103','terrestrial_ranges_merged',NULL,'2019-01-07',NULL);
CREATE TABLE `lkpModtype` (
	`MODTYPE`	CHARACTER ( 1 ),
	`MODTYPE_desc`	text,
	PRIMARY KEY(`MODTYPE`)
);
CREATE TABLE `lkpModelers` (
	`ModelerID`	INTEGER NOT NULL,
	`ProgramName`	TEXT,
	`FullOrganizationName`	TEXT,
	`City`	TEXT,
	`State`	TEXT,
	PRIMARY KEY(`ModelerID`)
);
INSERT INTO lkpModelers VALUES(1,'Virginia Natural Heritage Program','Virginia Department of Conservation and Recreation - Division of Natural Heritage','Richmond','VA');
INSERT INTO lkpModelers VALUES(3,'Pennsylvania Natural Heritage Program','Pennsylvania Department of Conservation and Natural Resources and Western Pennsylvania Conservancy','Pittsburgh','PA');
INSERT INTO lkpModelers VALUES(4,'New York Natural Heritage Program','The Research Foundation of SUNY, College of Environmental Science and Forestry','Albany ','NY');
CREATE TABLE `lkpEnvVarsAqua` (
	`fullName`	TEXT,
	`gridName`	TEXT,
	`fileName`	TEXT,
	`description`	TEXT,
	`dataType`	TEXT,
	`multiplier`	TEXT,
	`category`	TEXT,
	`comments`	TEXT,
	`distToGrid`	INTEGER,
	`correlatedVarGroupings`	INTEGER,
	`use_A`	INTEGER,
	`use_R`	INTEGER,
	`use_L`	INTEGER,
	PRIMARY KEY(`gridName`)
);
INSERT INTO lkpEnvVarsAqua VALUES('al2o3cat','al2o3cat',NULL,'Mean % of lithological aluminum oxide (Al2O3) content in surface or near surface geology within catchment',NULL,NULL,'StreamCat',NULL,NULL,28,0,NULL,NULL);
INSERT INTO lkpEnvVarsAqua VALUES('al2o3ws','al2o3ws',NULL,'Mean % of lithological aluminum oxide (Al2O3) content in surface or near surface geology within watershed',NULL,NULL,'StreamCat',NULL,NULL,28,0,NULL,NULL);
INSERT INTO lkpEnvVarsAqua VALUES('avgwetindxcat','avgwetindxcat',NULL,'Mean Composite Topographic Index (CTI)[Wetness Index] within catchment',NULL,NULL,'StreamCat',NULL,NULL,NULL,0,NULL,NULL);
INSERT INTO lkpEnvVarsAqua VALUES('avgwetindxws','avgwetindxws',NULL,'Mean Composite Topographic Index (CTI)[Wetness Index] within watershed',NULL,NULL,'StreamCat',NULL,NULL,NULL,1,NULL,NULL);
INSERT INTO lkpEnvVarsAqua VALUES('bficat','bficat',NULL,'Base flow is the component of streamflow that can be attributed to ground-water discharge into streams. The BFI is the ratio of base flow to total flow, expressed as a percentage, within catchment',NULL,NULL,'StreamCat',NULL,NULL,18,0,NULL,NULL);
INSERT INTO lkpEnvVarsAqua VALUES('bfiws','bfiws',NULL,'Base flow is the component of streamflow that can be attributed to ground-water discharge into streams. The BFI is the ratio of base flow to total flow, expressed as a percentage, within watershed',NULL,NULL,'StreamCat',NULL,NULL,18,1,NULL,NULL);
INSERT INTO lkpEnvVarsAqua VALUES('canaldenscat','canaldenscat',NULL,'Density of NHDPlus line features classified as canal, ditch, or pipeline within the catchment (km/ square km)',NULL,NULL,'StreamCat',NULL,NULL,NULL,1,NULL,NULL);
INSERT INTO lkpEnvVarsAqua VALUES('canaldensws','canaldensws',NULL,'Density of NHDPlus line features classified as canal, ditch, or pipeline within the upstream watershed (km/ square km)',NULL,NULL,'StreamCat',NULL,NULL,NULL,1,NULL,NULL);
INSERT INTO lkpEnvVarsAqua VALUES('caocat','caocat',NULL,'Mean % of lithological calcium oxide (CaO) content in surface or near surface geology within catchment',NULL,NULL,'StreamCat',NULL,NULL,29,0,NULL,NULL);
INSERT INTO lkpEnvVarsAqua VALUES('caows','caows',NULL,'Mean % of lithological calcium oxide (CaO) content in surface or near surface geology within watershed',NULL,NULL,'StreamCat',NULL,NULL,36,0,NULL,NULL);
INSERT INTO lkpEnvVarsAqua VALUES('cbnfcat','cbnfcat',NULL,'Mean rate of biological nitrogen fixation from the cultivation of crops in kg N/ha/yr, within catchment',NULL,NULL,'StreamCat',NULL,NULL,NULL,0,NULL,NULL);
INSERT INTO lkpEnvVarsAqua VALUES('cbnfws','cbnfws',NULL,'Mean rate of biological nitrogen fixation from the cultivation of crops in kg N/ha/yr, within watershed',NULL,NULL,'StreamCat',NULL,NULL,NULL,1,NULL,NULL);
INSERT INTO lkpEnvVarsAqua VALUES('coalminedenscat','coalminedenscat',NULL,'Density of coal mines sites within catchment (mines/square km)',NULL,NULL,'StreamCat',NULL,NULL,NULL,1,NULL,NULL);
INSERT INTO lkpEnvVarsAqua VALUES('coalminedensws','coalminedensws',NULL,'Density of coal mines sites within watershed (mines/square km)',NULL,NULL,'StreamCat',NULL,NULL,NULL,1,NULL,NULL);
INSERT INTO lkpEnvVarsAqua VALUES('compstrgthcat','compstrgthcat',NULL,'Mean lithological uniaxial compressive strength (megaPascals) content in surface or near surface geology within catchment',NULL,NULL,'StreamCat',NULL,NULL,44,0,NULL,NULL);
INSERT INTO lkpEnvVarsAqua VALUES('compstrgthws','compstrgthws',NULL,'Mean lithological uniaxial compressive strength (megaPascals) content in surface or near surface geology within watershed',NULL,NULL,'StreamCat',NULL,NULL,44,0,NULL,NULL);
INSERT INTO lkpEnvVarsAqua VALUES('damdenscat','damdenscat',NULL,'Density of georeferenced dams within catchment (dams/ square km)',NULL,NULL,'StreamCat',NULL,NULL,NULL,1,NULL,NULL);
INSERT INTO lkpEnvVarsAqua VALUES('damdensws','damdensws',NULL,'Density of georeferenced dams within watershed (dams/ square km)',NULL,NULL,'StreamCat',NULL,NULL,NULL,1,NULL,NULL);
INSERT INTO lkpEnvVarsAqua VALUES('damnidstorm3cat','damnidstorm3cat',NULL,'Volume all reservoirs (NID_STORA in NID) per unit area of catchment (cubic meters/square km)',NULL,NULL,'StreamCat',NULL,NULL,24,1,NULL,NULL);
INSERT INTO lkpEnvVarsAqua VALUES('damnidstorm3ws','damnidstorm3ws',NULL,'Volume all reservoirs (NID_STORA in NID) per unit area of watershed (cubic meters/square km)',NULL,NULL,'StreamCat',NULL,NULL,NULL,1,NULL,NULL);
INSERT INTO lkpEnvVarsAqua VALUES('damnrmstorm3cat','damnrmstorm3cat',NULL,'Volume all reservoirs (NORM_STORA in NID) per unit area of catchment (cubic meters/square km)',NULL,NULL,'StreamCat',NULL,NULL,24,1,NULL,NULL);
INSERT INTO lkpEnvVarsAqua VALUES('damnrmstorm3ws','damnrmstorm3ws',NULL,'Volume all reservoirs (NORM_STORA in NID) per unit area of watershed (cubic meters/square km)',NULL,NULL,'StreamCat',NULL,NULL,NULL,1,NULL,NULL);
INSERT INTO lkpEnvVarsAqua VALUES('elevcat','elevcat',NULL,'Mean catchment elevation (m)',NULL,NULL,'StreamCat',NULL,NULL,7,1,NULL,NULL);
INSERT INTO lkpEnvVarsAqua VALUES('elevws','elevws',NULL,'Mean watershed elevation (m)',NULL,NULL,'StreamCat',NULL,NULL,7,1,NULL,NULL);
INSERT INTO lkpEnvVarsAqua VALUES('fe2o3cat','fe2o3cat',NULL,'Mean % of lithological ferric oxide (Fe2O3) content in surface or near surface geology within catchment',NULL,NULL,'StreamCat',NULL,NULL,30,0,NULL,NULL);
INSERT INTO lkpEnvVarsAqua VALUES('fe2o3ws','fe2o3ws',NULL,'Mean % of lithological ferric oxide (Fe2O3) content in surface or near surface geology within watershed',NULL,NULL,'StreamCat',NULL,NULL,30,0,NULL,NULL);
INSERT INTO lkpEnvVarsAqua VALUES('fertcat','fertcat',NULL,'Mean rate of synthetic nitrogen fertilizer application to agricultural land in kg N/ha/yr, within the catchment',NULL,NULL,'StreamCat',NULL,NULL,NULL,0,NULL,NULL);
INSERT INTO lkpEnvVarsAqua VALUES('fertws','fertws',NULL,'Mean rate of synthetic nitrogen fertilizer application to agricultural land in kg N/ha/yr, within watershed',NULL,NULL,'StreamCat',NULL,NULL,NULL,1,NULL,NULL);
INSERT INTO lkpEnvVarsAqua VALUES('frac_d','frac_d',NULL,'Fractal dimension of reach line.',NULL,NULL,'nhplusv2_derived',NULL,NULL,NULL,1,NULL,NULL);
INSERT INTO lkpEnvVarsAqua VALUES('huden2010cat','huden2010cat',NULL,'Mean housing unit density (housing units/square km) within catchment',NULL,NULL,'StreamCat',NULL,NULL,77,1,NULL,NULL);
INSERT INTO lkpEnvVarsAqua VALUES('huden2010catrp100','huden2010catrp100',NULL,'Mean housing unit density (housing units/square km) within catchment and within a 100-m buffer of NHD stream lines',NULL,NULL,'StreamCat',NULL,NULL,77,0,NULL,NULL);
INSERT INTO lkpEnvVarsAqua VALUES('huden2010ws','huden2010ws',NULL,'Mean housing unit density (housing units/square km) within watershed',NULL,NULL,'StreamCat',NULL,NULL,78,1,NULL,NULL);
INSERT INTO lkpEnvVarsAqua VALUES('huden2010wsrp100','huden2010wsrp100',NULL,'Mean housing unit density (housing units/square km) within watershed and within a 100-m buffer of NHD stream lines',NULL,NULL,'StreamCat',NULL,NULL,78,1,NULL,NULL);
INSERT INTO lkpEnvVarsAqua VALUES('hydrlcondcat','hydrlcondcat',NULL,'Mean lithological hydraulic conductivity (micrometers per second) content in surface or near surface geology within catchment',NULL,NULL,'StreamCat',NULL,NULL,NULL,0,NULL,NULL);
INSERT INTO lkpEnvVarsAqua VALUES('hydrlcondws','hydrlcondws',NULL,'Mean lithological hydraulic conductivity (micrometers per second) content in surface or near surface geology within watershed',NULL,NULL,'StreamCat',NULL,NULL,NULL,0,NULL,NULL);
INSERT INTO lkpEnvVarsAqua VALUES('inorgnwetdep_2008cat','inorgnwetdep_2008cat',NULL,'Annual gradient map of preciptiation-weighted mean deposition for inorganic nitrogen wet deposition from nitrate and ammonium for 2008 in kg of N/ha/yr, within catchment',NULL,NULL,'StreamCat',NULL,NULL,63,0,NULL,NULL);
INSERT INTO lkpEnvVarsAqua VALUES('k2ocat','k2ocat',NULL,'Mean % of lithological potassium oxide (K2O) content in surface or near surface geology within catchment',NULL,NULL,'StreamCat',NULL,NULL,NULL,0,NULL,NULL);
INSERT INTO lkpEnvVarsAqua VALUES('k2ows','k2ows',NULL,'Mean % of lithological potassium oxide (K2O) content in surface or near surface geology within watershed',NULL,NULL,'StreamCat',NULL,NULL,NULL,0,NULL,NULL);
INSERT INTO lkpEnvVarsAqua VALUES('kffactcat','kffactcat',NULL,'The Kffactor is used in the Universal Soil Loss Equation (USLE) and represents a relative index of susceptibility of bare, cultivated soil to particle detachment and transport by rainfall within catchment',NULL,NULL,'StreamCat',NULL,NULL,45,1,NULL,NULL);
INSERT INTO lkpEnvVarsAqua VALUES('kffactws','kffactws',NULL,'The Kffactor is used in the Universal Soil Loss Equation (USLE) and represents a relative index of susceptibility of bare, cultivated soil to particle detachment and transport by rainfall within watershed',NULL,NULL,'StreamCat',NULL,NULL,45,1,NULL,NULL);
INSERT INTO lkpEnvVarsAqua VALUES('manurecat','manurecat',NULL,'Mean rate of manure application to agricultural land from confined animal feeding operations in kg N/ha/yr, within catchment',NULL,NULL,'StreamCat',NULL,NULL,NULL,0,NULL,NULL);
INSERT INTO lkpEnvVarsAqua VALUES('manurews','manurews',NULL,'Mean rate of manure application to agricultural land from confined animal feeding operations in kg N/ha/yr, within watershed',NULL,NULL,'StreamCat',NULL,NULL,NULL,1,NULL,NULL);
INSERT INTO lkpEnvVarsAqua VALUES('mean_elev_cm','mean_elev_cm',NULL,'Mean elevation of reach [(maxelevsmo + minelevsmo)/2]',NULL,NULL,'nhplusv2_derived',NULL,NULL,7,1,NULL,NULL);
INSERT INTO lkpEnvVarsAqua VALUES('mgocat','mgocat',NULL,'Mean % of lithological magnesium oxide (MgO) content in surface or near surface geology within catchment',NULL,NULL,'StreamCat',NULL,NULL,NULL,0,NULL,NULL);
INSERT INTO lkpEnvVarsAqua VALUES('mgows','mgows',NULL,'Mean % of lithological magnesium oxide (MgO) content in surface or near surface geology within watershed',NULL,NULL,'StreamCat',NULL,NULL,NULL,0,NULL,NULL);
INSERT INTO lkpEnvVarsAqua VALUES('minedenscat','minedenscat',NULL,'Density of mines sites within catchment (mines/square km)',NULL,NULL,'StreamCat',NULL,NULL,NULL,1,NULL,NULL);
INSERT INTO lkpEnvVarsAqua VALUES('minedenscatrp100','minedenscatrp100',NULL,'Density of mines sites within catchment and within 100-m buffer of NHD stream lines (mines/square km)',NULL,NULL,'StreamCat',NULL,NULL,NULL,0,NULL,NULL);
INSERT INTO lkpEnvVarsAqua VALUES('minedensws','minedensws',NULL,'Density of mines sites within watershed (mines/square km)',NULL,NULL,'StreamCat',NULL,NULL,NULL,1,NULL,NULL);
INSERT INTO lkpEnvVarsAqua VALUES('minedenswsrp100','minedenswsrp100',NULL,'Density of mines sites within watershed and within 100-m buffer of NHD stream lines (mines/square km)',NULL,NULL,'StreamCat',NULL,NULL,NULL,1,NULL,NULL);
INSERT INTO lkpEnvVarsAqua VALUES('mnann_streamtemp','mnann_streamtemp',NULL,'Mean annual stream temperature (averaged over 4 years from StreamCat [mast_] variables.)',NULL,NULL,'StreamCat_derived',NULL,NULL,NULL,0,NULL,NULL);
INSERT INTO lkpEnvVarsAqua VALUES('mnsum_streamtemp','mnsum_streamtemp',NULL,'Mean summer stream temperature (averaged over 4 years from StreamCat [msst_] variables.)',NULL,NULL,'StreamCat_derived',NULL,NULL,NULL,0,NULL,NULL);
INSERT INTO lkpEnvVarsAqua VALUES('mnwin_streamtemp','mnwin_streamtemp',NULL,'Mean winter stream temperature (averaged over 4 years from StreamCat [mwst_] variables.)',NULL,NULL,'StreamCat_derived',NULL,NULL,NULL,0,NULL,NULL);
INSERT INTO lkpEnvVarsAqua VALUES('na2ocat','na2ocat',NULL,'Mean % of lithological sodium oxide (Na2O) content in surface or near surface geology within catchment',NULL,NULL,'StreamCat',NULL,NULL,33,0,NULL,NULL);
INSERT INTO lkpEnvVarsAqua VALUES('na2ows','na2ows',NULL,'Mean % of lithological sodium oxide (Na2O) content in surface or near surface geology within watershed',NULL,NULL,'StreamCat',NULL,NULL,33,0,NULL,NULL);
INSERT INTO lkpEnvVarsAqua VALUES('ncat','ncat',NULL,'Mean % of lithological nitrogen (N) content in surface or near surface geology within catchment',NULL,NULL,'StreamCat',NULL,NULL,41,0,NULL,NULL);
INSERT INTO lkpEnvVarsAqua VALUES('nh4_2008cat','nh4_2008cat',NULL,'Annual gradient map of preciptiation-weighted mean deposition for ammonium ion concentration wet deposition for 2008 in kg of NH4/ha/yr, within catchment',NULL,NULL,'StreamCat',NULL,NULL,NULL,0,NULL,NULL);
INSERT INTO lkpEnvVarsAqua VALUES('no3_2008cat','no3_2008cat',NULL,'Annual gradient map of preciptiation-weighted mean deposition for nitrate ion concentration wet deposition for 2008 in kg of NO3/ha/yr, within catchment',NULL,NULL,'StreamCat',NULL,NULL,63,0,NULL,NULL);
INSERT INTO lkpEnvVarsAqua VALUES('nws','nws',NULL,'Mean % of lithological nitrogen (N) content in surface or near surface geology within watershed',NULL,NULL,'StreamCat',NULL,NULL,41,0,NULL,NULL);
INSERT INTO lkpEnvVarsAqua VALUES('p2o5cat','p2o5cat',NULL,'Mean % of lithological phosphorous oxide (P2O5) content in surface or near surface geology within catchment',NULL,NULL,'StreamCat',NULL,NULL,NULL,0,NULL,NULL);
INSERT INTO lkpEnvVarsAqua VALUES('p2o5ws','p2o5ws',NULL,'Mean % of lithological phosphorous oxide (P2O5) content in surface or near surface geology within watershed',NULL,NULL,'StreamCat',NULL,NULL,NULL,0,NULL,NULL);
INSERT INTO lkpEnvVarsAqua VALUES('pct_nlcddfrcat','pct_nlcddfrcat',NULL,'Percentage of deciduous forest (classes 41, 0.5*43) in catchment (NLCD 2011)',NULL,NULL,'StreamCat_derived',NULL,NULL,NULL,1,NULL,NULL);
INSERT INTO lkpEnvVarsAqua VALUES('pct_nlcddfrcatrp100','pct_nlcddfrcatrp100',NULL,'Percentage of deciduous forest (classes 41, 0.5*43) in catchment riparian 100m buffer (NLCD 2011)',NULL,NULL,'StreamCat_derived',NULL,NULL,NULL,0,NULL,NULL);
INSERT INTO lkpEnvVarsAqua VALUES('pct_nlcddfrws','pct_nlcddfrws',NULL,'Percentage of deciduous forest (classes 41, 0.5*43) in watershed (NLCD 2011)',NULL,NULL,'StreamCat_derived',NULL,NULL,NULL,1,NULL,NULL);
INSERT INTO lkpEnvVarsAqua VALUES('pct_nlcddfrwsrp100','pct_nlcddfrwsrp100',NULL,'Percentage of deciduous forest (classes 41, 0.5*43) in watershed riparian 100m buffer (NLCD 2011)',NULL,NULL,'StreamCat_derived',NULL,NULL,NULL,1,NULL,NULL);
INSERT INTO lkpEnvVarsAqua VALUES('pct_nlcdefrcat','pct_nlcdefrcat',NULL,'Percentage of evergreen forest (classes 42, 0.5*43) in catchment (NLCD 2011)',NULL,NULL,'StreamCat_derived',NULL,NULL,NULL,1,NULL,NULL);
INSERT INTO lkpEnvVarsAqua VALUES('pct_nlcdefrcatrp100','pct_nlcdefrcatrp100',NULL,'Percentage of evergreen forest (classes 42, 0.5*43) in catchment riparian 100m buffer (NLCD 2011)',NULL,NULL,'StreamCat_derived',NULL,NULL,NULL,0,NULL,NULL);
INSERT INTO lkpEnvVarsAqua VALUES('pct_nlcdefrws','pct_nlcdefrws',NULL,'Percentage of evergreen forest (classes 42, 0.5*43) in watershed (NLCD 2011)',NULL,NULL,'StreamCat_derived',NULL,NULL,NULL,1,NULL,NULL);
INSERT INTO lkpEnvVarsAqua VALUES('pct_nlcdefrwsrp100','pct_nlcdefrwsrp100',NULL,'Percentage of evergreen forest (classes 42, 0.5*43) in watershed riparian 100m buffer (NLCD 2011)',NULL,NULL,'StreamCat_derived',NULL,NULL,NULL,1,NULL,NULL);
INSERT INTO lkpEnvVarsAqua VALUES('pct_nlcdopncat','pct_nlcdopncat',NULL,'Percentage of open land covers (classes 31, 71, 81, 82) in catchment (NLCD 2011)',NULL,NULL,'StreamCat_derived',NULL,NULL,NULL,1,NULL,NULL);
INSERT INTO lkpEnvVarsAqua VALUES('pct_nlcdopncatrp100','pct_nlcdopncatrp100',NULL,'Percentage of open land covers (classes 31, 71, 81, 82) in catchment riparian 100m buffer (NLCD 2011)',NULL,NULL,'StreamCat_derived',NULL,NULL,NULL,0,NULL,NULL);
INSERT INTO lkpEnvVarsAqua VALUES('pct_nlcdopnws','pct_nlcdopnws',NULL,'Percentage of open land covers (classes 31, 71, 81, 82) in watershed (NLCD 2011)',NULL,NULL,'StreamCat_derived',NULL,NULL,NULL,1,NULL,NULL);
INSERT INTO lkpEnvVarsAqua VALUES('pct_nlcdopnwsrp100','pct_nlcdopnwsrp100',NULL,'Percentage of open land covers (classes 31, 71, 81, 82) in watershed riparian 100m buffer (NLCD 2011)',NULL,NULL,'StreamCat_derived',NULL,NULL,NULL,1,NULL,NULL);
INSERT INTO lkpEnvVarsAqua VALUES('pct_nlcdshbcat','pct_nlcdshbcat',NULL,'Percentage of shrub/scrub land covers (class 52) in catchment (NLCD 2011)',NULL,NULL,'StreamCat_derived',NULL,NULL,NULL,1,NULL,NULL);
INSERT INTO lkpEnvVarsAqua VALUES('pct_nlcdshbcatrp100','pct_nlcdshbcatrp100',NULL,'Percentage of shrub/scrub land covers (class 52) in catchment riparian 100m buffer (NLCD 2011)',NULL,NULL,'StreamCat_derived',NULL,NULL,NULL,0,NULL,NULL);
INSERT INTO lkpEnvVarsAqua VALUES('pct_nlcdshbws','pct_nlcdshbws',NULL,'Percentage of shrub/scrub land covers (class 52) in watershed (NLCD 2011)',NULL,NULL,'StreamCat_derived',NULL,NULL,NULL,1,NULL,NULL);
INSERT INTO lkpEnvVarsAqua VALUES('pct_nlcdshbwsrp100','pct_nlcdshbwsrp100',NULL,'Percentage of shrub/scrub land covers (class 52) in watershed riparian 100m buffer (NLCD 2011)',NULL,NULL,'StreamCat_derived',NULL,NULL,NULL,1,NULL,NULL);
INSERT INTO lkpEnvVarsAqua VALUES('pct_nlcdwatcat','pct_nlcdwatcat',NULL,'Percentage of open water land covers (class 11) in catchment (NLCD 2011)',NULL,NULL,'StreamCat_derived',NULL,NULL,NULL,1,NULL,NULL);
INSERT INTO lkpEnvVarsAqua VALUES('pct_nlcdwatcatrp100','pct_nlcdwatcatrp100',NULL,'Percentage of open water land covers (class 11) in catchment riparian 100m buffer (NLCD 2011)',NULL,NULL,'StreamCat_derived',NULL,NULL,NULL,0,NULL,NULL);
INSERT INTO lkpEnvVarsAqua VALUES('pct_nlcdwatws','pct_nlcdwatws',NULL,'Percentage of open water land covers (class 11) in watershed (NLCD 2011)',NULL,NULL,'StreamCat_derived',NULL,NULL,NULL,1,NULL,NULL);
INSERT INTO lkpEnvVarsAqua VALUES('pct_nlcdwatwsrp100','pct_nlcdwatwsrp100',NULL,'Percentage of open water land covers (class 11) in watershed riparian 100m buffer (NLCD 2011)',NULL,NULL,'StreamCat_derived',NULL,NULL,NULL,1,NULL,NULL);
INSERT INTO lkpEnvVarsAqua VALUES('pct_nlcdwetcat','pct_nlcdwetcat',NULL,'Percentage of wetland land covers (classes 90, 95) in catchment (NLCD 2011)',NULL,NULL,'StreamCat_derived',NULL,NULL,NULL,1,NULL,NULL);
INSERT INTO lkpEnvVarsAqua VALUES('pct_nlcdwetcatrp100','pct_nlcdwetcatrp100',NULL,'Percentage of wetland land covers (classes 90, 95) in catchment riparian 100m buffer (NLCD 2011)',NULL,NULL,'StreamCat_derived',NULL,NULL,NULL,0,NULL,NULL);
INSERT INTO lkpEnvVarsAqua VALUES('pct_nlcdwetws','pct_nlcdwetws',NULL,'Percentage of wetland land covers (classes 90, 95) in watershed (NLCD 2011)',NULL,NULL,'StreamCat_derived',NULL,NULL,NULL,1,NULL,NULL);
INSERT INTO lkpEnvVarsAqua VALUES('pct_nlcdwetwsrp100','pct_nlcdwetwsrp100',NULL,'Percentage of wetland land covers (classes 90, 95) in watershed riparian 100m buffer (NLCD 2011)',NULL,NULL,'StreamCat_derived',NULL,NULL,NULL,1,NULL,NULL);
INSERT INTO lkpEnvVarsAqua VALUES('pctalluvcoastcat','pctalluvcoastcat',NULL,'% of catchment area classified as lithology type: alluvium and fine-textured coastal zone sediment',NULL,NULL,'StreamCat',NULL,NULL,52,0,NULL,NULL);
INSERT INTO lkpEnvVarsAqua VALUES('pctalluvcoastws','pctalluvcoastws',NULL,'% of watershed area classified as as lithology type: alluvium and fine-textured coastal zone sediment',NULL,NULL,'StreamCat',NULL,NULL,52,1,NULL,NULL);
INSERT INTO lkpEnvVarsAqua VALUES('pctcarbresidcat','pctcarbresidcat',NULL,'% of catchment area classified as lithology type: carbonate residual material',NULL,NULL,'StreamCat',NULL,NULL,NULL,0,NULL,NULL);
INSERT INTO lkpEnvVarsAqua VALUES('pctcarbresidws','pctcarbresidws',NULL,'% of watershed area classified as as lithology type: carbonate residual material',NULL,NULL,'StreamCat',NULL,NULL,NULL,1,NULL,NULL);
INSERT INTO lkpEnvVarsAqua VALUES('pctcoastcrscat','pctcoastcrscat',NULL,'% of catchment area classified as lithology type: coastal zone sediment, coarse-textured',NULL,NULL,'StreamCat',NULL,NULL,53,0,NULL,NULL);
INSERT INTO lkpEnvVarsAqua VALUES('pctcoastcrsws','pctcoastcrsws',NULL,'% of watershed area classified as as lithology type: coastal zone sediment, coarse-textured',NULL,NULL,'StreamCat',NULL,NULL,53,1,NULL,NULL);
INSERT INTO lkpEnvVarsAqua VALUES('pctcolluvsedcat','pctcolluvsedcat',NULL,'% of catchment area classified as lithology type: colluvial sediment',NULL,NULL,'StreamCat',NULL,NULL,49,0,NULL,NULL);
INSERT INTO lkpEnvVarsAqua VALUES('pctcolluvsedws','pctcolluvsedws',NULL,'% of watershed area classified as as lithology type: colluvial sediment',NULL,NULL,'StreamCat',NULL,NULL,49,1,NULL,NULL);
INSERT INTO lkpEnvVarsAqua VALUES('pcteolcrscat','pcteolcrscat',NULL,'% of catchment area classified as lithology type: eolian sediment, coarse-textured (sand dunes)',NULL,NULL,'StreamCat',NULL,NULL,51,0,NULL,NULL);
INSERT INTO lkpEnvVarsAqua VALUES('pcteolcrsws','pcteolcrsws',NULL,'% of watershed area classified as as lithology type: eolian sediment, coarse-textured (sand dunes)',NULL,NULL,'StreamCat',NULL,NULL,51,1,NULL,NULL);
INSERT INTO lkpEnvVarsAqua VALUES('pcthydriccat','pcthydriccat',NULL,'% of catchment area classified as lithology type: hydric, peat and muck',NULL,NULL,'StreamCat',NULL,NULL,NULL,0,NULL,NULL);
INSERT INTO lkpEnvVarsAqua VALUES('pcthydricws','pcthydricws',NULL,'% of watershed area classified as as lithology type: hydric, peat and muck',NULL,NULL,'StreamCat',NULL,NULL,NULL,1,NULL,NULL);
INSERT INTO lkpEnvVarsAqua VALUES('pctnonagintrodmanagvegcat','pctnonagintrodmanagvegcat',NULL,'% Nonagriculture nonnative introduced or managed vegetation landcover type reclassed from LANDFIRE Existing Vegetation Type (EVT), within catchment',NULL,NULL,'StreamCat',NULL,NULL,NULL,1,NULL,NULL);
INSERT INTO lkpEnvVarsAqua VALUES('pctnonagintrodmanagvegcatrp100','pctnonagintrodmanagvegcatrp100',NULL,'% Nonagriculture nonnative introduced or managed vegetation landcover type reclassed from LANDFIRE Existing Vegetation Type (EVT), within catchment and within 100-m buffer of NHD stream lines',NULL,NULL,'StreamCat',NULL,NULL,NULL,0,NULL,NULL);
INSERT INTO lkpEnvVarsAqua VALUES('pctnonagintrodmanagvegws','pctnonagintrodmanagvegws',NULL,'% Nonagriculture nonnative introduced or managed vegetation landcover type reclassed from LANDFIRE Existing Vegetation Type (EVT), within watershed',NULL,NULL,'StreamCat',NULL,NULL,66,1,NULL,NULL);
INSERT INTO lkpEnvVarsAqua VALUES('pctnonagintrodmanagvegwsrp100','pctnonagintrodmanagvegwsrp100',NULL,'% Nonagriculture nonnative introduced or managed vegetation landcover type reclassed from LANDFIRE Existing Vegetation Type (EVT), within catchment and within 100-m buffer of NHD stream lines',NULL,NULL,'StreamCat',NULL,NULL,66,1,NULL,NULL);
INSERT INTO lkpEnvVarsAqua VALUES('pctnoncarbresidcat','pctnoncarbresidcat',NULL,'% of catchment area classified as lithology type: non-carbonate residual material',NULL,NULL,'StreamCat',NULL,NULL,NULL,0,NULL,NULL);
INSERT INTO lkpEnvVarsAqua VALUES('pctnoncarbresidws','pctnoncarbresidws',NULL,'% of watershed area classified as as lithology type: non-carbonate residual material',NULL,NULL,'StreamCat',NULL,NULL,NULL,1,NULL,NULL);
INSERT INTO lkpEnvVarsAqua VALUES('pctsiliciccat','pctsiliciccat',NULL,'% of catchment area classified as lithology type: silicic residual material',NULL,NULL,'StreamCat',NULL,NULL,48,0,NULL,NULL);
INSERT INTO lkpEnvVarsAqua VALUES('pctsilicicws','pctsilicicws',NULL,'% of watershed area classified as as lithology type: silicic residual material',NULL,NULL,'StreamCat',NULL,NULL,48,1,NULL,NULL);
INSERT INTO lkpEnvVarsAqua VALUES('pctwatercat','pctwatercat',NULL,'% of catchment area classified as lithology type: water',NULL,NULL,'StreamCat',NULL,NULL,NULL,0,NULL,NULL);
INSERT INTO lkpEnvVarsAqua VALUES('pctwaterws','pctwaterws',NULL,'% of watershed area classified as as lithology type: water',NULL,NULL,'StreamCat',NULL,NULL,NULL,1,NULL,NULL);
INSERT INTO lkpEnvVarsAqua VALUES('popden2010cat','popden2010cat',NULL,'Mean populating density (people/square km) within catchment',NULL,NULL,'StreamCat',NULL,NULL,77,1,NULL,NULL);
INSERT INTO lkpEnvVarsAqua VALUES('popden2010catrp100','popden2010catrp100',NULL,'Mean populating density (people/square km) within catchment and within a 100-m buffer of NHD stream lines',NULL,NULL,'StreamCat',NULL,NULL,77,0,NULL,NULL);
INSERT INTO lkpEnvVarsAqua VALUES('popden2010ws','popden2010ws',NULL,'Mean populating density (people/square km) within watershed',NULL,NULL,'StreamCat',NULL,NULL,78,1,NULL,NULL);
INSERT INTO lkpEnvVarsAqua VALUES('precip8110cat','precip8110cat',NULL,'PRISM climate data - 30-year normal mean precipitation (mm): Annual period: 1981-2010 within the catchment',NULL,NULL,'StreamCat',NULL,NULL,68,1,NULL,NULL);
INSERT INTO lkpEnvVarsAqua VALUES('precip8110ws','precip8110ws',NULL,'PRISM climate data - 30-year normal mean precipitation (mm): Annual period: 1981-2010 within the watershed',NULL,NULL,'StreamCat',NULL,NULL,68,1,NULL,NULL);
INSERT INTO lkpEnvVarsAqua VALUES('qc_ma','qc_ma',NULL,'Mean Annual Flow with Reference Gage Regression applied to QB (cfs).',NULL,NULL,'nhdplusv2',NULL,NULL,2,1,NULL,NULL);
INSERT INTO lkpEnvVarsAqua VALUES('qc_max_mth','qc_max_mth',NULL,'Maximum mean monthly flow (max value of QC_[01-12])',NULL,NULL,'nhplusv2_derived',NULL,NULL,2,1,NULL,NULL);
INSERT INTO lkpEnvVarsAqua VALUES('qc_min_mth','qc_min_mth',NULL,'Minimum mean monthly flow (min value of QC_[01-12])',NULL,NULL,'nhplusv2_derived',NULL,NULL,2,1,NULL,NULL);
INSERT INTO lkpEnvVarsAqua VALUES('rdcrscat','rdcrscat',NULL,'Density of roads-stream intersections (2010 Census Tiger Lines-NHD stream lines) within catchment (crossings/square km)',NULL,NULL,'StreamCat',NULL,NULL,NULL,1,NULL,NULL);
INSERT INTO lkpEnvVarsAqua VALUES('rdcrsslpwtdcat','rdcrsslpwtdcat',NULL,'Density of roads-stream intersections (2010 Census Tiger Lines-NHD stream lines) multiplied by NHDPlusV21 slope within catchment (crossings*slope/square km)',NULL,NULL,'StreamCat',NULL,NULL,NULL,1,NULL,NULL);
INSERT INTO lkpEnvVarsAqua VALUES('rdcrsws','rdcrsws',NULL,'Density of roads-stream intersections (2010 Census Tiger Lines-NHD stream lines) within watershed (crossings/square km)',NULL,NULL,'StreamCat',NULL,NULL,NULL,1,NULL,NULL);
INSERT INTO lkpEnvVarsAqua VALUES('rddenscat','rddenscat',NULL,'Density of roads (2010 Census Tiger Lines) within catchment (km/square km)',NULL,NULL,'StreamCat',NULL,NULL,71,0,NULL,NULL);
INSERT INTO lkpEnvVarsAqua VALUES('rddenscatrp100','rddenscatrp100',NULL,'Density of roads (2010 Census Tiger Lines) within catchment and within a 100-m buffer of NHD stream lines (km/square km)',NULL,NULL,'StreamCat',NULL,NULL,71,0,NULL,NULL);
INSERT INTO lkpEnvVarsAqua VALUES('rddensws','rddensws',NULL,'Density of roads (2010 Census Tiger Lines) within watershed (km/square km)',NULL,NULL,'StreamCat',NULL,NULL,72,1,NULL,NULL);
INSERT INTO lkpEnvVarsAqua VALUES('rddenswsrp100','rddenswsrp100',NULL,'Density of roads (2010 Census Tiger Lines) within watershed and within a 100-m buffer of NHD stream lines (km/square km)',NULL,NULL,'StreamCat',NULL,NULL,72,1,NULL,NULL);
INSERT INTO lkpEnvVarsAqua VALUES('runoffcat','runoffcat',NULL,'Mean runoff (mm) within catchment',NULL,NULL,'StreamCat',NULL,NULL,76,1,NULL,NULL);
INSERT INTO lkpEnvVarsAqua VALUES('runoffws','runoffws',NULL,'Mean runoff (mm) within watershed',NULL,NULL,'StreamCat',NULL,NULL,76,1,NULL,NULL);
INSERT INTO lkpEnvVarsAqua VALUES('scat','scat',NULL,'Mean % of lithological sulfur (S) content in surface or near surface geology within catchment',NULL,NULL,'StreamCat',NULL,NULL,NULL,0,NULL,NULL);
INSERT INTO lkpEnvVarsAqua VALUES('sin','sin',NULL,'Sinuousity of reach line.',NULL,NULL,'nhplusv2_derived',NULL,NULL,NULL,1,NULL,NULL);
INSERT INTO lkpEnvVarsAqua VALUES('sio2cat','sio2cat',NULL,'Mean % of lithological silicon dioxide (SiO2) content in surface or near surface geology within catchment',NULL,NULL,'StreamCat',NULL,NULL,29,0,NULL,NULL);
INSERT INTO lkpEnvVarsAqua VALUES('sio2ws','sio2ws',NULL,'Mean % of lithological silicon dioxide (SiO2) content in surface or near surface geology within watershed',NULL,NULL,'StreamCat',NULL,NULL,36,0,NULL,NULL);
INSERT INTO lkpEnvVarsAqua VALUES('slope','slope',NULL,'Slope of reach (max elevation - min elevation) / length).',NULL,NULL,'nhdplusv2',NULL,NULL,NULL,1,NULL,NULL);
INSERT INTO lkpEnvVarsAqua VALUES('sn_2008cat','sn_2008cat',NULL,'Annual gradient map of preciptiation-weighted mean deposition for average sulfur & nitrogen wet deposition for 2008 in kg of S+N/ha/yr, within catchment',NULL,NULL,'StreamCat',NULL,NULL,63,0,NULL,NULL);
INSERT INTO lkpEnvVarsAqua VALUES('sws','sws',NULL,'Mean % of lithological sulfur (S) content in surface or near surface geology within watershed',NULL,NULL,'StreamCat',NULL,NULL,NULL,0,NULL,NULL);
INSERT INTO lkpEnvVarsAqua VALUES('tmax8110cat','tmax8110cat',NULL,'PRISM climate data - 30-year normal maximum temperature (CÂ°): Annual period: 1981-2010 within the catchment',NULL,NULL,'StreamCat',NULL,NULL,69,1,NULL,NULL);
INSERT INTO lkpEnvVarsAqua VALUES('tmax8110ws','tmax8110ws',NULL,'PRISM climate data - 30-year normal maximum temperature (CÂ°): Annual period: 1981-2010 within the watershed',NULL,NULL,'StreamCat',NULL,NULL,69,1,NULL,NULL);
INSERT INTO lkpEnvVarsAqua VALUES('tmean8110cat','tmean8110cat',NULL,'PRISM climate data - 30-year normal mean temperature (CÂ°): Annual period: 1981-2010 within the catchment',NULL,NULL,'StreamCat',NULL,NULL,69,1,NULL,NULL);
INSERT INTO lkpEnvVarsAqua VALUES('tmean8110ws','tmean8110ws',NULL,'PRISM climate data - 30-year normal mean temperature (CÂ°): Annual period: 1981-2010 within the watershed',NULL,NULL,'StreamCat',NULL,NULL,69,1,NULL,NULL);
INSERT INTO lkpEnvVarsAqua VALUES('tmin8110cat','tmin8110cat',NULL,'PRISM climate data - 30-year normal minimum temperature (CÂ°): Annual period: 1981-2010 within the catchment',NULL,NULL,'StreamCat',NULL,NULL,70,1,NULL,NULL);
INSERT INTO lkpEnvVarsAqua VALUES('tmin8110ws','tmin8110ws',NULL,'PRISM climate data - 30-year normal minimum temperature (CÂ°): Annual period: 1981-2010 within the watershed',NULL,NULL,'StreamCat',NULL,NULL,70,1,NULL,NULL);
INSERT INTO lkpEnvVarsAqua VALUES('vc_ma','vc_ma',NULL,'Mean Annual Velocity for QC (fps).',NULL,NULL,'nhdplusv2',NULL,NULL,3,0,NULL,NULL);
INSERT INTO lkpEnvVarsAqua VALUES('vc_max_mth','vc_max_mth',NULL,'Maximum mean monthly flow velocity (max value of VC_[01-12])',NULL,NULL,'nhplusv2_derived',NULL,NULL,NULL,0,NULL,NULL);
INSERT INTO lkpEnvVarsAqua VALUES('vc_min_mth','vc_min_mth',NULL,'Minimum mean monthly flow velocity (min value of VC_[01-12])',NULL,NULL,'nhplusv2_derived',NULL,NULL,3,0,NULL,NULL);
INSERT INTO lkpEnvVarsAqua VALUES('wsareasqkm','wsareasqkm',NULL,'Watershed area (square km) at NHDPlus stream segment outlet, i.e., at the most downstream location of the vector line segment',NULL,NULL,'StreamCat',NULL,NULL,NULL,1,NULL,NULL);
CREATE TABLE `lkpEnvVars` (
	`fullName`	TEXT,
	`gridName`	TEXT,
	`fileName`	TEXT,
	`description`	TEXT,
	`dataType`	TEXT,
	`min`	TEXT,
	`max`	TEXT,
	`mean`	TEXT,
	`stdev`	TEXT,
	`multiplier`	TEXT,
	`windowShape`	TEXT,
	`windowSize`	TEXT,
	`derivedFrom`	TEXT,
	`originalSource`	TEXT,
	`comments`	TEXT,
	`distToGrid`	INTEGER,
	`correlatedVarGroupings`	INTEGER,
	`use_T`	INTEGER,
	PRIMARY KEY(`gridName`)
);
INSERT INTO lkpEnvVars VALUES('Elevation','elevx10','NED_2018_Esri_Elevation_x10_AlbConEA_Int_crop.tif','Elevation in decimeters (originally in meters)','continuous','-856','44128','','','10','n/a','n/a','1 arc-second digital elevation model','USGS NED','DEM prior to conversion from meters to centimeters is used as the basis for all other topo variables.',0,NULL,1);
INSERT INTO lkpEnvVars VALUES('Slope','slopex100','NED_2018_Esri_Slope_Deg_x100_Int_ACEA_32bInt_crop.tif','The inclination of slope in degrees.','continuous','0','8645','','','100','set by tool (square)','set by tool (3x3)','1 arc-second digital elevation model','USGS NED','',0,NULL,1);
INSERT INTO lkpEnvVars VALUES('Slope curvature','crvslpx100','NED_2018_Esri_Curvature_x100_1ArcSec_AlbConEA_Int_crop.tif','The curvature of a cell as fitted through that cell and its neighbors.','continuous','-2112386176','1948144384','','','100','set by tool (square)','set by tool (3x3)','1 arc-second digital elevation model','USGS NED','',0,NULL,1);
INSERT INTO lkpEnvVars VALUES('Profile curvature','crvprox100','NED_2018_Esri_ProfileCurvature_x100_1ArcSec_AlbConEA_Int_crop.tif','The curvature of a cell in the direction of the maximum slope. Affects the acceleration and deceleration of flow and, therefore, influences erosion and deposition','continuous','-980030912','1761490176','','','100','set by tool (square)','set by tool (3x3)','1 arc-second digital elevation model','USGS NED','',0,NULL,1);
INSERT INTO lkpEnvVars VALUES('Plan curvature','crvplax100','NED_2018_Esri_PlanCurvature_x100_1ArcSec_AlbConEA_Int_crop.tif','The curvature of a cell perpendicular to the direction of the maximum slope. Influences convergence and divergence of flow','continuous','-1776961152','1235005568','','','100','set by tool (square)','set by tool (3x3)','1 arc-second digital elevation model','USGS NED','',0,NULL,1);
INSERT INTO lkpEnvVars VALUES('Solar radiation summer solstice','radsumsol','NED_2018_Esri_SolarRad_SummerSol_AlbConEA_Int16_crop.tif','Total insolation derived from direct and diffuse, but not reflected, radiation for the summer solstice','continuous','3','8708','','','1','set by tool (square)','default (200 x 200 viewshed, sky direction, and sun position rasters)','1 arc-second digital elevation model','USGS NED','Expected to be the most important solar radiation variable. Team concurred to use all tool defaults.',0,6,1);
INSERT INTO lkpEnvVars VALUES('Solar radiation equinox','radequinx','NED_2018_Esri_SolarRad_Equinox_AlbConEA_Int16_crop.tif','Total insolation derived from direct and diffuse, but not reflected, radiation for the equinox','continuous','0','5982','','','1','set by tool (square)','default (200 by 200 viewshed, sky direction, and sun position rasters)','1 arc-second digital elevation model','USGS NED','Team agreement that this variable could be useful. Team concurred to use all tool defaults. Fall and spring equinox values are equivalent, so only need one variable instead of two as previously thought.',0,6,1);
INSERT INTO lkpEnvVars VALUES('Solar radiation winter solstice','radwinsol','NED_2018_Esri_SolarRad_WinterSol_AlbConEA_Int16_crop.tif','Total insolation derived from direct and diffuse, but not reflected, radiation for the winter solstice','continuous','0','2416','','','1','set by tool (square)','default (200 by 200 viewshed, sky direction, and sun position rasters)','1 arc-second digital elevation model','USGS NED','The findings of Piedallu and Gegout 2008 suggest that this one might be important. Team concurred to use all tool defaults.',0,6,1);
INSERT INTO lkpEnvVars VALUES('Roughness 1-cell square','rgh1cx100','NED_2018_Esri_Roughness_3x3_x100_AlbConEA_Int16u_crop.tif','The standard deviation of elevation values within the neighborhood immediately surrounding the center cell.','continuous','0','34354','','','100','square','1','1 arc-second digital elevation model','USGS NED','',0,NULL,1);
INSERT INTO lkpEnvVars VALUES('Roughness 10-cell circle','rgh10cx100','NED_2018_Esri_Roughness_10r_x100_AlbConEA_Int16u_crop.tif','The standard deviation of elevation values within a circular neighborhood with a radius of 10 cells.','continuous','0','41380','','','100','circle','10','1 arc-second digital elevation model','USGS NED','',0,NULL,1);
INSERT INTO lkpEnvVars VALUES('Roughness 100-cell circle','rgh100x100','NED_2018_Esri_Roughness_100r_x100_AlbConEA_Int16u_crop.tif','The standard deviation of elevation values within a circular neighborhood with a radius of 100 cells.','continuous','0','72932','','','100','circle','100','1 arc-second digital elevation model','USGS NED','',0,NULL,1);
INSERT INTO lkpEnvVars VALUES('Beers aspect','beersx1000','NED_2018_Esri_BeersAspect_x1000_AlbConEA_Int_crop.tif','Beers et al. (1966) transformation of slope direction. Original scale is 0 (SW, most exposed) to 2 (NE, most sheltered), with values grading equivalently in both directions between the extremes.','continuous','0','2000','','','1000','set by tool (square)','set by tool (3x3)','1 arc-second digital elevation model','USGS NED','Formula: Beers Aspect = (cos((45-Aspect)*pi/180)+1). Any cells with slope less than 3 degrees are assigned neutral value of 1.',0,NULL,1);
INSERT INTO lkpEnvVars VALUES('Dist to fresh waters','distfrwat','NHD_2017_Esri_DistanceToFreshWater_CONUS_AlbConEA_Int32_crop.tif','Euclidean distance to nearest stream, river, or other inland waterbody (excluding estuaries)','continuous','0','136470.6094','','','1','n/a','n/a','high-res hydrography features; 30-m pixel land cover; 1 arc-second digital elevation model','National Hydrography Dataset; National Land Cover Database; USGS NED','',1,NULL,1);
INSERT INTO lkpEnvVars VALUES('Dist to stream line','diststrml','NHD_2017_Esri_DistanceToStreamLine_CONUS_AlbConEA_Int32_crop.tif','Euclidean distance to nearest stream (features represented by lines)','continuous','','','','','1','n/a','n/a','high-res hydrography features; 30-m pixel land cover; 1 arc-second digital elevation model','National Hydrography Dataset; National Land Cover Database; USGS NED','',1,NULL,1);
INSERT INTO lkpEnvVars VALUES('Dist to lake or pond','distlake','NHD_2017_Esri_DistanceToLakePonds_CONUS_AlbConEA_Int32_crop.tif','Euclidean distance to nearest river or other inland waterbody (features represented by polygons)','continuous','0','158909.875','','','1','n/a','n/a','high-res hydrography features; 30-m pixel land cover; 1 arc-second digital elevation model','National Hydrography Dataset; National Land Cover Database; USGS NED','',1,NULL,1);
INSERT INTO lkpEnvVars VALUES('Dist to ocean','distocean','Sayre_2018_Esri_DistanceToOcean_CONUS2_AlbConEA_Int32_crop.tif','Euclidean distance to nearest sea/ocean','continuous','0','1912422','','','1','n/a','n/a','high-res hydrography features; 30-m pixel land cover; 1 arc-second digital elevation model','National Hydrography Dataset; National Land Cover Database; USGS NED','',1,NULL,1);
INSERT INTO lkpEnvVars VALUES('Flowpath dist to water or wetland','downdist','','The downslope distance along the flow path to a water or wetland feature.','continuous','','','','','TBD','n/a','n/a','flow direction and high-res hydrography','EPA/USGS NHD Plus v. 2; NHDH','In combination with Slope Length, replaces Relative Slope Position',1,NULL,1);
INSERT INTO lkpEnvVars VALUES('Topographic moisture','flowacc','USGS_2018_Esri_FlowAccumulation_AlbConEA_Int32_crop.tif','Flow accumulation is used as a proxy for topographic moisture. For each cell, this is determined by summing the weights of all cells flowing into it. This does not account for flow differences over different soil types.','continuous','0','327799330','','','n/a','n/a','n/a','30-m, medium-resolution flow accumulation rasters; high-resolution hydrography','EPA/USGS NHD Plus v. 2; NHDH','Flow accumulation rasters were obtained from NHDPlus. These rasters were mosaicked to produce a single raw flow accumulation raster. The raw flow accumulation was modified by truncating values at 99,999. Then, wetlands and open waters were "burned in" with values as follows: wetlands: 99,999; streams: 199,999; rivers and lakes: 299,999; estuaries: 399,999; oceans: 499,999.',0,NULL,1);
INSERT INTO lkpEnvVars VALUES('Canopy 1-cell mean','canopy1','NLCD_2011_ESRI_MeanUSFSCartoTreeCanopyPct_3x3r_AlbConEA_16bitU.tif','mean percent canopy cover in 1-cell radius (30 meter cells)','continuous','0','25500','','','100','square','1','decision-tree classification of circa 2011 Landsat satellite data (30m)','nlcd_2011_canopy_2011_edition_2014_10_10','',0,NULL,1);
INSERT INTO lkpEnvVars VALUES('Canopy 10-cell mean','canopy10','NLCD_2011_ESRI_MeanUSFSCartoTreeCanopyPct_10r_AlbConEA_16bitU.tif','mean percent canopy cover in 10-cell radius (30 meter cells)','continuous','0','25500','','','100','circle','10','decision-tree classification of circa 2011 Landsat satellite data (30m)','nlcd_2011_canopy_2011_edition_2014_10_10','',0,NULL,1);
INSERT INTO lkpEnvVars VALUES('Canopy 100-cell mean','canopy100','NLCD_2011_ESRI_MeanUSFSCartoTreeCanopyPct_100r_AlbConEA_16bitU.tif','mean percent canopy cover in 100-cell radius (30 meter cells)','continuous','0','25500','','','100','circle','100','decision-tree classification of circa 2011 Landsat satellite data (30m)','nlcd_2011_canopy_2011_edition_2014_10_10','',0,NULL,1);
INSERT INTO lkpEnvVars VALUES('Impervious surface 1-cell mean','impsur1','NLCD_2011_ESRI_MeanImperviousSurface_3x3r_AlbConEA_16butU.tif','mean percent impervious cover in 1-cell radius (30 meter cells)','continuous','0','10000','','','100','square','1','decision-tree classification of circa 2011 Landsat satellite data (30m)','nlcd_2011_impervious_2011_edition_2014_10_10','',0,NULL,1);
INSERT INTO lkpEnvVars VALUES('Impervious surface 10-cell mean','impsur10','NLCD_2011_ESRI_MeanImperviousSurface_10r_AlbConEA_16butU.tif','mean percent impervious cover in 10-cell radius (30 meter cells)','continuous','0','10000','','','100','circle','10','decision-tree classification of circa 2011 Landsat satellite data (30m)','nlcd_2011_impervious_2011_edition_2014_10_10','',0,NULL,1);
INSERT INTO lkpEnvVars VALUES('Impervious surface 100-cell mean','impsur100','NLCD_2011_ESRI_MeanImperviousSurface_100r_AlbConEA_16butU.tif','mean percent impervious cover in 100-cell radius (30 meter cells)','continuous','0','9700','','','100','circle','100','decision-tree classification of circa 2011 Landsat satellite data (30m)','nlcd_2011_impervious_2011_edition_2014_10_10','',0,NULL,1);
INSERT INTO lkpEnvVars VALUES('Coniferous forest cover 100-cell mean','nlcdcfr100','NLCD_2011_ESRI_MeanConiferousForest_100r_AlbConEA_16bitU.tif','mean coniferous forest cover within 100 cell radius','continuous','0','10000','','','10000','circle','100','decision-tree classification of circa 2011 Landsat satellite data (30m)','nlcd_2011_impervious_2011_edition_2014_10_10','',0,NULL,1);
INSERT INTO lkpEnvVars VALUES('Coniferous forest cover 10-cell mean','nlcdcfr10','NLCD_2011_ESRI_MeanConiferousForest_10r_AlbConEA_16bitU.tif','mean coniferous forest cover within 10-cell radius','continuous','0','10000','','','10000','circle','10','decision-tree classification of circa 2011 Landsat satellite data (30m)','nlcd_2011_impervious_2011_edition_2014_10_10','',0,NULL,1);
INSERT INTO lkpEnvVars VALUES('Coniferous forest cover 1-cell mean','nlcdcfr1','NLCD_2011_ESRI_MeanConiferousForest_3x3r_AlbConEA_16bitU.tif','mean coniferous forest cover within 1-cell radius','continuous','0','10000','','','10000','square','1','decision-tree classification of circa 2011 Landsat satellite data (30m)','nlcd_2011_impervious_2011_edition_2014_10_10','',0,NULL,1);
INSERT INTO lkpEnvVars VALUES('Deciduous forest cover 100-cell mean','nlcddfr100','NLCD_2011_ESRI_MeanDeciduousForest_100r_AlbConEA_16bitU.tif','mean deciduous forest cover within 100 cell radius','continuous','0','9999','','','10000','circle','100','decision-tree classification of circa 2011 Landsat satellite data (30m)','nlcd_2011_impervious_2011_edition_2014_10_10','',0,NULL,1);
INSERT INTO lkpEnvVars VALUES('Deciduous forest cover 10-cell mean','nlcddfr10','NLCD_2011_ESRI_MeanDeciduousForest_10r_AlbConEA_16bitU.tif','mean deciduous forest cover within 10-cell radius','continuous','0','10000','','','10000','circle','10','decision-tree classification of circa 2011 Landsat satellite data (30m)','nlcd_2011_impervious_2011_edition_2014_10_10','',0,NULL,1);
INSERT INTO lkpEnvVars VALUES('Deciduous forest cover 1-cell mean','nlcddfr1','NLCD_2011_ESRI_MeanDeciduousForest_3x3r_AlbConEA_16bitU.tif','mean deciduous forest cover within 1-cell radius','continuous','0','10000','','','10000','square','1','decision-tree classification of circa 2011 Landsat satellite data (30m)','nlcd_2011_impervious_2011_edition_2014_10_10','',0,NULL,1);
INSERT INTO lkpEnvVars VALUES('Open cover 1-cell mean','nlcdopn1','NLCD_2011_ESRI_MeanOpenCover_3x3r_AlbConEA_16bitU.tif','mean open cover within 1-cell radius','continuous','0','10000','','','10000','square','1','decision-tree classification of circa 2011 Landsat satellite data (30m)','nlcd_2011_landcover_2011_edition_2014_10_10','',0,NULL,1);
INSERT INTO lkpEnvVars VALUES('Open cover 10-cell mean','nlcdopn10','NLCD_2011_ESRI_MeanOpenCover_10r_AlbConEA_16bitU.tif','mean open cover within 10-cell radius','continuous','0','10000','','','10000','circle','10','decision-tree classification of circa 2011 Landsat satellite data (30m)','nlcd_2011_landcover_2011_edition_2014_10_10','',0,NULL,1);
INSERT INTO lkpEnvVars VALUES('Open cover 100-cell mean','nlcdopn100','NLCD_2011_ESRI_MeanOpenCover_100r_AlbConEA_16bitU.tif','mean open cover within 100 cell radius','continuous','0','10000','','','10000','circle','100','decision-tree classification of circa 2011 Landsat satellite data (30m)','nlcd_2011_landcover_2011_edition_2014_10_10','',0,NULL,1);
INSERT INTO lkpEnvVars VALUES('Shrub cover 1-cell mean','nlcdshb1','NLCD_2011_ESRI_MeanShrubScrub_3x3r_AlbConEA_16bitU.tif','mean shrub cover within 1-cell radius','continuous','0','10000','','','10000','square','1','decision-tree classification of circa 2011 Landsat satellite data (30m)','nlcd_2011_landcover_2011_edition_2014_10_10','',0,NULL,1);
INSERT INTO lkpEnvVars VALUES('Shrub cover 10-cell mean','nlcdshb10','NLCD_2011_ESRI_MeanShrubScrub_10r_AlbConEA_16bitU.tif','mean shrub cover within 10-cell radius','continuous','0','10000','','','10000','circle','10','decision-tree classification of circa 2011 Landsat satellite data (30m)','nlcd_2011_landcover_2011_edition_2014_10_10','',0,NULL,1);
INSERT INTO lkpEnvVars VALUES('Shrub cover 100-cell mean','nlcdshb100','NLCD_2011_ESRI_MeanShrubScrub_100r_AlbConEA_16bitU.tif','mean shrub cover within 100 cell radius','continuous','0','10000','','','10000','circle','100','decision-tree classification of circa 2011 Landsat satellite data (30m)','nlcd_2011_landcover_2011_edition_2014_10_10','',0,NULL,1);
INSERT INTO lkpEnvVars VALUES('Water cover 1-cell mean','nlcdwat1','NLCD_2011_ESRI_MeanOpenWater_3x3r_AlbConEA_16bitU.tif','mean open water cover within 1-cell radius','continuous','0','10000','','','10000','square','1','decision-tree classification of circa 2011 Landsat satellite data (30m)','nlcd_2011_landcover_2011_edition_2014_10_10','',0,NULL,1);
INSERT INTO lkpEnvVars VALUES('Water cover 10-cell mean','nlcdwat10','NLCD_2011_ESRI_MeanOpenWater_10r_AlbConEA_16bitU.tif','mean open water cover within 10-cell radius','continuous','0','10000','','','10000','circle','10','decision-tree classification of circa 2011 Landsat satellite data (30m)','nlcd_2011_landcover_2011_edition_2014_10_10','',0,NULL,1);
INSERT INTO lkpEnvVars VALUES('Water cover 100-cell mean','nlcdwat100','NLCD_2011_ESRI_MeanOpenWater_100r_AlbConEA_16bitU.tif','mean open water cover within 100 cell radius','continuous','0','10000','','','10000','circle','100','decision-tree classification of circa 2011 Landsat satellite data (30m)','nlcd_2011_landcover_2011_edition_2014_10_10','',0,NULL,1);
INSERT INTO lkpEnvVars VALUES('Wetland cover 1-cell mean','nlcdwet1','','mean wetland cover within 1-cell radius','continuous','','','','','10000','square','1','decision-tree classification of circa 2011 Landsat satellite data (30m)','nlcd_2011_landcover_2011_edition_2014_10_10','',0,NULL,1);
INSERT INTO lkpEnvVars VALUES('Wetland cover 10-cell mean','nlcdwet10','','mean wetland cover within 10-cell radius','continuous','','','','','10000','circle','10','decision-tree classification of circa 2011 Landsat satellite data (30m)','nlcd_2011_landcover_2011_edition_2014_10_10','',0,NULL,1);
INSERT INTO lkpEnvVars VALUES('Wetland cover 100-cell mean','nlcdwet100','','mean wetland cover within 100 cell radius','continuous','','','','','10000','circle','100','decision-tree classification of circa 2011 Landsat satellite data (30m)','nlcd_2011_landcover_2011_edition_2014_10_10','',0,NULL,1);
INSERT INTO lkpEnvVars VALUES('Woody wetlands 100-cell mean','nlcdwwt100','NLCD_2011_ESRI_MeanWoodyWetlands_100r_AlbConEA_16bitU.tif','mean woody wetland cover within 100 cell radius','','0','10000','','','','','','','','',0,NULL,1);
INSERT INTO lkpEnvVars VALUES('Woody wetlands 10-cell mean','nlcdwwt10','NLCD_2011_ESRI_MeanWoodyWetlands_10r_AlbConEA_16bitU.tif','mean woody wetland cover within 10 cell radius','','0','10000','','','','','','','','',0,NULL,1);
INSERT INTO lkpEnvVars VALUES('Woody wetlands 1-cell mean','nlcdwwt1','NLCD_2011_ESRI_MeanWoodyWetlands_3x3r_AlbConEA_16bitU.tif','mean woody wetland cover within 1 cell radius','','0','10000','','','','','','','','',0,NULL,1);
INSERT INTO lkpEnvVars VALUES('Dist to woody wetland','dnwiffw','NWI_2018_Esri_Dist2_ForestedFreshwaterWetlands_AlbConEA_Int32_naFill.tif','Distance to forested palustrine wetland','continuous','0','50000','','','1','n/a','n/a','National Wetland Inventory polygon data with attributes','National Wetland Inventory','',1,NULL,1);
INSERT INTO lkpEnvVars VALUES('Dist to fresh marsh','dnwifemw','NWI_2018_Esri_Dist2_FreshwaterEmergentWetlands_AlbConEA_Int32_naFill.tif','Distance to freshwater emergent wetland','continuous','0','50000','','','1','n/a','n/a','National Wetland Inventory polygon data with attributes','National Wetland Inventory','',1,NULL,1);
INSERT INTO lkpEnvVars VALUES('Dist to salt marsh','dnwisemw','NWI_2018_Esri_Dist2_SaltwaterEmergentWetlands_AlbConEA_Int32_naFill.tif','Distance to saltwater emergent wetland','continuous','0','1.00E+05','','','1','n/a','n/a','National Wetland Inventory polygon data with attributes','National Wetland Inventory','',1,NULL,1);
INSERT INTO lkpEnvVars VALUES('Growing degree days','gddays','','Growing degree days','continuous','','','','','1','n/a','n/a','30 year normals PRISM temperature data (tmin,tmax)','PRISM_tmax_30yr_normal_800mM2,PRISM_tmin_30yr_normal_800mM2','',0,NULL,1);
INSERT INTO lkpEnvVars VALUES('July precip','JulyPrecip','','July precipitation','continuous','','','','','100','n/a','n/a','','PRISM Climate Data monthly 30yr normal 800m tmin, tmax, ppt','Applied  Project Raster and then Extract by Mask on Original Data Source with Environmental Settings: Raster Analysis Cell Size 30m; Raster Storage Resample method Cubic convolution.',0,NULL,1);
INSERT INTO lkpEnvVars VALUES('June precip','JunePrecip','','June precipiation','continuous','','','','','100','n/a','3','','PRISM Climate Data monthly 30yr normal 800m tmin, tmax, ppt','Applied  Project Raster and then Extract by Mask on Original Data Source with Environmental Settings: Raster Analysis Cell Size 30m; Raster Storage Resample method Cubic convolution.',0,NULL,1);
INSERT INTO lkpEnvVars VALUES('May precip','MayPrecip','','May precipitation','continuous','','','','','100','n/a','3','','PRISM Climate Data monthly 30yr normal 800m tmin, tmax, ppt','Applied  Project Raster and then Extract by Mask on Original Data Source with Environmental Settings: Raster Analysis Cell Size 30m; Raster Storage Resample method Cubic convolution.',0,NULL,1);
INSERT INTO lkpEnvVars VALUES('Normalized dispersion of precip','NrmDspPrcp','','normalized dispersion (CV) of precipitation','continuous','','','','','100','square','3','','PRISM Climate Data monthly 30yr normal 800m tmin, tmax, ppt','Applied  Project Raster and then Extract by Mask on Original Data Source with Environmental Settings: Raster Analysis Cell Size 30m; Raster Storage Resample method Cubic convolution. Ran 30m monthly climate rasters through BioVars function in R producing 19 climatic variables.',0,NULL,1);
INSERT INTO lkpEnvVars VALUES('Precip of coldest quarter','PrcpCldQtr','','precipitation of coldest quarter','continuous','','','','','100','n/a','3','','PRISM Climate Data monthly 30yr normal 800m tmin, tmax, ppt','Applied  Project Raster and then Extract by Mask on Original Data Source with Environmental Settings: Raster Analysis Cell Size 30m; Raster Storage Resample method Cubic convolution. Ran 30m monthly climate rasters through BioVars function in R producing 19 climatic variables.',0,NULL,1);
INSERT INTO lkpEnvVars VALUES('Precip of driest month','PrcpDryMth','','precipitation of driest month','continuous','','','','','100','n/a','200','','PRISM Climate Data monthly 30yr normal 800m tmin, tmax, ppt','Applied  Project Raster and then Extract by Mask on Original Data Source with Environmental Settings: Raster Analysis Cell Size 30m; Raster Storage Resample method Cubic convolution. Ran 30m monthly climate rasters through BioVars function in R producing 19 climatic variables.',0,3,1);
INSERT INTO lkpEnvVars VALUES('Precip of driest quarter','PrcpDryQtr','','precipitation of driest quarter','continuous','','','','','100','n/a','n/a','','PRISM Climate Data monthly 30yr normal 800m tmin, tmax, ppt','Applied  Project Raster and then Extract by Mask on Original Data Source with Environmental Settings: Raster Analysis Cell Size 30m; Raster Storage Resample method Cubic convolution. Ran 30m monthly climate rasters through BioVars function in R producing 19 climatic variables.',0,3,1);
INSERT INTO lkpEnvVars VALUES('Precip of warmest quarter','PrcpWrmQtr','','precipitation of warmest quarter','continuous','','','','','100','n/a','n/a','','PRISM Climate Data monthly 30yr normal 800m tmin, tmax, ppt','Applied  Project Raster and then Extract by Mask on Original Data Source with Environmental Settings: Raster Analysis Cell Size 30m; Raster Storage Resample method Cubic convolution. Ran 30m monthly climate rasters through BioVars function in R producing 19 climatic variables.',0,4,1);
INSERT INTO lkpEnvVars VALUES('Precip of wettest month','PrcpWetMth','','precipitation of wettest month','continuous','','','','','100','n/a','n/a','','PRISM Climate Data monthly 30yr normal 800m tmin, tmax, ppt','Applied  Project Raster and then Extract by Mask on Original Data Source with Environmental Settings: Raster Analysis Cell Size 30m; Raster Storage Resample method Cubic convolution. Ran 30m monthly climate rasters through BioVars function in R producing 19 climatic variables.',0,4,1);
INSERT INTO lkpEnvVars VALUES('Precip of wettest quarter','PrcpWetQtr','','precipitation of wettest quarter','continuous','','','','','100','n/a','n/a','','PRISM Climate Data monthly 30yr normal 800m tmin, tmax, ppt','Applied  Project Raster and then Extract by Mask on Original Data Source with Environmental Settings: Raster Analysis Cell Size 30m; Raster Storage Resample method Cubic convolution. Ran 30m monthly climate rasters through BioVars function in R producing 19 climatic variables.',0,4,1);
INSERT INTO lkpEnvVars VALUES('Total annual precip','TtlAnnPrcp','','total annual precipitation','continuous','','','','','100','n/a','n/a','','PRISM Climate Data monthly 30yr normal 800m tmin, tmax, ppt','Applied  Project Raster and then Extract by Mask on Original Data Source with Environmental Settings: Raster Analysis Cell Size 30m; Raster Storage Resample method Cubic convolution. Ran 30m monthly climate rasters through BioVars function in R producing 19 climatic variables.',0,NULL,1);
INSERT INTO lkpEnvVars VALUES('Annual mean temp','AnnMnTemp','','annual mean temperature','continuous','','','','','100','n/a','n/a','','PRISM Climate Data monthly 30yr normal 800m tmin, tmax, ppt','Applied  Project Raster and then Extract by Mask on Original Data Source with Environmental Settings: Raster Analysis Cell Size 30m; Raster Storage Resample method Cubic convolution. Ran 30m monthly climate rasters through BioVars function in R producing 19 climatic variables.',0,NULL,1);
INSERT INTO lkpEnvVars VALUES('Isothermality','Isotherm','','comparison of day-to-night and summer-to-winter temperature oscillations','continuous','','','','','100','n/a','n/a','','PRISM Climate Data monthly 30yr normal 800m tmin, tmax, ppt','Applied  Project Raster and then Extract by Mask on Original Data Source with Environmental Settings: Raster Analysis Cell Size 30m; Raster Storage Resample method Cubic convolution. Ran 30m monthly climate rasters through BioVars function in R producing 19 climatic variables.',0,NULL,1);
INSERT INTO lkpEnvVars VALUES('Max temp of warmest month','MxTpWrmMth','','maximum temperature of warmest month','continuous','','','','','100','n/a','n/a','','PRISM Climate Data monthly 30yr normal 800m tmin, tmax, ppt','Applied  Project Raster and then Extract by Mask on Original Data Source with Environmental Settings: Raster Analysis Cell Size 30m; Raster Storage Resample method Cubic convolution. Ran 30m monthly climate rasters through BioVars function in R producing 19 climatic variables.',0,2,1);
INSERT INTO lkpEnvVars VALUES('Mean diurnal range','MnDiurnRng','','(mean of monthly (max temp - min temp))','continuous','','','','','100','n/a','n/a','','PRISM Climate Data monthly 30yr normal 800m tmin, tmax, ppt','Applied  Project Raster and then Extract by Mask on Original Data Source with Environmental Settings: Raster Analysis Cell Size 30m; Raster Storage Resample method Cubic convolution. Ran 30m monthly climate rasters through BioVars function in R producing 19 climatic variables.',0,NULL,1);
INSERT INTO lkpEnvVars VALUES('Mean temp of coldest quarter','MnTpCldQtr','','mean temperature of coldest quarter','continuous','','','','','100','n/a','n/a','','PRISM Climate Data monthly 30yr normal 800m tmin, tmax, ppt','Applied  Project Raster and then Extract by Mask on Original Data Source with Environmental Settings: Raster Analysis Cell Size 30m; Raster Storage Resample method Cubic convolution. Ran 30m monthly climate rasters through BioVars function in R producing 19 climatic variables.',0,1,1);
INSERT INTO lkpEnvVars VALUES('Mean temp of driest quarter','MnTpDryQtr','','mean temperature of driest quarter','continuous','','','','','100','n/a','n/a','','PRISM Climate Data monthly 30yr normal 800m tmin, tmax, ppt','Applied  Project Raster and then Extract by Mask on Original Data Source with Environmental Settings: Raster Analysis Cell Size 30m; Raster Storage Resample method Cubic convolution. Ran 30m monthly climate rasters through BioVars function in R producing 19 climatic variables.',0,NULL,1);
INSERT INTO lkpEnvVars VALUES('Mean temp of warmest quarter','MnTpWrmQtr','','mean temperature of warmest quarter','continuous','','','','','100','n/a','n/a','','PRISM Climate Data monthly 30yr normal 800m tmin, tmax, ppt','Applied  Project Raster and then Extract by Mask on Original Data Source with Environmental Settings: Raster Analysis Cell Size 30m; Raster Storage Resample method Cubic convolution. Ran 30m monthly climate rasters through BioVars function in R producing 19 climatic variables.',0,2,1);
INSERT INTO lkpEnvVars VALUES('Mean temp of wettest quarter','MnTpWetQtr','','mean temperature of wettest quarter','continuous','','','','','100','n/a','n/a','','PRISM Climate Data monthly 30yr normal 800m tmin, tmax, ppt','Applied  Project Raster and then Extract by Mask on Original Data Source with Environmental Settings: Raster Analysis Cell Size 30m; Raster Storage Resample method Cubic convolution. Ran 30m monthly climate rasters through BioVars function in R producing 19 climatic variables.',0,NULL,1);
INSERT INTO lkpEnvVars VALUES('Min temp of coldest month','MnTpCldMth','','minimum temperature of coldest month','continuous','','','','','100','n/a','n/a','','PRISM Climate Data monthly 30yr normal 800m tmin, tmax, ppt','Applied  Project Raster and then Extract by Mask on Original Data Source with Environmental Settings: Raster Analysis Cell Size 30m; Raster Storage Resample method Cubic convolution. Ran 30m monthly climate rasters through BioVars function in R producing 19 climatic variables.',0,1,1);
INSERT INTO lkpEnvVars VALUES('Temp annual range','TempAnnRng','','(max temp warmest month - min temp coldest month)','continuous','','','','','100','n/a','n/a','','PRISM Climate Data monthly 30yr normal 800m tmin, tmax, ppt','Applied  Project Raster and then Extract by Mask on Original Data Source with Environmental Settings: Raster Analysis Cell Size 30m; Raster Storage Resample method Cubic convolution. Ran 30m monthly climate rasters through BioVars function in R producing 19 climatic variables.',0,5,1);
INSERT INTO lkpEnvVars VALUES('Temp seasonality','TempSeason','','(STD * 100)','continuous','','','','','100','n/a','n/a','','PRISM Climate Data monthly 30yr normal 800m tmin, tmax, ppt','Applied  Project Raster and then Extract by Mask on Original Data Source with Environmental Settings: Raster Analysis Cell Size 30m; Raster Storage Resample method Cubic convolution. Ran 30m monthly climate rasters through BioVars function in R producing 19 climatic variables.',0,5,1);
INSERT INTO lkpEnvVars VALUES('Dist to river','distriver','','Euclidean distance to nearest stream/river','continuous','','','','','1','n/a','n/a','high-res hydrography features; 30-m pixel land cover; 1 arc-second digital elevation model','National Hydrography Dataset; National Land Cover Database; USGS NED','',1,NULL,1);
INSERT INTO lkpEnvVars VALUES('Dist to sm pond','distsmpond','','Euclidean distance to nearest lake/pond/resevoir <= 1 ha','continuous','','','','','1','n/a','n/a','high-res hydrography features; 30-m pixel land cover; 1 arc-second digital elevation model','National Hydrography Dataset; National Land Cover Database; USGS NED','',1,NULL,1);
INSERT INTO lkpEnvVars VALUES('Dist to lg lake','distlglake','','Euclidean distance to nearest lake/pond/resevoir > 1 ha','continuous','','','','','1','n/a','n/a','high-res hydrography features; 30-m pixel land cover; 1 arc-second digital elevation model','National Hydrography Dataset; National Land Cover Database; USGS NED','',1,NULL,1);
INSERT INTO lkpEnvVars VALUES('Topographic postion index 1-cell square','tp001x1000','NED_2018_Esri_TPI_3x3_x1000_AlbConEA_Int32_crop.tif','Topographic position index using elevation values within the neighborhood immediately surrounding the center cell','continuous','-2147483648','881397','','','1000','square','1','1 arc-second digital elevation model','USGS NED','',0,NULL,1);
INSERT INTO lkpEnvVars VALUES('Topographic postion index 10-cell radius','tp010x1000','NED_2018_Esri_TPI_10r_x1000_AlbConEA_Int32_crop.tif','Topographic position index using elevation values within a circular neighborhood with a radius of 10 cells.','continuous','-2147483648','1063501','','','1000','circle','10','1 arc-second digital elevation model','USGS NED','',0,NULL,1);
INSERT INTO lkpEnvVars VALUES('Topographic postion index 100-cell radius','tp100x1000','NED_2018_Esri_TPI_100r_x1000_AlbConEA_Int32_crop.tif','Topographic position index using elevation values within a circular neighborhood with a radius of 100 cells.','continuous','-2147483648','1139643','','','1000','circle','100','1 arc-second digital elevation model','USGS NED','',0,NULL,1);
INSERT INTO lkpEnvVars VALUES('Dist to Carbonate Residual Material','geoCarb','USGS_1_Dist2CarbResMat_crop_naFill.tif','Euclidean distance to surficial geology type: Carbonate Residual Material','continuous','0','50000','','','','n/a','n/a','1km Surficial Geology Layer','USGS Surficial Lithology, https://pubs.usgs.gov/sim/3126/','',1,NULL,1);
INSERT INTO lkpEnvVars VALUES('Dist to Non-Carbonate Residual Material','geoNCarb','USGS_3_Dist2NonCarbResMat_crop_naFill.tif','Euclidean distance to surficial geology type: Non-Carbonate Residual Material','continuous','0','50000','','','','n/a','n/a','1km Surficial Geology Layer','USGS Surficial Lithology, https://pubs.usgs.gov/sim/3126/','',1,NULL,1);
INSERT INTO lkpEnvVars VALUES('Dist to Alkaline Intrusive Volcanic Rock','geoAlk','USGS_4_Dist2AlkIntVolRock_crop_naFill.tif','Euclidean distance to surficial geology type: Alkaline Intrusive Volcanic Rock','continuous','','','','','','n/a','n/a','1km Surficial Geology Layer','USGS Surficial Lithology, https://pubs.usgs.gov/sim/3126/','',1,NULL,1);
INSERT INTO lkpEnvVars VALUES('Dist to Silicic Residual Material','geoSilic','USGS_5_Dist2Silicic2_int_ND_crop.tif','Euclidean distance to surficial geology type: Silicic Residual Material','continuous','','','','','','n/a','n/a','1km Surficial Geology Layer','USGS Surficial Lithology, https://pubs.usgs.gov/sim/3126/','',1,NULL,1);
INSERT INTO lkpEnvVars VALUES('Dist to Extrusive Volcanic Rock','geoExtru','USGS_7_Dist2ExtrVolRock_crop_naFill.tif','Euclidean distance to surficial geology type: Extrusive Volcanic Rock','continuous','','','','','','n/a','n/a','1km Surficial Geology Layer','USGS Surficial Lithology, https://pubs.usgs.gov/sim/3126/','',1,NULL,1);
INSERT INTO lkpEnvVars VALUES('Dist to Colluvial Sediment','geoCollu','USGS_8_Dist2CollSed_crop_naFill.tif','Euclidean distance to surficial geology type: Colluvial Sediment','continuous','','','','','','n/a','n/a','1km Surficial Geology Layer','USGS Surficial Lithology, https://pubs.usgs.gov/sim/3126/','',1,NULL,1);
INSERT INTO lkpEnvVars VALUES('Dist to Clayey Glacial Till','geoClay','USGS_9_Dist2GlacialClay_crop_naFill.tif','Euclidean distance to surficial geology type: Glacial Till, Clayey','continuous','0','50000','','','','n/a','n/a','1km Surficial Geology Layer','USGS Surficial Lithology, https://pubs.usgs.gov/sim/3126/','',1,NULL,1);
INSERT INTO lkpEnvVars VALUES('Dist to Loamy Glacial Till','geoLoam','USGS_10_Dist2GlacialLoam_crop_naFill.tif','Euclidean distance to surficial geology type: Glacial Till, Loamy','continuous','0','50000','','','','n/a','n/a','1km Surficial Geology Layer','USGS Surficial Lithology, https://pubs.usgs.gov/sim/3126/','',1,NULL,1);
INSERT INTO lkpEnvVars VALUES('Dist to Coarse Glacial Till','geoCTill','USGS_11_Dist2GlacialCoarse2_int_ND_crop.tif','Euclidean distance to surficial geology type: Glacial Till, Coarse-Textured','continuous','','','','','','n/a','n/a','1km Surficial Geology Layer','USGS Surficial Lithology, https://pubs.usgs.gov/sim/3126/','',1,NULL,1);
INSERT INTO lkpEnvVars VALUES('Dist to Coarse Glacial Outwash','geoCOutw','USGS_14_Dist2GlacialOutwashCoarse_crop_naFill.tif','Euclidean distance to surficial geology type: Glacial Outwash and Glacial Lake Sediment, Coarse-Textured*','continuous','','','','','','n/a','n/a','1km Surficial Geology Layer','USGS Surficial Lithology, https://pubs.usgs.gov/sim/3126/','',1,NULL,1);
INSERT INTO lkpEnvVars VALUES('Dist to Fine Glacial Lake Sediment','geoFSed','USGS_13_Dist2GlacialOutWashFine_crop_naFill.tif','Euclidean distance to surficial geology type: Glacial Lake Sediment, Fine-Textured','continuous','','','','','','n/a','n/a','1km Surficial Geology Layer','USGS Surficial Lithology, https://pubs.usgs.gov/sim/3126/','',1,NULL,1);
INSERT INTO lkpEnvVars VALUES('Dist to Peat and Muck','geoPeat','USGS_15_Dist2HydricPeatMuck_crop_naFill.tif','Euclidean distance to surficial geology type: Hydric, Peat and Muck','continuous','','','','','','n/a','n/a','1km Surficial Geology Layer','USGS Surficial Lithology, https://pubs.usgs.gov/sim/3126/','',1,NULL,1);
INSERT INTO lkpEnvVars VALUES('Dist to Sand Dunes','geoSDune','USGS_16_Dist2SandDunes_crop_naFill.tif','Euclidean distance to surficial geology type: Eolian Sediment, Coarse-Textured (Sand Dunes)','continuous','0','50000','','','','n/a','n/a','1km Surficial Geology Layer','USGS Surficial Lithology, https://pubs.usgs.gov/sim/3126/','',1,NULL,1);
INSERT INTO lkpEnvVars VALUES('Dist to Glacial Loess','geoLoess','USGS_17_Dist2GlacialLoess_crop_naFill.tif','Euclidean distance to surficial geology type: Eolian Sediment, Fine-Textured (Glacial Loess)','continuous','0','50000','','','','n/a','n/a','1km Surficial Geology Layer','USGS Surficial Lithology, https://pubs.usgs.gov/sim/3126/','',1,NULL,1);
INSERT INTO lkpEnvVars VALUES('Dist to Saline Lake Sediment','geoSalSed','USGS_18_Dist2SalineLakeSed_int_ND_crop.tif','Euclidean distance to surficial geology type: Saline Lake Sediment','continuous','','','','','','n/a','n/a','1km Surficial Geology Layer','USGS Surficial Lithology, https://pubs.usgs.gov/sim/3126/','',1,NULL,1);
INSERT INTO lkpEnvVars VALUES('Dist to Fine Coastal Zone Sediment','geoFCSed','USGS_19_Dist2AlluviumFineCoastal_crop_naFill.tif','Euclidean distance to surficial geology type: Alluvium and Fine-Textured Coastal Zone Sediment','continuous','','','','','','n/a','n/a','1km Surficial Geology Layer','USGS Surficial Lithology, https://pubs.usgs.gov/sim/3126/','',1,NULL,1);
INSERT INTO lkpEnvVars VALUES('Dist to Coarse Coastal Zone Sediment','geoCCSed','USGS_20_Dist2CoastalSedCoarse_crop_naFill.tif','Euclidean distance to surficial geology type: Coastal Zone Sediment, Coarse-Textured','continuous','0','50000','','','','n/a','n/a','1km Surficial Geology Layer','USGS Surficial Lithology, https://pubs.usgs.gov/sim/3126/','',1,NULL,1);
INSERT INTO lkpEnvVars VALUES('Dist to stream poly','diststrmp','NHD_2017_Esri_DistanceToStreamPoly_CONUS_AlbConEA_Int32_crop.tif','Euclidean distance to nearest stream (features represented by polygons)','continuous','','','','','1','n/a','n/a','high-res hydrography features; 30-m pixel land cover; 1 arc-second digital elevation model','National Hydrography Dataset; National Land Cover Database; USGS NED','',1,NULL,1);
INSERT INTO lkpEnvVars VALUES('Dist to Gabbro','distgabbro','DistanceToGabbro.tif','Euclidean distance to gabbro geology (PTYPE = gb)','continuous','30','65535','','','','n/a','n/a','shapefile containing CA geology types','Geologic Map of California (Jennings et al. 2013)','',1,'',1);
INSERT INTO lkpEnvVars VALUES('Dist to Limestone','distlmstne','DistanceToLimestone.tif','Euclidean distance to limestone geology (PTYPE = C, D, ls)','continuous','30','65535','','','','n/a','n/a','shapefile containing CA geology types','Geologic Map of California (Jennings et al. 2013)','',1,'',1);
INSERT INTO lkpEnvVars VALUES('Dist to Serpentine','distsrpntn','DistanceToSerpentine.tif','Euclidean distance to serpentine geology (PTYPE = um)','continuous','30','65535','','','','n/a','n/a','shapefile containing CA geology types','Geologic Map of California (Jennings et al. 2013)','',1,'',1);
INSERT INTO lkpEnvVars VALUES('Beach Dune Coastal Grassland 1-cell mean','beachdn1','BeachDuneCstlGrassland_3x3.tif','mean percent beach dune coastal grassland cover in 1-cell radius (30 meter cells)','continuous','11','90','','','','square','1','high-res 30-m pixel land cover','FL Cooperative Land Cover Dataset','',0,'',1);
INSERT INTO lkpEnvVars VALUES('Beach Dune Coastal Grassland 10-cell mean','beachdn10','BeachDuneCstlGrassland_10cells.tif','mean percent beach dune coastal grassland cover in 10-cell radius (30 meter cells)','continuous','1','100','','','','circle','10','high-res 30-m pixel land cover','FL Cooperative Land Cover Dataset','',0,'',1);
INSERT INTO lkpEnvVars VALUES('Beach Dune Coastal Grassland 100-cell mean','beachdn100','BeachDuneCstlGrassland_100cells.tif','mean percent beach dune coastal grassland cover in 100-cell radius (30 meter cells)','continuous','1','100','','','','circle','100','high-res 30-m pixel land cover','FL Cooperative Land Cover Dataset','',0,'',1);
INSERT INTO lkpEnvVars VALUES('Coastal Strand 1-cell mean','coastal1','CoastalStrand_3x3.tif','mean percent coastal strand cover in 1-cell radius (30 meter cells)','continuous','11','100','','','','square','1','high-res 30-m pixel land cover','FL Cooperative Land Cover Dataset','',0,'',1);
INSERT INTO lkpEnvVars VALUES('Coastal Strand 10-cell mean','coastal10','CoastalStrand_10cells.tif','mean percent coastal strand cover in 10-cell radius (30 meter cells)','continuous','1','98','','','','circle','10','high-res 30-m pixel land cover','FL Cooperative Land Cover Dataset','',0,'',1);
INSERT INTO lkpEnvVars VALUES('Coastal Strand 100-cell mean','coastal100','CoastalStrand_100cells.tif','mean percent coastal strand cover in 100-cell radius (30 meter cells)','continuous','1','21','','','','circle','100','high-res 30-m pixel land cover','FL Cooperative Land Cover Dataset','',0,'',1);
INSERT INTO lkpEnvVars VALUES('Dry Prairie 1-cell mean','prairie1','DryPrairie_3x3.tif','mean percent dry prairie cover in 1-cell radius (30 meter cells)','continuous','11','100','','','','square','1','high-res 30-m pixel land cover','FL Cooperative Land Cover Dataset','',0,'',1);
INSERT INTO lkpEnvVars VALUES('Dry Prairie 10-cell mean','prairie10','DryPrairie_10cells.tif','mean percent dry prairie cover in 10-cell radius (30 meter cells)','continuous','1','100','','','','circle','10','high-res 30-m pixel land cover','FL Cooperative Land Cover Dataset','',0,'',1);
INSERT INTO lkpEnvVars VALUES('Dry Prairie 100-cell mean','prairie100','DryPrairie_100cells.tif','mean percent dry prairie cover in 100-cell radius (30 meter cells)','continuous','1','68','','','','circle','100','high-res 30-m pixel land cover','FL Cooperative Land Cover Dataset','',0,'',1);
INSERT INTO lkpEnvVars VALUES('Keys Tidal Rock Barren 1-cell mean','keytidl1','KeysTidalRockBarren_3x3.tif','mean percent keys tidal rock barren cover in 1-cell radius (30 meter cells)','continuous','11','100','','','','square','1','high-res 30-m pixel land cover','FL Cooperative Land Cover Dataset','',0,'',1);
INSERT INTO lkpEnvVars VALUES('Keys Tidal Rock Barren 10-cell mean','keytidl10','KeysTidalRockBarren_10cells.tif','mean percent keys tidal rock barren cover in 10-cell radius (30 meter cells)','continuous','1','93','','','','circle','10','high-res 30-m pixel land cover','FL Cooperative Land Cover Dataset','',0,'',1);
INSERT INTO lkpEnvVars VALUES('Keys Tidal Rock Barren 100-cell mean','keytidl100','KeysTidalRockBarren_100cells.tif','mean percent keys tidal rock barren cover in 100-cell radius (30 meter cells)','continuous','1','18','','','','circle','100','high-res 30-m pixel land cover','FL Cooperative Land Cover Dataset','',0,'',1);
INSERT INTO lkpEnvVars VALUES('Mangrove Swamp 1-cell mean','mangrve1','MangroveSwamp_3x3.tif','mean percent mangrove swamp cover in 1-cell radius (30 meter cells)','continuous','11','100','','','','square','1','high-res 30-m pixel land cover','FL Cooperative Land Cover Dataset','',0,'',1);
INSERT INTO lkpEnvVars VALUES('Mangrove Swamp 10-cell mean','mangrve10','MangroveSwamp_10cells.tif','mean percent mangrove swamp cover in 10-cell radius (30 meter cells)','continuous','1','100','','','','circle','10','high-res 30-m pixel land cover','FL Cooperative Land Cover Dataset','',0,'',1);
INSERT INTO lkpEnvVars VALUES('Mangrove Swamp 100-cell mean','mangrve100','MangroveSwamp_100cells.tif','mean percent mangrove swamp cover in 100-cell radius (30 meter cells)','continuous','1','100','','','','circle','100','high-res 30-m pixel land cover','FL Cooperative Land Cover Dataset','',0,'',1);
INSERT INTO lkpEnvVars VALUES('Maritime Hammock 1-cell mean','maritim1','MaritimeHammock_3x3.tif','mean percent maritime hammock cover in 1-cell radius (30 meter cells)','continuous','11','100','','','','square','1','high-res 30-m pixel land cover','FL Cooperative Land Cover Dataset','',0,'',1);
INSERT INTO lkpEnvVars VALUES('Maritime Hammock 10-cell mean','maritim10','MaritimeHammock_10cells.tif','mean percent maritime hammock cover in 10-cell radius (30 meter cells)','continuous','1','100','','','','circle','10','high-res 30-m pixel land cover','FL Cooperative Land Cover Dataset','',0,'',1);
INSERT INTO lkpEnvVars VALUES('Maritime Hammock 100-cell mean','maritim100','MaritimeHammock_100cells.tif','mean percent maritime hammock cover in 100-cell radius (30 meter cells)','continuous','1','27','','','','circle','100','high-res 30-m pixel land cover','FL Cooperative Land Cover Dataset','',0,'',1);
INSERT INTO lkpEnvVars VALUES('Pine Rockland 1-cell mean','pinerck1','PineRockland_3x3.tif','mean percent pine rockland cover in 1-cell radius (30 meter cells)','continuous','11','100','','','','square','1','high-res 30-m pixel land cover','FL Cooperative Land Cover Dataset','',0,'',1);
INSERT INTO lkpEnvVars VALUES('Pine Rockland 10-cell mean','pinerck10','PineRockland_10cells.tif','mean percent pine rockland cover in 10-cell radius (30 meter cells)','continuous','1','100','','','','circle','10','high-res 30-m pixel land cover','FL Cooperative Land Cover Dataset','',0,'',1);
INSERT INTO lkpEnvVars VALUES('Pine Rockland 100-cell mean','pinerck100','PineRockland_100cells.tif','mean percent pine rockland cover in 100-cell radius (30 meter cells)','continuous','1','69','','','','circle','100','high-res 30-m pixel land cover','FL Cooperative Land Cover Dataset','',0,'',1);
INSERT INTO lkpEnvVars VALUES('Rockland Hammock 1-cell mean','rckhmck1','RocklandHammock_3x3.tif','mean percent rockland hammock cover in 1-cell radius (30 meter cells)','continuous','11','100','','','','square','1','high-res 30-m pixel land cover','FL Cooperative Land Cover Dataset','',0,'',1);
INSERT INTO lkpEnvVars VALUES('Rockland Hammock 10-cell mean','rckhmck10','RocklandHammock_10cells.tif','mean percent rockland hammock cover in 10-cell radius (30 meter cells)','continuous','1','100','','','','circle','10','high-res 30-m pixel land cover','FL Cooperative Land Cover Dataset','',0,'',1);
INSERT INTO lkpEnvVars VALUES('Rockland Hammock 100-cell mean','rckhmck100','RocklandHammock_100cells.tif','mean percent rockland hammock cover in 100-cell radius (30 meter cells)','continuous','1','25','','','','circle','100','high-res 30-m pixel land cover','FL Cooperative Land Cover Dataset','',0,'',1);
INSERT INTO lkpEnvVars VALUES('Salt Marsh 1-cell mean','sltmrsh1','SaltMarsh_3x3.tif','mean percent salt marsh cover in 1-cell radius (30 meter cells)','continuous','11','100','','','','square','1','high-res 30-m pixel land cover','FL Cooperative Land Cover Dataset','',0,'',1);
INSERT INTO lkpEnvVars VALUES('Salt Marsh 10-cell mean','sltmrsh10','SaltMarsh_10cells.tif','mean percent salt marsh cover in 10-cell radius (30 meter cells)','continuous','1','100','','','','circle','10','high-res 30-m pixel land cover','FL Cooperative Land Cover Dataset','',0,'',1);
INSERT INTO lkpEnvVars VALUES('Salt Marsh 100-cell mean','sltmrsh100','SaltMarsh_100cells.tif','mean percent salt marsh cover in 100-cell radius (30 meter cells)','continuous','1','100','','','','circle','100','high-res 30-m pixel land cover','FL Cooperative Land Cover Dataset','',0,'',1);
INSERT INTO lkpEnvVars VALUES('Sand Beach Dry 1-cell mean','sandbch1','SandBeachDry_3x3.tif','mean percent sand beach dry cover in 1-cell radius (30 meter cells)','continuous','11','100','','','','square','1','high-res 30-m pixel land cover','FL Cooperative Land Cover Dataset','',0,'',1);
INSERT INTO lkpEnvVars VALUES('Sand Beach Dry 10-cell mean','sandbch10','SandBeachDry_10cells.tif','mean percent sand beach dry cover in 10-cell radius (30 meter cells)','continuous','1','100','','','','circle','10','high-res 30-m pixel land cover','FL Cooperative Land Cover Dataset','',0,'',1);
INSERT INTO lkpEnvVars VALUES('Sand Beach Dry 100-cell mean','sandbch100','SandBeachDry_100cells.tif','mean percent sand beach dry cover in 100-cell radius (30 meter cells)','continuous','1','100','','','','circle','100','high-res 30-m pixel land cover','FL Cooperative Land Cover Dataset','',0,'',1);
INSERT INTO lkpEnvVars VALUES('Sandhill 1-cell mean','sandhil1','Sandhill_3x3.tif','mean percent sandhill cover in 1-cell radius (30 meter cells)','continuous','11','100','','','','square','1','high-res 30-m pixel land cover','FL Cooperative Land Cover Dataset','',0,'',1);
INSERT INTO lkpEnvVars VALUES('Sandhill 10-cell mean','sandhil10','Sandhill_10cells.tif','mean percent sandhill cover in 10-cell radius (30 meter cells)','continuous','1','100','','','','circle','10','high-res 30-m pixel land cover','FL Cooperative Land Cover Dataset','',0,'',1);
INSERT INTO lkpEnvVars VALUES('Sandhill 100-cell mean','sandhil100','Sandhill_100cells.tif','mean percent sandhill cover in 100-cell radius (30 meter cells)','continuous','1','97','','','','circle','100','high-res 30-m pixel land cover','FL Cooperative Land Cover Dataset','',0,'',1);
INSERT INTO lkpEnvVars VALUES('Scrub 1-cell mean','scrub1','Scrub_3x3.tif','mean percent scrub cover in 1-cell radius (30 meter cells)','continuous','11','100','','','','square','1','high-res 30-m pixel land cover','FL Cooperative Land Cover Dataset','',0,'',1);
INSERT INTO lkpEnvVars VALUES('Scrub 10-cell mean','scrub10','Scrub_10cells.tif','mean percent scrub cover in 10-cell radius (30 meter cells)','continuous','1','100','','','','circle','10','high-res 30-m pixel land cover','FL Cooperative Land Cover Dataset','',0,'',1);
INSERT INTO lkpEnvVars VALUES('Scrub 100-cell mean','scrub100','Scrub_100cells.tif','mean percent scrub cover in 100-cell radius (30 meter cells)','continuous','1','98','','','','circle','100','high-res 30-m pixel land cover','FL Cooperative Land Cover Dataset','',0,'',1);
INSERT INTO lkpEnvVars VALUES('Upland Glade 1-cell mean','upglade1','UplandGlade_3x3.tif','mean percent upland glade cover in 1-cell radius (30 meter cells)','continuous','11','100','','','','square','1','high-res 30-m pixel land cover','FL Cooperative Land Cover Dataset','',0,'',1);
INSERT INTO lkpEnvVars VALUES('Upland Glade 10-cell mean','upglade10','UplandGlade_10cells.tif','mean percent upland glade cover in 10-cell radius (30 meter cells)','continuous','1','11','','','','circle','10','high-res 30-m pixel land cover','FL Cooperative Land Cover Dataset','',0,'',1);
INSERT INTO lkpEnvVars VALUES('Upland Glade 100-cell mean','upglade100','UplandGlade_100cells.tif','mean percent upland glade cover in 100-cell radius (30 meter cells)','continuous','','','','','','circle','100','high-res 30-m pixel land cover','FL Cooperative Land Cover Dataset','',0,'',1);
INSERT INTO lkpEnvVars VALUES('Upland Pine 1-cell mean','uppine1','UplandPine_3x3.tif','mean percent upland pine cover in 1-cell radius (30 meter cells)','continuous','11','100','','','','square','1','high-res 30-m pixel land cover','FL Cooperative Land Cover Dataset','',0,'',1);
INSERT INTO lkpEnvVars VALUES('Upland Pine 10-cell mean','uppine10','UplandPine_10cells.tif','mean percent upland pine cover in 10-cell radius (30 meter cells)','continuous','1','100','','','','circle','10','high-res 30-m pixel land cover','FL Cooperative Land Cover Dataset','',0,'',1);
INSERT INTO lkpEnvVars VALUES('Upland Pine 100-cell mean','uppine100','UplandPine_100cells.tif','mean percent upland pine cover in 100-cell radius (30 meter cells)','continuous','1','82','','','','circle','100','high-res 30-m pixel land cover','FL Cooperative Land Cover Dataset','',0,'',1);
INSERT INTO lkpEnvVars VALUES('Mountain/divide 1-cell mean','mountain1','Landform_1_3x3.tif','mean percent Mountain/divide in 1-cell mean (30 meter cells)','continuous','0','1000','','','','square','1','high-res 30-m pixel landform','USA Landforms (Theobald et al. 2015)','',0,'',1);
INSERT INTO lkpEnvVars VALUES('Mountain/divide 10-cell mean','mountain10','Landform_1_10cell.tif','mean percent Mountain/divide in 10-cell mean (30 meter cells)','continuous','0','1000','','','','circle','10','high-res 30-m pixel landform','USA Landforms (Theobald et al. 2015)','',0,'',1);
INSERT INTO lkpEnvVars VALUES('Peak/ridge (cool) 1-cell mean','peakcool1','Landform_2_3x3.tif','mean percent Peak/ridge (cool) in 1-cell mean (30 meter cells)','continuous','0','1000','','','','square','1','high-res 30-m pixel landform','USA Landforms (Theobald et al. 2015)','',0,'',1);
INSERT INTO lkpEnvVars VALUES('Peak/ridge (cool) 10-cell mean','peakcool10','Landform_2_10cell.tif','mean percent Peak/ridge (cool) in 10-cell mean (30 meter cells)','continuous','0','1000','','','','circle','10','high-res 30-m pixel landform','USA Landforms (Theobald et al. 2015)','',0,'',1);
INSERT INTO lkpEnvVars VALUES('Peak/ridge 1-cell mean','peak1','Landform_3_3x3.tif','mean percent Peak/ridge in 1-cell mean (30 meter cells)','continuous','0','1000','','','','square','1','high-res 30-m pixel landform','USA Landforms (Theobald et al. 2015)','',0,'',1);
INSERT INTO lkpEnvVars VALUES('Peak/ridge 10-cell mean','peak10','Landform_3_10cell.tif','mean percent Peak/ridge in 10-cell mean (30 meter cells)','continuous','0','627.76','','','','circle','10','high-res 30-m pixel landform','USA Landforms (Theobald et al. 2015)','',0,'',1);
INSERT INTO lkpEnvVars VALUES('Peak/ridge (warm) 1-cell mean','peakwarm1','Landform_4_3x3.tif','mean percent Peak/ridge (warm) in 1-cell mean (30 meter cells)','continuous','0','1000','','','','square','1','high-res 30-m pixel landform','USA Landforms (Theobald et al. 2015)','',0,'',1);
INSERT INTO lkpEnvVars VALUES('Peak/ridge (warm) 10-cell mean','peakwarm10','Landform_4_10cell.tif','mean percent Peak/ridge (warm) in 10-cell mean (30 meter cells)','continuous','0','1000','','','','circle','10','high-res 30-m pixel landform','USA Landforms (Theobald et al. 2015)','',0,'',1);
INSERT INTO lkpEnvVars VALUES('Cliff 1-cell mean','cliff1','Landform_5_3x3.tif','mean percent Cliff in 1-cell mean (30 meter cells)','continuous','0','1000','','','','square','1','high-res 30-m pixel landform','USA Landforms (Theobald et al. 2015)','',0,'',1);
INSERT INTO lkpEnvVars VALUES('Cliff 10-cell mean','cliff10','Landform_5_10cell.tif','mean percent Cliff in 10-cell mean (30 meter cells)','continuous','0','977.918','','','','circle','10','high-res 30-m pixel landform','USA Landforms (Theobald et al. 2015)','',0,'',1);
INSERT INTO lkpEnvVars VALUES('Upper slope (flat) 1-cell mean','upslpflt1','Landform_6_3x3.tif','mean percent Upper slope (flat) in 1-cell mean (30 meter cells)','continuous','0','1000','','','','square','1','high-res 30-m pixel landform','USA Landforms (Theobald et al. 2015)','',0,'',1);
INSERT INTO lkpEnvVars VALUES('Upper slope (flat) 10-cell mean','upslpflt10','Landform_6_10cell.tif','mean percent Upper slope (flat) in 10-cell mean (30 meter cells)','continuous','0','1000','','','','circle','10','high-res 30-m pixel landform','USA Landforms (Theobald et al. 2015)','',0,'',1);
INSERT INTO lkpEnvVars VALUES('Upper slope (cool) 1-cell mean','upslpcl1','Landform_7_3x3.tif','mean percent Upper slope (cool) in 1-cell mean (30 meter cells)','continuous','0','1000','','','','square','1','high-res 30-m pixel landform','USA Landforms (Theobald et al. 2015)','',0,'',1);
INSERT INTO lkpEnvVars VALUES('Upper slope (cool) 10-cell mean','upslpcl10','Landform_7_10cell.tif','mean percent Upper slope (cool) in 10-cell mean (30 meter cells)','continuous','0','1000','','','','circle','10','high-res 30-m pixel landform','USA Landforms (Theobald et al. 2015)','',0,'',1);
INSERT INTO lkpEnvVars VALUES('Upper slope 1-cell mean','upslope1','Landform_8_3x3.tif','mean percent Upper slope in 1-cell mean (30 meter cells)','continuous','0','1000','','','','square','1','high-res 30-m pixel landform','USA Landforms (Theobald et al. 2015)','',0,'',1);
INSERT INTO lkpEnvVars VALUES('Upper slope 10-cell mean','upslope10','Landform_8_10cell.tif','mean percent Upper slope in 10-cell mean (30 meter cells)','continuous','0','1000','','','','circle','10','high-res 30-m pixel landform','USA Landforms (Theobald et al. 2015)','',0,'',1);
INSERT INTO lkpEnvVars VALUES('Upper slope (warm) 1-cell mean','upslpwrm1','Landform_9_3x3.tif','mean percent Upper slope (warm) in 1-cell mean (30 meter cells)','continuous','0','1000','','','','square','1','high-res 30-m pixel landform','USA Landforms (Theobald et al. 2015)','',0,'',1);
INSERT INTO lkpEnvVars VALUES('Upper slope (warm) 10-cell mean','upslpwrm10','Landform_9_10cell.tif','mean percent Upper slope (warm) in 10-cell mean (30 meter cells)','continuous','0','1000','','','','circle','10','high-res 30-m pixel landform','USA Landforms (Theobald et al. 2015)','',0,'',1);
INSERT INTO lkpEnvVars VALUES('Lower slope (flat) 1-cell mean','lwslpflt1','Landform_10_3x3.tif','mean percent Lower slope (flat) in 1-cell mean (30 meter cells)','continuous','0','1000','','','','square','1','high-res 30-m pixel landform','USA Landforms (Theobald et al. 2015)','',0,'',1);
INSERT INTO lkpEnvVars VALUES('Lower slope (flat) 10-cell mean','lwslpflt10','Landform_10_10cell.tif','mean percent Lower slope (flat) in 10-cell mean (30 meter cells)','continuous','0','1000','','','','circle','10','high-res 30-m pixel landform','USA Landforms (Theobald et al. 2015)','',0,'',1);
INSERT INTO lkpEnvVars VALUES('Lower slope (cool) 1-cell mean','lwslpcl1','Landform_11_3x3.tif','mean percent Lower slope (cool) in 1-cell mean (30 meter cells)','continuous','0','0','','','','square','1','high-res 30-m pixel landform','USA Landforms (Theobald et al. 2015)','',0,'',1);
INSERT INTO lkpEnvVars VALUES('Lower slope (cool) 10-cell mean','lwslpcl10','Landform_11_10cell.tif','mean percent Lower slope (cool) in 10-cell mean (30 meter cells)','continuous','0','0','','','','circle','10','high-res 30-m pixel landform','USA Landforms (Theobald et al. 2015)','',0,'',1);
INSERT INTO lkpEnvVars VALUES('Lower slope 1-cell mean','lowslope1','Landform_12_3x3.tif','mean percent Lower slope in 1-cell mean (30 meter cells)','continuous','0','1000','','','','square','1','high-res 30-m pixel landform','USA Landforms (Theobald et al. 2015)','',0,'',1);
INSERT INTO lkpEnvVars VALUES('Lower slope 10-cell mean','lowslope10','Landform_12_10cell.tif','mean percent Lower slope in 10-cell mean (30 meter cells)','continuous','0','1000','','','','circle','10','high-res 30-m pixel landform','USA Landforms (Theobald et al. 2015)','',0,'',1);
INSERT INTO lkpEnvVars VALUES('Lower slope (warm) 1-cell mean','lwslpwrm1','Landform_13_3x3.tif','mean percent Lower slope (warm) in 1-cell mean (30 meter cells)','continuous','0','1000','','','','square','1','high-res 30-m pixel landform','USA Landforms (Theobald et al. 2015)','',0,'',1);
INSERT INTO lkpEnvVars VALUES('Lower slope (warm) 10-cell mean','lwslpwrm10','Landform_13_10cell.tif','mean percent Lower slope (warm) in 10-cell mean (30 meter cells)','continuous','0','1000','','','','circle','10','high-res 30-m pixel landform','USA Landforms (Theobald et al. 2015)','',0,'',1);
INSERT INTO lkpEnvVars VALUES('Valley 1-cell mean','valley1','Landform_14_3x3.tif','mean percent Valley in 1-cell mean (30 meter cells)','continuous','0','1000','','','','square','1','high-res 30-m pixel landform','USA Landforms (Theobald et al. 2015)','',0,'',1);
INSERT INTO lkpEnvVars VALUES('Valley 10-cell mean','valley10','Landform_14_10cell.tif','mean percent Valley in 10-cell mean (30 meter cells)','continuous','0','1000','','','','circle','10','high-res 30-m pixel landform','USA Landforms (Theobald et al. 2015)','',0,'',1);
INSERT INTO lkpEnvVars VALUES('Valley (narrow) 1-cell mean','vallynrw1','Landform_15_3x3.tif','mean percent Valley (narrow) in 1-cell mean (30 meter cells)','continuous','0','1000','','','','square','1','high-res 30-m pixel landform','USA Landforms (Theobald et al. 2015)','',0,'',1);
INSERT INTO lkpEnvVars VALUES('Valley (narrow) 10-cell mean','vallynrw10','Landform_15_10cell.tif','mean percent Valley (narrow) in 10-cell mean (30 meter cells)','continuous','0','1000','','','','circle','10','high-res 30-m pixel landform','USA Landforms (Theobald et al. 2015)','',0,'',1);
INSERT INTO lkpEnvVars VALUES('NAIP NIR texture factor 1','fa_1_01','fa_1_01.tif','Single-scale texture from aerial imagery near-infrared band reduced with factor analysis, factor 1.','continuous','1','64350','','','','square','1','high-res 0.6-m near-infrared band','0.6-m NAIP (2016)','Single-resolution textures tend to be strongly affected by noise, this dataset was put into a factor analysis to attempt to segregate some of the noise out.',0,'',1);
INSERT INTO lkpEnvVars VALUES('NAIP NIR texture factor 2','fa_1_02','fa_1_02.tif','Single scale texture from aerial imagery near-infrared band reduced with factor analysis, factor 2.','continuous','1','65443','','','','square','1','high-res 0.6-m near-infrared band','0.6-m NAIP (2016)','Single-resolution textures tend to be strongly affected by noise, this dataset was put into a factor analysis to attempt to segregate some of the noise out.',0,'',1);
INSERT INTO lkpEnvVars VALUES('NAIP NIR texture factor 4','fa_1_04','fa_1_04.tif','Single scale texture from aerial imagery near-infrared band reduced with factor analysis, factor 4.','continuous','1','65472','','','','square','1','high-res 0.6-m near-infrared band','0.6-m NAIP (2016)','Single-resolution textures tend to be strongly affected by noise, this dataset was put into a factor analysis to attempt to segregate some of the noise out.',0,'',1);
INSERT INTO lkpEnvVars VALUES('NAIP NIR texture factor 5','fa_1_05','fa_1_05.tif','Single scale texture from aerial imagery near-infrared band reduced with factor analysis, factor 5.','continuous','1','65107','','','','square','1','high-res 0.6-m near-infrared band','0.6-m NAIP (2016)','Single-resolution textures tend to be strongly affected by noise, this dataset was put into a factor analysis to attempt to segregate some of the noise out.',0,'',1);
INSERT INTO lkpEnvVars VALUES('NAIP NIR texture factor 8','fa_1_08','fa_1_08.tif','Single scale texture from aerial imagery near-infrared band reduced with factor analysis, factor 8.','continuous','1','60172','','','','square','1','high-res 0.6-m near-infrared band','0.6-m NAIP (2016)','Single-resolution textures tend to be strongly affected by noise, this dataset was put into a factor analysis to attempt to segregate some of the noise out.',0,'',1);
INSERT INTO lkpEnvVars VALUES('NAIP Red texture factor 1','fa_3_01','fa_3_01.tif','Single scale texture from aerial imagery red band reduced with factor analysis, factor 1.','continuous','1','57403','','','','square','1','high-res 0.6-m red band','0.6-m NAIP (2016)','Single-resolution textures tend to be strongly affected by noise, this dataset was put into a factor analysis to attempt to segregate some of the noise out.',0,'',1);
INSERT INTO lkpEnvVars VALUES('NAIP Red texture factor 2','fa_3_02','fa_3_02.tif','Single scale texture from aerial imagery red band reduced with factor analysis, factor 2.','continuous','1','64584','','','','square','1','high-res 0.6-m red band','0.6-m NAIP (2016)','Single-resolution textures tend to be strongly affected by noise, this dataset was put into a factor analysis to attempt to segregate some of the noise out.',0,'',1);
INSERT INTO lkpEnvVars VALUES('NAIP Red texture factor 4','fa_3_04','fa_3_04.tif','Single scale texture from aerial imagery red band reduced with factor analysis, factor 4.','continuous','1','65308','','','','square','1','high-res 0.6-m red band','0.6-m NAIP (2016)','Single-resolution textures tend to be strongly affected by noise, this dataset was put into a factor analysis to attempt to segregate some of the noise out.',0,'',1);
INSERT INTO lkpEnvVars VALUES('NAIP Red texture factor 5','fa_3_05','fa_3_05.tif','Single scale texture from aerial imagery red band reduced with factor analysis, factor 5.','continuous','1','65313','','','','square','1','high-res 0.6-m red band','0.6-m NAIP (2016)','Single-resolution textures tend to be strongly affected by noise, this dataset was put into a factor analysis to attempt to segregate some of the noise out.',0,'',1);
INSERT INTO lkpEnvVars VALUES('NAIP Red texture factor 7','fa_3_07','fa_3_07.tif','Single scale texture from aerial imagery red band reduced with factor analysis, factor 7.','continuous','1','64186','','','','square','1','high-res 0.6-m red band','0.6-m NAIP (2016)','Single-resolution textures tend to be strongly affected by noise, this dataset was put into a factor analysis to attempt to segregate some of the noise out.',0,'',1);
INSERT INTO lkpEnvVars VALUES('NAIP Red texture factor 8','fa_3_08','fa_3_08.tif','Single scale texture from aerial imagery red band reduced with factor analysis, factor 8.','continuous','1','60066','','','','square','1','high-res 0.6-m red band','0.6-m NAIP (2016)','Single-resolution textures tend to be strongly affected by noise, this dataset was put into a factor analysis to attempt to segregate some of the noise out.',0,'',1);
INSERT INTO lkpEnvVars VALUES('NAIP Red texture factor 11','fa_3_11','fa_3_11.tif','Single scale texture from aerial imagery red band reduced with factor analysis, factor 11.','continuous','1','64704','','','','square','1','high-res 0.6-m red band','0.6-m NAIP (2016)','Single-resolution textures tend to be strongly affected by noise, this dataset was put into a factor analysis to attempt to segregate some of the noise out.',0,'',1);
INSERT INTO lkpEnvVars VALUES('NAIP Red texture factor 12','fa_3_12','fa_3_12.tif','Single scale texture from aerial imagery red band reduced with factor analysis, factor 12.','continuous','1','63517','','','','square','1','high-res 0.6-m red band','0.6-m NAIP (2016)','Single-resolution textures tend to be strongly affected by noise, this dataset was put into a factor analysis to attempt to segregate some of the noise out.',0,'',1);
INSERT INTO lkpEnvVars VALUES('NAIP NIR multi-scale (0.6 vs 1.8) texture (algorithm c)','nc_mn_13','nc_mn_13.tif','Multi-scale (NDTI) texture from aerial imagery near-infrared band (30 meter cells). Uses a summed alternating pixel convolution filter to contrast local 3x3 cell variance at a 0.6m cell size vs. 3x3 cell variance at a 1.8m cell size.','continuous','81','1977','','','','square','1','high-res 0.6-m near-infrared band','0.6-m NAIP (2016)','',0,'',1);
INSERT INTO lkpEnvVars VALUES('NAIP NIR multi-scale (0.6 vs 1.8) texture (algorithm a)','na_mn_13','na_mn_13.tif','Multi-scale (NDTI) texture from aerial imagery near-infrared band (30 meter cells). Uses local standard deviation to contrast local 3x3 cell variance at a 0.6m cell size vs. 3x3 cell variance at a 1.8m cell size.','continuous','8','1990','','','','square','1','high-res 0.6-m near-infrared band','0.6-m NAIP (2016)','',0,'',1);
INSERT INTO lkpEnvVars VALUES('NAIP NIR multi-scale (1.2 vs 3.6) texture (algorithm a)','na_mn_26','na_mn_26.tif','Multi-scale (NDTI) texture from aerial imagery near-infrared band (30 meter cells). Uses local standard deviation to contrast local 3x3 cell variance at a 1.2m cell size vs. 3x3 cell variance at a 3.6m cell size.','continuous','10','1991','','','','square','1','high-res 0.6-m near-infrared band','0.6-m NAIP (2016)','',0,'',1);
INSERT INTO lkpEnvVars VALUES('NAIP NIR multi-scale (1.8 vs 5.4) texture (algorithm a)','na_mn_39','na_mn_39.tif','Multi-scale (NDTI) texture from near-infrared band (30 meter cells). Uses local standard deviation to contrast local 3x3 cell variance at a 1.8m cell size vs. 3x3 cell variance at a 5.4m cell size.','continuous','11','1991','','','','square','1','high-res 0.6-m near-infrared band','0.6-m NAIP (2016)','',0,'',1);
INSERT INTO lkpEnvVars VALUES('NAIP NIR multi-scale (2.4 vs 7.2) texture (algorithm a)','na_mn_4c','na_mn_4c.tif','Multi-scale (NDTI) texture from aerial imagery near-infrared band (30 meter cells). Uses local standard deviation to contrast local 3x3 cell variance at a 2.4m cell size vs. 3x3 cell variance at a 7.2m cell size.','continuous','8','1991','','','','square','1','high-res 0.6-m near-infrared band','0.6-m NAIP (2016)','',0,'',1);
INSERT INTO lkpEnvVars VALUES('NAIP NIR multi-scale (3.6 vs 10.8) texture (algorithm a)','na_mn_6d','na_mn_6d.tif','Multi-scale (NDTI) texture from aerial imagery near-infrared band (30 meter cells). Uses local standard deviation to contrast local 3x3 cell variance at a 3.6m cell size vs. 3x3 cell variance at a 10.8m cell size.','continuous','9','1991','','','','square','1','high-res 0.6-m near-infrared band','0.6-m NAIP (2016)','',0,'',1);
INSERT INTO lkpEnvVars VALUES('NAIP NIR multi-scale (5.4 vs 16.2) texture (algorithm a)','na_mn_9e','na_mn_9e.tif','Multi-scale (NDTI) texture from aerial imagery near-infrared band (30 meter cells). Uses local standard deviation to contrast local 3x3 cell variance at a 5.4m cell size vs. 3x3 cell variance at a 16.2m cell size.','continuous','7','1991','','','','square','1','high-res 0.6-m near-infrared band','0.6-m NAIP (2016)','',0,'',1);
INSERT INTO lkpEnvVars VALUES('NAIP NIR multi-scale (7.2 vs 21.6) texture (algorithm a)','na_mn_cf','na_mn_cf.tif','Multi-scale (NDTI) texture from aerial imagery near-infrared band (30 meter cells). Uses local standard deviation to contrast local 3x3 cell variance at a 7.2m cell size vs. 3x3 cell variance at a 21.6m cell size.','continuous','8','1990','','','','square','1','high-res 0.6-m near-infrared band','0.6-m NAIP (2016)','',0,'',1);
INSERT INTO lkpEnvVars VALUES('NAIP Red multi-scale (0.6 vs 1.8) texture (algorithm c)','rc_mn_13','rc_mn_13.tif','Multi-scale (NDTI) texture from aerial imagery red band (30 meter cells). Uses a summed alternating pixel convolution filter to contrast local 3x3 cell variance at a 0.6m cell size vs. 3x3 cell variance at a 1.8m cell size.','continuous','126','1981','','','','square','1','high-res 0.6-m red band','0.6-m NAIP (2016)','',0,'',1);
INSERT INTO lkpEnvVars VALUES('NAIP Red multi-scale (0.6 vs 1.8) texture (algorithm a)','ra_mn_13','ra_mn_13.tif','Multi-scale (NDTI) texture from aerial imagery red band (30 meter cells).  Uses local standard deviation to contrast local 3x3 cell variance at a 0.6m cell size vs. 3x3 cell variance at a 1.8m cell size.','continuous','44','1994','','','','square','1','high-res 0.6-m red band','0.6-m NAIP (2016)','',0,'',1);
INSERT INTO lkpEnvVars VALUES('NAIP Red multi-scale (1.2 vs 3.6) texture (algorithm a)','ra_mn_26','ra_mn_26.tif','Multi-scale (NDTI) texture from aerial imagery red band (30 meter cells). Uses local standard deviation to contrast local 3x3 cell variance at a 1.2m cell size vs. 3x3 cell variance at a 3.6m cell size.','continuous','12','1997','','','','square','1','high-res 0.6-m red band','0.6-m NAIP (2016)','',0,'',1);
INSERT INTO lkpEnvVars VALUES('NAIP Red multi-scale (1.8 vs 5.4) texture (algorithm a)','ra_mn_39','ra_mn_39.tif','Multi-scale (NDTI) texture from aerial imagery red band (30 meter cells). Uses local standard deviation to contrast local 3x3 cell variance at a 1.8m cell size vs. 3x3 cell variance at a 5.4m cell size.','continuous','23','1999','','','','square','1','high-res 0.6-m red band','0.6-m NAIP (2016)','',0,'',1);
INSERT INTO lkpEnvVars VALUES('NAIP Red multi-scale (2.4 vs 7.2) texture (algorithm a)','ra_mn_4c','ra_mn_4c.tif','Multi-scale (NDTI) texture from aerial imagery red band (30 meter cells). Uses local standard deviation to contrast local 3x3 cell variance at a 2.4m cell size vs. 3x3 cell variance at a 7.2m cell size.','continuous','17','1999','','','','square','1','high-res 0.6-m red band','0.6-m NAIP (2016)','',0,'',1);
INSERT INTO lkpEnvVars VALUES('NAIP Red multi-scale (3.6 vs 10.8) texture (algorithm a)','ra_mn_6d','ra_mn_6d.tif','Multi-scale (NDTI) texture from aerial imagery red band (30 meter cells). Uses local standard deviation to contrast local 3x3 cell variance at a 3.6m cell size vs. 3x3 cell variance at a 10.8m cell size.','continuous','13','1999','','','','square','1','high-res 0.6-m red band','0.6-m NAIP (2016)','',0,'',1);
INSERT INTO lkpEnvVars VALUES('NAIP Red multi-scale (5.4 vs 16.2) texture (algorithm a)','ra_mn_9e','ra_mn_9e.tif','Multi-scale (NDTI) texture from aerial imagery red band (30 meter cells). Uses local standard deviation to contrast local 3x3 cell variance at a 5.4m cell size vs. 3x3 cell variance at a 16.2m cell size.','continuous','1','1999','','','','square','1','high-res 0.6-m red band','0.6-m NAIP (2016)','',0,'',1);
INSERT INTO lkpEnvVars VALUES('NAIP Red multi-scale (7.2 vs 21.6) texture (algorithm a)','ra_mn_cf','ra_mn_cf.tif','Multi-scale (NDTI) texture from aerial imagery red band (30 meter cells). Uses local standard deviation to contrast local 3x3 cell variance at a 7.2m cell size vs. 3x3 cell variance at a 21.6m cell size.','continuous','1','2000','','','','square','1','high-res 0.6-m red band','0.6-m NAIP (2016)','',0,'',1);
INSERT INTO lkpEnvVars VALUES('Annual Mean Temperature','clim_bio1','bio1_HolesFilled.tif','','continuous','-8.11','26.50','','','','?','?','','','',0,'',1);
INSERT INTO lkpEnvVars VALUES('Mean Temperature of Warmest Quarter','clim_bio10','bio10_HolesFilled.tif','','continuous','-1.16','39.20','','','','?','?','','','',0,'',1);
INSERT INTO lkpEnvVars VALUES('Mean Temperature of Coldest Quarter','clim_bio11','bio11_HolesFilled.tif','','continuous','-15.78','22.15','','','','?','?','','','',0,'',1);
INSERT INTO lkpEnvVars VALUES('Annual Precipitation','clim_bio12','bio12_HolesFilled.tif','','continuous','59.20','6923.27','','','','?','?','','','',0,'',1);
INSERT INTO lkpEnvVars VALUES('Precipitation of Wettest Quarter','clim_bio16','bio16_HolesFilled.tif','','continuous','45.75','3552.75','','','','?','?','','','',0,'',1);
INSERT INTO lkpEnvVars VALUES('Precipitation of Driest Quarter','clim_bio17','bio17_HolesFilled.tif','','continuous','0.17','415.39','','','','?','?','','','',0,'',1);
INSERT INTO lkpEnvVars VALUES('Climate Water Deficit','clim_CWD','CWD_HolesFilled.tif','','continuous','0.00','1834.65','','','','?','?','','','',0,'',1);
INSERT INTO lkpEnvVars VALUES('Growing Degree Days','clim_GDD','GDD_HolesFilled.tif','','continuous','0.00','8085.89','','','','?','?','','','',0,'',1);
INSERT INTO lkpEnvVars VALUES('Polaris soils bulk density','bulkDens','Polaris_Sep2018_Esri_BulkDensity_x1000_AlbConEA_Int16_fillNull_mask_Int_crop.tif','Polaris soils bulk density','continuous',NULL,NULL,NULL,NULL,NULL,'NA','NA','POLARIS',NULL,NULL,0,NULL,1);
INSERT INTO lkpEnvVars VALUES('Polaris soils percent clay','clay','Polaris_Sep2018_Esri_Clay_x100_AlbConEA_Int16_fillNull_mask_Int_crop.tif','Polaris soils percent clay','continuous',NULL,NULL,NULL,NULL,NULL,'NA','NA','POLARIS',NULL,NULL,0,NULL,1);
INSERT INTO lkpEnvVars VALUES('Polaris soils pore size distribution (N)','poreSize','Polaris_Sep2018_Esri_N_x1000_AlbConEA_Int16_fillNull_mask_Int_crop.tif','Polaris soils pore size distribution (N)','continuous',NULL,NULL,NULL,NULL,NULL,'NA','NA','POLARIS',NULL,NULL,0,NULL,1);
INSERT INTO lkpEnvVars VALUES('Polaris soils pH','soil_pH','Polaris_Sep2018_Esri_ph_x100_AlbConEA_Int16_fillNull_mask_Int_crop.tif','Polaris soils pH','continuous',NULL,NULL,NULL,NULL,NULL,'NA','NA','POLARIS',NULL,NULL,0,NULL,1);
CREATE TABLE `lkpDataSources` (
	`DataSourcesID`	INTEGER NOT NULL,
	`ProgramName`	TEXT,
	`State`	TEXT,
	`DataProvidedDate`	TEXT,
	PRIMARY KEY(`DataSourcesID`)
);
INSERT INTO lkpDataSources VALUES(1,'New York Natural Heritage Program','NY','');
INSERT INTO lkpDataSources VALUES(2,'Virginia Natural Heritage Program','VA','');
INSERT INTO lkpDataSources VALUES(3,'Florida Natural Areas Inventory','FL','');
INSERT INTO lkpDataSources VALUES(4,'Pennsylvania Natural Heritage Program','PA','');
INSERT INTO lkpDataSources VALUES(5,'Maine Natural Areas Program, Maine Department of Agriculture, Conservation and Forestry','ME','');
INSERT INTO lkpDataSources VALUES(6,'New Hampshire Natural Heritage Bureau','NH','');
INSERT INTO lkpDataSources VALUES(7,'Vermont Wildlife Diversity and Natural Heritage Inventory, Vermont Fish and Wildlife Department','VT','');
INSERT INTO lkpDataSources VALUES(8,'Massachusetts Natural Heritage and Endangered Species Program, Massachusetts Division of Fisheries & Wildlife','MA','');
INSERT INTO lkpDataSources VALUES(9,'Connecticut Natural Diversity Database, Connecticut Department of Energy & Environmental Protection','CT','');
INSERT INTO lkpDataSources VALUES(10,'Delaware Species Conservation and Research Program, Delaware Division of Fish and Wildlife','DE','');
INSERT INTO lkpDataSources VALUES(11,'New Jersey Natural Heritage Program','NJ','');
INSERT INTO lkpDataSources VALUES(12,'Rhode Island Natural History Survey','RI','');
INSERT INTO lkpDataSources VALUES(13,'Maryland Natural Heritage Program, Maryland Department of Natural Resources, Wildlife and Heritage Service','MD','');
INSERT INTO lkpDataSources VALUES(14,'West Virginia Natural Heritage Program','WV','');
INSERT INTO lkpDataSources VALUES(15,'Alabama Natural Heritage Program','AL','');
INSERT INTO lkpDataSources VALUES(16,'Georgia Department of Natural Resources, Wildlife Resources Division','GA','');
INSERT INTO lkpDataSources VALUES(17,'North Carolina Natural Heritage Program','NC','');
INSERT INTO lkpDataSources VALUES(18,'South Carolina Department of Natural Resources, Heritage Trust Program','SC','');
INSERT INTO lkpDataSources VALUES(19,'Allen Bridgeman, South Carolina DNR','SC','');
INSERT INTO lkpDataSources VALUES(20,'Jeff Glitzstein, Tall Timbers Research Station','FL','');
INSERT INTO lkpDataSources VALUES(21,'Tracey Tuberville, Savannah River Ecology Laboratory','SC','');
INSERT INTO lkpDataSources VALUES(22,'David Caldwell, US Fish and Wildlife Service','GA','');
INSERT INTO lkpDataSources VALUES(23,'Robyn Mackie and Geoff Holden, US Forest Service','SC','');
INSERT INTO lkpDataSources VALUES(24,'New Jersey Division of Fish and Wildlife - Endangered and Nongame Species Program','NJ','');
INSERT INTO lkpDataSources VALUES(25,'NatureServe, Multi-jurisdictional Database from Heritage Network Programs',NULL,NULL);
INSERT INTO lkpDataSources VALUES(26,'Global Biodiversity Information Facility (GBIF; www.gbif.org)',NULL,NULL);
INSERT INTO lkpDataSources VALUES(27,'Biodiversity Information Serving Our Nation (BISON; bison.usgs.gov)',NULL,NULL);
INSERT INTO lkpDataSources VALUES(28,'iNaturalist (www.inaturalist.org)',NULL,NULL);
CREATE TABLE `lkpRankDefinitions` (
	`rank`	,
	`rankname`	,
	`definition`	
);
INSERT INTO lkpRankDefinitions VALUES('GX','Presumed Extinct','Not located despite intensive searches and virtually no likelihood of rediscovery.');
INSERT INTO lkpRankDefinitions VALUES('GH','Possibly Extinct','Missing; known from only historical occurrences but still some hope of rediscovery.');
INSERT INTO lkpRankDefinitions VALUES('G1','Critically Imperiled','At very high risk of extinction due to extreme rarity (often 5 or fewer populations), very steep declines, or other factors.');
INSERT INTO lkpRankDefinitions VALUES('G2','Imperiled','At high risk of extinction due to very restricted range, very few populations (often 20 or fewer), steep declines, or other factors.');
INSERT INTO lkpRankDefinitions VALUES('G3','Vulnerable','At moderate risk of extinction due to a restricted range, relatively few populations (often 80 or fewer), recent and widespread declines, or other factors.');
INSERT INTO lkpRankDefinitions VALUES('G4','Apparently Secure','Uncommon but not rare; some cause for long-term concern due to declines or other factors.');
INSERT INTO lkpRankDefinitions VALUES('G5','Secure','Common; widespread and abundant.');
INSERT INTO lkpRankDefinitions VALUES('GU','Unrankable','Currently unrankable due to lack of information or due to substantially conflicting information about status or trends.');
INSERT INTO lkpRankDefinitions VALUES('GNR','Unranked','Global rank not yet assessed.');
INSERT INTO lkpRankDefinitions VALUES('GNA','Not Applicable','A conservation status rank is not applicable because the species is not a suitable target for conservation activities.');
CREATE TABLE IF NOT EXISTS "tblModelResults" (
	`model_run_name`	TEXT,
	`EGT_ID`	INTEGER,
	`table_code`	TEXT,
	`internal_comments`	TEXT,
	`metadata_comments`	TEXT,
	`model_comp_name`	TEXT,
	`modeller`	TEXT,
	`model_start_time`	TEXT,
	`model_end_time`	TEXT,
	`r_version`	TEXT,
	`repo_head`	TEXT,
	`seed`	INTEGER,
	PRIMARY KEY(`model_run_name`),
	FOREIGN KEY(`model_run_name`) REFERENCES `tblModelResults`(`model_run_name`)
);
CREATE TABLE IF NOT EXISTS "lkpSpeciesRubric" (
	`EGT_ID`	INTEGER,
	`sp_code`	TEXT,
	`spdata_dataqual`	TEXT DEFAULT 'A',
	`spdata_abs`	TEXT DEFAULT 'A',
	`spdata_eval`	TEXT DEFAULT 'A',
	`envvar_relevance`	TEXT DEFAULT 'A',
	`envvar_align`	TEXT DEFAULT 'A',
	`process_algo`	TEXT DEFAULT 'A',
	`process_sens`	TEXT DEFAULT 'A',
	`process_rigor`	TEXT DEFAULT 'A',
	`process_perform`	TEXT DEFAULT 'A',
	`process_review`	TEXT DEFAULT 'C',
	`products_mapped`	TEXT DEFAULT 'A',
	`products_support`	TEXT DEFAULT 'I',
	`products_repo`	TEXT DEFAULT 'I',
	`interative`	TEXT DEFAULT 'C',
	`spdata_dataqualNotes`	TEXT DEFAULT 'Heritage Network data augmented with outside data which may or may not be vetted for accuracy or weighted for spatial representation.',
	`spdata_absNotes`	TEXT DEFAULT 'Background points randomly placed throughout study area excluding species locations.',
	`spdata_evalNotes`	TEXT DEFAULT 'Models are validated by jackknifing (i.e. leave-one-out).',
	`envvar_relevanceNotes`	TEXT DEFAULT 'Selection of predictor variables were based on previous modeling experience by the Natural Heritage Network. Time constraints of this project prevented making species specific selections.',
	`envvar_alignNotes`	TEXT DEFAULT 'Reasonable attempts to align predictor and presence data were made.',
	`process_algoNotes`	TEXT DEFAULT 'Random Forest is highly rated classification model that is well documented as suitable for modeling rare species.',
	`process_sensNotes`	TEXT DEFAULT 'Settings for Random Forest were adjusted to best model the species; however, different models/parameters were not tested within one model run.',
	`process_rigorNotes`	TEXT DEFAULT 'Collinearity of predictors recognized and addressed; presence points grouped to minimize sample bias and minimize spatial autocorrelation boost during validation; other assumptions recognized and considered.',
	`process_performNotes`	TEXT DEFAULT 'Model TSS >= 0.6. Mapped model output is evaluated for ecological plausibility by expert review.',
	`process_reviewNotes`	TEXT DEFAULT 'Model was not reviewed by regional, taxonomic experts.',
	`products_mappedNotes`	TEXT DEFAULT 'Single calculated threshold selected for all final models to be integrated into MoBI.',
	`products_supportNotes`	TEXT DEFAULT 'All standards met.',
	`products_repoNotes`	TEXT DEFAULT 'All standards met.',
	`interativeNotes`	TEXT DEFAULT 'Model not revised.'
);
CREATE TABLE IF NOT EXISTS "lkpSpecies" (
	`EGT_ID`	INTEGER NOT NULL,
	`sp_code`	TEXT NOT NULL UNIQUE,
	`broad_group`	INTEGER,
	`tax_group`	INTEGER,
	`scientific_name`	TEXT,
	`common_name`	TEXT,
	`g_rank`	INTEGER,
	`rounded_g_rank`	INTEGER,
	`esa_status`	TEXT,
	`ModelerID`	INTEGER,
	`Comment`	TEXT,
	`modtype`	TEXT,
	PRIMARY KEY(`EGT_ID`)
);
DELETE FROM sqlite_sequence;
INSERT INTO sqlite_sequence VALUES('lkpRange',545633);
CREATE INDEX `hucIndex` ON `lkpRange` (
	`huc10_id`	ASC
);
COMMIT;
