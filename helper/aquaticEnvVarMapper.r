library(here)
library(RSQLite)
library(sf)

nm_bkg <- c(here("_data","env_vars","tabular", "background.sqlite"), "background_reaches")

#MAhuc <- c("0204","0205","0206","0207","0411","0412","0413","0501","0502","0503")


dbEV <- dbConnect(SQLite(),dbname=nm_bkg[1])
# subset to huc if requested
SQLQuery <- paste0("SELECT * FROM ",nm_bkg[2]) #," WHERE substr(huc12, 1, 4) IN ('", paste(MAhuc, collapse = "','"),"');")
#SQLQuery <- paste0("SELECT * FROM ",nm_bkg[2]," WHERE substr(huc12, 1, 4) IN ('", paste(MAhuc, collapse = "','"),"');") 
shapef <- dbGetQuery(dbEV, SQLQuery)

SQLQuery <- paste0("SELECT proj4string p FROM lkpCRS WHERE table_name = '", nm_bkg[2], "';") 
proj4 <- dbGetQuery(dbEV, SQLQuery)$p

names(shapef) <- tolower(names(shapef))
shapef1 <- st_sf(shapef[c("comid", "huc12")], geometry=st_as_sfc(shapef$wkt), crs=proj4)
shapef1 <- st_zm(shapef1)

###################


# get desired env. var. columns + comid for presence
# SQLite database integration for Env Vars
dbEV <- dbConnect(SQLite(),dbname=nm_bkg[1])
SQLQuery <- paste0("SELECT * FROM ",nm_bkg[2],"_att WHERE COMID IN ('", paste(shapef1$comid, collapse = "','"),"')") 
EnvVars <- dbGetQuery(dbEV, SQLQuery)
names(EnvVars) <- tolower(names(EnvVars))

dbDisconnect(dbEV)

# merge two data frames by COMID
reaches_attributed <- merge(shapef1,EnvVars,by="comid")

st_write(reaches_attributed, "N:/chris/aqvar/aquatic_envvar.shp", driver = "ESRI Shapefile")

