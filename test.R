# =========================================
# Load dataset
# Test code provided on GitHub
# https://github.com/rstats-gsoc/gsoc2016/wiki/Managing-and-visualizing-movement-data-with-PostGIS-and-R
# =========================================

library("spacetime")
library("sp")

data(fires)
fires$X <- fires$X * 100000
fires$Y <- fires$Y * 100000
fires$Time <- as.POSIXct(as.Date("1960-01-01")+(fires$Time-1))

coordinates(fires) <- c("X", "Y")
proj4string(fires) <- CRS("+init=epsg:2229 +ellps=GRS80")

# =========================================
# Code written by Balázs Dukai starts here
# balazs.dukai@gmail.com
# https://github.com/balazsdukai
# =========================================

library("RPostgreSQL")
library("rpostgis")
library("rgdal")
library("rgeos")

# create and set up a rpostgisLT database with PostGIS extension
# shell: createdb -U rpostgisLT -h localhost rpostgisLT
drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, user="rpostgisLT", password="rpostgisLT", dbname="rpostgisLT",
                 host="localhost")
dbSendQuery(con, "create extension if not exists postgis")

# load the "fires" data frame into PostgreSQL
writeOGR(fires, driver = "PostgreSQL", "PG:dbname=rpostgisLT host=localhost", layer = "fires")
dbListFields(con, "fires")
query <- "SELECT column_name, data_type FROM information_schema.columns
WHERE table_name = 'fires'"
res <- dbSendQuery(con, query)
dbFetch(res)
dbClearResult(res)

# the field "time" which contain the timestamps is of type "character varying"
pgAsDate(con, "fires", date = "time") # convert field type to timestamp

# create indexes
# ogc_fid —> primary key. The UID field. It was already created by the writeOGR function.
# time —> B-Tree. Because timestamps are 1D and ordered.
# wkb_geometry —> GiST. Because data is 2D and irregular. And because PostGIS doesn't support pure R-Trees any more. Thus literally GiST is the only spatial indexing method available in PostGIS at the moment.
pgIndex(con, "fires", "time", "time_idx", method = "btree")
pgIndex(con, "fires", "wkb_geometry", "geom_idx", method = "gist")

# a Shiny gadget to subset points in the database
subsetPoints <- function(conn, name, geom){
    # ==============================
    # Interactively subsets a group of points in a PostGIS database by on-screen selection and time range.
    # Outputs the the ID of the selected points.
    # Input:
    #     name – Character. Name of the PostGIS table which contains the points.
    #     geom – Character. Name of the geometry field in the PostGIS table.
    #     conn – PostgreSQLConnection.
    # Output:
    #     List of the selected point IDs
    # Requires: sp, rpostgresql, rgeos, miniUI, shiny, ggplot2
    # Reference: rpostgis (https://github.com/mablab/rpostgis/blob/master/R/pgGetPts.r)
    # ==============================
    
    lowL <- paste0("ST_Point(",lower_left[1],",",lower_left[2],")")
    uppR <- paste0("ST_Point(",upper_right[1],",",upper_right[2],")")
    minT <- as.character(format(minTime, "%Y-%m-%d"))
    maxT <- as.character(format(maxTime, "%Y-%m-%d"))
    
    # Retrieve the EPSG (not SRID, because the sp.CRS() works only with EPSG codes, and SRID not equal EPSG)
    str <- paste0("SELECT auth_srid FROM spatial_ref_sys,(SELECT DISTINCT(ST_SRID(",geom,")) FROM ",name," WHERE ",geom," IS NOT NULL) as f WHERE srid = f.st_srid;")
    epgs <- as.character(dbGetQuery(con, str)[1,1])
    # Check if the EPSG is unique, otherwise throw an error
    if (length(epgs) == 0) {
        stop("Multiple EPSGs in the point geometry")        
    }

    # Coerce the SQL query to get the points from PostGIS
    query <- paste0("SELECT ogc_fid, ST_AsText(", geom,") As geom FROM ", name," WHERE ", geom," && ST_SetSRID(ST_MakeBox2D(", lowL,",", uppR,"),", epgs,") AND fires.time >= '", minT,"' AND fires.time < '", maxT,"';")
    
    # Alternative SQL query:
    #     
    # SELECT ST_AsText(ST_Collect(f.geom))
    # FROM (
    #     SELECT ST_AsText(wkb_geometry) As geom
    #     FROM fires
    #     WHERE wkb_geometry && ST_SetSRID(ST_MakeBox2D(ST_Point(6400000, 1950000),
    #                                                   ST_Point(6500000 ,2050000)),2229) AND
    #     fires.time >= '1990-01-01' AND fires.time < '2000-01-01') as f;
    # 
    # Which retrieves a single MULTIPOINT WKT, however up to now there is no R function 
    # which is able to parse MULTIPOINT WKT. 

    # retrieve the data from the database
    res <- dbGetQuery(conn, query)
    
    # cast the WKT back into a SpatialPointsDataFrame with the correct CRS
    row.names(res) <- res$ogc_fid
    
    # set projection
    p4s = CRS(paste0("+init=epsg:", epgs))
    
    # Read the WKTs into a data frame with their projection.
    spTemp <-  readWKT(res$geom[1], res$ogc_fid[1], p4s)
    for (i in 2:nrow(res)) {
        spTemp <-  rbind(spTemp, readWKT(res$geom[i], res$ogc_fid[i], p4s))
    }
    
    # cast the data frame into a SpatialPointsDataFrame
    subs <-  SpatialPointsDataFrame(spTemp, res[-2])
    
    dbDisconnect(con)
    return(subs)
    
}

# # subset parameters
# lower_left <- c(6400000, 1950000)
# upper_right <- c(6500000 ,2050000)
# minTime <- as.POSIXct("1990-01-01")
# maxTime <- as.POSIXct("2000-01-01")
# 
# subset_pt <- subsetPoints(con, name="fires", geom="wkb_geometry", lower_left, upper_right, minTime, maxTime)
# 
# # plot the points
# plot(fires, pch = 3)
# points(subset_pt, col = "red")

