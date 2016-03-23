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
plot(fires, pch = 3)

# =========================================
# Query spatial and temporal extent
# Test code provided on GitHub
# https://github.com/rstats-gsoc/gsoc2016/wiki/Managing-and-visualizing-movement-data-with-PostGIS-and-R
# =========================================
(subfires <- subset(fires, coordinates(fires)[, 1] >= 6400000 
                    & coordinates(fires)[, 1] <= 6500000 
                    & coordinates(fires)[, 2] >= 1950000 
                    & coordinates(fires)[, 2] <= 2050000 
                    & fires$Time >= as.POSIXct("1990-01-01") 
                    & fires$Time < as.POSIXct("2000-01-01")))

rect(6400000, 1950000, 6500000, 2050000, border = "red", lwd = 2)
points(subfires, col = "red")

# =========================================
# Code written by Balázs Dukai starts here
# balazs.dukai@gmail.com
# https://github.com/balazsdukai
# =========================================

library("RPostgreSQL")
library("rpostgis")
library("rgdal")
library("rgeos")

# create and set up a test database with PostGIS extension
# shell: createdb -U bdukai test
drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, user="bdukai", password="bdukai", dbname="test")
dbSendQuery(con, "create extension postgis")

# load the "fires" data frame into PostgreSQL
writeOGR(fires, driver = "PostgreSQL", "PG:dbname=test host=localhost", layer = "fires")
dbListFields(con, "fires")
query <- "select column_name, data_type from information_schema.columns
where table_name = 'fires'"
res <- dbSendQuery(con, query)
dbFetch(res)
dbClearResult(res)
# the field "time" which contain the timestamps is of type "character varying"
pgAsDate(con, "fires", date = "time") # convert field type to timestamp

# create indexes
# ogc_fid —> primary key. The UID field. It was already created by the writeOGR function.
# time —> B-Tree. Because timestamps are 1D and ordered.
# wkb_geometry —> GiST. Because data is 2D and irregular.
pgIndex(con, "fires", "time", "time_idx", method = "btree")
pgIndex(con, "fires", "wkb_geometry", "geom_idx", method = "gist")

# retrieve the subset of points form the database
subsetPoints <- function(lower_left, upper_right, minTime, maxTime, SRID, conn){
    # ==============================
    # Subsets a group of points in a PostGIS database by a bounding box and time range.
    # Outputs the subset as a SpatialPointsDataFrame object.
    # Input:
    #     lower_left – Numeric. The (x,y) coordinate tuple of the lower left corner of the bounding box
    #     upper_right – Numeric. The (x,y) coordinate tuple of the upper right corner of the bounding box
    #     minTime – POSIX. Beginning of the time range (inclusive), given as e.g. "1990-01-01"
    #     maxTime – POSIX. End of the time range (exclusive)
    #     SRID – Character. The SRID identifier of the CRS.
    #     conn – PostgreSQLConnection.
    # Output:
    #     SpatialPointsDataFrame
    # Requires: sp, rpostgresql, rgeos
    # Reference: rpostgis (https://github.com/mablab/rpostgis/blob/master/R/pgGetPts.r)
    # ==============================
    
    lowL <- paste0("ST_Point(",lower_left[1],",",lower_left[2],")")
    uppR <- paste0("ST_Point(",upper_right[1],",",upper_right[2],")")
    minT <- as.character(format(minTime, "%Y-%m-%d"))
    maxT <- as.character(format(maxTime, "%Y-%m-%d"))

    # coerce the SQL query
    query <- paste0("SELECT ogc_fid, ST_AsText(wkb_geometry) As geom FROM fires WHERE wkb_geometry && ST_SetSRID(ST_MakeBox2D(",lowL,",",uppR,"),",SRID,") AND fires.time >= '",minT,"' AND fires.time < '",maxT,"';")
    
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
    
    p4s = CRS(paste0("+init=epsg:", SRID))
    for (i in seq(nrow(res))) {
        if (i == 1) {
            spTemp <-  readWKT(res$geom[i], res$ogc_fid[i], p4s)
        }
        else {
            spTemp <-  rbind(
                spTemp, readWKT(res$geom[i], res$ogc_fid[i], p4s)
            )
        }
    }
    subs <-  SpatialPointsDataFrame(spTemp, res[-2])
    
    dbClearResult(res)
    dbDisconnect(con)
    return(subs)
    
}





# subset parameters
lower_left <- c(6400000, 1950000)
upper_right <- c(6500000 ,2050000)
minTime <- as.POSIXct("1990-01-01")
maxTime <- as.POSIXct("2000-01-01")
srid <- "2229"

subset_pt <- subsetPoints(lower_left, upper_right, minTime, maxTime, srid, con)

# plot the points
points(subset_pt, col = "red")

