# =========================================
# Load dataset
# Test code provided on GitHub
# https://github.com/rstats-gsoc/gsoc2016/wiki/Managing-and-visualizing-movement-data-with-PostGIS-and-R
# =========================================

library("spacetime")
library("sp")

# Dataset from spacetime package
# The spatial units are in scaled feet, taken from the NAD 83 state-plane coordinate system.

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
library(shiny)
library(miniUI)
library(leaflet)

# Set up database ==============================================================
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


# Shiny gadget to subset points in the database ================================

subsetPoints <- function(conn, name, geom, time){
    # # # # # # # #
    # Interactively subsets a group of points in a PostGIS database by on-screen selection and time range.
    # Outputs the the ID of the selected points.
    # Input:
    #     name – Character. Name of the PostGIS table which contains the points.
    #     geom – Character. Name of the geometry field in the PostGIS table.
    #     conn – PostgreSQLConnection.
    #     time – Character. Name of the timestamp field in the PostGIS table.
    # Output:
    #     List of the selected point IDs
    # Requires: sp, rpostgresql, rgeos, miniUI, shiny
    # Reference: 
    # rpostgis (https://github.com/mablab/rpostgis/blob/master/R/pgGetPts.r)
    # Shiny gadget (http://shiny.rstudio.com/articles/gadgets.html)
    # # # # # # # #
    
    # Get values for the UI from the database
    queryMinT <- paste0("SELECT min(",time,")::date FROM ", name,";")
    minT <- dbGetQuery(conn, queryMinT)[1,1] # data.frame -> Date
    queryMaxT <- paste0("SELECT max(",time,")::date FROM ", name,";")
    maxT <- dbGetQuery(conn, queryMaxT)[1,1]
    
    
    ui <- miniPage(
        gadgetTitleBar("Instruction"),
        miniTabstripPanel(
            miniTabPanel("Map", icon = icon("map-o"),
                        miniContentPanel(padding = 0,
                            leafletOutput("map", height = "100%")
                        )
            ),
            miniTabPanel("Parameters", icon = icon("sliders"),
                        miniContentPanel(
                            dateRangeInput("year", "Select date range", start=minT, end=maxT, 
                                   min=minT, max=maxT, sep = "")
                        )
            ),
            miniTabPanel("Testing", icon = icon("sliders"),
                         miniContentPanel(
                             textOutput("text")
                         )
            )
        )
    )
    
    server <- function(input, output, session) {
        
        queryDB <- function(conn, name, geom, time){
            # # # # # # # #
            # Database communication 
            #
            # Retrieve the EPSG (not SRID, because the sp.CRS() works only with
            # EPSG codes, and SRID not equal EPSG)
            str <- paste0("SELECT auth_srid FROM spatial_ref_sys,(SELECT DISTINCT(ST_SRID(",geom,")) FROM ",name," WHERE ",geom," IS NOT NULL) as f WHERE srid = f.st_srid;")
            epgs <- as.character(dbGetQuery(con, str)[1,1])
            # Check if the EPSG is unique, otherwise throw an error
            if (length(epgs) == 0) {
                stop("Multiple EPSGs in the point geometry")        
            }
            
            
            # The SQL query to subset the points, based on the user input parameters in the UI
            query <- paste0("SELECT ogc_fid, ST_AsText(", geom,") As geom FROM ", name," WHERE ",name,".",time," >= '",input$year[1],"' AND ",name,".",time," < '",input$year[2],"' ;")
            
            
            # retrieve the data from the database
            res <- dbGetQuery(conn, query)
            # cast the WKT back into a SpatialPointsDataFrame with the correct CRS
            row.names(res) <- res$ogc_fid
            # set projection
            p4s <-  CRS(paste0("+init=epsg:", epgs))
            # Read the WKTs into a data frame with their projection.
            spTemp <-  readWKT(res$geom[1], res$ogc_fid[1], p4s)
            for (i in 2:nrow(res)) {
                spTemp <-  rbind(spTemp, readWKT(res$geom[i], res$ogc_fid[i], p4s))
            }
            # cast the data frame into a SpatialPointsDataFrame
            subs <-  SpatialPointsDataFrame(spTemp, res[-2])
            
            return(subs)
        }
        
        # # # # # # # #
        # Gadget 

        output$map <- renderLeaflet({
            
            subs <- queryDB(conn, name, geom, time)
            # Plot the data 
            subs_tr <- spTransform(subs, CRS("+proj=longlat +datum=WGS84 +no_defs")) # projects data to leaflet's default CRS
            
            leaflet(subs_tr) %>% addTiles() %>% 
                addCircleMarkers(
                    radius = 6,
                    stroke = FALSE,
                    fillOpacity = 0.5,
                    color = "red"
                )
        })
        
        output$text <- renderText({
            paste("Date range selected:", paste(as.character(input$year), collapse = " to "))
        })
        
        # Handle the Done button being pressed.
        observeEvent(input$done, {
            # Return the brushed points. See ?shiny::brushedPoints.
            stopApp(TRUE)
        })
    }
    
    runGadget(ui, server)
    
}

subsetPoints(con, "fires", "wkb_geometry", "time")

