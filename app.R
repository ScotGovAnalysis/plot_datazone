library(shiny)
library(sf)
library(tmap)
library(leaflet)
library(SPARQL)

ui <- fluidPage(

    titlePanel("What does your data zone look like?"),

    sidebarLayout(
        sidebarPanel(
            a(href = "https://data.gov.uk/dataset/ab9f1f20-3b7f-4efa-9bd2-239acf63b540/data-zone-boundaries-2011", "Data zones"),
            "are the key geography for the dissemination of small area statistics in Scotland and are widely used across the public and private sector. Type in your postcode to see what your data zone looks like.",
            br(),
            br(),
            textInput(inputId = "postcode", label = "Postcode:", value = "AB101AB"),
            actionButton("go", "Go"),
            br(),
            br(),
            a(href = "https://github.com/DataScienceScotland/plot_datazone", "R code")
        ),

        mainPanel(
           leafletOutput("dataZonePlot")
        )
    )
)

server <- function(input, output) {

    output$dataZonePlot <- renderLeaflet({
        
        postcode <- eventReactive(input$go, {
            toupper(gsub(" ", "", input$postcode, fixed = TRUE))
            })
        
        endpoint <- "http://statistics.gov.scot/sparql"
        
        query <- paste0("SELECT ?2011DzCode
                        WHERE {
                          <http://statistics.gov.scot/id/postcodeunit/", postcode(), "> <http://statistics.gov.scot/def/postcode/dataZone2011> ?2011DzURI .
                          ?2011DzURI <http://publishmydata.com/def/ontology/foi/code> ?2011DzCode
                        }")
        
        # Get the data zone of the given postcode from statistics.gov.scot
        dz_code <- SPARQL(endpoint,query)[["results"]][[1]][[1]]
        
        # Get the shapefile of the data zone from spatialdata.gov.scot:
                # Find 2011 Data Zones here: https://spatialdata.gov.scot/geonetwork/srv/eng/catalog.search#/metadata/7d3e8709-98fa-4d71-867c-d5c8293823f2
                # Go to the ESRI REST Service: http://sedsh127.sedsh.gov.uk/arcgis/rest/services/ScotGov/StatisticalUnits/MapServer/2
                # Click on query: http://sedsh127.sedsh.gov.uk/arcgis/rest/services/ScotGov/StatisticalUnits/MapServer/2/query
                
                # Build the query
                # Add `DataZone='XXXXXXXXX'` to the `Where:` field where XXXXXXXXX is a DataZone code
                # Add all the available fields (listed on the previous page) to the `Out Fields:` field: OBJECTID_1, OBJECTID, DataZone, Name, TotPop2011, ResPop2011, HHCnt2011, StdAreaHa, StdAreaKm2, Shape_Leng, Shape, Shape.STArea(), Shape.STLength()
                # Select `Format:` as GeoJSON
                # Click `Query(GET)`
            
                # Copy and paste the URL into `sf::st_read` like this:
                # dz <- st_read("http://sedsh127.sedsh.gov.uk/arcgis/rest/services/ScotGov/StatisticalUnits/MapServer/2/query?where=DataZone%3D%27S01009308%27&text=&objectIds=&time=&geometry=&geometryType=esriGeometryEnvelope&inSR=&spatialRel=esriSpatialRelWithin&relationParam=&outFields=OBJECTID_1%2C+OBJECTID%2C+DataZone%2C+Name%2C+TotPop2011%2C+ResPop2011%2C+HHCnt2011%2C+StdAreaHa%2C+StdAreaKm2%2C+Shape_Leng%2C+Shape%2C+Shape.STArea%28%29%2C+Shape.STLength%28%29&returnGeometry=true&returnTrueCurves=false&maxAllowableOffset=&geometryPrecision=&outSR=&returnIdsOnly=false&returnCountOnly=false&orderByFields=&groupByFieldsForStatistics=&outStatistics=&returnZ=false&returnM=false&gdbVersion=&returnDistinctValues=false&resultOffset=&resultRecordCount=&f=geojson")
        
        dz <- st_read(paste0("http://sedsh127.sedsh.gov.uk/arcgis/rest/services/ScotGov/StatisticalUnits/MapServer/2/query?",
                             "where=DataZone%3D%27",
                             dz_code,
                             "%27&text=&objectIds=&time=&geometry=&geometryType=esriGeometryEnvelope&inSR=&spatialRel=esriSpatialRelWithin&relationParam=&outFields=OBJECTID_1%2C+OBJECTID%2C+DataZone%2C+Name%2C+TotPop2011%2C+ResPop2011%2C+HHCnt2011%2C+StdAreaHa%2C+StdAreaKm2%2C+Shape_Leng%2C+Shape%2C+Shape.STArea%28%29%2C+Shape.STLength%28%29&returnGeometry=true&returnTrueCurves=false&maxAllowableOffset=&geometryPrecision=&outSR=&returnIdsOnly=false&returnCountOnly=false&orderByFields=&groupByFieldsForStatistics=&outStatistics=&returnZ=false&returnM=false&gdbVersion=&returnDistinctValues=false&resultOffset=&resultRecordCount=&f=geojson"))
        
        # Plot the shapefile with tmap
        tmap_mode(mode = "view")
        
        tm <- tm_shape(dz) +
            tm_polygons(alpha = 0)
        
        tmap_leaflet(tm)
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
