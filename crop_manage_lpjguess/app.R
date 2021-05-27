library(raster)
library(tidyverse)
library(RColorBrewer)
library(plotly)
library(htmltools)
library(DT)
library(RColorBrewer)
library(readr)
library(reshape2)
library(shiny)
library(shinydashboard)
library(leaflet)
library(sf)
library(rgeos)

# input data
# v <- getData("GADM", country="KEN", level=1, path="data")

# simulation table
sv <- read.csv("data/simulation_settings.csv", stringsAsFactors = FALSE)

# input data
ff <- list.files(path = "data", pattern = "summary_stat_vector", full.names = TRUE)
# ff <- list.files(path = "lpj_guess_explorer/data", pattern = "summary_stat_vector", full.names = TRUE)
ff <- grep("ccnonbnf", ff, invert = TRUE, value = TRUE)

dds <- list()

for(i in 1:length(ff)){
    vname <- gsub("A1-|_summary_stat_vector.rds","",basename(ff[i]))
    dds[[i]] <- readRDS(ff[i])
    # assign(paste0("model_",vname), v)
    names(dds)[[i]] <- paste0("model_",vname)
}

# dd <- grep("model", ls(), value = TRUE)
dd <- names(dds)

rdf <- data.frame(outvarsid = c("soc_2020_2049_TeCo_2014","flux_2020_2049_TeCo_2014",
                                "leach_2020_2049_TeCo_2014","yield_2020_2049_TeCo_2014"),
                  outvarsname = c("Soil organic carbon", "N flux", "N leach", "Yield"),
                  units = c("kg C/m^2","kg N/ha","kg N/ha","ton/ha"),
                  outvarspalette = c("YlOrBr", "PuBuGn", "PuBuGn", "YlGn"),
                  stringsAsFactors = FALSE)

# color <- colorFactor(topo.colors(7), crime$CR)

#############################################################################################
# Define UI 
ui <- dashboardPage(dashboardHeader(title = "LPJ GUESS explorer", titleWidth = 300),
    
    ## Sidebar content
    dashboardSidebar(
        width = 200,
        sidebarMenu(
            menuItem("Information",tabName = "intro", icon = icon("info")),
            menuItem("Map",tabName = "map",icon = icon("map")),
            menuItem("Cost benefit analysis",tabName = "cba",icon = icon("search-dollar"))
        )             
    ),
    
    ## Body content
    dashboardBody(
        tags$head(
            tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
        ),
        tabItems(
            # First tab content
            tabItem(tabName = "intro",
                fluidRow(
                    h2(HTML("<strong>Background</strong>"), style = "font-size:20px;"),
                    h3(tags$p("The LPJ-GUESS global vegetation model has been applied to assess the impacts of large scale adoption of improved crop management practices, e.g. residue retention, manure application and reduced tillage, on SOC, yield and nitrogen leaching in cropland areas in Kenya and Ethiopia. As part of this work, the model has been developed to include biological nitrogen fixation and applied at 10 minute resolution using downscaled CRU JRA climate data. Here we show the results for pure or monoculture maize) simulation."), style = "font-size:15px;"),
                    h3(HTML("<strong>Simulation settings used for comparison of soil fertility and yields with different managements</strong>"), style = "font-size:18px;"),
                    DT::dataTableOutput("simtable", width = "auto", height = "auto")
                )
            ), 
            # Second tab content
            tabItem(tabName = "map",
                #drop down menu    
                fluidRow(
                    column(width = 6,
                           selectInput(
                               inputId = "simulation",
                               label = "Select simulation",
                               selected = NULL, 
                               choices =c("",
                                   "Cover crops"="cc",
                                   "No tillage"="nt",
                                   "Residue retention"="rr",
                                   "No manure"="nman",
                                   "Mineral N fertilizer replacement"="mineralN",
                                   "Conservation Agriculture"="conserv",
                                   "Irrigation"="irri",
                                   "More realistic management"="A2"
                               )) 
                    ), # end column 1 input; start output
                    column(width = 6,
                           selectInput(
                               inputId = "outvar",
                               label = "Select output",
                               selected = NULL,
                               choices =c("",
                                   "Soil organic carbon"="soc_2020_2049_TeCo_2014",
                                   "N flux"="flux_2020_2049_TeCo_2014",
                                   "N leach"="leach_2020_2049_TeCo_2014",
                                   "Yield"="yield_2020_2049_TeCo_2014"
                               ))
                    )
                ),
                leafletOutput("map")
                # column(4, downloadButton("downloadMap", "Download Map")),
                # column(4, downloadButton("downloadTable", "Download Table"))
            ),
            # third tab content
            tabItem(tabName = "cba",
                    fluidRow(
                        h2(HTML("<strong>Required Input</strong>"), style = "font-size:15px;"),
                        column(4, numericInput("discountrate", "Discount Rate", 0.1, min = 0, max = 1, width=100)),
                        column(4, numericInput("exchangerate", "Exchange Rate", 100, min = 1, max = 1000, width=100)),
                        column(4, numericInput("timehorizon", "Time Horizon", 45, min = 1, max = 1000, width=100)),
                        # column(4, numericInput("popfarmers", "% Farmers", 45, min = 1, max = 1000, width=100)),
                        
                        # h2(HTML("<strong>Option 1</strong>"), style = "font-size:15px;"),
                        box(
                            title = "Option 1: Cost of CA parctices",
                            sliderInput("slider", "% increase in cost", 1, 100, 1)
                        ),
                        
                        # h2(HTML("<strong>Option 2</strong>"), style = "font-size:15px;"),
                        #Selector for file upload
                        fileInput('datafile', 'Option 2: Cost of practicess (choose CSV file)',
                                  accept=c('text/csv', 'text/comma-separated-values,text/plain')),
                        
                        # numericInput("farmcost", "Cost of Conventional Practice", 1000, min = 0, max = 10000),
                        # numericInput("csacost", "Cost of CSA Practice", 100, min = 1, max = 1000)
                        
                        #submitButton("Run CBA analysis"),
                        
                        column(4, downloadButton("downloadCBAresults", "Download CBA report"))

                    )
            )
        )
    )
)


# Define server logic
server <- function(input, output) {
    output$simtable = DT::renderDataTable({
        datatable(sv %>% as.data.frame(), options = list(pageLength = 10, autoWidth = TRUE), rownames= FALSE)
    })
    
    # listen for input
    # y1 <- reactive({
    #     req(input$simulation)
    #     input$simulation
    # })
    
    spdata <- eventReactive(input$simulation, {
        req(input$simulation)
        mod <- grep(input$simulation, dd, value = TRUE)
        # spdata <- get(mod)
        out <- dds[grepl(mod, names(dds))][[1]]
        out
    })
    
    sdf <- eventReactive(input$outvar, {
        req(input$outvar)
        out <- rdf[grep(input$outvar, rdf$outvarsid, ignore.case = TRUE),]
        # sdf <- rdf[grep(outvar, rdf$outvarsid, ignore.case = TRUE),]
        out
    })
    
    output$map <- renderLeaflet({
        req(spdata(), sdf())
        
        spdataframe <- as.data.frame(spdata())
        
        val <- as.numeric(spdataframe[,sdf()$outvarsid])
        pal <- colorBin(sdf()$outvarspalette, domain = val)
        
        bins <- round(seq(from = min(val), to = max(val), length.out = 10),2)
        pal <- colorBin(sdf()$outvarspalette, bins = bins)
        
        yld <- paste0(round(spdata()$yield_2020_2049_TeCo_2014, 2), " ton/ha")
        soc <- paste0(round(spdata()$soc_2020_2049_TeCo_2014, 2), " kg C/m^2")
        
        sdata <- st_as_sf(spdata())
        
        sdata %>%
            mutate(popup = paste("<strong>", NAME_1, "</strong>","<br/>",
                                 "Yield ", yld,"<br/>",
                                 "SOC ", soc) %>%
                       map(htmltools::HTML)) %>%
            leaflet() %>%
            setView(lng = 39, lat = 6, zoom = 5) %>%
            addTiles() %>%
            addLayersControl(
                position = "bottomright",
                options = layersControlOptions(collapsed = FALSE)) %>%
            addProviderTiles(providers$CartoDB.Positron) %>%
            addPolygons(label = ~popup,
                        fillColor = ~pal(val),
                        color = "#444444",
                        weight = 1,
                        smoothFactor = 0.5,
                        opacity = 0.2,
                        fillOpacity = 1,
                        highlightOptions = highlightOptions(color = "red",
                                                            weight = 1,
                                                            bringToFront = TRUE),
                        labelOptions = labelOptions(
                            style = list("font-weight" = "normal", padding = "3px 8px"),
                            textsize = "15px",
                            direction = "auto")) %>%
            addLegend(pal = pal,
                      values = ~val,
                      opacity = 1,
                      position = "bottomleft",
                      title = sdf()$outvarsname)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)


# better
# https://github.com/eparker12/nCoV_tracker
# how to add summary values https://github.com/rstudio/shiny-gallery/tree/master/nz-trade-dash
# also streaming CRAN data https://rstudio.github.io/shinydashboard/examples.html
# more customization appsilon blog with two new packages
# https://www.paulamoraga.com/book-geospatial/sec-dashboardswithshiny.html
# rsconnect::deployApp('lpj_guess_explorer')