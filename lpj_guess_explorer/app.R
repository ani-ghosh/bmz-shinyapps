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

# input data
# v <- getData("GADM", country="KEN", level=1, path="data")

# input data
ff <- list.files(path = "data", pattern = "summary_stat_vector", full.names = TRUE)
# ff <- list.files(path = "lpj_guess_explorer/data", pattern = "summary_stat_vector", full.names = TRUE)
ff <- grep("ccnonbnf", ff, invert = TRUE, value = TRUE)

for(i in 1:length(ff)){
    vname <- gsub("A1-|_summary_stat_vector.rds","",basename(ff[i]))
    v <- readRDS(ff[i])
    assign(paste0("model_",vname), v)
}

dd <- grep("model",ls(), value = TRUE)

rdf <- data.frame(outvarsid = c("soc_2020_2049_TeCo_2014","flux_2020_2049_TeCo_2014",
                                "leach_2020_2049_TeCo_2014","yield_2020_2049_TeCo_2014"),
                  outvarsname = c("Soil organic carbon", "N flux", "N leach", "Yield"),
                  outvarspalette = c("YlOrBr", "PuBuGn", "PuBuGn", "YlGn"),
                  stringsAsFactors = FALSE)

# color <- colorFactor(topo.colors(7), crime$CR)

#############################################################################################
# Define UI 
ui <- dashboardPage(dashboardHeader(title = "LPJ GUESS explorer", titleWidth = 300),
    
    ## Sidebar content
    dashboardSidebar(
        width = 300,
        sidebarMenu(
            menuItem("Information",tabName = "intro", icon = icon("info")),
            menuItem("Map",tabName = "map",icon = icon("map")),
            menuItem("Cost benefit analysis",tabName = "cba",icon = icon("search-dollar"))
        )             
    ),
    
    ## Body content
    dashboardBody(
        
        tabItems(
            # First tab content
            tabItem(tabName = "intro",
                fluidRow(
                    h2(HTML("<strong>Enhancing soil carbon</strong>")),
                    h3(tags$p("1st paragraph"))
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
                               choices =c(
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
                               choices =c(
                                   "Soil organic carbon"="soc_2020_2049_TeCo_2014",
                                   "N flux"="flux_2020_2049_TeCo_2014",
                                   "N leach"="leach_2020_2049_TeCo_2014",
                                   "Yield"="yield_2020_2049_TeCo_2014"
                               ))
                    )
                ),
                leafletOutput("map")
            ),
            # third tab content
            tabItem(tabName = "cba",
                    fluidRow(
                        box(
                            title = "Cost of CA parctices",
                            sliderInput("slider", "% increase in cost", 1, 100, 1)
                        )
                    )
            )
        )
    )
)


# Define server logic
server <- function(input, output) {
    # summary table
    observeEvent(input$simulation, {
        mod <- grep(input$simulation, dd, value = TRUE)
        data <- get(mod)
    })
    
    observeEvent(input$outvar, {
        # output$my_tmap = renderTmap({
        #     tm_shape(data) + tm_polygons(input$outvar, legend.title = "Did it work")
        # })
        sdf <- rdf[rdf$outvarsid==input$outvar,]
        val <- as.numeric(data@data[, sdf$outvarsid])
        pal <- colorBin(sdf$outvarspalette, domain = val)
        
        bins <- round(seq(from = min(val), to = max(val), length.out = 10),2)
        pal <- colorBin(sdf$outvarspalette, bins = bins)
        
        yld <- paste0(round(data$yield_2020_2049_TeCo_2014, 2), " kg/ha")
        soc <- paste0(round(data$soc_2020_2049_TeCo_2014, 2), " kg/C-m^2")
        
        sdata <- st_as_sf(data)
        
        output$map <- renderLeaflet({
            sdata %>%
                mutate(popup = paste("<strong>", NAME_1, "</strong>","<br/>",
                                     "Yield ", yld,"<br/>",
                                     "SOC ", soc) %>%
                           map(htmltools::HTML)) %>%
                leaflet() %>%
                setView(lng = 39, lat = 6, zoom = 5) %>%
                addTiles("CartoDB.Positron", options = providerTileOptions(noWrap = TRUE)) %>%
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
                          title = sdf$outvarsname)
        })
    })
}

# Run the application 
shinyApp(ui = ui, server = server)



##########################################################################################
# summary input
# fluidRow(
#     # Dynamic infoBoxes
#     infoBoxOutput("yield"),
#     infoBoxOutput("soc"),
#     infoBoxOutput("leach"),
#     infoBoxOutput("flux"),
#     infoBoxOutput("fertilizer")
# )
# not used
# output$yield <- renderInfoBox({
#     infoBox(
#         "yield", paste("80", "kg/ha"), icon = icon("wheat"), color = "black")
# })
# output$soc <- renderInfoBox({
#     infoBox(
#         "soc", paste("80", "kg/ha"), icon = icon("seedling"))
# })
# output$flux <- renderInfoBox({
#     infoBox(
#         "flux", paste("80", "bags"), icon = icon("leaf"))
# })
# output$leach <- renderInfoBox({
#     infoBox(
#         "leach", paste("80", "bags"), icon = icon("humidity"))
# })
# output$fertilizer <- renderInfoBox({
#     infoBox(
#         "fertilizer", paste("80", "bags"), icon = icon("sack"))
# })

