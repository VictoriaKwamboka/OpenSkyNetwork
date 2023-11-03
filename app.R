library(shiny)
library(httr)
library(jsonlite)
library(shinydashboard)
library(ggplot2)
library(dashboardthemes)
library(dplyr)
library(jsonlite)
library(leaflet)
library(plotly)
library(shinythemes)
library(countrycode)


#####get data from open sky api------------------------------------------------------

res<- GET(paste("https://opensky-network.org/api/states/all?time=&icao24="))
data<- fromJSON(rawToChar(res$content))
data<- data[['states']]
data<- as.data.frame(data)

colnames(data)<- c("icao24","callsign","origin_country","	
time_position","last_contact","longitude","latitude","baro_altitude","on_ground",
                   "velocity","true_track","vertical_rate","sensors","geo_altitude",
                   "squawk","spi","position_source")
data$latitude<- as.numeric(data$latitude)
data$longitude<- as.numeric(data$longitude)
data$continent<- countrycode(sourcevar = data[,"origin_country"],
                             origin = "country.name",
                             destination = "continent")


## define user interface------------------------------------------------
ui <- fluidPage(
                tags$style(HTML("body { background-color: #cadbda; padding: 0;
            margin: 0}")),
                div(
                  style = "margin-left:40%;margin-bottom:2%;",
                  titlePanel(h2("Open Sky Network",style="color:teal;"),
                             ),
                  br(),
                ),
                br(),
                br(),
                # Sidebar layout with input and output definitions -----------------
                
                div(style = "padding-bottom: 6em;
                             margin-top:-5em;",
                    
                    sidebarLayout(
                      # Sidebar panel for inputs ---------------------------------------
                      
                      sidebarPanel(
                        width = 2,
                        # Input: Drop down
                        # selectInput("continent", "Select Continent", choices = sort(data$continent), selected = as.factor(levels(data$continent))),
                        selectInput("country", "Select A Country", choices = sort(data$origin_country), selected = as.factor(levels(data$origin_country)[1])),
                        
                        
                        div(style = "font-size: 15px",
                            fluidRow(
                              column(12,
                                     h3("Reference"),
                                     a("Open Sky Network",
                                       href = "https://openskynetwork.github.io/opensky-api/rest.html#own-state-vectors"),
                                     br()))),
                        br(),
                        br(),
                        br(),
                        br(),
                        br(),
                        br(),
                        br()
                        
                        
                      ),
                      
                      # Main panel for displaying outputs ----------------
                      
                      mainPanel(
                        
                        tabsetPanel(type = "tabs",
                                    tabPanel("Open Street Map", 
                                             
                                             tags$style(HTML("body { padding:0; margin:0; position:relative;}")),
                                             div( class = 'wrapper', style = {"top:0; bottom:0;height:100%; width:100%"},
                                                  leafletOutput("mymap")
                                             )
                                    ),
                                    tabPanel("Data", 
                                             
                                             DT::dataTableOutput("tbl"))
                        )
                        
                        
                        
                       
                        
                        
                      )
                      
                    )
                )
)


server <- function (input, output, session){
  
  ## observe event--------------------------------
  # observe({
  #                subcat<- unique(data$country[data$continent== input$continent])
  #                updateSelectInput(session, "country",
  #                                  choices = subcat
  #                                  
  #                )
  #   })
  

  ## icon -------------------------------------------
  airplaneicon <- makeIcon(
    iconUrl = "https://upload.wikimedia.org/wikipedia/commons/thumb/6/64/Plane_font_awesome.svg/1200px-Plane_font_awesome.svg.png",
    iconWidth = 30,
    iconHeight = 30,
    iconAnchorX = 30,
    iconAnchorY = 30
  )
  ## output map----------------------------------------------------------
  output$mymap <- renderLeaflet({
    selected_data<- subset(data, origin_country == input$country)
   leaflet(selected_data)%>%
      addTiles()%>%
      addMarkers(lng = ~longitude, lat = ~latitude, icon = airplaneicon)
  })
  
  
  output$tbl = DT::renderDataTable(
    data
  )
  
  
}


shinyApp(ui, server)