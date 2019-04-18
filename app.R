### Author: Kevin Zolea ###
### This app will analyze air quality data for the state of NJ ###
### Install and load packages ###
library("devtools")
library("shiny")
library("tidyverse")
library("DT")
library("shinycssloaders")
library("leaflet")
library("leaflet.extras")
library("shinydashboard")
library("shinythemes")
library("shinyWidgets")
library("sf")
library("readr")
library("plotly")
library("shinyWidgets")
### Read in site information dataset for monitoring sites in NJ ###
sites_nj<-read_csv("Ambient_Air_Quality_Monitors_of_New_Jersey.csv",col_names = T)%>%
  dplyr::select(-X,-Y,-OBJECTID,-AIRS_CODE)
### Read in pollutant data for 2018 ###
df<-read_csv("air_quality_data.csv",col_names = T)
### Read in shapefile of NJ counties ###
counties_nj<-st_read(dsn = getwd(),layer = "NJ_counties")
### Change projection to work with leaflet map ###
counties_nj<-st_transform(counties_nj, crs="+init=epsg:4326")
###############################################################################
### Theme for plots ###
shiny_plot_theme<- theme_linedraw()+
  theme(plot.title=element_text(size=15, face="bold",vjust=0.5,hjust = 0.1),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        plot.background = element_blank(),
        panel.background = element_blank(),
        legend.position = "bottom",
        legend.background = element_blank(),
        legend.text=element_text(size=10, face="bold"),
        legend.title = element_blank(),
        plot.subtitle = element_text(size=15, face="bold",vjust=0.5,hjust = 0.1))
###############################################################################
### Define UI for application ###
ui <- navbarPage(theme = shinytheme("yeti"),
                 tags$b("NJ Air Quality Data Viewer"),
                 tabPanel("About App",
                              tags$head(
                                #includeCSS("/Users/kevinzolea/Desktop/Temp_Impairments/www/styles.css")),
                                includeCSS("C:/Users/kzolea/Desktop/Air_Quality_App/www/styles.css")),
                              class = "my_style_1",
                              h2("Introduction:"),
                              h3("The purpose of this app is to obtain data from the 
                                  Environmental Protection Agency's (EPA) Air Quality System Data Mart. 
                                  The EPA provides access to outdoor air quality data collected from state,
                                 local, and tribal monitoring agencies across the United States. This app will focus
                                 on analyzing data for the monitoring sites throughout New Jersey."),
                          useShinydashboard(),
                          br(),
                          h2("Learn more about air quality:"),
                          br(),
                          tags$table(class="my_table",
                                               tags$tr(
                                                 tags$th(class="header",style ="border-bottom: 1px solid #ddd;","Resource Title"),
                                                 tags$th(class="header",style ="border-bottom: 1px solid #ddd;","Website Link")
                                                 
                                               ),
                                     tags$tr(
                                       tags$td(class="row",style ="border-bottom: 1px solid #ddd;","Air Data Basic Info"),
                                       tags$td(class="row",style ="border-bottom: 1px solid #ddd;",tags$a(href="https://www.epa.gov/outdoor-air-quality-data/air-data-basic-information","Air Data Basics",target="_blank"))),
                                     tags$tr(
                                       tags$td(class="row",style ="border-bottom: 1px solid #ddd;","NAAQS Definiton"),
                                       tags$td(class="row",style ="border-bottom: 1px solid #ddd;",tags$a(href="http://ohioepa.custhelp.com/app/answers/detail/a_id/907/~/definition-of-national-ambient-air-quality-standards-(naaqs)","NAAQS Overview",target="_blank"))
                                     ),
                                     tags$tr(
                                       tags$td(class="row",style ="border-bottom: 1px solid #ddd;","Air Quality"),
                                       tags$td(class="row",style ="border-bottom: 1px solid #ddd;",tags$a(href="https://www3.epa.gov/airquality/cleanair.html","Air Quality Information",target = "_blank"))
                                     ),
                                     tags$tr(
                                       tags$td(class="row",style ="border-bottom: 1px solid #ddd;","Air Monitoring"),
                                       tags$td(class="row",style ="border-bottom: 1px solid #ddd;",tags$a(href="https://www3.epa.gov/airquality/montring.html","Air Monitoring Information",target = "_blank"))
                                     )
                                     )
                        ),
                 tabPanel("Data",
                          DT::dataTableOutput("data")%>%
                            withSpinner(type = 5, color = "blue")),
                 tabPanel("Map",
                          panel("For more information on NJ's ambient air monitoring network",a("click here",
                                href= "http://www.njaqinow.net/NJ-Network-Plan-2018.pdf",target = "_blank"),
                                status = "info"),
                          leafletOutput("map1", height = "95vh")%>%
                            withSpinner(type = 5, color = "blue"),
                          absolutePanel(top = 125, left = 50,draggable = T,
                                        wellPanel(HTML("<font color = 'white'> The markers on this map <br>
                                                                  indicate the ambient air quality <br>
                                                                    monitoring stations
                                                                  throughout the state

                                                       </font>")))),
                 tabPanel("Plots",
                          sidebarLayout(
                            sidebarPanel(width = 3,
                                         tags$style(".well {background-color:#222222;}"),
                                         #selectInput("sites",label = strong("Select Air Quality Site:",
                                          #                                  style = "color:white;font-weight: bold;font-size:1.3em;"),
                                          #           choices = df$Site_Name),
                                         selectizeInput("pollutant",label = strong("Select Pollutant:",
                                                                               style = "color:white;font-weight: bold;font-size:1.3em;"),
                                                        choices = df$AQS_PARAMETER_DESC,multiple = F),
                                         HTML("<font color = 'white'>Table to help understand AQI</font>"),br(),
                                         tags$img(src= "Capture.png",style = "max-width: 100%; width: 100%; height: auto;"),
                                         HTML("<font color = 'white'>Author: Kevin Zolea\n (kevin.zolea@gmail.com)</font>")),
                            mainPanel(plotlyOutput("plot1")%>%withSpinner(type = 5, color = "blue"),
                                      plotlyOutput("plot2")%>%withSpinner(type = 5, color = "blue")))))
###############################################################################
server <- function(input, output,session) {
############################################################################### 
  ### Creates a data table for the data tab ###
  output$data <- DT::renderDataTable({
    
      DT::datatable(df,filter = 'top',options = list(scrollX = TRUE,
                                                            pageLength = 100))
    
  })
###############################################################################
### Create custom icon for markers on map ###
  aq_icon<-makeIcon(
    iconUrl = "https://www.pca.state.mn.us/sites/default/files/aqi-icon-airdata.png",
    iconWidth = 25 , iconHeight = 25,
    iconAnchorX =30 , iconAnchorY = 30
  )
###############################################################################  
### Create map of ambient air monitoring stations in NJ ###
  output$map1<-renderLeaflet({
    leaflet(data = sites_nj, options = leafletOptions(minZoom = 7))%>%
      addTiles()%>%
      addResetMapButton()%>%
      addTiles(group = "OSM (default)") %>%
      addPolygons(data = counties_nj,popup= paste("County:",counties_nj$COUNTY,sep=""),color = "black",
                  fillColor = "#00FFFFFF",weight = 1,
                  highlightOptions = highlightOptions(color = "blue",
                                                      weight = 2,bringToFront = TRUE))%>%
      addProviderTiles(providers$OpenStreetMap.BlackAndWhite, group = "Grey")%>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Satellite")%>%
      setView(lng = -74.4 ,lat =40, zoom = 8)%>%
      addMarkers(~LON,~LAT, popup = ~paste("<h4> Site Name:</h4>",SITE_NAME,sep = ""),
                 label = ~SITE_NAME,icon = aq_icon)%>%
      addLayersControl(
        baseGroups = c("OSM (default)", "Grey", "Satellite"),
        options = layersControlOptions(collapsed = FALSE))
  })
###############################################################################      
  ### Make reactive dataframe ###
  
  df_final<-reactive({
    req(input$pollutant)
    df%>%
      dplyr::filter(AQS_PARAMETER_DESC == input$pollutant)
  })
  
############################################################################### 
 
  df_aqi<-reactive({
    req(input$pollutant)
    df%>%
      dplyr::filter(AQS_PARAMETER_DESC == input$pollutant)%>%
      dplyr::group_by(Date,Site_Name)%>%
      dplyr::summarise(value = mean(DAILY_AQI_VALUE))
  })
  
  
############################################################################### 
   ### Updates Pollutant drop down based on what site is picked  ###
 # observe({
 #   req(input$sites)
 #   pollutant_choices<- as.list(df_final()$AQS_PARAMETER_DESC[df_final()$Site_Name==input$sites])
 #   names(pollutant_choices)<-df_final()$AQS_PARAMETER_DESC[df_final()$Site_Name==input$sites]
 #   
 #   updateSelectizeInput(session,"pollutant",choices = pollutant_choices,selected = pollutant_choices)
 #  
 #   
 #   
 # })
 # 
###############################################################################
###############################################################################      
  ### Creates plot ###
  
  output$plot1<-renderPlotly({
    p<-ggplot(data = df_final())+
      geom_line(aes(x=Date,y=val,color = Site_Name))+
      ggtitle(paste(input$pollutant,"(",df_final()$UNITS,")"," Concentration (2018)",sep = ""))+
      shiny_plot_theme 
      
    
    
    ggplotly(p,dynamicTicks = "x",tooltip = "text")%>%
      config(displaylogo=F,modeBarButtonsToRemove = list("lasso2d","hoverClosestCartesian",
                                                                         "hoverCompareCartesian","select2d"))
    
    
    })
###############################################################################      
  output$plot2<-renderPlotly({
    g<-ggplot(data=df_aqi(),aes(x=Date,y=value,fill = Site_Name))+
      geom_bar(stat="identity",position = position_dodge())+
      geom_hline(aes(yintercept = 50, color = "green"))+
      shiny_plot_theme+
      ggtitle(paste("Average Daily Air Quality Index Value (2018)\n",input$pollutant,sep = ""))
    

    
    
    ggplotly(g,dynamicTicks = "x",tooltip = "text")%>%
      config(displaylogo=F,modeBarButtonsToRemove = list("lasso2d","hoverClosestCartesian",
                                                                         "hoverCompareCartesian","select2d"))
    
  })
   
  }
###############################################################################
### Run the application 
shinyApp(ui = ui, server = server)







