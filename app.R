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
library("readxl")
### Read in site information dataset for monitoring sites in NJ ###
sites_nj<-read_csv("Ambient_Air_Quality_Monitors_of_New_Jersey.csv",col_names = T)%>%
  dplyr::select(-X,-Y,-OBJECTID,-AIRS_CODE)
### Read in pollutant data from 1990-2018 ###
ozone<-read_xlsx("ozone_1990_2018.xlsx",col_names = T)
SO2<-read_xlsx("SO2_1990_2018.xlsx",col_names = T)
NO2<-read_xlsx("NO2_1990_2018.xlsx",col_names = T)
PM2.5<-read_xlsx("PM2.5_1999_2018.xlsx",col_names = T)
### Get rid of * inside annual average column and make column numeric ###
PM2.5$weighted_arithmetic_meanC = as.numeric(gsub("\\*", "", PM2.5$weighted_arithmetic_meanC))
CO<-read_xlsx("CO_1990_2018.xlsx",col_names = T)
### Create a list of all the dataframes ###
mylist = list(ozone=ozone,SO2=SO2,NO2=NO2,CO=CO,
              PM2.5=PM2.5)
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
                 windowTitle = "NJ Air Quality Data Viewer",
                 tags$b("NJ Air Quality Data Viewer"),
                 tabPanel("About App",
                              tags$head(
                                #includeCSS("/Users/kevinzolea/Desktop/Temp_Impairments/www/styles.css")),
                                includeCSS("C:/Users/kzolea/Desktop/Air_Quality_App/www/styles.css")),
                              class = "my_style_1",
                              h2("Introduction:"),
                              h3("The purpose of this app is to understand the different trends for the criteria air pollutants throughtout the
                                 state's air quality network. Users can filter the data to see each station's criteria pollutant trends."),
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
                          tabPanel("Data",
                                   selectInput(inputId="pollutant",label="Select Pollutant:", 
                                               choices=c("ozone"="ozone","SO2"="SO2","NO2"="NO2","CO"="CO",
                                                         "PM2.5"="PM2.5")),
                          DT::dataTableOutput("data")%>%
                            withSpinner(type = 5, color = "blue"))),
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
                                         #selectizeInput("pollutant",label = strong("Select Pollutant:",
                                         #                                      style = "color:white;font-weight: bold;font-size:1.3em;"),
                                         #               choices = df$AQS_PARAMETER_DESC,multiple = F),
                                         selectInput(inputId="pollutant2",label=strong("Select Pollutant:",
                                                                                       style = "color:white;font-weight: bold;font-size:1.3em;"),             
                                                     choices=c("ozone"="ozone","SO2"="SO2","NO2"="NO2","CO"="CO",
                                                               "PM2.5"="PM2.5")),
                                         HTML("<font color = 'white'>Author: Kevin Zolea\n (kevin.zolea@gmail.com)</font>"),br(),
                                         tags$a(href="https://www.nj.gov/dep/", target="_blank",
                                                img(width= 100,height = 100,src="https://www.nj.gov/dep/awards/images/deplogoB.jpg",class="road_pics"))),
                            mainPanel(plotlyOutput("plot1")%>%withSpinner(type = 5, color = "blue"))),
                          hr(),
                          h4("The Data provided for these plots can be found here:"),a(href="https://www.epa.gov/outdoor-air-quality-data","EPA Air Data",target="_blank"),br(),
                          h4("To find out more about NJ's air quality data and network click",a(href="http://www.njaqinow.net/","here",target="_blank"))))
                                      #plotlyOutput("plot2")%>%withSpinner(type = 5, color = "blue")))))
###############################################################################
server <- function(input, output,session) {
############################################################################### 
  ### Creates a data table for the data tab ###
  output$data <- DT::renderDataTable(
    
      DT::datatable(data.frame(mylist[input$pollutant]),filter = 'top',options = list(scrollX = TRUE,
                                                            pageLength = 100))
    
  )
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
  ### Creates plots ###
  
  output$plot1<-renderPlotly({
    if(input$pollutant2 == "ozone"){
    p<-ggplot(data = ozone)+
      geom_line(aes(x=Year,y=fourth_max_8hr,color = Station_Name,
                    text = paste("Station:",Station_Name)))+
      ylab("Concentration, Parts per Million (ppm)") +
      ggtitle(paste(input$pollutant2," Trend 4th-Highest Daily Maximum 8-Hour Concentration (ppm)",sep = ""))+
      geom_segment(aes(x=1997,xend=2008,y=0.08,yend=0.08,
                   text="1997 8−Hour NAAQS = 0.08 ppm"),color="red",
                   size =1.3,linetype = "dashed")+
      geom_segment(aes(x=2008,xend=2016,y=0.075,yend=0.075,
                       text = "2008 8−Hour NAAQS = 0.075 ppm"),color="red",
                   size =1.3,linetype = "dashed")+
      geom_segment(aes(x=2016,xend=2018,y=0.070,yend=0.070,
                       text="2016 8−Hour NAAQS"),color="red",size =1.3,linetype = "dashed")+
      shiny_plot_theme

      
    
    
    ggplotly(p,dynamicTicks = "x",tooltip = "text")%>%
      config(displaylogo=F,modeBarButtonsToRemove = list("lasso2d","hoverClosestCartesian",
                                                                         "hoverCompareCartesian","select2d"))
    }
    else if(input$pollutant2 == "CO"){
      p<-ggplot(data = CO)+
        geom_line(aes(x=Year,y=second_max_8hr,color = Station_Name))+
        ylab("Concentration, Parts per Million (ppm)") +
        ggtitle(paste(input$pollutant2," Trend 2nd Highest 8-Hour Average Concentration (ppm)",sep = ""))+
        geom_segment(aes(x=1990,xend=2018,y=9,yend=9),color="red",size =1.3,linetype = "dashed")+
        shiny_plot_theme 
      
      
      
      ggplotly(p,dynamicTicks = "x",tooltip = "text")%>%
        config(displaylogo=F,modeBarButtonsToRemove = list("lasso2d","hoverClosestCartesian",
                                                           "hoverCompareCartesian","select2d"))
    }
    
    else if (input$pollutant2 == "SO2"){
      p<-ggplot(data = SO2)+
        geom_line(aes(x=Year,y=Percentile_99th,color = Station_Name))+
        ggtitle(paste(input$pollutant2," Concentration",sep = ""))+
        geom_segment(aes(x=2010,xend=2018,y=75,yend=75),color="red",size =1.3,linetype = "dashed")+
        shiny_plot_theme 
      
      
      
      ggplotly(p,dynamicTicks = "x",tooltip = "text")%>%
        config(displaylogo=F,modeBarButtonsToRemove = list("lasso2d","hoverClosestCartesian",
                                                           "hoverCompareCartesian","select2d"))
    }
    
    else if (input$pollutant2 == "NO2"){
      p<-ggplot(data = NO2)+
        geom_line(aes(x=Year,y=Percentile_98th,color = Station_Name))+
        ggtitle(paste(input$pollutant2," Concentration",sep = ""))+
        geom_segment(aes(x=2010,xend=2018,y=100 ,yend=100 ),color="red",size =1.3,linetype = "dashed")+
        shiny_plot_theme 
      
      
      
      ggplotly(p,dynamicTicks = "x",tooltip = "text")%>%
        config(displaylogo=F,modeBarButtonsToRemove = list("lasso2d","hoverClosestCartesian",
                                                           "hoverCompareCartesian","select2d"))
    }
    else if (input$pollutant2 == "PM2.5"){
      p<-ggplot(data = PM2.5)+
        geom_line(aes(x=Year,y=weighted_arithmetic_meanC,color = Station_Name))+
        ggtitle(paste(input$pollutant2," Concentration",sep = ""))+
        geom_segment(aes(x=1999,xend=2013,y=15,yend=15),color="red",size =1.3,linetype = "dashed")+
        geom_segment(aes(x=2013,xend=2018,y=12,yend=12),color="red",size =1.3,linetype = "dashed")+
        shiny_plot_theme 
      
      
      
      ggplotly(p,dynamicTicks = "x",tooltip = "text")%>%
        config(displaylogo=F,modeBarButtonsToRemove = list("lasso2d","hoverClosestCartesian",
                                                           "hoverCompareCartesian","select2d"))
    }
    
    })

}
###############################################################################
### Run the application 
shinyApp(ui = ui, server = server)







