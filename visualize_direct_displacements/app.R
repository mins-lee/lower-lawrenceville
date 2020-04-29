#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
rm(list=ls())
library(shiny)
library(dplyr)
library(sf)
library(leaflet)
library(tidyr)
library(viridis)
library(shinythemes)
library(scales)
library(ggplot2)
load("files for direct displacement viz.Rdata")

included_neighborhoods<-c("East Liberty", "Homewood","Homewood West", "Homewood South","Homewood North",
                          "Larimer","Point Breeze","Point Breeze North", "East Hills", "Swisshelm Park",
                          "Shadyside", "Squirrel Hill North", "Squirrel Hill South", "Garfield", "Bloomfield",
                          "Highland Park", "Morningside",
                          "Greenfield", "Hazelwood","Glen Hazel", "Lincoln-Lemington-Belmar",
                          "Lower Lawrenceville", "Upper Lawrenceville", "Central Lawrenceville",
                          "Morningside", "Stanton Heights")



library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("Redevelopment and Housing Voucher Recipient Movement"),
    #add theme
    fluidPage(theme = shinytheme("slate")),
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            #add checkboxes for apartment/development type
            checkboxGroupInput("event_type",
                               "Development Type:",
                               selected = unique(bnp_parcels$Type),
                               choiceNames = unique(bnp_parcels$Type),
                               choiceValues = unique(bnp_parcels$Type)),
            sliderInput("event_year",
                        "Select timespan for development completion:",
                        min = min(bnp_parcels$`Date-Completed`),
                        max = max(bnp_parcels$`Date-Completed`),
                        value = c(min(bnp_parcels$`Date-Completed`),
                                  max(bnp_parcels$`Date-Completed`)),
                        sep="")
            
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            HTML("The map below shows parcels of land that have experienced major redevelopment over the past 20 years
                 where housing choice voucher recipients lived at some point in our study period.</br>
                 Click on a parcel to view the time-trend of voucher recipients on that property. 
                 </br>Use the options on the left to limit the developments displayed."),
            leafletOutput("map"),
            plotOutput("time_series"))
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    

    #create the base leaflet map
    output$map<-renderLeaflet({
        
        #st_transform(crs = "+init=epsg:4326") %>%
        leaflet()%>%
            setView(lng=-79.924577,lat=40.4609065,zoom=13.6) %>%
            addProviderTiles(provider = "CartoDB.Positron") %>%
            #add legend for bnps
            addLegend(position="bottomright",
                      pal=pal2,
                      values=unique(bnp_parcels$Type),
                      title = "Development Type")
    })
    
    # create the same subset for bnp parcels
    bnp_parcel_subset<-reactive({
        bnp_parcels%>%
            filter(`Date-Completed`>=input$event_year[1] & `Date-Completed`<=input$event_year[2])%>%
            #limit just to bnp_parcels
            filter(Development.Projects%in%parcel_resident_panel$Development.Projects)%>%
            #limit to just selected event types
            filter(Type%in%input$event_type)
            
    })
    
    #create a palette for parcels
    #create a palette for development type
    pal2<-colorFactor(palette = "viridis",
                      domain = unique(bnp_parcels$Type))
    #add bnp_parcel_subset to map
    
    #create an observer that adds bnp parcels to the map
    observe({
        leafletProxy("map")%>%
            #clear out bnps stays from previous selection
            clearGroup("bnps")%>%
            addPolygons(data=bnp_parcel_subset()%>%
                            st_as_sf(),
                        #stroke=FALSE,
                        color=~pal2(Type),
                        opacity=0.6,
                        group="bnps",
                        layerId = ~Development.Projects,
                        popup = ~paste(Development.Projects,'</br>Completed in ',`Date-Completed`),
                        label = ~paste(Development.Projects,`Date-Completed`,sep=", "))
    })
    
    #create a reactive corresponding to july 1 of the year of the event completion
    development_date<-reactive({
        if(!is.null(input$map_shape_click$id)){
            temp_year<-bnp_parcels%>%
                as.data.frame()%>%
                filter(`Development.Projects`==input$map_shape_click$id)%>%
                select(`Date-Completed`)%>%
                unique()
            
            as.Date(paste0(temp_year,"-06-01"),format("%Y-%m-%d"))
        }else{
            NA
        }
        
    })
    observe({
        print(development_date())
    })
    
    
    #create a reactive object of a ggplot graph showing 
    output$time_series<-resident_plot<-renderPlot({
        if(length(input$map_shape_click$id)>0){
            #store reactive of development date in a fixed variable so it only runs once per click
            development_date_click<-development_date()
            rez_subset<-parcel_resident_panel
            residents_in_parcel<-parcel_resident_panel%>%
                filter(`Development.Projects`==input$map_shape_click$id)%>%
                mutate(date=as.Date(date,origin="1970-1-1"))

            residents_in_parcel%>%
                # group_by(date)%>%
                # summarise(residents=n_distinct(CLIENT_ID))%>%
                # ungroup()%>%
                ggplot(aes(x=date,y=residents,ymin=0,ymax=max(residents)))+
                geom_bar(aes(x=date,y=residents),stat="identity",fill="green",alpha=.5)+
                theme_minimal()+
                geom_smooth()+
                labs(title=paste0("Voucher residents at site of ",
                                  input$map_shape_click$id,
                                  " development"),
                     y="Voucher Residents on site",
                     x="Date")+
                scale_color_viridis_d()+
                scale_y_continuous(label=comma)+
                scale_x_date(limits=c(as.Date("2003-01-01",format("%Y-%m-%d")),
                                      as.Date("2020-01-01",format("%Y-%m-%d"))))+
                #add vertical line for event date
                geom_vline(aes(xintercept=development_date_click))+
                geom_text(aes(x = development_date_click),
                          y = max(residents_in_parcel$residents)/2,
                          label = paste0(input$map_shape_click$id," development completed"))
        }
        
    })
    


}

# Run the application 
shinyApp(ui = ui, server = server)
