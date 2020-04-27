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

load("data for event study visualizer.Rdata")

included_neighborhoods<-c("East Liberty", "Homewood","Homewood West", "Homewood South","Homewood North",
                          "Larimer","Point Breeze","Point Breeze North", "East Hills", "Swisshelm Park",
                          "Shadyside", "Squirrel Hill North", "Squirrel Hill South", "Garfield", "Bloomfield",
                          "Highland Park", "Morningside",
                          "Greenfield", "Hazelwood","Glen Hazel", "Lincoln-Lemington-Belmar",
                          "Lower Lawrenceville", "Upper Lawrenceville", "Central Lawrenceville",
                          "Morningside", "Stanton Heights")

#limit voucher stays and blocks_and_parcels to 
voucher_stays<-voucher_stays%>%
    filter(hood%in%included_neighborhoods)

bnps<-bnps%>%
    filter(hood%in%included_neighborhoods)

bnp_parcels<-bnp_parcels%>%
    filter(Development.Projects%in%blocks_and_parcels$Development.Projects)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Visualize Event Study Treatments"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            # add a numeric input defining the buffer
            numericInput("buffer",
                        "Define radius of treatment range around development:",
                        value=250),
            # add checkboxes for race
            checkboxGroupInput("Race",
                        "Race:",
                        selected = unique(voucher_stays$RACE),
                        choiceNames=unique(paste0(voucher_stays$RACE)),
                        choiceValues=unique(voucher_stays$RACE)),
            #add checkboxes for apartment/development type
            checkboxGroupInput("event_type",
                               "Development Type:",
                               selected = unique(blocks_and_parcels$Type),
                               choiceNames = unique(blocks_and_parcels$Type),
                               choiceValues = unique(blocks_and_parcels$Type)),
            sliderInput("event_year",
                        "Select apartment/project completion year:",
                        min = min(bnp_blocks$`Date-Completed`),
                        max = max(bnp_blocks$`Date-Completed`),
                        value = min(2007),
                        sep=""),
            sliderInput("event_time",
                        "Select year of voucher stays relative to completion year:",
                        min = -6,
                        max = 6,
                        value = 0)
            
        ),

        # Show a plot of the generated distribution
        mainPanel(
            leafletOutput("map",height=700)
        )
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
                      values=unique(blocks_and_parcels$Type),
                      title = "Development Type")%>%
            addLegend(position="topright",
                      pal=pal,
                      values=c(0,1),
                      title = "Pct Res moving out")
    })
    
    #create a subset of voucher stays based on inputs
    voucher_stay_subset<-reactive({
        
        voucher_stays%>%
            filter(!is.na(MOVEOUTDATE))%>%
            #add year columns
            mutate(movein_year = as.numeric(format(MOVEINDATE,"%Y")),
                   moveout_year = as.numeric(format(MOVEOUTDATE,"%Y")))%>%
            #limit only to rows that fit the specified event time
            filter(movein_year<=as.numeric(input$event_year)+as.numeric(input$event_time) &
                       moveout_year>=as.numeric(input$event_year)+as.numeric(input$event_time))%>%
            #create a flag for whether or not the individual moved out in the given event year
            mutate(moved=moveout_year==input$event_year+input$event_time)%>%
            #filter race 
            filter(RACE%in%input$Race)%>%
            select(for_geocode_addr,CLIENT_ID,moved,movein_year,moveout_year)%>%
            unique()%>%
            #since multiple people live in the same address, need to group by address and summarize
            group_by(for_geocode_addr)%>%
            summarise(residents=n(),
                      moves=sum(moved))%>%
            ungroup()%>%
            mutate(pct_moved=moves/residents,
                   #make number of residents log base 2 + 1, 
                   log_res = log(residents,base=2)+3)%>%
            st_as_sf()
    })
    
    # create the same subset for bnp parcels
    bnp_parcel_subset<-reactive({
        bnp_parcels%>%
            filter(`Date-Completed`==input$event_year)%>%
            #limit to just selected event types
            filter(Type%in%input$event_type)%>%
            #change projection to a different crs so we can draw the buffer
            st_transform("+init=epsg:2163")
    })
    # create a subset of bnp based on selected event year
    bnp_subset<-reactive({
        temp<-bnps%>%
            filter(`Date-Completed`==input$event_year)%>%
            #limit to just selected event types
            filter(Type%in%input$event_type)%>%
            #change projection to a different crs so we can draw the buffer
            st_transform("+init=epsg:2163")%>%
            #draw buffer in meters
            st_buffer(input$buffer)
        for(i in 1:nrow(temp)){
            temp2<-temp[i,]
            temp3<-temp2%>%
                st_difference(bnp_parcel_subset()%>%
                                  filter(bnp_parcel_subset()$`Development.Projects`%in%temp2$Development.Projects))
                
            if(i==1){
                result=temp3
            }else{
                result=rbind(result,temp3)
            }
        }
     result%>%
         #convert back to 4326
         st_transform("+init=epsg:4326")       
    })
    

    #create a palette for move percentage
    #create color palette for moves in or out
    pal <-colorNumeric(palette = "RdYlGn",
                     domain = c(0,1),
                     reverse=TRUE)
    
    #create a palette for development type
    pal2<-colorFactor(palette = "viridis",
                     domain = unique(blocks_and_parcels$Type))
    
    #create an observer that adds bnp treatment zones to the map
    observe({
        leafletProxy("map")%>%
            #clear out bnps stays from previous selection
            clearGroup("bnps")%>%
            addPolygons(data=bnp_subset()%>%
                            st_as_sf(),
                        #stroke=FALSE,
                        color=~pal2(Type),
                        opacity=0.3,
                        group="bnps",
                        popup = ~Development.Projects)
    })
    #create an observer that adds bnp parcels zones to the map
    # observe({
    #     leafletProxy("map")%>%
    #         #clear parcels from previous selection
    #         clearGroup("bnp_parcels")%>%
    #         addPolygons(data=bnp_parcel_subset(),
    #                     #color="grey",
    #                     opacity=.5,
    #                     group="bnp_parcels",
    #                     popup = ~Development.Projects)
    # })

    #create an observer that adds voucher points to the map
    observe({
        leafletProxy("map")%>%
            #clear out voucher stays from previous selection
            clearGroup("voucher")%>%
            addCircleMarkers(data=voucher_stay_subset(),
                             stroke=FALSE,
                             radius = ~log_res,
                             opacity = .8,
                             #radius=~residents*2,
                             color=~pal(pct_moved),
                             group="voucher",
                             popup = ~paste("Residents: ",residents,"<br>",for_geocode_addr))
    })
    

}

# Run the application 
shinyApp(ui = ui, server = server)
