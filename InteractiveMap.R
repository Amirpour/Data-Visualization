###############################################
library(readr)
library(reshape2)
### With time
library(leaflet)
library(shiny)
#install.packages("sp") #will be needed to load rgdal
library(sp)
#install.packages("rgdal") #here's rgdal
library(rgdal) #enable it for your workflow
#install.packages(leaflet.minicharts)
library(leaflet.minicharts)
library(dplyr)
library(dygraphs)
library(rgdal)
library(xts)

setwd("C:/Users/amirpour/Documents/PostDoc/Paper/3rd paper")# 

#install.packages("sp") 
data=read_csv("FOIA_CYST2.csv")#,sep=",", stringsA
data<- data[-12:-49]
#format data
pie<-data.frame(data)
#as.Date(pie$Slau_Date, "%B %d,%Y")
pie$Slau_Date<-as.POSIXct(strptime(pie$Slau_Date, format = "%d-%b-%y"),"GMT")
#pie$Slau_Date<-str(pie$Slau_Date)
#strptime("16-Feb-05", format = "%d-%b-%y")
states <- map_data("state")
names(data)[3]<-"state.abb"
##arrests <-PD# USArrests#PD
#names(arrests) <- tolower(names(arrests))
statec <- data.frame(state.center, state.abb)
Year=as.POSIXct(c("2005-01-01","2006-01-01"),"GMT")

















  
  shinyApp(
    ui = fluidPage(
      sliderInput(inputId = "Year", 
                  label = "Date range:", 
                  min = as.Date("2005-01-01","%Y-%m-%d"),
                  max = as.Date("2015-01-01","%Y-%m-%d"),
                  value=c(as.Date("2005-01-01"),as.Date("2015-01-01")),timeFormat="%Y-%m-%d"),
      tags$div(title = "This input has a tool tip",
               selectInput(inputId = "Species", 
                           label = "Species", 
                           choices = sort(unique(pie$Species)))),
      leafletOutput("MapPlot1"),
      dygraphOutput("apple", width = "100%", height = "400px"),
      textOutput("message", container = h3)
      
    ),
    
    
    
    server = function(input, output, session) {
      #v <- reactiveValues(msg="")
      output$MapPlot1 <- renderLeaflet({
        leaflet() %>% 
          addProviderTiles("Stamen.TerrainBackground") %>% 
          setView(lng = -100, lat = 40, zoom =4)
        
          #layerId = Sites$state.abb %>%
          #addMinicharts(Sites$x, Sites$y, layerId = Sites$state.abb,
           #             type = "pie",
            #            chartdata = Sites[, 4:ncol(Sites)], 
                        #colorPalette = colors, 
             #           width = 20, transitionTime = 0
              #          )%>% 
      #    addCircleMarkers(Sites$x, Sites$y,layerId = Sites$state.abb)
        #%>%
        # addCircleMarkers(data=Sites, ~x , ~y, layerId=~state.abb, popup=~state.abb, radius=8 , color="black",  fillColor="red", stroke = TRUE, fillOpacity = 0.8)
      })
      
       observe({
        Year<- as.POSIXct(input$Year, tz="GMT")#as.POSIXct(,"GMT")#as.POSIXct(input$Year, tz="GMT")
        Species <-input$Species
        #v <- input$MapPlot1_marker_click#____________#https://rstudio.github.io/leaflet/shiny.html
        #https://stackoverflow.com/questions/42507943/click-event-on-leaflet-tile-map-in-shiny
        #https://stackoverflow.com/questions/28938642/marker-mouse-click-event-in-r-leaflet-for-shiny
        
        
        ## Observe mouse clicks and add circles
        #observeEvent(input$MapPlot1_marker_click, {
          ## Get the click info like had been doing
          click <- input$MapPlot1_marker_click
          #clat <- click$y#lat
          clng <- click$id#id#click$x#lng
        #})
        if(is.null(clng))
        # filter
        {Sites <- pie %>% 
          dplyr::filter(findInterval(pie$Slau_Date, c( as.POSIXct(Year[1],"GMT"),  as.POSIXct(Year[2],"GMT"))) == 1 &
                          pie$Species%in% input$Species)
        }else {
          
          Sites <- pie %>% 
            dplyr::filter(findInterval(pie$Slau_Date, c( as.POSIXct(Year[1],"GMT"),  as.POSIXct(Year[2],"GMT"))) == 1 &
                            pie$Species%in% input$Species & pie$State%in%clng) 
          
          
          
        }
        #######################
        TS <-Sites %>%
          dplyr::group_by(Slau_Date) %>% summarise(sumy = sum(PMCondemn))
        TS<-xts(TS,order.by=TS$Slau_Date)
        ###########
        
        Sites <-Sites %>%
          dplyr::group_by(State,Condemn_reason) %>% summarise(sumy = sum(PMCondemn))
        
        Sites <- melt(data = Sites, id.vars = c("State", "Condemn_reason"), measure.vars = "sumy")
        
        Sites<- dcast(Sites,  State ~ Condemn_reason ,  value.var="value",fun.aggregate = sum)
        
        colnames(Sites)[1] <- "state.abb"
        
        Sites<- merge(statec, Sites, sort = FALSE, by = "state.abb")
        
        leafletProxy("MapPlot1") %>% 
         addCircleMarkers(Sites$x, Sites$y ,radius=20,layerId = Sites$state.abb)%>% 
          addMinicharts(
            Sites$x, Sites$y,popup = popupArgs(),layerId = Sites$state.abb,
            type = "pie",
            chartdata = Sites[, 4:ncol(Sites)], 
            #colorPalette = colors, 
            width = 20, transitionTime = 0
          )%>%
          setView(lng = -100, lat = 40, zoom =4)
        ###Here you can add the Plot!!!
        output$apple <- renderDygraph({
          dygraph(TS)
        })
        
      })
      # })
    },
    options = list(height = 600)
  )
       
  ########################################################################



#https://stackoverflow.com/questions/31814037/integrating-time-series-graphs-and-leaflet-maps-using-r-shiny