library(shiny)
library(leaflet)

## renderLeaflet() is used at server side to render the leaflet map 

shinyServer(function(input, output) {
  
  content <- paste("Memorial Union, Tempe, AZ", "<button style=\"font-weight:bold\">Add Complaint</button>"
  )
  
  output$mymap <- renderLeaflet({
    # define the leaflet map object
    leaflet() %>%
      addTiles() %>%
      setView(lng = -111.934382, lat = 33.417840 , zoom = 15) %>%
      addMarkers(lng = -111.934382, lat = 33.417840, popup =  content) #%>%
      # addPopups(lng = -111.934382, lat = 33.417840, popup= content
      # )
    
  })
  
})

#Uncomment to clean the cache
#rm(list=ls())


