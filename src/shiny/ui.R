library(shiny)

shinyUI(fluidPage(
  titlePanel("The warehouse location problem"),
  
  sidebarLayout(
    
    
    mainPanel(
      imageOutput("image"),
      textOutput("subtitle")
    ),
    sidebarPanel(
      p("Black dots are customers. Light red triangles show potential warehouse locations."),
    ),
    
  )
))