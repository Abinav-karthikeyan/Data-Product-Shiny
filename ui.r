#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


library(shiny)

library(leaflet)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("KNN BASED PREDICTION MODEL"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            sliderInput("s","enter k value",min=1,max=7,value=2),
            checkboxInput("map","show map cluster",value=FALSE)
            
            
        ),
        

        # Show a plot of the generated distribution
        mainPanel(
            h3("MODEL ACCURACY CORRESPONDING TO THE K VALUE:"),
            textOutput("kn"),
            h3("MAP OF PERSONNEL LOCATION"),
            leafletOutput("p")
            
        )
    )
))
