#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(tidyverse)
library(dplyr)
library(shiny)
load("shiny_app_data.Rda")


ui <- fluidPage(
  headerPanel("Yelp Elite Reviews"),      
  sidebarPanel(
    wellPanel(
      selectInput(
        inputId = "restaurant",
        label = "Select Restaurant",
        choices = shiny_app_df$restaurant
      ),
      submitButton("Submit")
    )
    
  ),
  mainPanel(
    
    p(strong("Paired t-test: "),style="font-size:200%"),
    span(textOutput(outputId = "test"), style="font-size:120%"),
    
    
    p(strong("Diference in difference analysis: "),style="font-size:200%"),
    span(textOutput(outputId = "analysis"), style="font-size:120%"),
    
    
    textOutput(outputId = "contact_info")
    
  )      
)

server<-function(input,output){
  
  output$test <- renderText({
    paste("Review change: ", round(shiny_app_df[which(shiny_app_df$restaurant==input$restaurant),2],4),"Rating change: ",round(shiny_app_df[which(shiny_app_df$restaurant==input$restaurant),3],4))
  })
  
  
  output$analysis <- renderText({
    paste( "Review change: ", round(shiny_app_df[which(shiny_app_df$restaurant==input$restaurant),4], 4),"Rating change: ", round(shiny_app_df[which(shiny_app_df$restaurant==input$restaurant),5],4))
  })
  
  output$contact_info <- renderText({
    paste("App maintained by Zihan Tang. Contact Zihan Tang
              (ztang244@wisc.edu) for any questions or bugs. ")})
  
}

shinyApp(ui,server)
