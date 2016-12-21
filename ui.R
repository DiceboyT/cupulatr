library(shiny)
library(shinythemes)
library(ggplot2)
library(dplyr)
library(stringr)

ui <- fluidPage(theme = shinytheme("slate"), titlePanel("cupulatr"),
                             
                             textInput("upper_text",
                                       label = "Upper Text"),
                             
                             textInput("lower_text",
                                       label = "Lower Text"),
                
                             checkboxGroupInput("color", label = "Color",
                                                choices = c("red","black","blue")),
                
                actionButton("go","Create"),
                
                plotOutput("cups", width = "100%",height = "220px"),
                
                verbatimTextOutput("num_cups")
                
)
                

                     
                
                             
                             
                            

        
        
                
                
              
