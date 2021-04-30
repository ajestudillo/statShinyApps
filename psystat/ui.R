# PsyStat: A calculator for the book "Psychological Statistics: The Basics"
# Developed by Tom Faulkenberry and Keelyn Brennan
# version: March 26, 2021

# libraries
library(shiny)

# make navbar pages
fluidPage(
  shinyUI(navbarPage("PsyStat", 
                     tabPanel("Probability calculator",
                              source("prob.R")$value
                              ),

                     tabPanel("Bayes factor calculator",
                              source("bayes.R")$value
                              ),
                    
                     tabPanel("About",
                              source("about_page.R")$value
                              )
                     )
  )
)
                  


                 
