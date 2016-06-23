library(shiny)

shinyUI(pageWithSidebar(
  
  headerPanel("Calculator for Common Distributions"),
  
  sidebarPanel(
    #radio button or dropdown?

    selectInput(inputId = "dist",
                label = "Distribution:",
                choices = c("Normal"      = "rnorm",
                            "t"           = "rt",
                            "F"           = "rf"),
                selected = "rnorm"),
    

    br(),

    uiOutput("mean"),
    uiOutput("sd"),
    uiOutput("df1"),
    uiOutput("df2"),

    br(),
    br(),

    helpText("Model:"),
    div(textOutput("model"),style="text-indent:20px;font-size:100%;"),
    br(),

    uiOutput("tail"),
    uiOutput("lower_bound"),
    uiOutput("upper_bound"),
    

    uiOutput("a"),
    uiOutput("b"),
    
    br()),
  
  
  
  mainPanel(
    plotOutput("plot"),
    div(textOutput("area"), align = "center", style="font-size:150%;")
  )
))