library(shiny)

shinyUI(pageWithSidebar(
  
  headerPanel("Probability Calculator for Common Distributions"),
  
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
    
    br(),
    br(),
    p("Designed by ", a("Dr. Tom Faulkenberry",href="http://tomfaulkenberry.github.io"), ", modified from original code written by ", a("Dr. Mine Cetinkaya-Rundel",href="http://www2.stat.duke.edu/~mc301/")),
    p("For source code, vist my ", a("Github page", href="http://github.com/tomfaulkenberry/statShinyApps"))
  
  ),
  
  mainPanel(
    plotOutput("plot"),
    div(textOutput("area"), align = "center", style="font-size:150%;")
  )
))