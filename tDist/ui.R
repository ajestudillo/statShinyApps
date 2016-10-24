library("shiny")

shinyUI(pageWithSidebar(
  
  headerPanel("Comparing the t-Distribution and the Normal Distribution"),
  
  sidebarPanel(
    #radio button or dropdown?
    
    sliderInput(inputId = "df",
                label = "Degrees of Freedom",
                min = 1, max = 20, value = 1, step = 1,
                animate=animationOptions(interval=500, loop=T)),
    
    
    br(),
    br(),
    br(),
    p("Designed by ", a("Dr. Tom Faulkenberry",href="http://tomfaulkenberry.github.io"), ", modified from original code written by ", a("Dr. Jeffrey Arnold",href="http://jrnold.me")),
    p("For source code, vist my ", a("Github page", href="http://github.com/tomfaulkenberry/statShinyApps"))
    
  ),
  
  mainPanel(
    plotOutput("plot"),
    div(textOutput("area"), align = "center", style="font-size:150%;")
  )
))
