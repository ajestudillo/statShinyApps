library("shiny")

shinyUI(pageWithSidebar(
  
  headerPanel("How do unequal variances affect the t-test?"),
  
  sidebarPanel(
    sliderInput(inputId = "d",
                label = "Effect size",
                min = 0, max = 1, value = 0, step = 0.1
                ),
    
    selectInput(inputId = "var2",
                label = "Variance of Population 2",
                choices = c("Equal to Population 1" = 1,
                            "Less than Population 1" = 0.5,
                            "Greater than Population 1" = 2),
                selected = 1
                ),
    
    sliderInput(inputId = "N1",
                label = "Size of Sample 1",
                min = 10, max=100, value=50, step=1
                ),
    
    sliderInput(inputId = "N2",
                label = "Size of Sample 2",
                min = 10, max=100, value=50, step=1
                ),
    
    numericInput(inputId = "nSims",
                 label = "Number of simulations",
                 value = 10000
                 ),
    
    actionButton("runsim", "Re-plot distribution of p-values"),

    br(),
    br(),
    br(),
    p("Designed by ", a("Tom Faulkenberry", href="http://tomfaulkenberry.github.io"), " and ", a("Ruth Horry", href="https://www.swansea.ac.uk/staff/human-and-health-sciences/psychology/horry-r/")),
    p("For source code, vist my ", a("Github page", href="http://github.com/tomfaulkenberry/statShinyApps"))
    
  ),
  
  mainPanel(
    plotOutput("populations"),
    plotOutput("pValues"),
    div(textOutput("area"), align = "center", style="font-size:150%;")
    )
  )
)
