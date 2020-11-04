library(shiny)

shinyUI(pageWithSidebar(
  headerPanel("Bayes Factor Calculator"),
  
  sidebarPanel(
    
    radioButtons(inputId = "design",
                 label = "Design:",
                 choiceNames = c("Single/paired sample(s)",
                                 "Independent samples"),
                 choiceValues = c("single",
                                  "independent"),
                 selected = "single"
    ),
    
    radioButtons(inputId = "direction",
                 label = "Predicted direction:",
                 choiceNames = c("None",
                                 "Positive effect",
                                 "Negative effect"),
                 choiceValues = c("none", "positive", "negative"),
                 selected = "none"
                 ),
                 
    numericInput(inputId = "t",
                 label = "t-statistic:",
                 value = 0,
                 min = -1e5,
                 max = 1e5,
                 step=0.1
    ),
    
    uiOutput("sample1"),
     
    uiOutput("sample2"),
   
    numericInput(inputId = "pH0",
                 label = "Prior probability of null:",
                 value=0.5,
                 min=0,
                 max=1,
                 step=0.1
    ),
    
    br(),
    br(),
    p("Designed by ", a("Tom Faulkenberry",href="http://tomfaulkenberry.github.io")),
    p("For source code, vist my ", a("Github page", href="http://github.com/tomfaulkenberry/statShinyApps/"))
    
    
  ),
  
  mainPanel(
    fluidPage(
      title = 'Model definitions',
      withMathJax(),
      div(helpText('Model definitions:'), align = "left", style="font-size:140%"),
      div(sprintf('\\(\\mathcal{H}_0\\): effect size is 0'), style="font-size:130%"),
      div(uiOutput("altText"), style="font-size:130%")
    ),
    
    plotOutput("pizza"),
    div(helpText('Bayes factors:'), style="font-size:140%"),
    div(uiOutput("bfText"), align = "left", style="font-size:130%;"),
    div(uiOutput("description"), align = "left", style="font-size:130%"),
    br(),
    div(helpText('Posterior probabilities:'), style="font-size:140%"),
    div(uiOutput("postNull"), align = "left", style="font-size:130%;"),
    div(uiOutput("postAlt"), align = "left", style="font-size:130%")
    
  )
))