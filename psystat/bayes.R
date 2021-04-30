fluidRow(
  
  # input goes here
  column(4,

        ##change the title here
        div(helpText('Bayes factor calculator'), align = "left", style="font-size:140%"),
        
        br(),
        
        radioButtons(inputId = "test",
                     label = "Test:",
                     choiceNames = c("t-test",
                                     "ANOVA"),
                     choiceValues = c("ttest",
                                      "anova"),
                     selected = "ttest"
                     ),
        
        radioButtons(inputId = "design",
                     label = "Design:",
                     choiceNames = c("Single sample",
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
         
        uiOutput("statistic"),
        
        uiOutput("sample1"),
        
        uiOutput("sample2"),
        
        numericInput(inputId = "pH0",
                     label = "Prior probability of null:",
                     value=0.5,
                     min=0,
                     max=1,
                     step=0.1
        )
        
        #submitButton("Calculate")
        ), # close column 1
  
  # output goes here
  column(8,
         tabsetPanel(
           tabPanel("Summary",
                    div(helpText('Model definitions:'), align = "left", style="font-size:140%"),
                    div(uiOutput("nullText"), style="font-size:130%"),
                    div(uiOutput("altText"), style="font-size:130%"),
                    br(),
                    div(helpText('Predictive adequacy:'), align = "left", style="font-size:140%"),
                    
                    plotOutput("pizza"),
                    
                    div(helpText('Bayes factors:'), style="font-size:140%"),
                    div(uiOutput("bfText"), align = "left", style="font-size:130%;"),
                    div(uiOutput("description"), align = "left", style="font-size:130%"),
                    br(),
                    div(helpText('Posterior probabilities:'), style="font-size:140%"),
                    div(uiOutput("postNull"), align = "left", style="font-size:130%;"),
                    div(uiOutput("postAlt"), align = "left", style="font-size:130%")
           ),
           tabPanel("Help", HTML("<iframe width=\"560\" height=\"315\" 
                                 src=\"https://www.youtube.com/embed/gnqDx8Zp8tk\" 
                                  frameborder=\"0\" allow=\"autoplay; encrypted-media\" allowfullscreen></iframe>")
           )
         )
   ) # close column
) # close fluid row
