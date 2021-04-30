fluidRow(
  
  # input goes here
  column(4,

        ##change the title here
        div(helpText('Probability calculator'), align = "left", style="font-size:140%"),
        
        br(),
        
        selectInput(inputId = "dist",
                      label = "Distribution:",
                      choices = c("Normal"      = "rnorm",
                                  "t"           = "rt",
                                  "F"           = "rf"),
                      selected = "rnorm"),
        
        
        br(),
        
        uiOutput("mean"),
        uiOutput("sd"),
        uiOutput("df_1"),
        uiOutput("df_2"),
        
        br(),
        br(),
        
        helpText("Region of p-value:"),
        div(textOutput("region"),style="text-indent:20px;font-size:100%;"),
        br(),
        
        uiOutput("tail"),
        uiOutput("lower_bound"),
        uiOutput("upper_bound"),
        
        
        uiOutput("a"),
        uiOutput("b")  
  ), # close column 1

  # output goes here
  column(8,
         tabsetPanel(
           tabPanel("Summary",
                  plotOutput("plot"),
                  div(textOutput("area"), align = "center", style="font-size:150%;")
                  ),
           tabPanel("Help", 
                    HTML("<iframe width=\"560\" height=\"315\" 
                                 src=\"https://www.youtube.com/embed/ZL8Jtow7IlI\" 
                                  frameborder=\"0\" allow=\"autoplay; encrypted-media\" allowfullscreen></iframe>")
           )
         )
  ) # close column
) # close fluid row
