library(BayesFactor)
library(openintro)
source("functions.R")


shinyServer(function(input, output) { 
  
  # functions for Bayes factor calculator

  observeEvent(input$test, 
               {
                 if(input$test == "ttest"){
                   updateRadioButtons(inputId = "design",
                              label = "Design:",
                              choiceNames = c("Single sample",
                                              "Independent samples"),
                              choiceValues = c("single",
                                               "independent"),
                              selected = "single"
                 )
                   
                   updateRadioButtons(inputId = "direction",
                                label = "Predicted direction:",
                                choiceNames = c("None",
                                                "Positive effect",
                                                "Negative effect"),
                                choiceValues = c("none", "positive", "negative"),
                                selected = "none"
                   )
                 }
                 else {
                   updateRadioButtons(inputId = "design",
                                      label = "Design:",
                                      choiceNames = c("Between subjects",
                                                      "Repeated measures"),
                                      choiceValues = c("between",
                                                       "repeated"),
                                      selected = "between"
                                      )
                   updateRadioButtons(inputId = "direction",
                                label = "Predicted direction:",
                                choiceNames = c("None"),
                                choiceValues = c("none"),
                                selected = "none"
                   )
                 }
               }
  )
          
                 
  output$statistic = renderUI(
    if (input$test == "ttest"){
      numericInput(inputId = "t",
                   label = "t-statistic:",
                   value = 0,
                   min = -1e5,
                   max = 1e5,
                   step=0.1
      )
    }
    else {
      numericInput(inputId = "F",
                   label = "F-statistic:",
                   value = 1,
                   min = 0,
                   max = 1e5,
                   step=0.1
      )
    }
  )
  
  output$sample1 = renderUI(
    {
      if (input$design == "single"){
        numericInput(inputId = "n",
                     label = "Sample size:",
                     value = 10,
                     min = 2,
                     max = 1e5
        )
      }
      else if (input$design == "independent")
      {
        numericInput(inputId = "n1",
                     label = "Sample size for Group 1:",
                     value = 10,
                     min = 2,
                     max = 1e5
        )
      }
      else {
        numericInput(inputId = "df1",
                     label = "Numerator df:",
                     value = 1,
                     min = 1,
                     max = 10
                     )
      }
    })
  
  output$sample2 = renderUI(
    {
      if (input$design == "independent"){
        numericInput(inputId = "n2",
                     label = "Sample size for Group 2:",
                     value = 10,
                     min = 2,
                     max = 1e5
        )
      }
      else if (input$design == "between" | input$design == "repeated"){
        numericInput(inputId = "df2",
                     label = "Denominator df:",
                     value = 1,
                     min = 1,
                     max = 100
        )
      }
    })

  
  # get input and calculate Bayes factor
  
  values = reactiveValues()
  
  observe({
    
    if (input$direction == "none"){
      A <- -Inf
      B <- Inf
    }
    else if (input$direction == "positive"){
      A <- 0
      B <- Inf
    }
    else if (input$direction == "negative"){
      A <- -Inf
      B <- 0
    }
    
    if (input$design == "single"){
      if (is.null(input$n)){
        shiny:::flushReact()
        return()
      }
      req(input$n, input$t, input$pH0)
      n1 = max(input$n, 2)
      result = ttest.tstat(t = input$t, n1 = n1, nullInterval = c(A,B))
      values$BF10 <- exp(result[['bf']])
      values$BF01 <- 1/exp(result[['bf']])
      values$pOdds = input$pH0/(1-input$pH0)
    }
    else if (input$design == "independent"){
      if (is.null(input$n1) | is.null(input$n2)){
        shiny:::flushReact()
        return()
      }
      req(input$t, input$pH0, input$n1, input$n2)
      n1 = max(input$n1, 2)
      n2 = max(input$n2, 2)
      result = ttest.tstat(t = input$t, n1 = n1, n2 = n2, nullInterval = c(A,B))
      values$BF10 <- exp(result[['bf']])
      values$BF01 <- 1/exp(result[['bf']])
      values$pOdds = input$pH0/(1-input$pH0)
    }
    
    else if (input$design == "between"){
      if (is.null(input$df1) | is.null(input$df2)){
        shiny:::flushReact()
        return()
      }
      values$BF01 <- bf_bic(F=input$F, df1=input$df1, df2=input$df2, repeated=FALSE, report.as="BF01")
      values$BF10 <- bf_bic(F=input$F, df1=input$df1, df2=input$df2, repeated=FALSE, report.as="BF10")
      values$pOdds = input$pH0/(1-input$pH0)
    }
    
    else if (input$design == "repeated"){
      if (is.null(input$df1) | is.null(input$df2)){
        shiny:::flushReact()
        return()
      }
      values$BF01 <- bf_bic(F=input$F, df1=input$df1, df2=input$df2, repeated=TRUE, report.as="BF01")
      values$BF10 <- bf_bic(F=input$F, df1=input$df1, df2=input$df2, repeated=TRUE, report.as="BF10")
      values$pOdds = input$pH0/(1-input$pH0)
    }
    
  })
  
  
  
  
  # render outputs (plot and text)
  
  # Output 1 - bayes factor "pizza" plot
  output$pizza <- renderPlot({
    
    op <- par(mar = c(0, 0, 0, 0))
    
    xPos = 0.2
    yPos = 0.7
    radius = 0.2
    A = pi*radius^2
    alpha = 2/(1/values$BF01+1)*A/radius^2
    startpos = pi/2 - alpha/2
    plotrix::floating.pie(xPos,yPos,c(values$BF01,1),radius=radius,col=c("#4F2D7F","white"),lwd=2,startpos=startpos)
    text(xPos, yPos+1.2*radius, "data|H0", cex=1.5, font=2)
    text(xPos, yPos-1.22*radius, "data|H1", cex=1.5, font=2)
  },
  width=600,
  height=600
  )
  
  # Output 2 - text describing Bayes factor
  output$bfText <- renderUI({
    if (is.null(values$BF10)){
      shiny:::flushReact()
      return()
    }
    
    if (values$BF01 > 1){
      p(withMathJax(sprintf("The Bayes factor for the null is \\(\\text{BF}_{01}\\) = %.2f", values$BF01)))
    }
    
    else if (values$BF10 > 1){
      p(withMathJax(sprintf("The Bayes factor for the alternative is \\(\\text{BF}_{10}\\) = %.2f", values$BF10)))
    }
  })
  
  output$description <- renderUI({
    if (is.null(values$BF10)){
      shiny:::flushReact()
      return()
    }
    
    if(values$BF10 > 1){
      p(withMathJax(sprintf("This means that the observed data are approximately %.2f times more likely under \\(\\mathcal{H}_1\\) than under \\(\\mathcal{H}_0\\)", values$BF10)))
    }
    else{
      p(withMathJax(sprintf("This means that the observed data are approximately %.2f times more likely under \\(\\mathcal{H}_0\\) than under \\(\\mathcal{H}_1\\)", values$BF01)))
      
    }
  })
  
  # Output 3 - text describing posterior probabilities
  output$postNull <- renderUI({
    p(withMathJax(sprintf("The posterior probability for \\(\\mathcal{H}_0\\) is %.4f", (values$BF01*values$pOdds)/(1+(values$BF01*values$pOdds)))))
  })
  
  output$postAlt <- renderUI({
    p(withMathJax(sprintf("The posterior probability for \\(\\mathcal{H}_1\\) is %.4f", 1-(values$BF01*values$pOdds)/(1+(values$BF01*values$pOdds)))))
  })
  
  
  # Model definition outputs (at top of page)
  
  output$nullText <- renderUI({
    
    if (input$test == "ttest"){
      p(withMathJax(sprintf('\\(\\mathcal{H}_0\\): effect size is equal to 0')))
    }
    else if (input$test == "anova"){
      p(withMathJax(sprintf('\\(\\mathcal{H}_0\\): all condition means are equal')))
    }
    
  })
  
  output$altText <- renderUI({
    
    if (input$test == "ttest" & input$direction == "positive"){
      p(withMathJax(sprintf('\\(\\mathcal{H}_1\\): effect size is greater than 0')))
    }
    else if (input$test == "ttest" & input$direction == "negative"){
      p(withMathJax(sprintf('\\(\\mathcal{H}_1\\): effect size is less than 0')))
    }
    else if (input$test == "ttest" & input$direction == "none"){
      p(withMathJax(sprintf('\\(\\mathcal{H}_1\\): effect size is not equal to 0')))
    }
    else if (input$test == "anova"){
      p(withMathJax(sprintf('\\(\\mathcal{H}_1\\): not all condition means are equal')))
    }
    
  })

# functions for probability calculator
  
  output$tail = renderUI(
    {
      if (input$dist == "rf"){
        selectInput(inputId = "tail",
                    label = "Find Area:",
                    choices = c("Upper Tail"="upper"),
                    selected = "upper")
      }
      else
      {
        selectInput(inputId = "tail",
                    label = "Find Area:",
                    choices = c("Lower Tail"="lower", 
                                "Upper Tail"="upper", 
                                "Both Tails"="both",
                                "Middle"="middle"),
                    selected = "upper")
      }
    })
  
  get_region_text = reactive(
    {
      if (is.null(input$tail)){
        shiny:::flushReact()
        return()
      }
      
      low_less = "<"
      low_greater = ">"
      
      up_less = "<"
      up_greater = ">"
      
      text = ""
      if (length(input$tail) != 0)
      {
        if (input$tail == "lower")
        {
          # P(X < a)
          text = paste0("P(X ", low_less, " a)")
        }
        else if (input$tail == "upper")
        {
          # P(X > a)
          text = paste0("P(X ", low_greater, " a)")
        }
        else if (input$tail == "middle")
        {
          # P(a < X < b)
          text = paste0("P(a ", low_less, " X ", up_less, " b)")
        }
        else if (input$tail == "both")
        {
          # P(X < a or X > b)
          text = paste0("P(X ", low_less, " a or X ", up_greater, " b)")
        }
        else if (input$tail == "equal")
        {
          # P(X = a)
          text = paste0("P(X = a)")
        }
      }
      
      return(text)
    })
  
  output$region = renderText(
    {
      get_region_text()
    })
  
  
  #############################
  # Normal distribution stuff #
  ############################
  
  output$mean = renderUI(
    {
      if (input$dist == "rnorm")
      {
        numericInput("mu",
                     "Mean",
                     value = 0)
      }
    })
  
  output$sd = renderUI(
    {
      #print("sd")
      if (input$dist == "rnorm")
      {
        numericInput("sd",
                     "Standard deviation",
                     value = 1)
      }
    })
  
  output$df_1 = renderUI(
    {
      if (input$dist == "rt")
      {
        numericInput("df",
                     "Degrees of freedom",
                     value = 10,
                     step=1,
                     min=1)
      }
      else if (input$dist == "rf")
      {
        numericInput("df_1",
                     "Numerator degrees of freedom",
                     value = 2,
                     step=1,
                     min=1)
      }
    })
  
  output$df_2 = renderUI(
    {
      if (input$dist == "rf")
      {
        numericInput("df_2",
                     "Denominator degrees of freedom",
                     value = 10,
                     step=1,
                     min=1)
      }
    })
  
  output$a = renderUI(
    {
      value = 1
      min = 0
      max = 1
      step = 1
      
      if (input$dist == "rnorm")
      {
        
        if (is.null(input$mu) | is.null(input$sd)){
          shiny:::flushReact()
          return()
        }
        
        mu = input$mu
        sd = input$sd
        if (is.null(mu)) mu = 0
        if (is.null(sd)) sd = 1
        
        value = mu - 1.96 * sd
        min   = mu - 4 * sd
        max   = mu + 4 * sd
        step  = 0.01
        if (mu == 0 & sd == 1) {step = .01}
      }
      else if (input$dist == "rt")
      {
        if (is.null(input$df)){
          shiny:::flushReact()
          return()
        }
        value = round(qt(0.025, df=as.numeric(input$df)), digits=2) 
        min   = -4
        max   = 4
        step  = 0.01
      }
      else if (input$dist == "rf")
      {
        if (is.null(input$df_1) | is.null(input$df_2)){
          shiny:::flushReact()
          return()
        }
        value = round(qf(.95,as.numeric(input$df_1),as.numeric(input$df_2)),digits=2)
        min   = 0
        max   = round(qf(.995,as.numeric(input$df_1),as.numeric(input$df_2))*1.05,digits=2)
        step  = 0.01
      }
      
      numericInput("a", 
                   label = "a",
                   value = value,
                   step=step,
                   min=min,
                   max=max)
      
    })
  
  output$b = renderUI(
    {
      if (is.null(input$tail))
      {
        shiny:::flushReact()
        return()
      }
      
      if (input$tail %in% c("middle","both"))
      {
        value = 1
        min = 0
        max = 1
        step = 1
        
        if (input$dist == "rnorm")
        {
          if (is.null(input$mu) | is.null(input$sd)){
            shiny:::flushReact()
            return()
          }
          
          mu = input$mu
          sd = input$sd
          if (is.null(mu)) mu = 0
          if (is.null(sd)) sd = 1
          
          value = mu + 1.96 * sd
          min   = mu - 4 * sd
          max   = mu + 4 * sd
          step  = 0.01
        }
        else if (input$dist == "rt")
        {
          value = round(qt(0.975, df=as.numeric(input$df)), digits=2)
          min   = -4
          max   = 4
          step  = 0.01
        }
        
        numericInput("b",
                     label = "b",
                     value = value,
                     step=step,
                     min=min,
                     max=max)
      }
    })  
  
  ############
  # Plotting #
  ############
  
  output$plot = renderPlot(
    { 
      if (is.null(input$tail) | is.null(input$a))
      {
        shiny:::flushReact()
        return()
      }
      
      L = NULL
      U = NULL
      
      error = FALSE
      
      if (input$tail == "lower" | input$tail == "equal")
      {
        L = input$a 
      }
      else if (input$tail == "upper")
      {
        U = input$a 
      }
      else if (input$tail %in% c("both","middle"))
      {
        if (is.null(input$b)){
          shiny:::flushReact()
          return()
        }
        
        L = input$a
        U = input$b
        
        if (L > U)
          error = TRUE
      }
      
      if (error)
      {
        plot(0,0,type='n',axes=FALSE,xlab="",ylab="",mar=c(1,1,1,1))
        text(0,0,"Error: Lower bound greater than upper bound.",col="red",cex=2)
      }
      else
      {
        if (input$dist == "rnorm" | input$dist == "rt") 
        {
          M = NULL
          if (input$tail == "middle")
          {
            M = c(L,U)
            L = NULL
            U = NULL
          }
          
          if(input$dist == "rnorm")
          {
            if(is.null(input$mu) | is.null(input$sd))
            {
              shiny:::flushReact()
              return()
            }
            
            normTail(m=input$mu, s=input$sd, L=L, U=U, M=M, axes=3, cex.axis=1.5)
            title(main="Normal Distribution")
          }
          else if (input$dist == "rt")
          {
            if(is.null(input$df))
            {
              shiny:::flushReact()
              return()
            }
            
            normTail(m=0, s=1, df=input$df, L=L, U=U, M=M, axes=3, cex.axis=1.5)
            title(main="t Distribution")
          }
        }
        else if (input$dist == "rf")
        {        
          if(is.null(input$df_1) | is.null(input$df_2))
          {
            shiny:::flushReact()
            return()
          }
          
    
          FTail(U=U,df_n=input$df_1, df_d=input$df_2)
          title(main="F Distribution")
        }
        
      }
    })
  
  ################
  # Calculations #
  ################
  
  output$area = renderText(
    {
      if (is.null(input$tail) | is.null(input$a))
      {
        shiny:::flushReact()
        return()
      }
      
      L = input$a
      U = NULL
      
      if (input$tail %in% c("both","middle")) 
      {
        if (is.null(input$b))
        {
          shiny:::flushReact()
          return()
        }
        
        U = input$b
        
        error = FALSE
        if (L>U) error = TRUE
        if (error){
          return()
        }
      }
      
      
      
      f = function() NULL
      
      if (input$dist == "rnorm")
      {
        if (is.null(input$mu) | is.null(input$sd))
        {
          shiny:::flushReact()
          return()
        }
        
        f = function(x) pnorm(x,input$mu,input$sd)
      }  
      else if (input$dist == "rt")
      {
        if (is.null(input$df))
        {
          shiny:::flushReact()
          return()
        }
        
        f = function(x) pt(x,input$df)
      }
      else if (input$dist == "rf"){
        if (is.null(input$df_1) | is.null(input$df_2))
        {
          shiny:::flushReact()
          return()
        }
        
        f = function(x) pf(x,input$df_1,input$df_2)
      }    
      
      
      val = NA
      if (input$tail == "lower")
        val = f(L)
      else if (input$tail == "upper")
        val = 1-f(L)
      else if (input$tail == "equal")
        val = f(L)
      else if (input$tail == "both")
        val = f(L) + (1-f(U))
      else if (input$tail == "middle")
        val = f(U) - f(L)
      
      text = paste("p = ",signif(val,3))
      
      
      text = sub("a",input$a,text)
      if (input$tail %in% c("both","middle")) 
        text = sub("b",input$b,text)
      
      text
    })
  } # close server 

)

