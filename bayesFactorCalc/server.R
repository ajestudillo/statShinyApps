library(BayesFactor)

shinyServer(function(input, output) { 
  
  # change input options based on design
  
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
      else
      {
        numericInput(inputId = "n1",
                     label = "Sample size for Group 1:",
                     value = 10,
                     min = 2,
                     max = 1e5
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
      result = ttest.tstat(t = input$t, n1 = input$n, nullInterval = c(A,B))
    }
    else {
      if (is.null(input$n1) | is.null(input$n2)){
        shiny:::flushReact()
        return()
      }
      result = ttest.tstat(t = input$t, n1 = input$n1, n2 = input$n2, nullInterval = c(A,B))
    }
    values$BF10 <- exp(result[['bf']])
    values$BF01 <- 1/exp(result[['bf']])
    values$pOdds = input$pH0/(1-input$pH0)
  })
  
  
  
  
  # render outputs (plot and text)
  
  # Output 1 - bayes factor "pizza" plot
  output$pizza <- renderPlot({
    
    xPos = 0.25
    yPos = 0.75
    radius = 0.12
    A = pi*radius^2
    alpha = 2/(1/values$BF01+1)*A/radius^2
    startpos = pi/2 - alpha/2
    plotrix::floating.pie(xPos,yPos,c(values$BF01,1),radius=radius,col=c("#4F2D7F","white"),lwd=2,startpos=startpos)
    text(xPos, yPos+1.2*radius, "data|H0", cex=1.5, font=2)
    text(xPos, yPos-1.22*radius, "data|H1", cex=1.5, font=2)
  },
  width=800,
  height=800
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
  
  output$altText <- renderUI({
    
    if (input$direction == "none"){
      p(withMathJax(sprintf('\\(\\mathcal{H}_1\\): effect size is not 0')))
    }
    else if (input$direction == "positive"){
      p(withMathJax(sprintf('\\(\\mathcal{H}_1\\): effect size is greater than 0')))
    }
    else if (input$direction == "negative"){
      p(withMathJax(sprintf('\\(\\mathcal{H}_1\\): effect size is less than 0')))
    }
    
  })
}
)