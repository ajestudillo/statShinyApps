library("shiny")

shinyServer(function(input, output) {

    output$plot <- renderPlot({
      op <- par(cex.main = 1.5, mar = c(5, 6, 4, 5) + 0.1, mgp = c(3.5, 1, 0), cex.lab = 1.5, font.lab = 2, cex.axis = 1.3, bty = "n", las = 1)
      plot(function(x) dnorm(x,mean=0,sd=1),-3.5,3.5,xlim=c(-3.5,3.5),ylim=c(0,0.6),lwd=3,lty=2,xlab="",ylab="",axes=FALSE,main=sprintf("Degrees of freedom = %d",input$df))
      par(new=TRUE)
      plot(function(x) dt(x,df=input$df),-3.5,3.5,xlim=c(-3.5,3.5),ylim=c(0,0.6),lwd=3,lty=1,xlab="",ylab="",axes=FALSE)
      axis(1)
      axis(2)
      par(las=0)
      mtext("t",side=1, line=2.5, cex=1.5)
      mtext("Density",side=2, line=3, cex=1.8)
      
      lines(c(-3.5,-3),c(0.55,0.55),lwd=3,lty=2)
      text(-2.75,0.55, "Normal distribution",font=1,cex=1.4,pos=4)
      lines(c(-3.5,-3),c(0.475,0.475),lwd=3,lty=1)
      text(-2.75,0.475, "t-distribution",font=1,cex=1.4,pos=4)
      
    })
})



  

