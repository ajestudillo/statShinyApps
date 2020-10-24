library("shiny")

shinyServer(function(input, output) {

  # plot two population curves
    output$populations <- renderPlot({
      op <- par(cex.main = 1.5, mar = c(5, 6, 4, 5) + 0.1, mgp = c(3.5, 1, 0), cex.lab = 1.5, font.lab = 2, cex.axis = 1.3, bty = "n", las = 1)
      plot(function(x) dnorm(x,mean=0,sd=1), from=-3.5, to=3.5, 
           xlim=c(-3.5,3.5),
           ylim=c(0,0.8),
           lwd=3,
           lty=1,
           xlab="",
           ylab="",
           axes=FALSE,
           main=sprintf("Effect size = %0.1f",input$d)
           )
      
      par(new=TRUE)
      
      plot(function(x) dnorm(x, mean=input$d, sd=as.numeric(input$var2)), from=-3.5, to=3.5,
           xlim=c(-3.5,3.5),
           ylim=c(0,0.8),
           lwd=3,
           lty=1,
           col="blue",
           xlab="",
           ylab="",
           axes=FALSE
      )
      axis(1)
      axis(2)
      par(las=0)
      #mtext("t",side=1, line=2.5, cex=1.5)
      #mtext("Density",side=2, line=3, cex=1.8)
      
      lines(c(-3.5,-3), c(0.55,0.55), lwd=3, lty=1, col="black")
      text(-2.75,0.55, "Population 1", font=1, cex=1.4, pos=4)
      lines(c(-3.5,-3), c(0.475,0.475), lwd=3, lty=1, col="blue")
      text(-2.75,0.475, "Population 2", font=1, cex=1.4, pos=4)
      
    })
    
    
    # run simulation of t-tests from these two populations
    # and plot distribution of p-values
    
      
    output$pValues <- renderPlot({
      #Disable scientific notation (1.05e10)
      options(scipen=999)
      
      #if (input$runsim == 0)
      #  return()  # keep from plotting when app opens?
      
      input$runsim # watch for button press
      
      isolate({  # this keeps the simulation code from running every time an input is changed!
        nSims = input$nSims 
        p = numeric(nSims) #set up empty variable to store all simulated p-values
        bars = 100
        
        #Run simulation
        for(i in 1:nSims){ 
          x = rnorm(n = input$N1, mean = 0, sd = 1)
          y = rnorm(n = input$N2, mean = input$d, sd = as.numeric(input$var2))
          SS1 = sum((x-mean(x))^2)
          SS2 = sum((y-mean(y))^2)
          s = sqrt((SS1+SS2)/(input$N1 + input$N2 - 2))
          t = (mean(x)-mean(y))/(s*sqrt(1/input$N1 + 1/input$N2))
          p[i] = 2*(1-pt(abs(t), df=input$N1 + input$N2 - 2))
        }
      })
      
        #Plot figure
        op <- par(cex.main = 1.5, mar = c(5, 6, 4, 5) + 0.1, mgp = c(3.5, 1, 0), cex.lab = 1.5, font.lab = 2, cex.axis = 1.3, bty = "n", las = 1)
        hist(p, breaks=bars, xlab="p-values", ylab="Number of experiments\n", axes=FALSE,
             main=paste("Distribution of p-values"),
             col="grey", xlim=c(0,1),  ylim=c(0, nSims/10))
        axis(side=1, at=seq(0,1, 0.1), labels=seq(0,1,0.1))
        axis(side=2, at=seq(0, nSims/10, nSims/100), labels=seq(0, nSims/10, nSims/100), las=2)
        abline(h=nSims/bars, col="red", lty=2)
        abline(v=0.05,lty=2,col="red")
        
        
        numSig = length(p[p<0.05])
        numNonsig = length(p[p>0.05])
        #text(0.2, 100, paste("Number of sig results = ", numSig), pos=4)
        propSig = 100*numSig/nSims
        propNonsig = 100*numNonsig/nSims
        
        if (input$d == 0){
          text(0.2, nSims/20, paste("Type I error rate = ", propSig, "%"), pos=4, cex=1.8)
        }
        else {
          text(0.2, nSims/20, paste("Type II error rate = ", propNonsig, "%"), pos=4, cex=1.8)
        }
       
    })
})




  

