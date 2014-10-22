library(ggplot2)
shinyServer(
  function(input, output) {
    treeDF <- reactive({tempDF <- data.frame(pos = c(input$pos1, input$pos2, input$pos3, input$pos4,
                                 input$pos5, input$pos6, input$pos7, input$pos8),
                         neg = c(input$neg1, input$neg2, input$neg3, input$neg4,
                                 input$neg5, input$neg6, input$neg7, input$neg8),
                         risk = c(input$risk1, input$risk2, input$risk3, input$risk4,
                                  input$risk5, input$risk6, input$risk7, input$risk8))
                        tempDF[is.na(tempDF)] <- 0
                        tempDF})
    risks <- reactive(unique(treeDF()$risk))
    ROCDF <- reactive({tempDF <- data.frame(pos=sapply(risks(), function(g) sum(ifelse(g==(treeDF()$risk),treeDF()$pos,0))),
                                  neg=sapply(risks(), function(g) sum(ifelse(g==(treeDF()$risk),treeDF()$neg,0))),
                                  risk=risks())
                       tempDF <- tempDF[order(tempDF$risk, decreasing=TRUE),]
                       tempDF <- rbind(data.frame(pos=0,neg=0,risk=NA),tempDF)
                       tempDF$TP <- cumsum(tempDF$pos)/sum(tempDF$pos)
                       tempDF$FP <- cumsum(tempDF$neg)/sum(tempDF$neg)
                       tempDF
                       })

      # for AUC:
      #offset
      #calculate

    output$AUC <- renderText({## x[2:length(x)]-x[1:(length(x)-1)] - refer to prior values
                              trapHeight <- (ROCDF()[2:nrow(ROCDF()),'TP']+ROCDF()[1:(nrow(ROCDF())-1),'TP'])/2
                              trapWidth <- ROCDF()[2:nrow(ROCDF()),'FP']-ROCDF()[1:(nrow(ROCDF())-1),'FP']
                                paste0('AUC: ',round(sum(trapHeight*trapWidth),3))})
    output$ROC <- renderPlot({ggplot(data=ROCDF(), aes(x=FP, y=TP)) + geom_line() + geom_point() + ylab('Sensitivity') + xlab('1 - Specificity') + ggtitle('Receiver Operator Characteristic')})
    #output$myHist <- renderPlot({
    #  hist(galton$child, xlab='child height', col='lightblue',main='Histogram')
    #  mu <- input$mu
    #  lines(c(mu, mu), c(0, 200),col="red",lwd=5)
    #  mse <- mean((galton$child - mu)^2)
    #  text(63, 150, paste("mu = ", mu))
    #  text(63, 140, paste("MSE = ", round(mse, 2)))
    #})
    
  }
)