#SHINY SERVER SCRIPT
#Where all the R code is placed 

#Load all needed packages here 
require(ggplot2)
library(tools)

#Load in any datasets here or declare global variables that are used by a few functions

#Lab data for ANOVA examples
Lab1= c(4.9,5.7,5.1,5.3,5.4,5.5)
Lab2= c(5.4,5.5,4.8,4.9,5.2,5.4)
Lab3= c(5.8,6.0,6.0,5.5,5.9,5.8)
Lab4= c(4.5,4.9,4.7,4.7,4.4,4.8)
#Setting up boron data for use with GGPlot
Boron_data <- data.frame(Lab=as.factor(rep( 1:4, each =6 )), Boron=c(Lab1,Lab2,Lab3,Lab4))

#read in data from notes chapter 6 pg 2
Strength = c(3327,3240,3632,3435,4362,4236,4490,5556,5070,5796,6630,6410)
Diameter = c(5.0,5.0,5.5,5.5,5.5,6.0,6.0,6.5,6.5,6.5,7.0,7.0)





shinyServer(function(input, output) {
  
  
##########################  
## cHI SQUARE FUNCTIONS ##
##########################

  #table of gender and admission
  output$data <- renderTable({
    
    apply(UCBAdmissions,c(1,2),sum)
    
  })
  
  #gender and department
  output$data1 <- renderTable({
    apply(UCBAdmissions,c(2,3),sum)
    
  })
  
  output$data2 <- renderTable({
    apply(UCBAdmissions,c(1,3),sum)
    
  })
  
  output$proptable <-renderTable({
    proptable<-prop.table(apply(UCBAdmissions,c(1,2),sum),2)
    round(proptable,2)
    
  })
    
  output$mosaicadmit <- renderPlot ({
    mosaicplot(apply(UCBAdmissions,c(1,2),sum),
               xlab = "Admit", ylab = "Sex",
               main = "Admittance",color=TRUE)
  })
  
  #print all 6 departments
  output$mosaic <- renderPlot({
    opar <- par(mfrow = c(2, 3), oma = c(0, 0, 2, 0))
    for(i in 1:6)
      mosaicplot(UCBAdmissions[,,i],
                 xlab = "Admit", ylab = "Sex",
                 main = paste("Department", LETTERS[i]),col = hcl(c(10, 120)))
    mtext(expression(bold("Student admissions at UC Berkeley Mosaic Plots")),
          outer = TRUE, cex = 1.5)
    par(opar)
    
  })
  
  #switch statement to switch departments plot
  output$mosaicA <- renderPlot({
    if (is.null(input$mo))
      return()
    
    switch(input$mo,
           "A"=mosaicplot(UCBAdmissions[,,1],
                          xlab = "Admit", ylab = "Sex",
                          main = paste("Department", input$mo),col = hcl(c(240,0))),
           "B"=mosaicplot(UCBAdmissions[,,2],
                          xlab = "Admit", ylab = "Sex",
                          main = paste("Department", input$mo),col = hcl(c(240,0))),
           "C"=mosaicplot(UCBAdmissions[,,3],
                          xlab = "Admit", ylab = "Sex",
                          main = paste("Department", input$mo),col = hcl(c(240,0))),
           "D"=mosaicplot(UCBAdmissions[,,4],
                          xlab = "Admit", ylab = "Sex",
                          main = paste("Department", input$mo),col = hcl(c(240,0))),
           "E"=mosaicplot(UCBAdmissions[,,5],
                          xlab = "Admit", ylab = "Sex",
                          main = paste("Department", input$mo),col = hcl(c(240,0))),
           "F"=mosaicplot(UCBAdmissions[,,6],
                          xlab = "Admit", ylab = "Sex",
                          main = paste("Department", input$mo),col = hcl(c(240,0))),
            
           
    )
    
    
  })
  

  #chi square test sex and admit
  output$chi_sa <- renderPrint ({
    chisq.test(apply(UCBAdmissions,c(1,2),sum))
  })
  
  #chi square test admit and department
  output$chi_ad <- renderPrint ({
    chisq.test(apply(UCBAdmissions,c(1,3),sum))
  })
  
  #chi square test sex and department
  output$chi_sd <- renderPrint ({
    chisq.test(apply(UCBAdmissions,c(2,3),sum))
  })
  
  #switch statement to switch Chi square test for each department
  output$chi_dept <- renderPrint ({
    switch(input$pick,
           "A"= chisq.test(UCBAdmissions[,,1]),
           "B"= chisq.test(UCBAdmissions[,,2]),
           "C"= chisq.test(UCBAdmissions[,,3]),
           "D"= chisq.test(UCBAdmissions[,,4]),
           "E"= chisq.test(UCBAdmissions[,,5]),
           "F"= chisq.test(UCBAdmissions[,,6]),   
             )
    
  })
  
####################
##T-test Functions##
####################    
  
#plot of critical value and t statistic 
output$tstat <- renderPlot({
  x1 <- seq(-5, 5, len = 100)
  y1 <- dt(x1, 5)
  
  plot(x1, y1, type="l")
  lines(x1, rep(0, 100))
  
  x2 <- seq(5, 2.26, len = 100)
  y2<- rep(0, 100)
  x3 <- seq(2.26, 5, len = 100)
  y3<- dt(x3, 5)
  
  x4 <- seq(5,4.06,len=100)
  x5<- seq(4.06,5,len=100)
  y4<- dt(x5,5)
  
  polygon( c( x2, x3 ), c( y2, y3 ), density = 20, col = 2 )
  
  polygon( -c( x2, x3 ), c( y2, y3 ), density = 20, col = 2 )
  
  polygon( c( x4, x5 ), c( y2, y4 ), density = 20, col = 3 )
  
  polygon(- c( x4, x5 ), c( y2, y4 ), density = 20, col = 3 )
  
})


#plot of degrees of freedom
output$df <- renderPlot({
  x <- seq(-5, 5, length=100)
  hx <- dnorm(x)
  
  #plot the normal distribution
  plot(x, hx, type="l", lty=1, lwd=2.5, xlab="x value",
       ylab="Density", main="Comparison of t Distributions")
  
  #plot the line on top of it
  lines(x, dt(x,input$dfslide), lwd=1.5, col="red")
  
  
})

#Maths equation
output$math <- renderUI({
  
  withMathJax(helpText('Dynamic output 1:  $$\\alpha^2$$'))
})

#plot of sleep data
output$boxplot <- renderPlot({
  ggplot(sleep,aes(x=group,y=extra,fill=group))+ geom_boxplot()  
})

#boxplots of two groups and difference
output$Tboxplot <- renderPlot({
  
  sleep1 <- (sleep[1:10, 1] - sleep[11:20, 1])
  sleep1 <- data.frame(diff=sleep1)
  
  switch(input$tbox,
         "Boxplots of both groups"=
           ggplot(sleep,aes(x=group,y=extra,fill=group))+ geom_boxplot() ,                   
         "Boxplot of difference of two groups"=      
           ggplot(sleep1, aes(y=diff, x=factor(1)))+ geom_boxplot(fill="lightblue"),
  )
         
  
  
})

#output of paired t-test
output$paired <- renderPrint({
  t.test(extra ~ group, data = sleep, paired=TRUE)
})

#output for calculating critical value
output$crit <- renderPrint({
  qt(0.025, 9, lower.tail= FALSE)
})

#output for calculating P value
output$pvalue <- renderPrint({
  2*pt(4.06, 9, lower.tail= FALSE)
})


#sleep dataset in normal format
 output$sleep <- renderTable({
   sleep
   
 })

#####################
###Anova Functions###
#####################

output$boron <- renderTable({
 
  
  boron_table = data.frame(Lab1,Lab2,Lab3,Lab4)
  
  boron_table
    
})

#Boxplot of Boron data for ANOVA example
output$Anova_box <- renderPlot ({
  ggplot(Boron_data, aes(x=Lab, y=Boron,fill=Lab)) + geom_boxplot()
})
  
#Dotplot of Boron data for Anova example
output$Anova_dot <- renderPlot ({
  ggplot(Boron_data, aes(x=Lab, y=Boron,colour=Lab,size=5)) + geom_point()
})  
  

#Anova Table Function
output$Anova_table <- renderPrint ({
  x <- c(Lab1,Lab2,Lab3,Lab4)
  Lab <- rep( c("Lab1","Lab2","Lab3","Lab4"), each =6 )
  
  anova(lm(x~Lab))
  
})

####################
### Regression #####
####################

output$Regression <- renderPlot ({
  
  #construct dataset
  spot_welds <- data.frame(Strength=c(Strength),Diameter=c(Diameter))
  
  #linear model
  model <-lm(Strength~Diameter)
  #add prediction interval
  predict.int <- predict(model, interval = "prediction")
  #add confidence interval
  conf.int<- predict(model, interval = "confidence")
  
  #plot data
  plot(Strength~Diameter)
  
  
    
    #add regression line
    if (input$checkGroup[]==1){
      abline(model)
    }
    
    #add prediction interval
    if (input$checkGroup==2){
      lines(Diameter, predict.int[, 2], lty = 2, col =2)
      lines(Diameter, predict.int[, 3], lty = 2, col =2)
    }
    
    #add confidence interval
    if (input$checkGroup==3){
      lines(Diameter, conf.int[, 2], lty = 3, col = 3)
      lines(Diameter, conf.int[, 3], lty = 3, col = 3)
    }
    
    
  
  
  
  
  
  
  
  
  
})

output$test <- renderPrint({
  input$checkGroup
  
})

output$welds <- renderTable ({
  
  spot_welds <- data.frame(Strength=c(Strength),Diameter=c(Diameter))
  
  spot_welds
  
})




####File Upload ######


#Checks uploaded file to see if it is csv if not warning message displayed
output$upload <- renderTable({
  
  #assign uploaded file to a variable
  File <- input$file1        
  
  #catches null exception
  if (is.null(File))
    return(NULL)
  
  #validate statement to check file uploaded is format we want
  validate(
    need(file_ext(File$name) %in% c(
      'text/csv',
      'text/comma-separated-values',
      'text/tab-separated-values',
      'text/plain',
      'csv',
      'tsv'
    ), "Wrong File Format please try again!"))
  
  read.csv(File$datapath)
})

  
  
})