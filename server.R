library(shiny)
library(dplyr)
# Define server logic required to draw a histogram
shinyServer(function(input, output) {
        
        experiment <- data.frame(Butter = rep(c(40,50,60),4) , Temp = rep(c(180,180,180,200,200,200),2), F3 = rep("",12) , F4 = rep("",12) , Value = c(110,130,90,150,110,170,115,132,80,145,130,170) )
        output$intro <- renderTable({
            
            experiment
            
        })
        
        DF <- data.frame(matrix(nrow = 10000,ncol = 5))
        colnames(DF) <- c(paste("Factor_",c(1,2,3,4),sep=""),"Value")
        DF$Factor_1 <- " "
        DF$Factor_2 <- " "
        DF$Factor_3 <- " "
        DF$Factor_4 <- " "
        DF$Value <- " "
    
        values <- reactiveValues()
    
        observe({
        
            if(!is.null(input$dataEntry)){
                DF <- hot_to_r(input$dataEntry)
            } else {
            
                if(is.null(values[["DF"]])){
                    DF <- DF
                }else {
                    DF <- values[["DF"]] 
                }
            }
            values[["DF"]] <- DF
        })
    
        output$dataEntry <- renderRHandsontable({
        
            DF <- values[["DF"]]
            rhandsontable(data = DF,width = 500,height = 600)
        
        })

    #Save the data in the table
    observeEvent(input$save, {
        finalDF <- isolate(values[["DF"]])
        
        # Data to work with
        mantain <- which(finalDF$Factor_1 != " ")
        workingDF <- finalDF[mantain,]
        workingDF$Value <- as.numeric(workingDF$Value)
        
        #Format the data
        dataAnal <-
            if(input$num_factors==1){
                c(1,5)
            }else if(input$num_factors==2){
                c(1,2,5)
            }else if(input$num_factors==3){
                c(1,2,3,5)
            }else{
                c(1:5)
            }
        
        workingDF <- workingDF[,dataAnal]
        
        for (i in 1:(dim(workingDF)[2]-1)){
            workingDF[,i] <- as.factor(workingDF[,i])
        }
        
        ## Exploratory analysis
        
        F1MeanSD <-{
            
            if(input$num_factors>=1){
                tempMean <- tapply(workingDF$Value, INDEX = workingDF$Factor_1, mean)
                tempSD <- tapply(workingDF$Value, INDEX = workingDF$Factor_1, sd)
                data.frame(Level = names(tempMean),Mean = tempMean, SD = tempSD)
            }else {
                "Please select at least 1 factor"
            }
        }
        
        F2MeanSD <- {
            
            if(input$num_factors>=2){
                tempMean <- tapply(workingDF$Value, INDEX = workingDF$Factor_2, mean)
                tempSD <- tapply(workingDF$Value, INDEX = workingDF$Factor_2, sd)
                data.frame(Level = names(tempMean),Mean = tempMean, SD = tempSD)
            }else {
                "You selected less than 2 factors"
            }
        }
        
        F3MeanSD <-{
            
            if(input$num_factors>=3){
                tempMean <- tapply(workingDF$Value, INDEX = workingDF$Factor_3, mean)
                tempSD <- tapply(workingDF$Value, INDEX = workingDF$Factor_3, sd)
                data.frame(Level = names(tempMean),Mean = tempMean, SD = tempSD)
            }else {
                "You selected less than 3 factors"
            }
        }
        
        F4MeanSD <- {
            
            if(input$num_factors>=4){
                tempMean <- tapply(workingDF$Value, INDEX = workingDF$Factor_4, mean)
                tempSD <- tapply(workingDF$Value, INDEX = workingDF$Factor_4, sd)
                data.frame(Level = names(tempMean),Mean = tempMean, SD = tempSD)
            }else {
                "You selected less than 4 factors"
            }
        }
        
        output$plot1 <- renderPlot({
            
            if(input$num_factors >= 1){
                boxplot(workingDF$Value ~ workingDF$Factor_1, xlab = "Factor 1 levels", ylab="Response value")
                title(main="Boxplot for levels in factor 1")
            }else{
                F1MeanSD
            }
        })
        
        output$plot2 <- renderPlot({
            
            if(input$num_factors >= 2){
                boxplot(workingDF$Value ~ workingDF$Factor_2, xlab = "Factor 2 levels", ylab="Response value")
                title(main="Boxplot for levels in factor 2")
            }else{
                plot(x=0,y=0,main = "Error",xlab="",ylab="")
                text(x = 0, y=0,labels = "There is no second factor",cex = 2)
            }
        })
        
        output$plot3 <- renderPlot({
            
            if(input$num_factors >= 3){
                boxplot(workingDF$Value ~ workingDF$Factor_3, xlab = "Factor 3 levels", ylab="Response value")
                title(main="Boxplot for levels in factor 3")
            }else{
                plot(x=0,y=0,main = "Error",xlab="",ylab="")
                text(x = 0, y=0,labels = "There is no third factor",cex=2)
            }
        })
        
        output$plot4 <- renderPlot({
            
            if(input$num_factors >= 4){
                boxplot(workingDF$Value ~ workingDF$Factor_4, xlab = "Factor 4 levels", ylab="Response value")
                title(main="Boxplot for levels in factor 4")
            }else{
                plot(x=0,y=0,main = "Error",xlab="",ylab="")
                text(x = 0, y=0,labels = "There is no fourth factor",cex=2)
            }
        })
        ## Displaying basic statistics
        
        # Case where there is no info
        output$text1 <- renderText({ifelse(is.character(F1MeanSD)==TRUE,F1MeanSD,"Box plot and exploratory statistics")})
        
        output$text2 <- renderText({ifelse(is.character(F2MeanSD)==TRUE,F2MeanSD,"Box plot and exploratory statistics")})
        
        output$text3 <- renderText({ifelse(is.character(F3MeanSD)==TRUE,F3MeanSD,"Box plot and exploratory statistics")})
        
        output$text4 <- renderText({ifelse(is.character(F4MeanSD)==TRUE,F4MeanSD,"Box plot and exploratory statistics")})   
        
        #Case where the factors are in fact used
        
        output$table1 <- if(is.character(F1MeanSD)==FALSE){
                renderTable(F1MeanSD)
        }
        output$table2 <- if(is.character(F2MeanSD)==FALSE){
            renderTable(F2MeanSD)
        }
        output$table3 <- if(is.character(F3MeanSD)==FALSE){
            renderTable(F3MeanSD)
        }
        output$table4 <- if(is.character(F4MeanSD)==FALSE){
            renderTable(F4MeanSD)
        }
        
        ## linear regression
        linear <- if(input$interaction==FALSE){
            lm(Value ~.,data = workingDF)
        }else{
            if(input$num_factors==1){
                lm(Value ~ Factor_1, data = workingDF)
            }else if(input$num_factors==2){
                lm(Value ~ Factor_1*Factor_2, data = workingDF)
            }else if(input$num_factors==3){
                lm(Value ~ Factor_1*Factor_2*Factor_3, data = workingDF)
            }else{
                lm(Value ~ Factor_1*Factor_2*Factor_3*Factor_4, data = workingDF)
            }
        }
        ## Anova analysis
        model <- anova(linear)
        
        
        output$anovaTable <- renderTable({
            
            model <- data.frame(Factor = row.names(model),model)
            model
            
        })
        
        ## Output the analysis for the ANOVA
        
        output$analysis <- renderText({
            
            paste ("Based on the ANOVA table (p value smaller than significance level), it is concluded that ",
            
                   {
                       sum(model$`Pr(>F)`[-length(model$`Pr(>F)`)]< as.numeric(input$alpha))
                   }
            
                    ," factors are significant. This factors are:")
        })
        
        
        
        
        output$losFactores <- renderTable({
            
            TheFactors <- which(model$`Pr(>F)`[-length(model$`Pr(>F)`)]<as.numeric(input$alpha))
            model <- data.frame(Factor = row.names(model),model)
            model[TheFactors,c(1,6)]
            
        })
        
        ## assumptions graphs
        
        fitted <- predict(object = linear)
        res <- resid(linear)
        
        output$qqplot <- renderPlot({
            
            qqnorm(res,pch=19)
            qqline(res)
        })
        
        output$plotANOVA <- renderPlot({
            
            par(mfrow=c(2,1))
            plot(x = fitted,y=res,ylab="Residuals",xlab="Fitted values",main="Fitted values vs residuals",pch=19)
            abline(h = 0,col="red")
            
            plot(x = c(1:length(res)),y=res,xlab="Observation order",ylab="Residuals",main="Versus Order",type = "l")
            points(x = c(1:length(res)),y=res, pch=19)
            abline(h=0,col="red")
        })
        
        
        
        saveRDS(workingDF, file="finalDF.rds")
    })
    
    

})
