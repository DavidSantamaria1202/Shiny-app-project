library(shiny)
library(rhandsontable)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("ANOVA parameter selection"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            
            width=5,
            titlePanel("Options"),
            
            wellPanel(
                
                h4("Savig entered data & recalculate"),
                h6("Click the button to save the changes in the table and perform the analysis."),
                actionButton("save", "Save table & run")
            ),
            
            wellPanel(
                h4("Please fill the information required for the analysis of your data."),
                numericInput("num_factors","Select the number of factors you will analyze:",value=2,min=1,max=4,step=1),
                h5(" "),
                textInput("alpha","please write down the desired significance level (0.05 recommended)",value = "0.05"),
                h6("Analyze interaction between factors?"),
                checkboxInput("interaction","",value=FALSE),
            )
        ),

        # Show a plot of the generated distribution
        mainPanel(
            
            width=7,
            
            tabsetPanel(
                
                type="tabs",
                tabPanel("Instructions",
                         h5(""),
                         h5("Instructions for using the app."),
                         h5(""),
                         h6("This app is divided in two panels, at the left side we find the options panel, here you will be able to select the number of factor to analyze (between 1 and 4),  select the significance level for the analysis of variance (predetermined at 0.05 for a 95% confidence) and finally a check box to indicate if you want to analyze the interaction between the factor or not. It is important to mention that interaction between factors will only be analyzed if replicas of the experiments are registered and more than one factor is being studied."),
                         h5(""),
                         h6("The second part of the app is the main panel at the right side, here you will find different tabs which will display information about your experiment. The tabs are the following:"),
                         h5(""),
                         tags$div(
                             tags$ul(
                                 tags$li("Instructions"),
                                 tags$li("Data Input"),
                                 tags$li("Exploratory Analysis"),
                                 tags$li("ANOVA")
                             )
                         ),
                         h5(""),
                         h6("In the data input tab you will find a spreadsheet where each column is a factor and the last column is the response value (the variable of interest). For example, if you selected two factors in the left panel, only the first two columns might contain information with the levels of the factors you are evaluating, also the column value should be completed with the response values corresponding to each experiment."),
                         h5(""),
                         h6("When the data is complete press the save and run button in the left panel in order to save the data and process the information you have filled. When running the app it will now display the analysis on the remainder tabs."),
                         h5(""),
                         h6("For testing the app I've design a table that you could copy and paste in the spreadsheet, so you can see an example of how data should be registered. Suppose factor 1 is grams of butter for preparing 200g of cupcake mix and factor 2 is the baking temperature, finally the response value is the fluffiness of the cupcake measured in Pa."),
                         h5(""),
                         h4("Don't copy the headers of the table"),
                         tableOutput("intro"),
                         h5(""),
                         h6("The exploratory analysis tab will have itself 4 different tabs, one for each factor. Here a boxplot will be displayed for each level of the factor, also a summary table with the mean and standard deviation of the levels are displayed. "),
                         h5(""),
                         h6("The ANOVA tab will contain two different tabs: Analysis and Assumption graphs. The Analysis tab will contain the ANOVA table indicating the degrees of freedom, sum of squares for each factor, F statistic and p value for the factor. Then a brief analysis is displayed indicating how many and which factors are significant under the null hypothesis that the factor does not have a significant effect over the response variable with a confidence of 1 - alpha (the parameter specified in the left panel). The last panel of the ANOVA tab is the Assumptions graph where 3 different plots are displayed: a normal probability plot, a residual vs fitted values and a plot for residuals vs order of observation. These graphs will allow you to have a first glance over the 3 assumptions for a correct ANOVA analysis: Normal distribution of the residuals (they should follow the diagonal if the distribution is normal), homoscedasticity (equal variance for the residuals) and random error (meaning that there is no pattern when observing the residuals vs order of observation)."),
                         h5(""),
                         h6("Remember to always click the save and run button in order to see the changes in the analysis."),
                         h5(""),
                         h6("This app will allow you to run the analysis of 4 factors with as much levels as you desire, however the experiment design should be balanced, meaning that EVERY combination of levels should have a response value.")
                        ),
                tabPanel("Data input", 
                         h5("Please enter the data in a tidy format"),
                         rHandsontableOutput("dataEntry")),
                tabPanel("Exploratory analysis",
                         tabsetPanel(
                             type="tabs",
                             tabPanel("Factor 1",textOutput("text1"),h5(""),plotOutput("plot1"),tableOutput("table1")),
                             tabPanel("Factor 2",textOutput("text2"),h5(""),plotOutput("plot2"),tableOutput("table2")),
                             tabPanel("Factor 3",textOutput("text3"),h5(""),plotOutput("plot3"),tableOutput("table3")),
                             tabPanel("Factor 4",textOutput("text4"),h5(""),plotOutput("plot4"),tableOutput("table4"))
                            )
                         ),
                tabPanel("ANOVA",
                         tabsetPanel(
                             type="tabs",
                             tabPanel("Analysis",
                                      h4("Analysis of variance:"),
                                      h5(""),
                                      tableOutput("anovaTable"),
                                      h5(""),
                                      textOutput("analysis"),
                                      h5(""),
                                      tableOutput("losFactores")
                                      ),
                             tabPanel("Assumption graphs",
                                      h5("Normal probability plot"),
                                      h5(""),
                                      plotOutput("qqplot"),
                                      h5("Residual analysis"),
                                      h5(""),
                                      plotOutput("plotANOVA"))
                            )
                         )
                
            )
            
        )
    )
))
