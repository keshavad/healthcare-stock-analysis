library(shiny)
library(shinythemes) #getting the theme for the app

#assigning a theme, Project name, and other aesthetic details
shinyUI(fluidPage(theme = shinytheme("darkly"),
      titlePanel(title = h1("Analysing Healthcare Industry", align="center")),
      navbarPage("Visualization Tabs | ",
      tabPanel("Histogram",
      
      #Making the sidebar layout to select stocks and set the CI         
      sidebarLayout(
        sidebarPanel(
          column(12, align="Center", offset = 0,
          selectInput("select",label = h4("Select Stock"),choices = list("Johnson & Johnson" = 1,"Pfizer" = 2,"Novartis" = 3,"Merck & Co." = 4,"UnitedHealth Group" = 5,"GlaxoSmithKline" = 6,"Sanofi" = 7,"AbbVie" = 8,"Abbott"= 9,"Bristol Myers Squibb" = 10),selected = 1)
          ),
          column(12, align="Center", offset = 0,
          sliderInput("CI","Select a Confidence Level (%)",min = 0,max = 100,value = 95,post = " %",step = 0.1)
          )
        ),
        
        #Histogram of Log Returns - Plotting
        mainPanel(
          plotOutput("distribution"),
          h4(textOutput("CITextM"), style = "font-family: 'times'; font-si16pt"),
          h4(textOutput("CITextS"), style = "font-family: 'times'; font-si16pt")
        )
    )
    ),
    
    #Panel for box plots
    tabPanel("Box Plots",
               mainPanel(
                 column(12,
                        h3("This is a Box Plot for Johnson & Johnson", align="center"), 
                       plotOutput('box1'),
                       h3("This is a Box Plot for Pfizer Inc.", align="center"),
                       plotOutput('box2'),
                       h3("This is a Box Plot for Novartis AG (ADR)", align="center"),
                       plotOutput('box3'),
                       h3("This is a Box Plot for Merck & Co., Inc.", align="center"),
                       plotOutput('box4'),
                       h3("This is a Box Plot for UnitedHealth Group Inc", align="center"),
                       plotOutput('box5'),
                       h3("This is a Box Plot for GlaxoSmithKline plc (ADR)", align="center"),
                       plotOutput('box6'),
                       h3("This is a Box Plot for Sanofi SA (ADR)", align="center"),
                       plotOutput('box7'),
                       h3("This is a Box Plot for AbbVie Common Stock", align="center"),
                       plotOutput('box8'),
                       h3("This is a Box Plot for Abbott Laboratories", align="center"),
                       plotOutput('box9'),
                       h3("This is a Box Plot for Bristol-Myers Squibb Co", align="center"),
                       plotOutput('box10')
                        
                 )
               )
             ), 
    
    #Plotting Density Plots
    tabPanel("Density Plots",
             mainPanel(
               column(12, 
                      h3("This is a Density Plot for Johnson & Johnson", align="center"), 
                      plotOutput('DP1'),
                      h3("This is a Density Plot for Pfizer Inc.", align="center"),
                      plotOutput('DP2'),
                      h3("This is a Density Plot for Novartis AG (ADR)", align="center"),
                      plotOutput('DP3'),
                      h3("This is a Density Plot for Merck & Co., Inc.", align="center"),
                      plotOutput('DP4'),
                      h3("This is a Density Plot for UnitedHealth Group Inc", align="center"),
                      plotOutput('DP5'),
                      h3("This is a Density Plot for GlaxoSmithKline plc (ADR)", align="center"),
                      plotOutput('DP6'),
                      h3("This is a Density Plot for Sanofi SA (ADR)", align="center"),
                      plotOutput('DP7'),
                      h3("This is a Density Plot for AbbVie Common Stock", align="center"),
                      plotOutput('DP8'),
                      h3("This is a Density Plot for Abbott Laboratories", align="center"),
                      plotOutput('DP9'),
                      h3("This is a Density Plot for Bristol-Myers Squibb Co", align="center"),
                      plotOutput('DP10')
                      
               )
             )
    ), 
    
    
    #Panel for QQ plots
    tabPanel("QQ Plots",
             mainPanel(
               column(12,
                      h3("This is a QQ Plot for Johnson & Johnson", align="center"), 
                      plotOutput('qq1'),
                      h3("This is a QQ Plot for Pfizer Inc.", align="center"),
                      plotOutput('qq2'),
                      h3("This is a QQ Plot for Novartis AG (ADR)", align="center"),
                      plotOutput('qq3'),
                      h3("This is a QQ Plot for Merck & Co., Inc.", align="center"),
                      plotOutput('qq4'),
                      h3("This is a QQ Plot for UnitedHealth Group Inc", align="center"),
                      plotOutput('qq5'),
                      h3("This is a QQ Plot for GlaxoSmithKline plc (ADR)", align="center"),
                      plotOutput('qq6'),
                      h3("This is a QQ Plot for Sanofi SA (ADR)", align="center"),
                      plotOutput('qq7'),
                      h3("This is a QQ Plot for AbbVie Common Stock", align="center"),
                      plotOutput('qq8'),
                      h3("This is a QQ Plot for Abbott Laboratories", align="center"),
                      plotOutput('qq9'),
                      h3("This is a QQ Plot for Bristol-Myers Squibb Co", align="center"),
                      plotOutput('qq10')
                      
               )
             )
    ), 
    
    
    
    
    
    # Correlation Matrix
    tabPanel("Correlation Matrix Plot",
                 mainPanel(
                   column(10,
                          plotOutput("correlation")
                   )
                  )
                ),         
          
        #Linear Regression - with a dropdown with titles as mentioned in tabpanel
        navbarMenu("Predictive Modeling",
          tabPanel("Regression Plot for One Stock ",
           sidebarLayout(
            sidebarPanel(
             column(12, align="Center", offset = 0,
             selectInput("select_2",label = h4("Select Stock"),choices = list("Johnson & Johnson" = 1,"Pfizer" = 2,"Novartis" = 3,"Merck & Co." = 4,"UnitedHealth Group" = 5,"GlaxoSmithKline" = 6,"Sanofi" = 7,"AbbVie" = 8,"Abbott"= 9,"Bristol Myers Squibb" = 10),selected = 1),
             hr()
             )
             ),
            mainPanel(
             plotOutput("RegressionvsTime"),
             textOutput("vsTimeResults"),
             plotOutput("ResidualPlot")
            )
          )
         ),
          tabPanel("Paired t-Test",
           sidebarLayout(
            sidebarPanel(
              column(12, align="Center", offset = 0,
              selectInput("select_3",label = h4("Select Stock 1"),choices = list("Johnson & Johnson" = 1,"Pfizer" = 2,"Novartis" = 3,"Merck & Co." = 4,"UnitedHealth Group" = 5,"GlaxoSmithKline" = 6,"Sanofi" = 7,"AbbVie" = 8,"Abbott"= 9,"Bristol Myers Squibb" = 10),selected = 1),
              hr()
              ),
              column(12, align="Center", offset = 0,
              selectInput("select_4",label = h4("Select Select Stock 2"),choices = list("Johnson & Johnson" = 1,"Pfizer" = 2,"Novartis" = 3,"Merck & Co." = 4,"UnitedHealth Group" = 5,"GlaxoSmithKline" = 6,"Sanofi" = 7,"AbbVie" = 8,"Abbott"= 9,"Bristol Myers Squibb" = 10),selected = 2),
              hr()
              )
            ),
            mainPanel(
             plotOutput("TwoStockRegression"),
             h4(textOutput("SvSResult"), style = "font-family: 'times'; font-si16pt"),
             plotOutput("ResidualPlot_2"),
             h4(textOutput("Tnm2"), style = "font-family: 'times'; font-si16pt")
            )
                     
           )
      ),
    
    # Forecasting the Regression results
    tabPanel("Forecasting",
      sidebarLayout(
       sidebarPanel(
         column(12, align="Center", offset = 0,
         selectInput("select_5",label = h4("Select Stock"),choices = list("Johnson & Johnson" = 1,"Pfizer" = 2,"Novartis" = 3,"Merck & Co." = 4,"UnitedHealth Group" = 5,"GlaxoSmithKline" = 6,"Sanofi" = 7,"AbbVie" = 8,"Abbott"= 9,"Bristol Myers Squibb" = 10),selected = 1),
         hr()
         ),
         column(12, align="Center", offset = 0,
                sliderInput("day","Select Length for Sample Data:",min = 10,max = 250,value = 50,post = " Days",step = 1),
         hr()
                ),
         column(12, align="Center", offset = 0,
                sliderInput("PredictProb","Select Confidence Level:",min = 0,max = 99.9,value = 95,post = " %",step = 0.1),
                hr()
         )
      ),
       mainPanel(
          plotOutput("ForecastPlot")
       )   
      )
    )
)

)))
