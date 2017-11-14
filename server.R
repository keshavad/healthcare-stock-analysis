#Loading required libraries
install.packages("rsconnect")
install.packages("shiny")
install.packages("corrplot")
install.packages("e1071")
install.packages("XLConnect")
install.packages("datasets")
install.packages("shinythemes")


library(shiny) #for running shiny
library(corrplot) #for plotting correlation matrix
library(e1071) #for plotting density plot with statistics
library(rsconnect) #for displaying app online

#GetData is a function used to load data from the system into dataframes to analyse further
stocks <- function(GetData) {
  startDate = "12-01-2015" #startdate of data
  #Reading a csv into table, converting into matrix, removing NA values and coverting data into a numeric dataframe
  A <- read.csv("./StockData.csv", header = TRUE, row.names=NULL)
  B <- as.matrix(A)
  C <- B[0:253, 2:11]
  class(C) <- "numeric"
  dataUpload <- data.frame(C)
  
  n = 252
  daily_log_return <- data.frame(matrix("", ncol = 10, nrow = 251)) #Creating an empty dataframe to store log returns
  
  #Running a loop for calculating daily log returns for all tickers - 10 tickers
  for(i in seq(from=1,to=10,by=1)){
    R = dataUpload[,i]
    daily_log_return[,i] = log(R[2:n]) - log(R[1:n-1]) #log(day2) - log(day1)
  }
  #Setting column names for the tickers
  colnames(daily_log_return) <- c("JNJ", "PFE", "NVS", "MRK", "UNH", "GSK", "SNY", "ABBV", "ABT", "BMY") #Giving column names(tickers) to the dataframe
  
  return(daily_log_return) #returns the daily log returns valued dataframe
}

#creating ab array of ticker names - will be useful to call individually
stock_cols = as.character(
  c(
    "JNJ",
    "PFE",
    "NVS",
    "MRK",
    "UNH",
    "GSK",
    "SNY",
    "ABBV",
    "ABT",
    "BMY"
  )
)

#storing returned dataframe from GetData in daily_log_return
daily_log_return <- stocks(GetData)

shinyServer(function(input, output) {
  
  #To find the CI for unknown mean
  output$CITextM <- renderText({
    daily_log_return <- stocks(input$sdate) #giving the input data - here it is fixed to 12-01-2015 since we just have 1 year's data
    u_input <- strtoi(input$select) #converting the user input from string to integer to point to the correct stock
    stock_one <- daily_log_return[ , u_input] #getting the data for the selected stock
    n <- length(stock_one)
    alpha <- input$CI #setting confidence interval
    S <- sqrt(var(stock_one)) #calculating std dev for the sample
    c <- qt(c(.5 + alpha / 200), df = n - 1) #t test
    lside <- round(mean(stock_one) - c * S / sqrt(n) , digits = 5) #calculating lower bound
    rside <- round(mean(stock_one) + c * S / sqrt(n), digits = 5) #calculating upper bound
    paste("Confidence interval of the unknown mean is: [", lside, ",", rside, "]")
    
  })
  
  #To find CI for unknown variance
  output$CITextS <- renderText({
    daily_log_return <- stocks(input$sdate)
    u_input <- strtoi(input$select)
    stock_one <- daily_log_return[, u_input]
    n <- length(stock_one)
    alpha <- input$CI
    S <- sqrt(var(stock_one))
    chilower <- qchisq(c(.5 + alpha / 200), df = n - 1) #chi square for the unknown variance
    chiupper <- qchisq(c(.5 - alpha / 200), df = n - 1)
    lside <- round(((n - 1) * S ^ 2 / chilower), digits = 5) #calculating lower and upper bounds according to the formula
    rside <- round(((n - 1) * S ^ 2 / chiupper), digits = 5)
    paste("Confidence interval of the unknown variance: [",lside,",",rside,"]")
    
  })
  
  #Histogram plotting
  output$distribution <- renderPlot({
    daily_log_return <- stocks(input$sdate)
    u_input <- strtoi(input$select)
    stock_one    <- daily_log_return[, u_input]
    n <- length(stock_one)
    alpha <- input$CI
    S <- sqrt(var(stock_one))
    c <- qt(c(.5 + alpha / 200), df = n - 1) #t test
    lside <- round(mean(stock_one) - c * S / sqrt(n) , digits = 5) #calculating the upper and lower bounds
    rside <- round(mean(stock_one) + c * S / sqrt(n), digits = 5)
    h <- hist(stock_one, density = 20, breaks = 20)
    cond <- ifelse(h$breaks <= rside & h$breaks >= lside, "blue", "red")[-length(h$breaks)]
    hist(stock_one,density = 20, breaks = 20,col = cond,main = stock_cols[u_input] ,xlab = "Daily Log returns") #plotting the histogram
    curve(dnorm(x , mean(stock_one), S), col = 'darkblue', lwd = 2, add = TRUE) #plotting the normal curve
  })
  
  #Plotting the correlation matrix for all stocks against all stocks
  output$correlation <- renderPlot({
    daily_log_return <- stocks(input$sdate)
    colnames(daily_log_return) <- stock_cols[]
    correl <- cor(daily_log_return) 
    corrplot(correl,method="color") #plotting the correlation plot
    paste("Correlation between a pair of stocks")
  })
  
  #Print the Value
  output$value <- renderPrint({
    input$select
  })
  
  #Density Plots
  output$DP1 <- renderPlot({
    
    plot(density(daily_log_return$"JNJ"), main="Density Plot: JNJ", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(daily_log_return$JNJ), 2)))  # density plot for 'UHG
    polygon(density(daily_log_return$"JNJ"), col="red")
  })
  
  # Density
  output$DP2 <- renderPlot({
    
    plot(density(daily_log_return$"PFE"), main="Density Plot: PFE", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(daily_log_return$PFE), 2)))  # density plot for 'UHG
    polygon(density(daily_log_return$"PFE"), col="red")
  })
  
  # Density
  output$DP3 <- renderPlot({
    
    plot(density(daily_log_return$"NVS"), main="Density Plot: NVS", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(daily_log_return$NVS), 2)))  # density plot for 'UHG
    polygon(density(daily_log_return$"NVS"), col="red")
  })
  
  # Density
  output$DP4 <- renderPlot({
    
    plot(density(daily_log_return$"MRK"), main="Density Plot: MRK", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(daily_log_return$MRK), 2)))  # density plot for 'UHG
    polygon(density(daily_log_return$"MRK"), col="red")
  })
  # Density
  output$DP5 <- renderPlot({
    
    plot(density(daily_log_return$"UNH"), main="Density Plot: UNH", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(daily_log_return$UNH), 2)))  # density plot for 'UHG
    polygon(density(daily_log_return$"UNH"), col="red")
  })
  
  # Density
  output$DP6 <- renderPlot({
    
    plot(density(daily_log_return$"GSK"), main="Density Plot: GSK", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(daily_log_return$GSK), 2)))  # density plot for 'UHG
    polygon(density(daily_log_return$"GSK"), col="red")
  })
  
  # Density
  output$DP7 <- renderPlot({
    
    plot(density(daily_log_return$"SNY"), main="Density Plot: SNY", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(daily_log_return$SNY), 2)))  # density plot for 'UHG
    polygon(density(daily_log_return$"SNY"), col="red")
  })
  
  # Density
  output$DP8 <- renderPlot({
    
    plot(density(daily_log_return$"ABBV"), main="Density Plot: ABBV", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(daily_log_return$ABBV), 2)))  # density plot for 'UHG
    polygon(density(daily_log_return$"ABBV"), col="red")
  })
  
  # Density
  output$DP9 <- renderPlot({
    
    plot(density(daily_log_return$"ABT"), main="Density Plot: ABT", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(daily_log_return$ABT), 2)))  # density plot for 'UHG
    polygon(density(daily_log_return$"ABT"), col="red")
  })
  
  
  
  # Density
  output$DP10 <- renderPlot({
    
    plot(density(daily_log_return$"BMY"), main="Density Plot: BMY", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(daily_log_return$BMY), 2)))  # density plot for 'UHG
    polygon(density(daily_log_return$"BMY"), col="red")
  })
  
  #box plots
  output$box1 <- renderPlot({
    boxplot(daily_log_return$"JNJ")
  })
  
  #box plots
  output$box2 <- renderPlot({
    boxplot(daily_log_return$"PFE")
    
  })
  
  #box plots
  output$box3 <- renderPlot({
    
    boxplot(daily_log_return$"NVS")
    
  })
  
  #box plots
  output$box4 <- renderPlot({
    
    boxplot(daily_log_return$"MRK")
    
  })
  
  #box plots
  output$box5 <- renderPlot({
    
    boxplot(daily_log_return$"UNH")
    
  })
  
  #box plots
  output$box6 <- renderPlot({
    boxplot(daily_log_return$"GSK")
  })
  
  
  #box plots
  output$box7 <- renderPlot({
    boxplot(daily_log_return$"SNY")
  })
  
  #box plots
  output$box8 <- renderPlot({
    boxplot(daily_log_return$"ABBV")
  })
  
  #box plots
  output$box9 <- renderPlot({
    boxplot(daily_log_return$"ABT")
  })
  
  #box plots
  output$box10 <- renderPlot({
    boxplot(daily_log_return$"BMY")
  })
  
  #QQ plots
  output$qq1 <- renderPlot({
    qqnorm(daily_log_return$"JNJ")
    qqline(daily_log_return$"JNJ", col = 2)
  }) 
  
  #QQ plots
  output$qq2 <- renderPlot({
    qqnorm(daily_log_return$"PFE")
    qqline(daily_log_return$"PFE", col = 2)
    
  })
  
  #QQ plots
  output$qq3 <- renderPlot({
    
    qqnorm(daily_log_return$"NVS")
    qqline(daily_log_return$"NVS", col = 2)
    
  })
  
  #QQ plots
  output$qq4 <- renderPlot({
    
    qqnorm(daily_log_return$"MRK")
    qqline(daily_log_return$"MRK", col = 2)
    
  })
  
  #QQ plots
  output$qq5 <- renderPlot({
    
    qqnorm(daily_log_return$"UNH")
    qqline(daily_log_return$"UNH", col = 2)
    
  })
  
  #QQ plots
  output$qq6 <- renderPlot({
    qqnorm(daily_log_return$"GSK")
    qqline(daily_log_return$"GSK", col = 2)
  })
  
  
  #QQ plots
  output$qq7 <- renderPlot({
    qqnorm(daily_log_return$"SNY")
    qqline(daily_log_return$"SNY", col = 2)
  })
  
  #QQ plots
  output$qq8 <- renderPlot({
    qqnorm(daily_log_return$"ABBV")
    qqline(daily_log_return$"ABBV", col = 2)
  })
  
  #QQ plots
  output$qq9 <- renderPlot({
    qqnorm(daily_log_return$"ABT")
    qqline(daily_log_return$"ABT", col = 2)
  })
  
  #QQ plots
  output$qq10 <- renderPlot({
    qqnorm(daily_log_return$"BMY")
    qqline(daily_log_return$"BMY", col = 2)
  })
  
  
  
  
  
  
  
  
  # Linear Regression of a stock against time as the independent variable.
  output$RegressionvsTime <- renderPlot({
    daily_log_return <- stocks(input$sdate)
    u_input <- strtoi(input$select_2)
    stock_one <- daily_log_return[, u_input]
    n <- length(stock_one)
    daily <- seq(from = 1, to = n, by = 1) #making a dataframe for the number of days for which data is available
    slr_model <- lm(stock_one ~ daily) #linear regression with response as the log returns and terms as the days
    plot(daily, stock_one, xlab = "Day", ylab = stock_cols[u_input])
    abline(slr_model) #adding thr straightline - trendline
    
    # Residual plot for regression against time
    output$ResidualPlot <- renderPlot({
      plot(resid(slr_model),ylim = c(min(resid(slr_model)), max(resid(slr_model))) ,main = "Graphical depiction of residuals",xlab = "",ylab = "Residual Plot")
    })
    
  })
  
  #Returning the text for the stock vs Time regression results
  output$vsTimeResults <- renderText({
    daily_log_return <- stocks(input$sdate)
    u_input <- strtoi(input$select_2)
    stock_one <- daily_log_return[, u_input]
    n <- length(stock_one)
    daily <- seq(from = 1, to = n, by = 1)
    slr_model <- lm(stock_one ~ daily)
    plot(daily, stock_one, xlab = "Time(over 251 days)", ylab = stock_cols[u_input])
    abline(slr_model)
    paste(
      "Slope of regression line = ",
      round(slr_model$coefficients[2], digits = 5),
      "
      ",
      "Intercept = ",
      round(slr_model$coefficients[1], digits = 5),
      "Coefficient of determination = ",
      round(summary(slr_model)$r.squared, digits = 5)
    )
    
  })
  
  #Linear Regression of 1st stock v/s 2nd stock - both stocks user selected
  output$TwoStockRegression <- renderPlot({
    daily_log_return <- stocks(input$sdate)
    user_var1 <- strtoi(input$select_3)
    user_var2 <- strtoi(input$select_4)
    stock_one <- daily_log_return[, user_var1]
    stock_two <- daily_log_return[, user_var2]
    n <- length(stock_one)
    slr_model <- lm(stock_one ~ stock_two)
    plot(stock_two, stock_one, xlab = stock_cols[user_var1], ylab = stock_cols[user_var2])
    abline(slr_model)
    output$ResidualPlot_2 <- renderPlot({
      plot(
        resid(slr_model),
        ylim = c(min(resid(slr_model)), max(resid(slr_model))) ,
        main = "Graphical depiction of residuals",
        xlab = "",
        ylab = "Residuals"
      )
      
    })
    
  })
  
  #Returning the text for the stock vs stock regression results
  output$SvSResult <- renderText({
    daily_log_return <- stocks(input$sdate)
    user_var1 <- strtoi(input$select_3)
    user_var2 <- strtoi(input$select_4)
    stock_one    <- daily_log_return[, user_var1]
    stock_two <- daily_log_return[, user_var2]
    n <- length(stock_one)
    slr_model <- lm(stock_one ~ stock_two)
    paste(
      "Slope of regression line = ",
      round(slr_model$coefficients[2], digits = 5),
      "
      ",
      "Intercept = ",
      round(slr_model$coefficients[1], digits = 5),
      "Coefficient of determination = ",
      round(summary(slr_model)$r.squared, digits = 5)
    )
    
  })
  
  #Testing equality of two population means - Paired T
  output$Tnm2 <- renderText(
    {
      if (input$select_3 != input$select_4){
        user_var1 <- strtoi(input$select_3)
        user_var2 <- strtoi(input$select_4)
        daily_log_return <- stocks(input$sdate)
        stock_one    <- daily_log_return[,user_var1]  
        stock_two <- daily_log_return[,user_var2]
        test1 <- t.test(stock_one,stock_two,paired=TRUE)
        paste("Paired t-test -> Ho is rejected for P value higher than ",round(test1$p.value *100,digits = 3)," %" )
      }
      else(paste())      
    })
  
  #Predicting for number of input days
  output$ForecastPlot <- renderPlot({
    
    daily_log_return <- stocks(input$sdate)
    u_input <- strtoi(input$select_5)
    stock_one <- daily_log_return[,u_input] 
    n <- length(stock_one)
    alpha <- input$PredictProb
    d <- input$day
    t <- qt(c( .5 +alpha/200 ), df=d-1 )
    Forecast <- matrix(nrow = n-d,ncol = 2)
    for (j in (d+1):n){
      start <- j-d
      end <- j-1
      sm <- mean(stock_one[start:end])
      S <- sqrt(var(stock_one[start:end])) 
      l <-round( sm - (t*S)*(sqrt(1+(1/d))) , digits = 5) #Prediction intervals - lower and upper
      r <- round( sm+ (t*S)*(sqrt(1+(1/d))), digits = 5)
      Forecast[start,1] <- l
      Forecast[start,2] <- r
      
    }
    #Plotting the forecase plot
    plot(Forecast[,2],type = "s",ylim = c(min(Forecast[,1],stock_one[(d+1):n]), max(Forecast[,2],stock_one[(d+1):n])),xlab = "Time (days)",ylab = "Log-returns" )
    points(Forecast[,1],type = "s")
    points(stock_one[(d+1):n],type = "p")
    
  })
  
})
