library(ggplot2)
library(ggthemes)
library(tidyverse)
library(dplyr)
library(plotly)
library(maps)
library(tidyverse)
library(readxl)
library(tibble)
library(janitor)
library(reshape2)
library(pheatmap)
library(zoo)
library(dendextend)
library(proxy)
library(DT)
library(magrittr)
library(multipanelfigure)
library(gam)
library(e1071)
library(randomForest)
library(cluster)
library(NbClust)
library(factoextra)
library(gridExtra)
library(ggdendro)
library(pastecs)
library(xlsx)

# If you want to update the data, set it as TRUE. 
# Or read csv directly from the link of the data
update_data <- FALSE

if (!dir.exists("data")) {
  dir.create("data")
}

if (update_data | !file.exists("data/owid-covid-data.csv")) {
  download.file("https://covid.ourworldindata.org/data/owid-covid-data.csv",
                destfile = file.path(getwd(), "/data/owid-covid-data.csv"))
  
}



covid_data <- read.csv("https://covid.ourworldindata.org/data/owid-covid-data.csv",
                       stringsAsFactors = FALSE,
                       check.names =  FALSE)

covid_data$date <- as.Date(covid_data$date)
covid_data$new_cases[covid_data$new_cases < 0] <- 0
covid_data$total_cases_per_million[covid_data$total_cases_per_million < 0] <- 0
covid_data$total_deaths_per_million[covid_data$total_deaths_per_million < 0] <- 0
covid_data$new_cases_smoothed[covid_data$new_cases_smoothed < 0] <- 0

## moving average
wrapRollmean <- function(data, k = 10){
  zoo::rollmean(data, k = k, fill = NA)
}

## loess
wrapLoess <- function(data, x=1:length(data), span=0.1) {
  ind <- !is.na(data); loess_fit <- rep(NA, length(data))
  loess_fit[ind] <- loess(data ~ as.numeric(x), span = span)$fitted
  loess_fit 
}

## Savitzky-Golay smoothing filter. 
wrapSG <- function(data, p=3, n=60){
  ind <- !is.na(data); fit <- rep(NA, length(data))
  fit[ind] <- signal::sgolayfilt(data[ind], p = p, n = n)
  fit
}

wrapLowess <- function(data, f) {
  lowess_fit <- lowess(data, f = f)
  lowess_fit$y
}

gradient <- function(covid_data, countries){
  covid_select_countries = covid_data[covid_data$location == toString(countries), ]
  data = covid_select_countries
  data$date <- as.Date(data$date)
  new_df = data.frame(covid_select_countries$date, covid_select_countries$new_cases_smoothed)
  
  i = 0
  temp_df <- data.frame(nrow = length(data$date), ncol = 2)
  colnames(temp_df) <- c("date", "gradient")
  temp_df$date <- as.Date(temp_df$date)
  
  while (i < length(data$date)){
    result = (data$new_cases_smoothed_per_million[i+1] - data$new_cases_smoothed_per_million[i])
    temp_df[i,1] <- data$date[i]
    temp_df[i,2] <- result
    i = i + 1
  }
  
  temp_df[is.na(temp_df)] <- 0
  temp_df
}

search <- function(data, day_dev, temp_df){
  avg_df <- data.frame(date=c(),average=c())
  avg_df$date <- as.Date(avg_df$date)
  i = 0
  day = 0
  avg = 0
  l <- c()
  
  while (i < length(data$date)){
    if (day%%day_dev != day_dev-1){
      result = (data$new_cases_smoothed_per_million[i+1] - data$new_cases_smoothed_per_million[i])
      l = c(l,result)
    }
    else {
      number = 0
      for (element in l){
        number = number + element
      }
      
      number = number/length(l)
      avg_df[i/(day_dev-1),'date'] <- data$date[i]
      avg_df[i/(day_dev-1),'average'] <- number
      day = 0
      l <- c()
      number = 0
    }
    day = day + 1
    i = i + 1
  }
  avg_df
}

findzero <- function(avg_df){
  i = 0 
  while (i < count(avg_df)){
    as.numeric(avg_df[i, 'average'])
    i = i + 1
  }
  
  i = 0
  l=c()
  while (i < count(avg_df)){
    
    if(length(which(!is.na(avg_df[i, 'average']))) == 0){
    }
    else if(avg_df[i, 'average'] == 0){
      
      str_avg = toString(avg_df[i, 'date'])
      l=c(l,str_avg)
    }
    i = i + 1
  }
  l
}

gamfit <- function(covid_data){
  
  new_df = data.frame(covid_data)
  fit <- gam( Lowess   ~ s(date), 
              data =new_df)
  
  new_df[,2] <- fitted.values(fit)
  new_df
}

svmfit <- function(covid_data){
  
  new_df = data.frame(covid_data)
  fit <- svm( Lowess  ~  date, 
              data =new_df)
  
  new_df[,2] <- fitted.values(fit)
  new_df
}


rffit <- function(covid_data){
  
  new_df = data.frame(covid_data)
  fit <-  randomForest( Lowess  ~ date , 
                        data =new_df)
  
  new_df[,2] <- predict(fit)
  new_df
}

clustering <- function() {
  total <- NULL
  
  for(i in unique( c(select_countries,select_countries2)) ) {
    
    countries <- i
    
    covid_select_countries = covid_data[covid_data$location == toString(countries), ]
    
    sRollmean <- wrapRollmean(covid_select_countries$new_cases, k = 7)
    sLowess <- wrapLowess(covid_select_countries$new_cases, f = 0.1)
    sLoess <- wrapLoess(covid_select_countries$new_cases, x = covid_select_countries$date, span = 0.1)
    sSavitzkyGolay <- wrapSG(covid_select_countries$new_cases, p = 3, n = 61)
    covid_data_smooth <- cbind(aveSmoother = sRollmean, Lowess = sLowess, Loess = sLoess, SavitzkyGolay = sSavitzkyGolay )
    
    dd=  data.frame(date = covid_select_countries$date, nc = covid_select_countries$new_cases, covid_data_smooth)
    
    dd = na.omit(dd)
    
    temp_df <- gamfit(dd)
    
    temp_df2 <- svmfit(dd)
    
    temp_df3 <- rffit(dd)
    
    
    acc1 <- mean((temp_df$nc - temp_df$Lowess)^2)
    acc2 <- mean((temp_df2$nc - temp_df$Lowess)^2)
    acc3 <- mean((temp_df3$nc - temp_df$Lowess)^2)
    total <- rbind(total, c(acc1,acc2,acc1))
  }
  total <- data.frame( total)
  colnames(total) <- c("GAM", "SVM","Randomforest")
  row.names(total) <- unique( c(select_countries,select_countries2))
  total
}


plot12 <- function(l_m){
  l_m <- l_m %>% mutate(value=as.Date(value, format = "%Y-%m-%d"))
  l_date2 <- l_m$value[1:length(l_m$value)-1]
  i = 1
  l=c(0)
  while(i < length(l_m$value)){
    first_date = l_m$value[i]
    number = i+1
    second_date = l_m$value[number]
    diff = as.numeric(second_date - first_date)
    l=c(l,diff)
    i = i+1
  }
  outliers = boxplot(l, plot = FALSE)$out
  l_m$range <- l
  l_m <- l_m[-1,]
  l_m$value2 <- l_date2
  l_m <- l_m %>% relocate(value, .after = value2)
  colnames(l_m) <- c("range", "date1","date2")
  l_m <- l_m[order(-l_m$range),]
  df <- outlierfunction(l_m, outliers)
  df
}

accuracy_compare <- function(covid_data){
  
  totaldf <- data.frame(covid_data)
  
  new_df =   totaldf[1:(0.8*nrow(totaldf)),]
  test_df =   totaldf[-c(1:(0.8*nrow(totaldf))),]
  
  fit <- gam( Lowess   ~ s(date), 
              data =new_df)
  
  y1 <- predict(fit, test_df)
  
  mse1 <- sqrt(mean((y1 - test_df$Lowess)^2))
  
  new_df = data.frame(covid_data)
  fit <- svm( Lowess  ~  date, 
              data =new_df)
  
  y1 <- predict(fit, test_df)
  
  mse2 <- sqrt(mean((y1 - test_df$Lowess)^2))
  
  new_df = data.frame(covid_data)
  fit <-  randomForest( Lowess  ~ date , 
                        data =new_df)
  
  y1 <- predict(fit, test_df)
  
  mse3 <- sqrt(mean((y1 - test_df$Lowess)^2))
  
  data.frame(models = c("GAM","SVM","RandomForest"),
             RMSE = c(mse1,mse2, mse3))
  
}


outlierfunction<- function(l_m,outliers){
  new_list = c()
  i = 1
  while (i <length(l_m$range)){
    j = 1
    while (j <= length(outliers)){
      if (l_m$range[i] == outliers[j]){
        new_list = c(new_list, l_m$date1[i])
        new_list = c(new_list, l_m$date2[i])
      }
      j = j+1
    }
    i = i+1
  }
  
  df <- data.frame(new_list)
  df
}


selected <- c("New Zealand","Australia","Singapore",
              "Brazil","United Kingdom","Russia",
              "Spain","United States","Peru","Nigeria",
              "Andorra","Sudan","Thailand")

covid_data_new <- covid_data[covid_data$location %in% selected, ]

select_countries <- c("Brazil", "Germany", "Australia", "United Kingdom", "Italy", "New Zealand",
                      "United States", "Spain", "Singapore", "India", "Japan", "Norway")

select_countries2 <- c("Australia","Singapore","Spain","Norway","United States", 
                       "New Zealand","Kazakhstan", "Nepal", "Pakistan", "Paraguay")

