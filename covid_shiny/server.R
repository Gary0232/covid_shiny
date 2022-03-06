# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  # Plot of the new cases with selected countries ----
  # This expression that generates a plot is wrapped in a call
  # to renderPlot to indicate that:
  #
  # 1. It is "reactive" and therefore should be automatically
  #    re-executed when inputs (input$bins) change
  # 2. Its output type is a plot
  
  
  output$new_cases_plot <- renderPlot({
    countries <- input$country2
    
    ggplot(covid_data %>% filter(location == countries), 
           aes(x = date, y = new_cases, group = location, color = location)) +
      geom_point(colour = "grey") + 
      geom_line(lwd = 1) + 
      theme_bw() + ylab("Number of new cases")+
      xlab("")+labs(color = "Country/ Region") +
      scale_x_date(date_breaks = "1 month")
    
  })
  
  output$new_cases_plot2 <- renderPlot({
    
    countries <- input$country2
    
    covid_select_countries = covid_data[covid_data$location == toString(countries), ]
    
    sRollmean <- wrapRollmean(covid_select_countries$new_cases, k = 7)
    sLowess <- wrapLowess(covid_select_countries$new_cases, f = 0.1)
    sLoess <- wrapLoess(covid_select_countries$new_cases, x = covid_select_countries$date, span = 0.1)
    sSavitzkyGolay <- wrapSG(covid_select_countries$new_cases, p = 3, n = 61)
    covid_data_smooth <- cbind(aveSmoother = sRollmean, Lowess = sLowess, Loess = sLoess, SavitzkyGolay = sSavitzkyGolay )
    
    plot_data <- melt(data.frame(date = covid_select_countries$date, nc = covid_select_countries$new_cases, covid_data_smooth), id.var="date")
    
    ggplot(plot_data, aes(x = date, y = value)) +
      geom_point(data = subset(plot_data, variable=="nc"), colour = "grey") + 
      geom_line(data = subset(plot_data, variable!="nc"), aes(group=variable, colour=variable)) + 
      theme_bw() + ylab("Number of new cases") +
      xlab("")+labs(color = "Country/ Region") +
      scale_x_date(date_breaks = "1 month")
  })
  
  
  
  output$new_cases_plot3 <- renderPlot({
    
    countries <- input$country2
    temp_df <- gradient(covid_data, countries)
    
    ggplot(data=temp_df, mapping = aes(x = date, y = gradient, color = gradient)) + 
      geom_line(lwd = 1) + theme_bw()+
      ylab("Gradient")+
      xlab("")+labs(color = "Country/ Region") +
      scale_x_date(date_breaks = "1 month")
  })
  
  output$new_cases_plot4 <- renderPlot({
    
    countries <- input$country2
    temp_df <- gradient(covid_data, countries)
    
    max_grad <- max(temp_df$gradient)
    min_grad <- min(temp_df$gradient)
    max_time <- temp_df$date[which.max(temp_df$gradient)]
    min_time <- temp_df$date[which.min(temp_df$gradient)]
    
    ggplot(data=temp_df, mapping = aes(x = date, y = gradient, color = gradient)) + 
      geom_line(lwd = 1) + theme_bw()+
      ylab("Gradient")+
      xlab("")+labs(color = "Country/ Region") +
      scale_x_date(date_breaks = "1 month") + 
      geom_vline(xintercept=max_time, color = "green", size = 1) +
      geom_vline(xintercept=min_time, color = "red", size = 1)
  })
  
  output$new_cases_plot5 <- renderPlot({
    countries <- input$country4
    
    ggplot(covid_data %>% filter(location == countries), 
           aes(x = date, y = new_cases, group = location, color = location)) +
      geom_point(colour = "grey") + 
      geom_line(lwd = 1) + 
      theme_bw() + ylab("Number of new cases")+
      xlab("")+labs(color = "Country/ Region") +
      scale_x_date(date_breaks = "1 month")
    
  })
  
  
  
  output$new_cases_plot6 <- renderPlot({
    
    countries <- input$country4
    
    covid_select_countries = covid_data[covid_data$location == toString(countries), ]
    
    sRollmean <- wrapRollmean(covid_select_countries$new_cases, k = 7)
    sLowess <- wrapLowess(covid_select_countries$new_cases, f = 0.1)
    sLoess <- wrapLoess(covid_select_countries$new_cases, x = covid_select_countries$date, span = 0.1)
    sSavitzkyGolay <- wrapSG(covid_select_countries$new_cases, p = 3, n = 61)
    covid_data_smooth <- cbind(aveSmoother = sRollmean, Lowess = sLowess, Loess = sLoess, SavitzkyGolay = sSavitzkyGolay )
    
    plot_data <- melt(data.frame(date = covid_select_countries$date, nc = covid_select_countries$new_cases, covid_data_smooth), id.var="date")
    
    ggplot(plot_data, aes(x = date, y = value)) +
      geom_point(data = subset(plot_data, variable=="nc"), colour = "grey") + 
      geom_line(data = subset(plot_data, variable!="nc"), aes(group=variable, colour=variable)) + 
      theme_bw() + ylab("Number of new cases") +
      xlab("")+labs(color = "Country/ Region") +
      scale_x_date(date_breaks = "1 month")
  })
  
  
  output$new_cases_plot7 <- renderDataTable({
    
    countries <- input$country2
    covid_select_countries = covid_data[covid_data$location == toString(countries), ]
    temp_df <- gradient(covid_data, countries)
    avg_df <- search(covid_select_countries,4,temp_df)
    l <- findzero(avg_df)
    require(reshape2)
    l_m <- melt(l)
    
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
    l_m <- l_m %>% mutate(date1=as.character(date1, format = "%Y-%m-%d"))
    l_m <- l_m %>% mutate(date2=as.character(date2, format = "%Y-%m-%d"))
    l_m
      
  })
  
  
  output$new_cases_plot8 <- renderPlot({
    
    countries <- input$country2
    winsize <- input$winsize
    covid_select_countries = covid_data[covid_data$location == toString(countries), ]
    temp_df <- gradient(covid_data, countries)
    avg_df <- search(covid_select_countries,winsize,temp_df)
    l <- findzero(avg_df)
    require(reshape2)
    l_m <- melt(l)
    
    ggplot(data=temp_df, mapping = aes(x = date, y = gradient, color = gradient)) + 
      geom_line(lwd = 1) + theme_bw()+
      ylab("Gradient")+
      xlab("")+labs(color = "Country/ Region") +
      scale_x_date(date_breaks = "1 month") + 
      geom_vline(data=l_m, aes(xintercept = as.Date(value)), color = "green", size = 1)
    
  })
  
  output$new_cases_plot12 <- renderPlot({
    
    countries <- input$country2
    winsize <- input$winsize
    covid_select_countries = covid_data[covid_data$location == toString(countries), ]
    temp_df <- gradient(covid_data, countries)
    avg_df <- search(covid_select_countries,winsize,temp_df)
    l <- findzero(avg_df)
    
    require(reshape2)
    l_m <- melt(l)
    df <- plot12(l_m)
    
    ggplot(data=temp_df, mapping = aes(x = date, y = gradient, color = gradient)) + 
      geom_line(lwd = 1) + theme_bw()+
      ylab("Gradient")+
      xlab("")+labs(color = "Country/ Region") +
      scale_x_date(date_breaks = "1 month") + 
      geom_vline(data = df, aes(xintercept = as.Date(new_list)), color = "red", size = 1) 
  })
  
  
  output$new_cases_plot9 <- renderPlot({
    
    countries <- input$country4
    
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
    temp_df$model <- "GAM"
    temp_df2$model <- "SVM"
    temp_df3$model <- "Randomforest"
    df <- rbind(temp_df, temp_df2,temp_df3)
    
    p1 <- ggplot(data=df, mapping = aes(x = date, y = nc, color = model)) + 
      geom_line(lwd = 1) + theme_bw()+
      ylab("Fitted")+
      xlab("")+labs(color = "Supervised models") +
      scale_x_date(date_breaks = "1 month") 
    p1
  })
  
  output$new_cases_plot10 <- renderPlot({
    
    total <- clustering()
    totalscaled = scale(total)
    k3 <- kmeans(totalscaled, 3)
    
    p1 <- fviz_cluster(k3, data = totalscaled)
    d = dist(totalscaled)
    h = hclust(d)
    
    p2 <- ggdendrogram(h)
    
    grid.arrange(p1,p2,nrow = 1)
  })
  
  output$new_cases_plot11 <- renderPlot({
    countries <- input$country4
    
    covid_select_countries = covid_data[covid_data$location == toString(countries), ]
    
    sRollmean <- wrapRollmean(covid_select_countries$new_cases, k = 7)
    sLowess <- wrapLowess(covid_select_countries$new_cases, f = 0.1)
    sLoess <- wrapLoess(covid_select_countries$new_cases, x = covid_select_countries$date, span = 0.1)
    sSavitzkyGolay <- wrapSG(covid_select_countries$new_cases, p = 3, n = 61)
    covid_data_smooth <- cbind(aveSmoother = sRollmean, Lowess = sLowess, Loess = sLoess, SavitzkyGolay = sSavitzkyGolay )
    covid_data_smooth = data.frame(date =    covid_select_countries $date,
                                   covid_data_smooth  )
    dd = na.omit(covid_data_smooth)
    dd = data.frame(dd)
    dd$SavitzkyGolay[dd$SavitzkyGolay < 0] <- 0
    
    tp <- turnpoints(dd$SavitzkyGolay)
    
    # Get counts for all turnpoints
    allcounts <- dd$SavitzkyGolay[pastecs::extract(tp, no.tp = FALSE, peak = TRUE, pit = 
                                                     TRUE)]
    
    # Get dates for all turnpoints
    alldates <- dd$date[pastecs::extract(tp, no.tp = FALSE, peak = TRUE, pit = TRUE)]
    
    # Get dates for "informative" turnpoints
    alldates <- alldates[tp$proba < 0.05]
    # Get dates for peaks only
    peaks <- dd$date[pastecs::extract(tp, no.tp = FALSE, peak = TRUE, pit = FALSE)]
    
    Sys.setlocale("LC_TIME","C")
    ggplot(dd, aes(date,SavitzkyGolay )) + geom_line(color = "darkblue") +
      geom_vline(xintercept = peaks, color = "darkred")
    
    
  })
  
  output$new_cases_plot13 <- renderPlot({
    
    countries <- input$country4
    
    covid_select_countries = covid_data[covid_data$location == toString(countries), ]
    
    sRollmean <- wrapRollmean(covid_select_countries$new_cases, k = 7)
    sLowess <- wrapLowess(covid_select_countries$new_cases, f = 0.1)
    sLoess <- wrapLoess(covid_select_countries$new_cases, x = covid_select_countries$date, span = 0.1)
    sSavitzkyGolay <- wrapSG(covid_select_countries$new_cases, p = 3, n = 61)
    covid_data_smooth <- cbind(aveSmoother = sRollmean, Lowess = sLowess, Loess = sLoess, SavitzkyGolay = sSavitzkyGolay )
    
    dd=  data.frame(date = covid_select_countries$date, nc = covid_select_countries$new_cases, covid_data_smooth)
    
    dd = na.omit(dd)
    
    d <- accuracy_compare(dd)
    
    p1 <- ggplot(data=d, mapping = aes(x = models , y = RMSE, fill = models)) + 
      geom_col() + theme_bw()+
      ylab("Root of mean squared error accuracy measurement")+
      xlab("Supervised models")+labs(fill = "Supervised models")
    p1
  })
  
  output$table <- renderDataTable({
    
    gradient_table <- readxl::read_xlsx("data/Book1.xlsx")
    gradient_table
  })
  
  output$conclusion <- renderDataTable({
    countries <- input$country5
    conclusion_table <- readxl::read_xlsx("data_worldbank/wrangled.xlsx")
    new <- conclusion_table[conclusion_table$`Country Name` == countries, ]
    new
  })
  
  output$plot <- renderPlot({
    
    gradient_table <- readxl::read_xlsx("data/Book1.xlsx")
    world_map <- map_data("world2")
    world_map <- world_map %>%
      mutate(region = replace(region, region == "UK","United Kingdom")) %>% 
      mutate(region = replace(region, region == "USA","United States")) 
    world_map_gradient <- merge(world_map, gradient_table, by="region", all.x = TRUE)
    breaks <- c(0, 50, 150, 250, 350, 500)
    world_map_gradient$Range[is.na(world_map_gradient$Range)] <- 0
    world_map_gradient$Range_category <- 
      cut(as.numeric(world_map_gradient$Range),
          breaks,include.lowest = TRUE, right = FALSE, dig.lab=10)
    
    reds_col <- RColorBrewer::brewer.pal(length(breaks) - 1, "Reds")
    names(reds_col) <- levels(world_map_gradient$Range_category)
    
    ggplot(world_map_gradient,  
           aes(x = long, y = lat, group = group, fill = Range_category)) +
      geom_polygon() +
      scale_fill_manual(values = reds_col) +
      xlab("") + ylab("") + ggtitle("Map of World") +
      theme_void() +
      theme(legend.position = "bottom", aspect.ratio = 0.6) +
      labs(title = paste('COVID19: Date Range of recover to Gradient Zero'),fill = "")
  })
  
}