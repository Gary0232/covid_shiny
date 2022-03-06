library(shiny)
library(bslib)
library(shinydashboard)

# Define UI for app that draws a histogram ----
ui <- dashboardPage(
  
  skin = "green",
  #theme = bs_theme(version = 4, bootswatch = "minty"),
  
  #titlePanel("COVID-19 visualisation"),
  #navbarPage(title = "DATA3888 HealthR_7",
  dashboardHeader(title = "DATA3888 HealthR_7"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Introduction", tabName = "introduction", icon = icon("sitemap")),
      menuItem("Profile", tabName = "profile", icon = icon("users")),
      menuItem("Result", tabName = "widgets", icon = icon("align-justify"),
               menuSubItem("Machine Learning", tabName = "sub2", icon = icon("award")),
               menuSubItem("Time-varying", tabName = "sub1", icon = icon("book")),
               menuSubItem("World Map", tabName = "sub3", icon = icon("globe-europe")),
               menuSubItem("Conclusion", tabName = "sub4", icon = icon("check-circle")))
      
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "introduction",
              fluidRow(
                box(width = 3
                ),
                box(
                  h1("HealthR_7"),
                  h4("Project team members: Jeremy Wong, Kun Ho Lim, Robert Yang, Nikki Zhang, Hanzhi Guo, Hongxing Wan"),
                  
                  h5(strong("OUR MOTIVATION")),
                  h5("Countries with similar phenoms will have similar speed of easing the new cases since easing new cases is based on the social level of countries. "),
                  h5("It is very important to predict covid-19 new cases for all of the countries over the world."),
                  
                  h5(strong("Hypothesis of our project:")),
                  h5("There are some similar phenomena countries in the COVID-19 depend on the trajectories of new cases and supervised learning of new cases."),
                  h5("Therefore we have two ways to find similar phenomena countries"),
                  h5("Similarity in trajectories of new cases (Time-varying)"),
                  h5("Similarity in smoothed new cases (Machine learning)"),
                  
                  h5(strong("Our target audience:")),
                  h5("Governments, social scientist, and People around the world who care about the COVID-19"),
                  
                  h5(strong("Data collection:")),
                  h5("The source of the data used in the project comes from the daily covid-19 records for all of the countries over the world,  GitHub repository https://github.com/owid/covid-19-data/tree/master/public/data/."),
                  
                  h5(strong("Shiny App overview:")),
                  
                  h5("From this shiny app, the users can check how we have chosed specific countries to investigate by clicking the machine learning part"),
                  h5("As a byproduct of this investigation, we created a algorithm that users can investigate on the maximum range of date that each country require to recover from the new case based on the user defined window size"),
                  h5("Do do this, users can lookup with drop down list and slider to check the range of recovery dates by countries with interacting with the sub section 'Time-varying' "),
                  h5("Moreover, in the world map section, we can see the summary of range and world map representation of it."),
                  
                  

                  width = 9
                  )
              )
      ),
      tabItem(tabName = "profile",
              fluidRow(
                box(width = 3
                ),
                box(
                h3("We are HealthR_7"),
                h5("Project team members: Jeremy Wong, Robert Yang, Nikki Zhang, Hanzhi Guo, Hongxing Wan, Kun Ho Lim"),
                
                h5(strong("KNOWLEDGE")),
                h5("We use machine learning, math, and data science knowledge to create Shiny app with R"),
                h5("This project combines ideas and contexts from different group members' backgrounds and skills."),
                
                h5(strong("Model Development")),
                h5("Machine learning:"),
                h5("3 machine models used in this study: SVM, GAM and Random forest. 
Machine models are built on the new cases of covid-19 data with response as the smoothed new cases in the countries by loess and predictor is the date time along the period."),

                h5("Time-varying:"),
                h5("gradient of the new cases (y=f'(x)) enabled us to visualise where the zero points of the gradient, indicating where a country would experience an increase in cases, and stabilise the new cases to a manageable extent."),
                
                width = 9
              )
            )
        
      ),
      tabItem(tabName = "sub1",
              
              sidebarLayout(
                
                sidebarPanel (
                  selectInput("country2", "Countries/Regions:", 
                              choices = unique(covid_data_new$location),
                              selected = "Australia"),
                  
                  sliderInput("winsize", "Adjust window size in Gardient Zero",
                              min = 3, max = 8,
                              value = 3),
                  
                  width = 3
                  
                ),
                
                
                mainPanel(
                  h1("HealthR_7"),
                  h5("Similarity in trajectories of new cases (Time-varying)"),
                  h5(strong("Question: How fast does a country reduce covid levels after an outbreak?")),
                  h5("Our steps:"),
                  h5("Step 1: Draw new cases graph"),
                  h5("Step 2: Draw new cases smoothed graph"),
                  h5("Step 3: Draw the graph of gradient of the new cases smoothed graph"),
                  h5("Step 4: Find the maximum(green line) and minimum(red line) gradient"),
                  h5("Step 5: show all zero gradient in the graph"),
                  h5("Step 6: Calculate the first time turning point as Date1"),
                  h5("Step 7: Calculate the date when gradient goes close to zero as Date2"),
                  h5("Step 8: Find how fast the country reduce covid levels after an outbreak"),
                  h5(strong("Line Representation")),
                  h5("The green line in the gradient zero represents the date where the gradients become zero, which means the fluxtuation of the new cases has become zero"),
                  h5("The red line represent the date of Gradient zero that are selected from the green line based on the occurance and relativity. From this, we can find recovery time range for countries"),
                  
                  h5(strong("How to investigate")),
                  h5("To investigate the maximum range of time that country require to recover from the new cases"),
                  h5("the user can change the Country drop down menu and window size range to experiment"),
                  h5("In some occasions, the graph will create Error: 'new_list not found', this represents that there was no gradient zero found from the user setting."),
                  h5("which means the country haven't recovered from the user stated state."),
                  h3("Time-varying Analysis"),
                  tabsetPanel(
                    tabPanel("New cases", plotOutput(outputId = "new_cases_plot")),
                    tabPanel("Smoothed new cases", plotOutput(outputId = "new_cases_plot2")),
                    tabPanel("Gradient", plotOutput(outputId = "new_cases_plot3")),
                    tabPanel("Max&Min Gradient", plotOutput(outputId = "new_cases_plot4")),
                    tabPanel("Gradient Zero", plotOutput(outputId = "new_cases_plot8")),
                    tabPanel("Gradient Zero df", dataTableOutput(outputId = "new_cases_plot7")),
                    tabPanel("Gradient Recovery", plotOutput(outputId = "new_cases_plot12"))
                  ),
                  width = 9
                )
              )
      ),
      tabItem(tabName = "sub2",
              sidebarLayout(
                
                sidebarPanel (
                  selectInput("country4", "Countries/Regions:",
                              choices = unique(covid_data$location),
                              selected = "Australia"),
                  width = 3
                ),
                mainPanel(
                  h1("HealthR_7"),
                  h5("Machine learning on COVID-19 new cases"),
                  h5(strong("Question: Predict new cases with similar prediction accuracy for different countries")),
                  
                  h5("Our steps:"),
                  h5("Step 1: Draw new cases graph"),
                  h5("Step 2: Draw new cases smoothed graph"),
                  h5("Step 3: Calculate the accuracy for smoothed new cases based on different model"),
                  h5("Step 4: Draw supervised learning fitting graph on 3 models"),
                  h5("Step 5: Find turning points based on smoothed new cases"),
                  
                  h5(strong("Result Representation")),
                  h5("The results show that USA and India are obviously far from other countries, the two countries are indeed consistent with the facts that COVID-19 situation is serious in the two countries in the current period which indicates the performance of machine learning models used in the smoothed new cases in forecasting accuracy and finding similar countries are well.  Also, a bar graph is finally shown in the plot of comparisons of performances among SVM, GAM and Random forest; it can be seen that the Random forest model shows the lowest error than other models."),
                  
                  
                  h5(strong("How to investigate")),
                  h5("Fitting 3 supervised model, and calculate 3 MSE, based on 3 MSE different countries,  get a N*3 matrix (N is the number of MSE different countries)"),
                  h5("Base on this matrix do K-MEANS clustering, divide in 3 categories to generate the K-means graph, X and Y is two main parts PC1, PC2. These two parts are based on NX3 matrix PCA analysis and show in graph."),
                  h5("It is clearly show that America and India clustering is far away from other countries."),
                  h5("Also do the hclust hierarchical clustering, this graph show how different countries clustering looks like."),
                  h3("Machine learning Analysis"),
                  tabsetPanel(
                    tabPanel("New cases", plotOutput(outputId = "new_cases_plot5")),
                    tabPanel("Smoothed new cases", plotOutput(outputId = "new_cases_plot6")),
                    tabPanel("Accuracy comparisons among supervised models", plotOutput(outputId = "new_cases_plot13")),
                    tabPanel("Smoothed supervised model new cases", plotOutput(outputId = "new_cases_plot9")),
                    tabPanel("Similar countries based on unsupervised learning", plotOutput(outputId = "new_cases_plot10")),
                    tabPanel("Turning points detection of smooth newcases", plotOutput(outputId = "new_cases_plot11"))
                  ),
                  width = 9
                )
              )
      ),
      tabItem(tabName = "sub3",
              sidebarPanel(width = 3),
              mainPanel(
                h3("World Map for countries new cases recover to zero"),
                h5("This section is to visualize and summarize the maximum time range of recovery for each countries in a world map and a table"),
                h5("Step 1: Summary the maximum time range of recovery (From first gradient zero to next gradient zero) for countries"),
                h5("Step 2: Map the recover date range to world map"),
                tabsetPanel(
                  tabPanel("Date range recover to Gardient zero for countries",dataTableOutput(outputId = "table")),
                  tabPanel("World map",plotOutput(outputId = "plot"))
                ),
                width = 9
              )
              
      ),
      tabItem(tabName = "sub4",
              sidebarLayout(
                
                sidebarPanel (
                  selectInput("country5", "Countries/Regions:", 
                              choices = unique(covid_data_new$location),
                              selected = "Australia"),
                  
                  width = 3
                  
                ),
              mainPanel(
                h3("Conclusion"),
                h5("Choose a country on the drop down menu to the left to view its health expenditure, GDP and HDI"),
                h5("use it to offer solutions to why a country did better/worse than another. "),
                h5("For example, Nigeria took 408 days to recover, whereas Australia and New Zealand took 285 and 168 days respectively. "),
                h5("This can come down to their health expenditure (around 4% for Nigeria vs 9% for Aust./NZ). "),
                tabsetPanel(
                  tabPanel("conclusion",dataTableOutput(outputId = "conclusion"))
                ),
                width = 9
              )
              )
              
      )
      
    )
    
    
  )
)


