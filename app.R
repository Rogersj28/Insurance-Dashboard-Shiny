# Load the data
insurance <- read.csv(file.choose(), header = TRUE)

# Load necessary packages
install.packages("shiny")
install.packages("shinythemes")
install.packages("shinydashboard")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("psych")
install.packages("DT")

library(shiny)
library(shinythemes)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(psych)
library(DT)

# Demographic segmentation
insurance$dob <- as.Date(insurance$dob, "%m/%d/%Y")
insurance$incident_date <- as.Date(insurance$incident_date, "%m/%d/%Y")
insurance$age <- as.numeric(difftime(insurance$incident_date, insurance$dob, units = "days")) / 365.25
insurance$age <- round(insurance$age, 0)


insurance_seg <- insurance[, c(12, 14, 18, 17, 19, 3, 5, 22)]
insurance_seg[, c(1, 2, 3, 6)] <- lapply(insurance_seg[, c(1, 2, 3, 6)], factor)

levels(insurance_seg$gender) <- c("Male", "Female")
levels(insurance_seg$ed_cat) <- c("No High School", "High School", "Some college", "College degree", "Post-undergraduate")
levels(insurance_seg$marital) <- c("Single", "Married")
levels(insurance_seg$claim_type) <- c("Wind/Hail", "Water Damage", "Fire/Smoke", "Contamination", "Theft/Vandalism")


# Name for variables
categorical_vars <- c( "Gender" = "gender","Educational Level" = "ed_cat",
                       "Marital Status" = "marital","Claim Type" = "claim_type")
continuous_vars <- c( "Age" = "age", "Residence Size" = "reside", 
                      "Cost of claim" = "claim_amount", "Household income" = "income")

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "Insurance Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Pie Chart", tabName = "piechart", icon = icon("pie-chart")),
      menuItem("Bar Chart", tabName = "barchart", icon = icon("bar-chart")),
      menuItem("Histogram", tabName = "histogram", icon = icon("chart-bar")),
      menuItem("Violin Plot", tabName = "violinplot", icon = icon("chart-area"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "piechart",
              h2("Pie Chart"),
              sidebarPanel(
                selectInput("continuous_var", "Continuous Variable:", choices = (continuous_vars)),
                selectInput("summary_stat", "Summary Statistic:", choices = c("Mean", "Count", "Total")),
                selectInput("categorical_var", "Categorical Variable:", choices = (categorical_vars)),
                checkboxInput("show_labels", "Show Labels", value = TRUE),
                sliderInput("font_size", "Font Size:", min = 5, max = 20, value = 8),
                selectInput("gender", "Gender:", c("All", unique(as.character(insurance_seg$gender)))),
                selectInput("ed_cat", "Educational Level:", c("All", unique(as.character(insurance_seg$ed_cat)))),
                selectInput("marital", "Marital Status:", c("All", unique(as.character(insurance_seg$marital)))),
                selectInput("claim_type", "Claim Type:", c("All", unique(as.character(insurance_seg$claim_type))))
              ),
              mainPanel(
                plotOutput("chart"),
                uiOutput("pie_image_display"),
                DT::dataTableOutput("table")
              )
      ),
      tabItem(tabName = "barchart",
              h2("Bar Chart"),
              sidebarPanel(
                selectInput("categorical_var", "Categorical Variable:", choices = (categorical_vars)),
                sliderInput("font_size", "Font Size:", min = 5, max = 20, value = 8),
                selectInput("gender", "Gender:", c("All", unique(as.character(insurance_seg$gender)))),
                selectInput("ed_cat", "Educational Level:", c("All", unique(as.character(insurance_seg$ed_cat)))),
                selectInput("marital", "Marital Status:", c("All", unique(as.character(insurance_seg$marital)))),
                selectInput("claim_type", "Claim Type:", c("All", unique(as.character(insurance_seg$claim_type))))
              ),
              mainPanel(
                plotOutput("histogram2"),
                uiOutput("bar_image_display"),
                DT::dataTableOutput("table1")
              )
      ),
      tabItem(tabName = "histogram",
              h2("Histogram"),
              sidebarPanel(
                selectInput("continuous_var", "Continuous Variable:", choices = (continuous_vars)),
                selectInput("summary_stat", "Summary Statistic:", choices = c("Mean", "Count", "Total")),
                sliderInput("bins1", "Bin width:", min = 1, max = 500, value = 15),
                sliderInput("font_size", "Font Size:", min = 5, max = 20, value = 8),
                selectInput("gender", "Gender:", c("All", unique(as.character(insurance_seg$gender)))),
                selectInput("ed_cat", "Educational Level:", c("All", unique(as.character(insurance_seg$ed_cat)))),
                selectInput("marital", "Marital Status:", c("All", unique(as.character(insurance_seg$marital)))),
                selectInput("claim_type", "Claim Type:", c("All", unique(as.character(insurance_seg$claim_type))))
              ),
              mainPanel(
                plotOutput("histogram"),
                uiOutput("hist_image_display"),
                DT::dataTableOutput("table2")
              )
      ),
      tabItem(tabName = "violinplot",
              h2("Violin Plot"),
              sidebarPanel(
                selectInput("continuous_var", "Continuous Variable:", choices = (continuous_vars)),
                selectInput("categorical_var", "Categorical Variable:", choices = (categorical_vars)),
                sliderInput("font_size", "Font Size:", min = 5, max = 20, value = 8),
                selectInput("gender", "Gender:", c("All", unique(as.character(insurance_seg$gender)))),
                selectInput("ed_cat", "Educational Level:", c("All", unique(as.character(insurance_seg$ed_cat)))),
                selectInput("marital", "Marital Status:", c("All", unique(as.character(insurance_seg$marital)))),
                selectInput("claim_type", "Claim Type:", c("All", unique(as.character(insurance_seg$claim_type))))
              ),
              mainPanel(
                plotOutput("violinPlot"),
                uiOutput("violin_image_display"),
                DT::dataTableOutput("table3")
              )
      )
    )
  )
)
# Define Server
server <- function(input, output) {
  
  # Pie chart
  output$chart <- renderPlot({
    continuous_var <- input$continuous_var
    summary_stat <- input$summary_stat
    categorical_var <- input$categorical_var
    show_labels <- input$show_labels
    
    summary_data <- insurance_seg %>%
      group_by(!!sym(categorical_var)) %>%
      summarize(value = switch(summary_stat,
                               "Mean" = mean(!!sym(continuous_var)),
                               "Count" = n(),
                               "Total" = sum(!!sym(continuous_var))))
    
    j <- ggplot(summary_data, aes(x = "", y = value, fill = !!sym(categorical_var))) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar("y", start = 0) +
      theme(legend.text = element_text(size = input$font_size + 4),
            axis.text = element_text(size = input$font_size + 4)) + 
      labs(title = paste("Pie Chart of", continuous_var, "per", categorical_var), size = input$font_size)
    
    if (show_labels) {
      j <- j + geom_text(aes(label = scales::comma(round(value, 2))), 
                         position = position_stack(vjust = 0.5), color = "black", size = input$font_size, show.legend = FALSE)
    }
    
    j
  })
  
  # Bar chart
  output$histogram2 <- renderPlot({
    categorical_var <- input$categorical_var
    
    ggplot(insurance_seg, aes(x = !!sym(categorical_var), fill = !!sym(categorical_var))) +
      geom_bar(width = 0.5) +
      stat_count(geom = "text", aes(label = paste0(round((after_stat(count)) / sum(after_stat(count)) * 100, 2), "%")),
                 position = position_stack(vjust = 0.5), 
                 color = "black", size = input$font_size, show.legend = FALSE) +
      theme(legend.text = element_text(size = input$font_size + 4),
            axis.text = element_text(size = input$font_size + 4)) +
      labs(title = paste("Bar Chart of", categorical_var), size = input$font_size)
  })
  
  # Histogram
  output$histogram <- renderPlot({
    continuous_var <- input$continuous_var
    
    ggplot(insurance_seg, aes(x = !!sym(input$continuous_var))) +
      geom_histogram(binwidth = input$bins1, fill = "steelblue", color = "black") +
      theme(legend.text = element_text(size = input$font_size + 4),
            axis.text = element_text(size = input$font_size + 4),
            axis.title = element_text(size = input$font_size + 4)) +
      labs(title = paste("Histogram of", continuous_var), 
           size = input$font_size)
  })
  
  # Violin Plot
  output$violinPlot <- renderPlot({
    continuous_var <- input$continuous_var
    categorical_var <- input$categorical_var
    
    ggplot(insurance_seg, aes(x = !!sym(categorical_var), 
                              y = !!sym(continuous_var), 
                              fill = !!sym(categorical_var))) +
      geom_violin() +
      theme(legend.text = element_text(size = input$font_size + 4),
            axis.text = element_text(size = input$font_size + 4),
            axis.title = element_text(size = input$font_size + 4)) +
      labs(title = paste("Violin Plot of", continuous_var), 
           size = input$font_size)
  })
  
  # Data Table for Pie Chart
  output$table <- DT::renderDataTable({
    data <- insurance_seg
    if (input$gender != "All") {
      data <- data[data$gender == input$gender,]
    }
    if (input$ed_cat != "All") {
      data <- data[data$ed_cat == input$ed_cat,]
    }
    if (input$marital != "All") {
      data <- data[data$marital == input$marital,]
    }
    if (input$claim_type != "All") {
      data <- data[data$claim_type == input$claim_type,]
    }
    
    # Ensure data is a data frame
    data <- as.data.frame(data)
    
    datatable(data) %>%
      formatStyle(
        'age',
        backgroundColor = styleInterval(30, c('white', 'yellow'))
      ) %>%
      formatStyle(
        'claim_type',
        backgroundColor = styleEqual(c('Wind/Hail', 'Water Damage', 'Fire/Smoke', 'Contamination', 'Theft/Vandalism'), 
                                     c('lightblue', 'lightgreen', 'lightcoral', 'lightgoldenrodyellow', 'lightpink'))
      )
  })
  
  # Data Table for Bar Chart
  output$table1 <- DT::renderDataTable({
    data <- insurance_seg
    if (input$gender != "All") {
      data <- data[data$gender == input$gender,]
    }
    if (input$ed_cat != "All") {
      data <- data[data$ed_cat == input$ed_cat,]
    }
    if (input$marital != "All") {
      data <- data[data$marital == input$marital,]
    }
    if (input$claim_type != "All") {
      data <- data[data$claim_type == input$claim_type,]
    }
    
    # Ensure data is a data frame
    data <- as.data.frame(data)
    
    datatable(data) %>% 
      formatStyle(
        'age',
        backgroundColor = styleInterval(30, c('white', 'yellow'))
      ) %>%
      formatStyle(
        'claim_type',
        backgroundColor = styleEqual(c('Wind/Hail', 'Water Damage', 'Fire/Smoke', 'Contamination', 'Theft/Vandalism'), 
                                     c('lightblue', 'lightgreen', 'lightcoral', 'lightgoldenrodyellow', 'lightpink'))
      )
  })
  # Data Table 
  output$table2 <- DT::renderDataTable({
    data <- insurance_seg
    if (input$gender != "All") {
      data <- data[data$gender == input$gender,]
    }
    if (input$ed_cat != "All") {
      data <- data[data$ed_cat == input$ed_cat,]
    }
    if (input$marital != "All") {
      data <- data[data$marital == input$marital,]
    }
    if (input$claim_type != "All") {
      data <- data[data$claim_type == input$claim_type,]
    }
    
    # Ensure data is a data frame
    data <- as.data.frame(data)
    
    datatable(data) %>%
      formatStyle(
        'age',
        backgroundColor = styleInterval(30, c('white', 'yellow'))
      ) %>%
      formatStyle(
        'claim_type',
        backgroundColor = styleEqual(c('Wind/Hail', 'Water Damage', 'Fire/Smoke', 'Contamination', 'Theft/Vandalism'), 
                                     c('lightblue', 'lightgreen', 'lightcoral', 'lightgoldenrodyellow', 'lightpink'))
      )
  })
  # Data Table 
  output$table3 <- DT::renderDataTable({
    data <- insurance_seg
    if (input$gender != "All") {
      data <- data[data$gender == input$gender,]
    }
    if (input$ed_cat != "All") {
      data <- data[data$ed_cat == input$ed_cat,]
    }
    if (input$marital != "All") {
      data <- data[data$marital == input$marital,]
    }
    if (input$claim_type != "All") {
      data <- data[data$claim_type == input$claim_type,]
    }
    
    # Ensure data is a data frame
    data <- as.data.frame(data)
    
    datatable(data) %>%
      formatStyle(
        'age',
        backgroundColor = styleInterval(30, c('white', 'yellow'))
      ) %>%
      formatStyle(
        'claim_type',
        backgroundColor = styleEqual(c('Wind/Hail', 'Water Damage', 'Fire/Smoke', 'Contamination', 'Theft/Vandalism'), 
                                     c('lightblue', 'lightgreen', 'lightcoral', 'lightgoldenrodyellow', 'lightpink'))
      )
  })
}

# Run the app
shinyApp(ui, server)


