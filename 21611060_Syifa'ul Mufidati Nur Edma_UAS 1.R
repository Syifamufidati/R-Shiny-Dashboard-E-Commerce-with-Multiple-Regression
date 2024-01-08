# Install stargazer package if not already installed
if (!requireNamespace("stargazer", quietly = TRUE)) {
  install.packages("stargazer")
}
# Load required libraries
library(shiny)
library(shinydashboard)
library(ggplot2)
library(plotly)
library(stargazer)
# Load the data
df <- data.frame(
  Month = c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'),
  X1 = c(150000, 160000, 170000, 180000, 190000, 200000, 210000, 220000, 230000, 240000, 250000, 260000),
  X2 = c(8000, 9500, 10000, 10500, 11000, 9000, 11500, 12000, 12500, 13000, 14000, 15000),
  X3 = c(5, 4.5, 4.8, 4.6, 5.1, 4.7, 4.9, 5.0, 5.2, 5.3, 5.4, 5.5),
  X4 = c(8.5, 8.2, 8.4, 8.5, 8.6, 8.7, 8.8, 8.9, 8.7, 8.8, 8.9, 8.0),
  X5 = c(20000, 22000, 25000, 23000, 30000, 28000, 27000, 35000, 40000, 45000, 50000, 60000),
  Y = c(120, 150, 160, 165, 180, 170, 190, 210, 230, 250, 300, 350)
)
# Define the UI
ui <- dashboardPage(
  dashboardHeader(title = "E-commerce Sales Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Data Table", tabName = "data_table", icon = icon("table")),
      menuItem("Regression Analysis", tabName = "regression", icon = icon("line-chart")),
      menuItem("Descriptive Statistics", tabName = "desc_stats", icon = icon("bar-chart")),
      menuItem("Comparison and Trend Analysis", tabName = "comparison_trend", icon = icon("line-chart"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "data_table",
        fluidPage(
          titlePanel("Data Table"),
          dataTableOutput("data_table")
        )
      ),
      tabItem(
        tabName = "regression",
        fluidPage(
          titlePanel("Regression Analysis"),
          plotOutput("regression_plot"),
          selectInput("x_variable_regression", "Choose X Variable", choices = colnames(df)[2:6], selected = colnames(df)[2])
        )
      ),
      tabItem(
        tabName = "desc_stats",
        fluidPage(
          titlePanel("Descriptive Statistics"),
          mainPanel(
            style = "margin: 0 auto; text-align: center; margin-left: 300px;",
            verbatimTextOutput("desc_stats_output"),
            plotlyOutput("barplot_output")
          )
        )
      ),
      tabItem(
        tabName = "comparison_trend",
        fluidPage(
          titlePanel("Comparison and Trend Analysis"),
          selectInput("x_variable_comparison", "Choose X Variable", choices = colnames(df)[2:6], selected = colnames(df)[2]),
          selectInput("y_variable_comparison", "Choose Y Variable", choices = colnames(df)[2:7], selected = colnames(df)[7]),
          plotOutput("line_plot"),
          plotOutput("bar_plot")
        )
      )
    )
  )
)

# Define the server logic
server <- function(input, output) {
  # Placeholder data for Comparison and Trend Analysis
  df_comparison_trend <- data.frame(
    Month = c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'),
    Y = c(120, 150, 160, 165, 180, 170, 190, 210, 230, 250, 300, 350),
    X1 = c(150000, 160000, 170000, 180000, 190000, 200000, 210000, 220000, 230000, 240000, 250000, 260000)
  )
  
  # Render the data table
  output$data_table <- renderDataTable({
    df
  })
  
  # Render the regression plot
  output$regression_plot <- renderPlot({
    selected_x_variable <- input$x_variable_regression
    model <- lm(Y ~ df[[selected_x_variable]], data = df)
    plot(df[[selected_x_variable]], df$Y, main = "Regression Analysis", xlab = selected_x_variable, ylab = "Y (Sales)")
    abline(model, col = "red")
  })
  
  # Descriptive Statistics
  output$desc_stats_output <- renderPrint({
    summary(df)
  })
  
  output$barplot_output <- renderPlotly({
    plots <- lapply(2:7, function(i) {
      plot_ly(df, x = ~df[, i], type = "histogram", color = I("#D6604D")) %>%
        layout(showlegend = FALSE, title = colnames(df)[i], yaxis = list(title = "Frequency"))
    })
    subplot(plots, nrows = 2, margin = 0.05)
  })
  
  # Line Plot for Comparison and Trend Analysis
  output$line_plot <- renderPlot({
    selected_x_variable <- input$x_variable_comparison
    selected_y_variable <- input$y_variable_comparison
    ggplot(df_comparison_trend, aes_string(x = selected_x_variable, y = selected_y_variable, group = 1)) +
      geom_line(color = "blue") +
      geom_point(color = "red") +
      labs(title = paste("Trend of", selected_y_variable, "over", selected_x_variable),
           x = selected_x_variable, y = selected_y_variable)
  })
  
  # Bar Plot for Comparison and Trend Analysis
  output$bar_plot <- renderPlot({
    selected_x_variable <- input$x_variable_comparison
    selected_y_variable <- input$y_variable_comparison
    ggplot(df_comparison_trend, aes_string(x = selected_x_variable, y = selected_y_variable)) +
      geom_bar(stat = "identity", fill = "lightblue") +
      labs(title = paste("Comparison of", selected_y_variable, "over", selected_x_variable),
           x = selected_x_variable, y = selected_y_variable)
  })
}

# Run the Shiny app
shinyApp(ui, server)
