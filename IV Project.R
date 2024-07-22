library(shinyDarkmode)
library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(DT)
library(plotly)
library(shinyjs)

df <- read.csv("D:/4th Year Material/First Term/Information Visualization/IV project/IV Project/mpg.csv")

df <- na.omit(df)
df<-df[!duplicated(df),]

summary(df)

ui <- dashboardPage(
  dashboardHeader(title = "Interactive Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      use_darkmode(),
      menuItem("Visualization Options", tabName = "visualization_options"),
      
      checkboxInput("togglemode","Dark Mode", value = FALSE),
      
      selectInput("SelectCategory", "Select Acceleration", choices = unique(df$acceleration), multiple = TRUE),
      checkboxGroupInput("Filter", "Select Cylinders", choices = unique(df$Cylinders), selected = 8),
      dateRangeInput("date_range", "Model Year Range",
                     start = min(df$model_year, na.rm = TRUE),
                     end = max(df$model_year, na.rm = TRUE)),
      checkboxGroupInput("visualizations", "Select Visualizations",
                         choices = c("scatter plot", "bar chart", "line chart", "box plot", "histogram", "violin plot", "density plot")),
      radioButtons("data_type", "Data Type", choices = c("Daily", "Cumulative"), selected = "Daily"),
      sliderInput("displacement", "Select displacement Range", min = 0, max = 350, value = c(70, 350))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "visualization_options",
        
          box(title = "Selected Data", status = "info", width = 14, DTOutput("selected_data_table")),
          box(title = "Selected Visualization", status = "info", width = 14, plotlyOutput("selected_chart")),
          box(title = "Instructions", status = "info", width = 14, tags$div(
            style = "font-size: 14px; text-align: left;",
            "1. Use the sidebar to select filters for your data visualization.",
            br(),
            "2. Toggle between dark and light mode for a personalized experience.",
            br(),
            "3. Explore different visualizations using the checkboxes and sliders.",
            br(),
            "4. View detailed information by hovering over data points in the visualizations."
          ))
        
      )
    ),
  )
)

server <- function(input, output) {
  
  filtered_data <- reactive({
    df_filtered <- df %>%
      filter(
        acceleration %in% input$SelectCategory,
        model_year >= input$date_range[1],
        model_year <= input$date_range[2],
        Cylinders %in% input$Filter,
        displacement >= input$displacement[1],
        displacement <= input$displacement[2]
      )
    
    if (input$data_type == "Daily") {
      return(df_filtered)
    } else {
      df_filtered$constant_group <- 1
      
      df_cumulative <- df_filtered %>%
        group_by(constant_group) %>%
        summarise(across(everything(), ~ sum(as.numeric(.))))  # Sum for numeric columns
      
      return(df_cumulative)
    }
  })
  
  darkmode_toggle(inputid = 'togglemode')
  
  output$selected_data_table <- renderDataTable({
    filtered_data()
  }, options = list(
    lengthMenu = c(4, 8, 12, 16, 20),
    pageLength = 8
  ))
  
  output$selected_chart <- renderPlotly({
    selected_vis <- input$visualizations
    
    if ("scatter plot" %in% selected_vis) {
      plot_output <- ggplot(filtered_data(), aes(x = horsepower, y = mpg, text = paste("Cylinders: ", Cylinders))) +
        geom_point() + labs(x = "horsepower", y = "mpg") 
      
    } else if ("bar chart" %in% selected_vis) {
      plot_output <- ggplot(filtered_data(), aes(x = factor(Cylinders), fill = factor(name), text = paste("Count: ", ..count..))) +
        geom_bar(position = "dodge") + labs(x = "Cylinders", y = "Count")
      
    } else if ("line chart" %in% selected_vis) {
      plot_output <- ggplot(filtered_data(), aes(x = model_year, y = mpg, color = factor(Cylinders), text = paste("Year: ", model_year, "<br>MPG: ", mpg))) +
        geom_line(size = 2) + labs(x = "model_year", y = "mpg") 
      
      } else if ("box plot" %in% selected_vis) {
      plot_output <- ggplot(filtered_data(), aes(x = factor(Cylinders), y = mpg, text = paste("Cylinders: ", Cylinders))) +
        geom_boxplot() + labs(x = "Cylinders", y = "mpg")
      
    } else if ("histogram" %in% selected_vis) {
      plot_output <- ggplot(filtered_data(), aes(x = mpg, text = paste("MPG: ", mpg))) +
        geom_histogram(binwidth = 6, fill = "blue", color = "black") + labs(x = "mpg", y = "Frequency") 
      
    } else if ("violin plot" %in% selected_vis) {
      plot_output <- ggplot(filtered_data(), aes(x = factor(Cylinders), y = mpg, fill = factor(origin), text = paste("Cylinders: ", Cylinders, "<br>MPG: ", mpg))) +
        geom_violin() + labs(x = "Cylinders", y = "mpg")
      
    } else if ("density plot" %in% selected_vis) {
      plot_output <- ggplot(filtered_data(), aes(x = mpg, fill = factor(Cylinders), text = paste("MPG: ", mpg))) +
        geom_density(alpha = 0.5) + labs(x = "mpg", y = "Density") 
      
    } else {
      plot_output <- ggplot() + theme_minimal()
    }
    
    ggplotly(plot_output, tooltip = "text")
  })
}

shinyApp(ui, server)