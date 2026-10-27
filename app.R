
library(shiny)
library(bslib)
library(readxl)
library(dplyr)
library(DT)

# load data
superstore <- read_excel(
  "data/superstore_data.xls",
  col_types = c(
    rep("guess", 11),
    "text",
    rep("guess", 9)
  )
)

# Numeric Variables for filtering 
num_choices <- c("Sales", "Quantity", "Discount", "Profit")

# Define the user interface
ui <- page_sidebar(
  title = "Superstore Sales",
  
  sidebar = sidebar(
    h4("Filter the Data"),
    
    # Categorical filter: State
    selectInput(
      inputId = "state",
      label = "Choose State(s):",
      choices = c(
        "All states" = "__ALL__",
        sort(unique(superstore$State))
      ),
      selected = "__ALL__",
      multiple = TRUE
    ),
    
    
    # Categorical filter: Category
    selectInput(
      inputId = "category",
      label = "Choose Product Category:",
      choices = c(
        "All categories" = "__ALL__",
        sort(unique(superstore$Category))
      ),
      selected = "__ALL__",
      multiple = TRUE
    ),
    
    # First numeric variable
    selectInput(
      inputId = "num_var1",
      label = "First numeric variable:",
      choices = num_choices,
      selected = "Sales"
    ),
    
    uiOutput("num_slider1"),
    
    # Second numeric variable
    selectInput(
      inputId = "num_var2",
      label = "Second numeric variable:",
      choices = num_choices,
      selected = "Profit"
    ),
    
    uiOutput("num_slider2"),
    
    # Apply selected filters
    actionButton(
      inputId = "apply_filters",
      label = "Apply Filters",
      class = "btn-primary"
    ),
    
    # Show the number of rows in the applied data
    textOutput("filter_status")
  ),
  
  # Main Panel Tabs
  
  navset_tab(
    nav_panel(
      "About",
      h3("About This App"),
      p("Explore sales, products, and profitability in the Superstore dataset.")
    ),
    
    nav_panel(
      "Download Data",
      h3("Download the Data"),
      p("This table will show data after your applied filters."),
    
    DT::dataTableOutput("data_table"),
    
    br(),
    
    downloadButton(
      outputId = "download_data",
      label = "Download the Filtered Data"
    )
  ),
    
    nav_panel(
      "Explore Data",
      h3("Explore the Data"),
      p("Your summaries and graphs will go here.")
    )
  )
)

# Define the server
server <- function(input, output, session) {
  
  # First numeric range slider
  output$num_slider1 <- renderUI({
    req(input$num_var1)
    
    values <- superstore[[input$num_var1]]
    
    sliderInput(
      inputId = "range1",
      label = paste("Choose a range for", input$num_var1),
      min = min(values, na.rm = TRUE),
      max = max(values, na.rm = TRUE),
      value = range(values, na.rm = TRUE)
    )
  })
  
  # Second numeric range slider
  output$num_slider2 <- renderUI({
    req(input$num_var2)
    
    values <- superstore[[input$num_var2]]
    
    sliderInput(
      inputId = "range2",
      label = paste("Choose a range for", input$num_var2),
      min = min(values, na.rm = TRUE),
      max = max(values, na.rm = TRUE),
      value = range(values, na.rm = TRUE)
    )
  })
  
  # Store filtered & applied data
  filtered_data <- reactiveValues(
    data = superstore
  )
  
  # Make sure data updates only when Apply Filters is clicked
  observeEvent(input$apply_filters, {
    
    filtered <- superstore
    
    # Filter by state unless all states are selected
    if (!is.null(input$state) &&
        !"__ALL__" %in% input$state) {
      filtered <- filtered |>
        filter(State %in% input$state)
    }
    
    # Filter by category unless all categories are selected
    if (!is.null(input$category) &&
        !"__ALL__" %in% input$category) {
      filtered <- filtered |>
        filter(Category %in% input$category)
    }
    
    # Filter by the first numeric variable
    req(input$range1)
    
    filtered <- filtered |>
      filter(
        .data[[input$num_var1]] >= input$range1[1],
        .data[[input$num_var1]] <= input$range1[2]
      )
    
    # Filter by the second numeric variable
    req(input$range2)
    
    filtered <- filtered |>
      filter(
        .data[[input$num_var2]] >= input$range2[1],
        .data[[input$num_var2]] <= input$range2[2]
      )
    
    # Save the filtered data
    filtered_data$data <- filtered
  })
  
  # Display the number of rows in the applied data
  output$filter_status <- renderText({
    paste(
      "Rows after applying filters:",
      nrow(filtered_data$data)
    )
  })
  
  # Display the filtered data in a table
  output$data_table <- DT::renderDataTable({
    filtered_data$data
  },
  options = list(
    pageLength = 10,
    scrollX = TRUE
  ))
  
  # Download the filtered data as a CSV file
  output$download_data <- downloadHandler(
    
    filename = function() {
      "superstore_filtered.csv"
    },
    
    content = function(file) {
      write.csv(
        filtered_data$data,
        file,
        row.names = FALSE
      )
    }
  )
}

# Run the application 
shinyApp(ui = ui, server = server)
