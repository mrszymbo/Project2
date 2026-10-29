
library(shiny)
library(bslib)
library(readxl)
library(dplyr)
library(DT)
library(shinycssloaders)

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
  title = "Exploring Superstore Sales",
  
  # Sidebar filters
  sidebar = sidebar(
    
    h4("Filter the Data"),
    
    # Filter by state
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
    
    # Filter by product category
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
    
    # First numeric filter
    selectInput(
      inputId = "num_var1",
      label = "First numeric variable:",
      choices = num_choices,
      selected = "Sales"
    ),
    
    uiOutput("num_slider1"),
    
    # Second numeric filter
    selectInput(
      inputId = "num_var2",
      label = "Second numeric variable:",
      choices = num_choices,
      selected = "Profit"
    ),
    
    uiOutput("num_slider2"),
    
    # Apply filters
    actionButton(
      inputId = "apply_filters",
      label = "Apply Filters",
      class = "btn-primary"
    ),
    
    # Number of rows after filtering
    textOutput("filter_status")
  ),
  
  # Main tabs
  navset_tab(
    
    # About tab
    nav_panel(
      "About",
      
      br(),
      
      h3("About This App"),
      
      p(
        "This app explores sales, profitability, and product trends using the Superstore dataset.",
        "It lets users investigate how sales and profits vary across products, customers, and locations."
    ),
    
    br(),
    
    h4("About the Dataset"),
    
    p(
      "The dataset was provided by Dr. Post and comes from Kaggle.",
      "It contains information about orders, customers, product categories, sales, dicsounts, quantities, and profits."
    ),
    
    p(
      "Data source: ",
      a(
        "Superstore Dataset on Kaggle",
        href = "https://www.kaggle.com/datasets/juhi1994/superstore/data",
        target = "_blank"
      )
    ),
    
    br(),
    
    h4("How to Use This App"),
    
    tags$ol(
      tags$li(
        "Use the sidebar to filter by state, product category, and numeric variable ranges."
      ),
      tags$li(
        "Click Apply Filters to update the data used throughout the app."
      ),
      tags$li(
        "Use the Download Data tab to view the filtered records or download them as a CSV file."
      ),
      tags$li(
        "Visit the Explore Data tab to view contingency tables, numeric summary statistics, and interactive visualizations."
      )
    ),
    
    br(),
    
    h4("What Can You Explore?"),
    
    tags$ul(
      tags$li("Which states and cities generate the most sales."),
      tags$li("How sales and profits vary across product categories."),
      tags$li("Relationships between numeric variables such as sales and profit."),
      tags$li("Differences in sales across groups and locations.")
    )
    ),
    
    # Download Data tab
    nav_panel(
      "Download Data",
      
      br(),
      
      h3("Download the Data"),
      p("This table shows the data after the filters are applied."),
      
      DT::dataTableOutput("data_table"),
      
      br(),
      br(),
      
      downloadButton(
        outputId = "download_data",
        label = "Download the Filtered Data"
      ),
      
      br(),
      br()
    ),
    
    # Explore Data tab
    nav_panel(
      "Explore Data",
      
      br(),
      
      h5("Choose how you want to explore the data."),
      
      br(),
      
      # Nested tabs
      navset_tab(
        # Summary Statistics tab
        nav_panel(
          "Summary Statistics",
          
          tags$div(
            style = "margin-bottom: 20px;"
          ),
          
          radioButtons(
            inputId = "explore_type",
            label = tags$div(
              style = "margin-bottom: 10px;",
              "Choose a type of summary:"
                             ),
            choices = c(
              "Categorical Summaries" = "categorical",
              "Numeric Summaries" = "numeric"
            ),
            selected = "categorical",
            inline = TRUE
          ),
          
          # Categorical summaries
          conditionalPanel(
            condition = "input.explore_type == 'categorical'",
            
            h4("Contingency Tables"),
            
            selectInput(
              inputId = "cat_var1",
              label = "First categorical variable:",
              choices = c(
                "Category", "Sub-Category", "State",
                "Region", "Segment", "Ship Mode"
              ),
              selected = "Category"
            ),
            
            selectInput(
              inputId = "cat_var2",
              label = "Second categorical variable (optional):",
              choices = c(
                "None" = "None",
                "Category" = "Category",
                "Sub-Category" = "Sub-Category",
                "State" = "State",
                "Region" = "Region",
                "Segment" = "Segment",
                "Ship Mode" = "Ship Mode"
              ),
              selected = "None"
            ),
            
            tableOutput("cat_summary")
          ),
          
          # Numeric summaries
          conditionalPanel(
            condition = "input.explore_type == 'numeric'",
            
            h4("Numeric Summary Statistics"),
            
            selectInput(
              inputId = "num_sum",
              label = "Numeric variable to summarize:",
              choices = c("Sales", "Profit", "Quantity", "Discount"),
              selected = "Sales"
            ),
            
            selectInput(
              inputId = "sum_group",
              label = "Summarize across the levels of:",
              choices = c(
                "Category", "Sub-Category", "State",
                "Region", "Segment", "Ship Mode"
              ),
              selected = "Category"
            ),
            
            tableOutput("num_summary")
          )
        ),
        
        # Data Visualizations tab
        nav_panel(
          "Data Visualizations",
          
          tags$div(
            style = "margin-bottom: 20px;"
          ),
          
          selectInput(
            inputId = "plot_type",
            label = "Choose a graph type:",
            choices = c(
              "Bar Chart" = "bar_chart",
              "Side-By-Side Bar Chart" = "side_bar_chart",
              "Scatterplot" = "scatterplot",
              "Boxplot" = "boxplot",
              "Faceted Bar Chart" = "facet_bar_chart",
              "Heatmap" = "heatmap"
            ),
            selected = "bar_chart"
          ),
          
          uiOutput("plot_controls"),
          
          withSpinner(
            plotOutput("superstore_plot"),
            type = 7
          )
        )
      )
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
      label = paste("Choose a range for",
                    input$num_var1),
      
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
      label = paste("Choose a range for",
                    input$num_var2),
      
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
  
  
  # Create a one-way or two-way contingency table
  output$cat_summary <- renderTable({
    
    data <- filtered_data$data
    
    req(input$cat_var1, input$cat_var2)
    
    validate(
      need(nrow(data) > 0, "No rows match the applied filters.")
    )
    
    if (input$cat_var2 == "None") {
      
      # One-way table
      as.data.frame(
        table(data[[input$cat_var1]]),
        responseName = "Frequency"
      ) |>
        dplyr::rename(!!input$cat_var1 := Var1)
      
    } else {
      
      # Two-way table
      as.data.frame.matrix(
        table(
          data[[input$cat_var1]],
          data[[input$cat_var2]]
        )
      ) |>
        tibble::rownames_to_column(var = input$cat_var1)
      
    }
    
  })

  # Calculate numeric summary statistics
  output$num_summary <- renderTable({
    
    data <- filtered_data$data
    
    req(input$num_sum)
    req(input$sum_group)
    
    validate(
      need(nrow(data) > 0, "No rows match the applied filters.")
    )
    
    data |>
      group_by(.data[[input$sum_group]]) |>
      summarize(
        count = sum(!is.na(.data[[input$num_sum]])),
        mean = round(mean(.data[[input$num_sum]], na.rm = TRUE), 2),
        median = round(median(.data[[input$num_sum]], na.rm = TRUE), 2),
        sd = round(sd(.data[[input$num_sum]], na.rm = TRUE), 2),
        .groups = "drop"
      )
  })
  
  # Create variable selectors based on the selected graph
  output$plot_controls <- renderUI({
    
    cat_vars <- c(
      "Category", "Sub-Category", "State",
      "City", "Region", "Segment", "Ship Mode"
    )
    
    num_vars <- c("Sales", "Profit", "Quantity", "Discount")
    
    switch(
      input$plot_type,
      
      "bar_chart" = tagList(
        selectInput("plot_var1", "Categorical variable:",
                    choices = cat_vars, selected = "Category"),
        selectInput("plot_var2", "Numeric variable:",
                    choices = num_vars, selected = "Sales")
      ),
      
      "side_bar_chart" = tagList(
        selectInput("plot_var1", "First categorical variable:",
                    choices = cat_vars, selected = "State"),
        selectInput("plot_var2", "Second categorical variable:",
                    choices = cat_vars, selected = "Category"),
        selectInput("plot_var3", "Numeric variable:",
                    choices = num_vars, selected = "Sales")
      ),
      
      "scatterplot" = tagList(
        selectInput("plot_var1", "X-axis (numeric):",
                    choices = num_vars, selected = "Sales"),
        selectInput("plot_var2", "Y-axis (numeric):",
                    choices = num_vars, selected = "Profit"),
        selectInput("plot_color", "Color points by:",
                    choices = c("None", cat_vars),
                    selected = "Region")
      ),
      
      "boxplot" = tagList(
        selectInput("plot_var1", "Categorical variable:",
                    choices = cat_vars, selected = "Category"),
        selectInput("plot_var2", "Numeric variable:",
                    choices = num_vars, selected = "Profit")
      ),
      
      "facet_bar_chart" = tagList(
        selectInput("plot_var1", "Categorical variable:",
                    choices = cat_vars, selected = "Sub-Category"),
        selectInput("plot_var2", "Numeric variable:",
                    choices = num_vars, selected = "Sales"),
        selectInput("plot_facet", "Separate panels by:",
                    choices = cat_vars, selected = "Category")
      ),
      
      "heatmap" = tagList(
        selectInput("plot_var1", "First categorical variable:",
                    choices = cat_vars, selected = "State"),
        selectInput("plot_var2", "Second categorical variable:",
                    choices = cat_vars, selected = "Category"),
        selectInput("plot_var3", "Numeric variable for tile color:",
                    choices = num_vars, selected = "Sales")
      )
    )
  })
  
  
  # Create the selected graph
  output$superstore_plot <- renderPlot({
    
    data <- filtered_data$data
    
    req(input$plot_type, input$plot_var1, input$plot_var2)
    
    validate(
      need(nrow(data) > 0, "No rows match the applied filters.")
    )
    
    # Check that selected variables match the graph type
    cat_vars <- c(
      "Category", "Sub-Category", "State",
      "City", "Region", "Segment", "Ship Mode"
    )
    
    num_vars <- c("Sales", "Profit", "Quantity", "Discount")
    
    if (input$plot_type %in% c("bar_chart", "boxplot", "facet_bar_chart")) {
      validate(
        need(
          input$plot_var1 %in% cat_vars,
          "Please select a categorical variable.")
      )
    }
    
    if (input$plot_type %in% c("side_bar_chart", "heatmap")) {
      validate(
        need(
          input$plot_var1 %in% cat_vars &&
            input$plot_var2 %in% cat_vars,
          "Please select categorical variables for both fields."
        )
      )
    }
    
    if (input$plot_type == "scatterplot") {
      validate(
        need(
          input$plot_var1 %in% num_vars &&
            input$plot_var2 %in% num_vars,
          "Please select numeric variables for both axes."
        )
      )
    }
    
    # Create the appropriate graph
    plot <- switch(
      input$plot_type,
      
      # Bar chart
      "bar_chart" = {
        plot_data <- data |>
          group_by(.data[[input$plot_var1]]) |>
          summarize(
            value = sum(.data[[input$plot_var2]], na.rm = TRUE),
            .groups = "drop"
          )
        
        ggplot(plot_data, aes(x = .data[[input$plot_var1]], y = value)) +
          geom_col() +
          labs(
            title = paste(
              input$plot_var2, "by", 
              input$plot_var1),
            
            x = input$plot_var1,
            y = paste("Total", input$plot_var2)
          )
      },
      
      # Side-by-side bar chart
      "side_bar_chart" = {
        req(input$plot_var3)
        
        validate(
          need(
            input$plot_var1 != input$plot_var2,
            "Choose two different categorical variables.")
        )
        
        plot_data <- data |>
          group_by(
            .data[[input$plot_var1]],
            .data[[input$plot_var2]]
          ) |>
          summarize(
            value = sum(.data[[input$plot_var3]], na.rm = TRUE),
            .groups = "drop"
          )
        
        ggplot(
          plot_data,
          aes(
            x = .data[[input$plot_var1]],
            y = value,
            fill = .data[[input$plot_var2]]
          )
        ) +
          geom_col(position = position_dodge()) +
          labs(
            title = paste(
              "Total", input$plot_var3, "by",
              input$plot_var1, "and", 
              input$plot_var2),
            
            x = input$plot_var1,
            y = paste("Total", input$plot_var3),
            fill = input$plot_var2
          )
      },
      
      # Scatterplot
      "scatterplot" = {
        req(input$plot_color)
        
        if (input$plot_color == "None") {
          plot <- ggplot(
            data,
            aes(
              x = .data[[input$plot_var1]],
              y = .data[[input$plot_var2]]
            )
          ) +
            geom_point(alpha = 0.5)
        } else {
          plot <- ggplot(
            data,
            aes(
              x = .data[[input$plot_var1]],
              y = .data[[input$plot_var2]],
              color = .data[[input$plot_color]]
            )
          )
        }
        
        plot + 
          geom_point(alpha = 0.5) +
          labs(
            title = paste(
              input$plot_var2, "vs.", 
              input$plot_var1),
            
            x = input$plot_var1,
            y = input$plot_var2,
            color = if (input$plot_color == "None") NULL
            else input$plot_color
          )
      },
      
      # Boxplot
      "boxplot" = {
        ggplot(
          data,
          aes(
            x = .data[[input$plot_var1]],
            y = .data[[input$plot_var2]],
            fill = .data[[input$plot_var1]]
          )
        ) +
          geom_boxplot() +
          labs(
            title = paste(
              input$plot_var2, "by", 
              input$plot_var1),
            
            x = input$plot_var1,
            y = input$plot_var2,
            fill = input$plot_var1
          )
      },
      
      # Faceted bar chart
      "facet_bar_chart" = {
        req(input$plot_facet)
        
        plot_data <- data |>
          group_by(
            .data[[input$plot_var1]],
            .data[[input$plot_facet]]
          ) |>
          summarize(
            value = sum(.data[[input$plot_var2]], na.rm = TRUE),
            .groups = "drop"
          )
        
        ggplot(
          plot_data,
          aes(
            x = .data[[input$plot_var1]],
            y = value,
            fill = .data[[input$plot_var1]]
          )
        ) +
          geom_col() +
          facet_wrap(vars(.data[[input$plot_facet]])) +
          labs(
            title = paste(input$plot_var2, "by",
                          input$plot_var1, 
                          "faceted by",
                          input$plot_facet),
            
            x = input$plot_var1,
            y = paste("Total", input$plot_var2),
            fill = input$plot_var1
          )
      },
      
      # Heatmap
      "heatmap" = {
        req(input$plot_var3)
        
        validate(
          need(
            input$plot_var1 != input$plot_var2,
            "Choose two different categorical variables.")
        )
        
        plot_data <- data |>
          group_by(
            .data[[input$plot_var1]],
            .data[[input$plot_var2]]
          ) |>
          summarize(
            value = sum(.data[[input$plot_var3]], na.rm = TRUE),
            .groups = "drop"
          )
        
        ggplot(
          plot_data,
          aes(
            x = .data[[input$plot_var1]],
            y = .data[[input$plot_var2]],
            fill = value
          )
        ) +
          geom_tile(color = "white", linewidth = 0.2) +
          labs(
            title = paste(
              input$plot_var3, "by",
              input$plot_var1, "and", 
              input$plot_var2),
            
            x = input$plot_var1,
            y = input$plot_var2,
            fill = paste("Total", input$plot_var3)
          )
      }
    )
    
    # Apply a consistent appearance to all graphs
    plot <- plot +
      theme_bw() +
      theme(
        plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
    
    # flip charts with long category labels
    if (
      input$plot_type == "bar_chart" &&
      input$plot_var1 %in% c("State", "City", "Sub-Category")
    ) {
      plot <- plot + coord_flip()
    }
    
    # Format numeric axes to two decimal places
    if (input$plot_type == "scatterplot") {
      
      plot <- plot +
        scale_x_continuous(
          labels = scales::label_number(accuracy = 0.01)
        ) +
        scale_y_continuous(
          labels = scales::label_number(accuracy = 0.01)
        )
      
    } else if (input$plot_type %in% c(
      "bar_chart", "side_bar_chart", "boxplot", "facet_bar_chart"
    )) {
      
      plot <- plot +
        scale_y_continuous(
          labels = scales::label_number(accuracy = 0.01)
        )
      
    } else if (input$plot_type == "heatmap") {
      
      plot <- plot +
        scale_fill_continuous(
          labels = scales::label_number(accuracy = 0.01)
        )
    }
    
    plot
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
