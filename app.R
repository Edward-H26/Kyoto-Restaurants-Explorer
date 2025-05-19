ui = navbarPage(
  title = "Kyoto Restaurants Explorer",
  tabPanel(
    title = "Kyoto Restaurants Table",
    fluidPage(
      titlePanel("Kyoto Restaurants Table"),
      sidebarLayout(
        sidebarPanel(
          textInput("name", "Search by Name", placeholder = "Enter restaurant name..."),
          selectInput("station", "Station", 
                      choices = unique(kyoto$Station), selected = NULL, multiple = TRUE),
          selectInput("firstCategory", "Primary Category", 
                      choices = unique(kyoto$`First Category`), selected = NULL, multiple = TRUE),
          selectInput("secondCategory", "Secondary Category", 
                      choices = unique(kyoto$`Second Category`), selected = NULL, multiple = TRUE),
          selectInput("dinnerPrice", "Dinner Price (Range)", 
                      choices = dinner_price, 
                      selected = NULL, multiple = TRUE),
          selectInput("lunchPrice", "Lunch Price (Range)", 
                      choices = lunch_price, 
                      selected = NULL, multiple = TRUE),
          sliderInput("rating", "Total Rating (1 to 5, precise)", 
                      min = 1, max = 5, step = 0.1, value = c(1, 5)),
          actionButton("reset", "Reset Filters")
        ),
        mainPanel(
          DT::DTOutput("full_table")
        )
      )
    )
  ),
  tabPanel(
    title = "Kyoto Restaurants Visualizations",
    fluidPage(
      titlePanel("Kyoto Restaurants Visualizations"),
      sidebarLayout(
        sidebarPanel(
          p("Visualize restaurants by Stations and Categories.")
        ),
        mainPanel(
          fluidRow(
            column(12, plotOutput("station_plot")), 
            column(12, plotOutput("category_plot1")),
            column(12, plotOutput("category_plot2"))
          )
        )
      )
    )
  ),
  tabPanel(
    title = "About The Kyoto Restaurants Explorer",
    fluidPage(
      titlePanel("A Guide to Kyoto Restaurants Explorer"),
      p("This application provides an interactive exploration of restaurants in Kyoto."),
      p("Please check the following sections for more detailed information")
    ),
    includeMarkdown("about.Rmd")
  )
)
server = function(input, output, session) {
  filtered_categories = reactive({
    data = kyoto
    if (!is.null(input$firstCategory)) {
      data = data %>% 
        filter(`First Category` %in% input$firstCategory)
    }
    if (!is.null(input$secondCategory)) {
      data = data %>% 
        filter(`Second Category` %in% input$secondCategory)
    }
    if (!is.null(input$station)) {
      data = data %>% 
        filter(`Station` %in% input$station)
    }
    data
  })
  observe({
    data = filtered_categories()
    dinner_choices = unique(data$`Dinner Price`)
    dinner_choices = na.omit(dinner_choices)
    dinner = dinner_choices[order(convert_price(dinner_choices), na.last = NA)]
    lunch_choices = unique(data$`Lunch Price`)
    lunch_choices = na.omit(lunch_choices)
    lunch = lunch_choices[order(convert_price(lunch_choices), na.last = NA)]
    updateSelectInput(
      session,
      "dinnerPrice",
      choices = dinner,
      selected = input$dinnerPrice
    )
    updateSelectInput(
      session,
      "lunchPrice",
      choices = lunch,
      selected = input$lunchPrice
    )
  })
  filtered_data = reactive({
    data = filtered_categories()
    if (input$name != "") {
      data = data %>% 
        filter(str_detect(Name, regex(input$name, ignore_case = TRUE)))
    }
    if (!is.null(input$station)) {
      data = data %>% 
        filter(Station %in% input$station)
    }
    if (!is.null(input$dinnerPrice)) {
      data = data %>% 
        filter(`Dinner Price` %in% input$dinnerPrice)
    }
    if (!is.null(input$lunchPrice)) {
      data = data %>% 
        filter(`Lunch Price` %in% input$lunchPrice)
    }
    data = data %>%
      filter(`Total Rating` >= input$rating[1], `Total Rating` <= input$rating[2])
    data
  })
  output$full_table = DT::renderDT({
    filtered_data()
  }, options = list(
    pageLength = -1,
    scrollY = "400px",
    scrollX = TRUE,
    autoWidth = TRUE,
    fixedHeader = TRUE,
    dom = 't'
  ), class = "display nowrap compact")
  output$station_plot = renderPlot({
    data = filtered_data()
    ggplot(data, aes(x = reorder(Station, -table(Station)[Station]), fill = Station)) +
      geom_bar(color = "black", show.legend = FALSE) +
      geom_text(stat = "count", aes(label = after_stat(count)), vjust = -0.5, size = 4) +
      scale_fill_viridis_d(option = "plasma") +
      theme_light() +
      labs(
        title = "Number of Restaurants by Station",
        x = "Station",
        y = "Count of Restaurants"
      ) +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.margin = margin(10, 10, 10, 10)
      )
  })
  output$category_plot1 = renderPlot({
    data = filtered_data()
    ggplot(data, aes(x = reorder(`First Category`, -table(`First Category`)[`First Category`]), fill = `First Category`)) +
      geom_bar(color = "black", show.legend = FALSE) +
      geom_text(stat = "count", aes(label = after_stat(count)), vjust = -0.5, size = 4) +
      scale_fill_viridis_d(option = "plasma") +
      theme_minimal() +
      labs(
        title = "Number of Restaurants by Primary Category",
        x = "Primary Category",
        y = "Count of Restaurants"
      ) +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.margin = margin(10, 10, 10, 10)
      )
  })
  output$category_plot2 = renderPlot({
    data = filtered_data()
    ggplot(data, aes(x = reorder(`Second Category`, -table(`Second Category`)[`Second Category`]), fill = `Second Category`)) +
      geom_bar(color = "black", show.legend = FALSE) +
      geom_text(stat = "count", aes(label = after_stat(count)), vjust = -0.5, size = 4) +
      scale_fill_viridis_d(option = "plasma") +
      theme_minimal() +
      labs(
        title = "Number of Restaurants by Second Category",
        x = "Second Category",
        y = "Count of Restaurants"
      ) +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.margin = margin(10, 10, 10, 10)
      )
  })
  observeEvent(input$reset, {
    updateTextInput(session, "name", value = "")
    updateSelectInput(session, "station", choices = unique(kyoto$Station), selected = NULL)
    updateSelectInput(session, "firstCategory", choices = unique(kyoto$`First Category`), selected = NULL)
    updateSelectInput(session, "secondCategory", choices = unique(kyoto$`Second Category`), selected = NULL)
    updateSelectInput(session, "dinnerPrice", choices = sort(unique(kyoto$`Dinner Price`)), selected = NULL)
    updateSelectInput(session, "lunchPrice", choices = sort(unique(kyoto$`Lunch Price`)), selected = NULL)
    updateSliderInput(session, "rating", value = c(1, 5))
  })
}
shinyApp(ui, server)