library(shiny)
library(bslib)
library(ggplot2)
library(DT)

# Create sample girlsboys dataset since it's not readily available
girlsboys <- data.frame(
  year = 1940:2000,
  boys = round(rnorm(61, mean = 1800000, sd = 200000)),
  girls = round(rnorm(61, mean = 1700000, sd = 200000))
)

ui <- page_sidebar(
  theme = bs_theme(
    bootswatch = "minty",
    primary = "#4F6D7A",     # Steel blue
    "enable-gradients" = TRUE,
    "navbar-bg" = "#4F6D7A"
  ),
  
  title = div(
    style = "display: flex; justify-content: space-between; align-items: center; width: 100%;",
    "Girls and Boys Birth Analysis",
    span(
      style = "font-size: 1rem; color: white; font-weight: bold;",
      "Developed by ",
      a(
        href = "https://www.linkedin.com/in/arman-manjian-7503481a8",
        style = "color: white; font-weight: bold;",
        "Arman Manjian",
        target = "_blank"
      )
    )
  ),
  
  sidebar = sidebar(
    title = "Filter Controls",
    sliderInput(
      "year_range",
      "Select Year Range:",
      min = min(girlsboys$year),
      max = max(girlsboys$year),
      value = c(min(girlsboys$year), max(girlsboys$year))
    ),
    sliderInput(
      "bins",
      "Number of bins:",
      min = 5,
      max = 30,
      value = 15
    )
  ),
  
  layout_column_wrap(
    width = 1/3,
    height = 150,
    
    value_box(
      title = "Average Boys/Girls Ratio",
      value = textOutput("avg_ratio"),
      showcase = bsicons::bs_icon("gender-ambiguous"),
      theme_color = "primary",
      fill = TRUE
    ),
    value_box(
      title = "Total Births",
      value = textOutput("total_births"),
      showcase = bsicons::bs_icon("people"),
      theme_color = "secondary",
      fill = TRUE
    ),
    value_box(
      title = "Years Analyzed",
      value = textOutput("year_count"),
      showcase = bsicons::bs_icon("calendar"),
      theme_color = "info",
      fill = TRUE
    )
  ),
  
  layout_column_wrap(
    width = 1/2,
    card(
      full_screen = TRUE,
      height = "500px",
      card_header(
        class = "bg-primary text-white",
        "Ratio Distribution"
      ),
      plotOutput("ratioPlot")
    ),
    
    card(
      full_screen = TRUE,
      height = "500px",
      card_header(
        class = "bg-secondary text-white",
        "Births Over Time"
      ),
      plotOutput("timePlot")
    )
  ),
  
  layout_column_wrap(
    width = 1,
    card(
      full_screen = TRUE,
      height = "500px",
      card_header(
        class = "bg-info text-white",
        "Birth Data Table"
      ),
      DTOutput("table")
    )
  )
)

server <- function(input, output) {
  
  colors <- list(
    primary = "#4F6D7A",     # Steel blue
    secondary = "#DD6E42",   # Burnt orange
    light = "#E8DAB2",      # Warm beige
    accent1 = "#C0D6DF",    # Light blue-gray
    accent2 = "#EAEAEA",    # Light gray
    text = "#2C363F",       # Dark slate
    table_boys = "#4F6D7A",  # Darker blue for boys
    table_girls = "#DD6E42"  # Darker orange for girls
  )
  
  # Reactive filtered data
  filtered_data <- reactive({
    data <- girlsboys[girlsboys$year >= input$year_range[1] & 
                        girlsboys$year <= input$year_range[2], ]
    data$ratio <- data$boys / data$girls
    data$total <- data$boys + data$girls
    data$boys_pct <- (data$boys / data$total) * 100
    data$girls_pct <- (data$girls / data$total) * 100
    data
  })
  
  output$avg_ratio <- renderText({
    ratio <- mean(filtered_data()$ratio)
    round(ratio, 3)
  })
  
  output$total_births <- renderText({
    total <- sum(filtered_data()$boys + filtered_data()$girls)
    format(total, big.mark = ",")
  })
  
  output$year_count <- renderText({
    nrow(filtered_data())
  })
  
  output$ratioPlot <- renderPlot({
    data <- filtered_data()
    bin_width <- (max(data$ratio) - min(data$ratio)) / input$bins
    
    ggplot(data, aes(x = ratio)) +
      geom_histogram(bins = input$bins, fill = colors$accent1, color = "white", alpha = 0.8) +
      geom_density(aes(y = after_stat(count) * bin_width), 
                   color = colors$secondary, linewidth = 1.5) +
      geom_vline(xintercept = mean(data$ratio), linetype = "dashed", color = colors$primary, linewidth = 1.2) +
      theme_minimal() +
      labs(title = "Distribution of Boys/Girls Ratio",
           subtitle = "With density curve and mean line",
           x = "Ratio", y = "Count") +
      theme(
        plot.title = element_text(face = "bold", color = colors$text),
        plot.subtitle = element_text(size = 8),
        panel.grid = element_line(color = colors$accent2),
        axis.title = element_text(color = colors$text)
      )
  })
  
  output$timePlot <- renderPlot({
    data <- filtered_data()
    data_long <- reshape2::melt(data[, c("year", "boys", "girls")], 
                                id.vars = "year", 
                                measure.vars = c("boys", "girls"))
    
    ggplot(data_long, aes(x = year, y = value, color = variable)) +
      geom_line(linewidth = 1.5) +
      geom_point(size = 3, alpha = 0.7) +
      theme_minimal() +
      scale_color_manual(values = c("boys" = colors$primary, "girls" = colors$secondary)) +
      labs(title = "Births Over Time",
           subtitle = "Number of boys and girls births by year",
           x = "Year", y = "Number of Births",
           color = "Gender") +
      theme(
        plot.title = element_text(face = "bold", color = colors$text),
        plot.subtitle = element_text(size = 8),
        panel.grid = element_line(color = colors$accent2),
        axis.title = element_text(color = colors$text),
        legend.title = element_text(color = colors$text)
      )
  })
  
  output$table <- renderDT({
    data <- filtered_data()
    
    datatable(
      data.frame(
        Year = data$year,
        Boys = data$boys,
        Girls = data$girls,
        Total = data$total,
        Boys_Pct = round(data$boys_pct, 1),
        Girls_Pct = round(data$girls_pct, 1),
        Ratio = round(data$ratio, 3)
      ),
      options = list(
        pageLength = 10,
        dom = '<"top"Bf>rt<"bottom"lip>',
        buttons = c('copy', 'csv', 'excel'),
        order = list(list(0, 'desc')),
        scrollX = TRUE,
        columnDefs = list(
          list(className = 'dt-center', targets = "_all")
        )
      ),
      rownames = FALSE,
      class = 'cell-border stripe',
      extensions = 'Buttons',
      caption = htmltools::tags$caption(
        style = 'caption-side: top; text-align: center; color: black; font-size: 16px; font-weight: bold;',
        'Birth Statistics by Year'
      )
    ) %>%
      formatCurrency(
        columns = c('Boys', 'Girls', 'Total'),
        currency = "",
        digits = 0,
        mark = ","
      ) %>%
      formatStyle(
        'Boys_Pct',
        background = styleColorBar(c(0,100), colors$table_boys, angle = -90),
        backgroundSize = '98% 88%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center',
        color = 'Black'
      ) %>%
      formatStyle(
        'Girls_Pct',
        background = styleColorBar(c(0,100), colors$table_girls, angle = -90),
        backgroundSize = '98% 88%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center',
        color = 'Black'
      )
  })
}

shinyApp(ui = ui, server = server)
