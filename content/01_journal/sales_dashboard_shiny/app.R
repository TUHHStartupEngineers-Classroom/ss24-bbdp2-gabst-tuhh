#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(tidyverse)

# Interactive Visualizations
library(plotly)

# Spatial Data
library(raster)
library(sf)
library(dplyr)
library(lubridate)
library(shinyWidgets)

library(dplyr)
library(ggplot2)
library(plotly)
library(shinythemes)

library(shiny)


format_to_euro <- function(x, suffix = " €") {
  
  scales::dollar(x,
                 suffix       = suffix,
                 prefix       = "",
                 big.mark     = ".",
                 decimal.mark = ",")
}

euro_format <- function(scale        = 1,
                        prefix       = "",
                        suffix       = " €",
                        big.mark     = ".",
                        decimal.mark = ",") {
  
  scales::dollar_format(suffix       = suffix,
                        prefix       = prefix,
                        big.mark     = big.mark,
                        decimal.mark = decimal.mark,
                        scale        = scale)
  
}

# Bike data
bikes_tbl      <- readRDS("bikes_tbl.rds")
bikeshops_tbl  <- readRDS("bikeshops_tbl.rds")
orderlines_tbl <- readRDS("orderlines_tbl.rds")

bike_orderlines_tbl <- orderlines_tbl %>%
  left_join(bikes_tbl,     by = c("product_id" = "bike_id")) %>%
  left_join(bikeshops_tbl, by = c("customer_id" = "bikeshop_id")) %>%
  mutate(total_price = price_euro * quantity)


# German spatial data
# germany_sp <- getData('GADM', country='DE', level=1) broken link
germany_sp <- readRDS("gadm36_DEU_1_sp.rds")
germany_sf <- st_as_sf(germany_sp) %>% 
  # Add english names
  mutate(VARNAME_1 = ifelse(is.na(VARNAME_1), NAME_1, VARNAME_1)) 

plot_all_sales <- function(data_tbl) {
  
  plot_data_for_unit <- function(unit) {
    data_tbl %>%
      mutate(rounded_date = floor_date(order_date, unit = unit)) %>%
      group_by(rounded_date) %>%
      summarise(total_sales = sum(total_price),  .groups = 'drop') %>%
      ungroup() %>%
      mutate(label_text = str_glue("Sales: {format(total_sales, big.mark = ',')} \nDate: {rounded_date %>% format('%B %Y')}"))
  }
  
  p <- plot_ly() %>%
    add_lines(
      data = plot_data_for_unit("day"),
      x = ~rounded_date,
      y = ~total_sales,
      text = ~label_text,
      hoverinfo = 'text',
      line = list(shape = 'linear'),
    ) %>%
    layout(
      title = "Total Sales",
      xaxis = list(title = "", type = "date"),
      yaxis = list(title = "", tickformat = ".0f"),
      updatemenus = list(
        list(
          type = "buttons",
          x = 0.5,  # Center horizontally
          y = 1,  # Above the plot (top)
          buttons = list(
            list(label = "Quarter", method = "restyle", args = list("x", list(plot_data_for_unit("quarter")$rounded_date),
                                                                    "y", list(plot_data_for_unit("quarter")$total_sales),
                                                                    "text", list(plot_data_for_unit("quarter")$label_text),
                                                                    "name", "Quarter")),
            list(label = "Month", method = "restyle", args = list("x", list(plot_data_for_unit("month")$rounded_date),
                                                                  "y", list(plot_data_for_unit("month")$total_sales),
                                                                  "text", list(plot_data_for_unit("month")$label_text),
                                                                  "name", "Month")),
            list(label = "Week", method = "restyle", args = list("x", list(plot_data_for_unit("week")$rounded_date),
                                                                 "y", list(plot_data_for_unit("week")$total_sales),
                                                                 "text", list(plot_data_for_unit("week")$label_text),
                                                                 "name", "Week")),
            list(label = "Day", method = "restyle", args = list("x", list(plot_data_for_unit("day")$rounded_date),
                                                                "y", list(plot_data_for_unit("day")$total_sales),
                                                                "text", list(plot_data_for_unit("day")$label_text),
                                                                "name", "Day"),  active = TRUE)
          ),
          direction = "right",
          showactive = TRUE
        )
      )
    )
  
  # Return the plotly object
  return(p)
}

DateSeq <- seq.Date(from = as.Date("2015-01-10"), to = as.Date("2024-12-31"), by = "day")


# Define UI
ui <- fluidPage( 
  theme = shinytheme("cerulean"),
  
  titlePanel("Sales Visualization"), 
  sidebarLayout(
    
    sidebarPanel(
      dateRangeInput("dateRange", "Date Range", format = "yyyy-mm-dd",
                     start=min(DateSeq), end=max(DateSeq)),
      pickerInput("bike_type", "Select Bike Type:",
                  choices = c("All", unique(bike_orderlines_tbl$category_1)),
                  selected = "All",
                  multiple = TRUE),
      pickerInput("bike_family", "Select Bike Family:",
                  choices = c("All", unique(bike_orderlines_tbl$category_2)),
                  selected = "All",
                  multiple = TRUE),
      width=3
    )
    ,
    mainPanel(
      column(6, plotlyOutput("geo_plot_tbl", height="800px"), style = "height: 100vh;"),
      column(6, plotlyOutput("timeseries_plot", height="800px"), style = "height: 100vh;"),
      
      width=9)
  ),
  style = "height: 100vh;",
)

# Define server logic
server <- function(input, output, session) {
  
  # Reactive data filtering
  filtered_data <- reactive({
    bike_orderlines_tbl %>%
      filter(order_date >= input$dateRange[1] & order_date <= input$dateRange[2]) %>%
      filter(if ("All" %in% input$bike_type || length(input$bike_type) == 0) TRUE else category_1 %in% input$bike_type) %>%
      filter(if ("All" %in% input$bike_family || length(input$bike_family) == 0) TRUE else category_2 %in% input$bike_family)
  })
  
  # Render geoplot
  output$geo_plot_tbl <- renderPlotly({
    geo_plot_tbl <- filtered_data() %>%
      group_by(state) %>%
      summarise(total_revenue = sum(total_price)) %>%
      ungroup() %>%
      right_join(germany_sf, by = c("state" = "VARNAME_1")) %>%
      mutate(total_revenue = ifelse(is.na(total_revenue), 0, total_revenue)) %>%
      mutate(label_text = str_glue("State: {state}
                                   Revenue: {format_to_euro(total_revenue)}")) %>%
      st_as_sf()
    
    plot_ly(geo_plot_tbl, 
            split      = ~NAME_1, 
            color      = ~total_revenue,
            colors     = "Blues",
            stroke     = I("black"),
            hoverinfo  = 'text', 
            text       = ~label_text, 
            hoveron    = "fills", 
            showlegend = FALSE) 
  })
  
  output$timeseries_plot <- renderPlotly({plot_all_sales(filtered_data())})
}

# Run the application 
shinyApp(ui = ui, server = server, options = list(height = 1300))

