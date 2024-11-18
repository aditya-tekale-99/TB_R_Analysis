library(shiny)
library(ggplot2)
library(dplyr)
library(leaflet)
library(plotly)
library(sf)
library(forecast)
library(rnaturalearth) # Make sure to install this if not already

# Load the dataset
tb_data <- read.csv("TB_Burden_Country.csv")

# UI Section
ui <- fluidPage(
  titlePanel("Tuberculosis Data Analysis"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("country", "Select Country:", 
                  choices = unique(tb_data$Country.or.territory.name), 
                  selected = "Afghanistan", multiple = TRUE),
      selectInput("year", "Select Year:", 
                  choices = unique(tb_data$Year), 
                  selected = 2000),
      actionButton("predict_btn", "Generate Prediction")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Time Series Plot", plotlyOutput("timeSeriesPlot")),
        tabPanel("Geospatial Plot", leafletOutput("geoPlot")),
        tabPanel("Bar Plot", plotlyOutput("barPlot")),
        tabPanel("Prediction Plot", plotlyOutput("predictPlot"))
      )
    )
  )
)

# Server Section
server <- function(input, output) {
  
  # Filtered data based on country selection
  filtered_data <- reactive({
    tb_data %>%
      filter(Country.or.territory.name %in% input$country)
  })
  
  # Time Series Plot
  output$timeSeriesPlot <- renderPlotly({
    time_series_data <- filtered_data()
    
    p <- ggplot(time_series_data, aes(x = Year, y = Estimated.prevalence.of.TB..all.forms..per.100.000.population, 
                                      color = Country.or.territory.name)) +
      geom_line() + geom_point() +
      labs(title = "TB Prevalence Over Time", x = "Year", y = "Prevalence per 100,000") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # Geospatial Plot
  output$geoPlot <- renderLeaflet({
    geo_data <- tb_data %>%
      filter(Year == input$year)
    
    world <- ne_countries(scale = "medium", returnclass = "sf") # use `rnaturalearth` package for country boundaries
    
    # Merge geo data with world map
    geo_data <- merge(world, geo_data, by.x = "iso_a3", by.y = "ISO.3.character.country.territory.code", all.x = TRUE)
    
    leaflet(data = geo_data) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(fillColor = ~colorBin("YlOrRd", geo_data$Estimated.incidence..all.forms..per.100.000.population)(Estimated.incidence..all.forms..per.100.000.population),
                  weight = 1,
                  opacity = 1,
                  color = "white",
                  dashArray = "3",
                  fillOpacity = 0.7,
                  highlightOptions = highlightOptions(weight = 2,
                                                      color = "#666",
                                                      dashArray = "",
                                                      fillOpacity = 0.7,
                                                      bringToFront = TRUE),
                  label = ~paste0(Country.or.territory.name, ": ", 
                                  Estimated.incidence..all.forms..per.100.000.population)) %>%
      addLegend(pal = colorBin("YlOrRd", geo_data$Estimated.incidence..all.forms..per.100.000.population),
                values = ~Estimated.incidence..all.forms..per.100.000.population, title = "TB Incidence")
  })
  
  # Bar Plot
  output$barPlot <- renderPlotly({
    bar_data <- tb_data %>%
      filter(Year == input$year) %>%
      group_by(Country.or.territory.name) %>%
      summarise(TB_Prevalence = mean(Estimated.prevalence.of.TB..all.forms..per.100.000.population, na.rm = TRUE)) %>%
      arrange(desc(TB_Prevalence)) %>%
      slice(1:10) # Top 10 countries
    
    p <- ggplot(bar_data, aes(x = reorder(Country.or.territory.name, -TB_Prevalence), y = TB_Prevalence)) +
      geom_bar(stat = "identity", fill = "steelblue") +
      coord_flip() +
      labs(title = "Top 10 Countries with Highest TB Prevalence", x = "Country", y = "TB Prevalence per 100,000") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # Prediction Plot
  output$predictPlot <- renderPlotly({
    req(input$predict_btn)
    
    predict_data <- filtered_data() %>%
      filter(Country.or.territory.name == input$country[1]) # Predict for the first selected country
    
    # Simple linear model for TB Incidence
    fit <- lm(Estimated.incidence..all.forms..per.100.000.population ~ Year, data = predict_data)
    future_years <- data.frame(Year = seq(max(predict_data$Year) + 1, max(predict_data$Year) + 10))
    future_preds <- predict(fit, newdata = future_years)
    
    prediction_data <- data.frame(Year = c(predict_data$Year, future_years$Year),
                                  TB_Incidence = c(predict_data$Estimated.incidence..all.forms..per.100.000.population, future_preds))
    
    p <- ggplot(prediction_data, aes(x = Year, y = TB_Incidence)) +
      geom_line(color = "darkred") +
      geom_point() +
      labs(title = "Forecasted TB Incidence for Selected Country", x = "Year", y = "TB Incidence per 100,000") +
      theme_minimal()
    
    ggplotly(p)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
