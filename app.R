#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(dplyr)
library(tidyverse)
library(ggplot2)

load("appAverages.RData")
load("appSeasons.RData")
load("appUniversal.RData")
load("appMode.RData")


# Define UI for the Shiny app
ui <- fluidPage(
  titlePanel("Bar Chart of Averages and Distributions"),
  tabsetPanel(
    # Tab for the 'appAverages.RData' dataset
    tabPanel(
      "Place",
      sidebarLayout(
        sidebarPanel(
          selectInput(
            inputId = "variable_averages",
            label = "Select a variable to plot:",
            choices = c(
              "Average Explicit" = "avg_explicit",
              "Average Danceability" = "avg_danceability",
              "Average Duration" = "avg_duration",
              "Average Loudness" = "avg_loudness",
              "Average Speechiness" = "avg_speechiness",
              "Average Acousticness" = "avg_acousticness",
              "Average Instrumentalness" = "avg_instrumentalness",
              "Average Liveness" = "avg_liveness",
              "Average Valence" = "avg_valence",
              "Average Tempo" = "avg_tempo"
            ),
            selected = "avg_explicit"
          )
        ),
        mainPanel(
          plotOutput(outputId = "barPlot_averages")
        )
      )
    ),
    # Tab for the 'appSeasons.RData' dataset
    tabPanel(
      "Season",
      sidebarLayout(
        sidebarPanel(
          selectInput(
            inputId = "variable_seasons",
            label = "Select a variable to plot:",
            choices = c(
              "Average Explicit" = "avg_explicit",
              "Average Danceability" = "avg_danceability",
              "Average Duration" = "avg_duration",
              "Average Loudness" = "avg_loudness",
              "Average Speechiness" = "avg_speechiness",
              "Average Acousticness" = "avg_acousticness",
              "Average Instrumentalness" = "avg_instrumentalness",
              "Average Liveness" = "avg_liveness",
              "Average Valence" = "avg_valence",
              "Average Tempo" = "avg_tempo"
            ),
            selected = "avg_explicit"
          )
        ),
        mainPanel(
          plotOutput(outputId = "barPlot_seasons")
        )
      )
    ),
    # Tab for the 'appMode.RData' dataset
    tabPanel(
      "Mode",
      sidebarLayout(
        sidebarPanel(
          selectInput(
            inputId = "variable_mode",
            label = "Select a variable to plot:",
            choices = c(
              "Average Explicit" = "avg_explicit",
              "Average Danceability" = "avg_danceability",
              "Average Duration" = "avg_duration",
              "Average Loudness" = "avg_loudness",
              "Average Speechiness" = "avg_speechiness",
              "Average Acousticness" = "avg_acousticness",
              "Average Instrumentalness" = "avg_instrumentalness",
              "Average Liveness" = "avg_liveness",
              "Average Valence" = "avg_valence",
              "Average Tempo" = "avg_tempo"
            ),
            selected = "avg_explicit"
          )
        ),
        mainPanel(
          plotOutput(outputId = "barPlot_mode")
        )
      )
    ),
    # Tab for the 'appUniversal.RData' dataset: Distributions
    tabPanel(
      "Distributions",
      sidebarLayout(
        sidebarPanel(
          selectInput(
            inputId = "variable_universal",
            label = "Select a variable to plot:",
            choices = c(
              "Popularity" = "popularity",
              "Duration (ms)" = "duration_ms",
              "Danceability" = "danceability",
              "Energy" = "energy",
              "Loudness" = "loudness",
              "Speechiness" = "speechiness",
              "Acousticness" = "acousticness",
              "Instrumentalness" = "instrumentalness",
              "Liveliness" = "liveliness",
              "Valence" = "valance",
              "Tempo" = "tempo",
              "Age" = "age"
            ),
            selected = "loudness"
          )
        ),
        mainPanel(
          plotOutput(outputId = "distPlot_universal")
        )
      )
    ),
    # Tab for the 'appUniversal.RData' dataset: Categorical Analysis
    tabPanel(
      "Categorical Analysis",
      sidebarLayout(
        sidebarPanel(
          selectInput(
            inputId = "variable_categorical",
            label = "Select a variable to plot:",
            choices = c(
              "Mode" = "mode",
              "Time Signature" = "time_signature",
              "Key" = "key",
              "Is Explicit" = "is_explicit"
            ),
            selected = "time_signature"
          )
        ),
        mainPanel(
          plotOutput(outputId = "categoricalPlot_universal")
        )
      )
    )
  )
)

# Define server logic for the Shiny app
server <- function(input, output) {
  # Render plot for the 'appAverages.RData' dataset
  output$barPlot_averages <- renderPlot({
    ggplot(all_averages, aes(x = place, y = .data[[input$variable_averages]])) +
      geom_bar(stat = "identity", fill = "gold") +
      labs(
        x = "Place",
        y = "Value",
        title = paste("Bar Chart of", input$variable_averages, "by Place")
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # Render plot for the 'appSeasons.RData' dataset
  output$barPlot_seasons <- renderPlot({
    ggplot(all_averages_seasons, aes(x = time, y = .data[[input$variable_seasons]])) +
      geom_bar(stat = "identity", fill = "gold") +
      labs(
        x = "Time",
        y = "Value",
        title = paste("Bar Chart of", input$variable_seasons, "by Time")
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # Render plot for the 'appMode.RData' dataset
  output$barPlot_mode <- renderPlot({
    ggplot(all_averages_mode, aes(x = place, y = .data[[input$variable_mode]], fill = mode)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(
        x = "Place",
        y = "Value",
        title = paste("Bar Chart of", input$variable_mode, "by Place")
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # Render plot for the 'appUniversal.RData' dataset: Distributions
  output$distPlot_universal <- renderPlot({
    ggplot(universal_top_spotify_songs, aes(x = .data[[input$variable_universal]], fill = region)) +
      geom_histogram( color = "black", alpha = 0.7) +
      labs(
        x = input$variable_universal,
        y = "Frequency",
        title = paste("Distribution of", input$variable_universal, "by Region")
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # Render plot for the 'appUniversal.RData' dataset: Categorical Analysis
  output$categoricalPlot_universal <- renderPlot({
    ggplot(universal_top_spotify_songs, aes(x = .data[[input$variable_categorical]], fill = region)) +
      geom_bar() +
      labs(
        x = input$variable_categorical,
        y = "Count",
        title = paste("Bar Chart of", input$variable_categorical, "by Region")
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)

