# Load necessary libraries
library(plyr)
library(dplyr)
library(devtools)
library(DT)
library(ggplot2)
library(ggrepel)
library(ggthemes)
library(gridExtra)
library(janitor)
library(plotly)
library(stringr)
library(tidyr)
library(tidyselect)
library(tidyverse)
library(data.table)
library(reactable)
library(lubridate)
library(readxl)
library(ggpubr)
library(shiny)
library(pbapply)

# Define UI
ui <- navbarPage(
  "College Football Game Predictions", theme = "flatly",
  
  tabPanel("College Football Game Predictions",
           sidebarLayout(
             sidebarPanel(
               selectInput("home_team", label = "Home Team",
                           choices = levels(as.factor(games$home_team))),
               
               selectInput("away_team", label = "Away Team",
                           choices = levels(as.factor(games$away_team))),
             ),
             
             mainPanel(
               fluidRow(DTOutput("metrics")),
               br(),
               fluidRow(plotOutput("Simulation_Results"))
             )
           )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Reactive for simulation
  simulation <- reactive({
    req(input$home_team, input$away_team)
    
    # Debugging: Print the inputs to verify correctness
    print(paste("Home Team:", input$home_team))
    print(paste("Away Team:", input$away_team))
    
    simulated_games <- simulate_matchup(
      input$home_team,
      input$away_team,
      params = params,
      team_ids = team_ids,
      n_simulations = 10000
    )
    
    # Debugging: Check the structure of the result
    print(str(simulated_games))
    
    simulated_games
  })
  
  # Reactive for metrics
  metrics <- reactive({
    req(simulation())
    metrics_result <- calculate_metrics(simulation())
    
    # Debugging: Print the metrics
    print(str(metrics_result))
    
    metrics_result
  })
  
  # Output for metrics
  output$metrics <- renderDT({
    datatable(metrics())
  })
  
  # Output for simulation results plot
  output$Simulation_Results <- renderPlot({
    req(simulation())
    
    score_data <- data.frame(
      score = c(simulation()$home_points, simulation()$away_points),
      team = rep(c(input$home_team, input$away_team), each = 10000)
    )
    
    # Debugging: Check the score data
    print(head(score_data))
    
    # Compute average score for each team
    average_score <- score_data %>%
      group_by(team) %>%
      summarise(avg_score = mean(score, na.rm = TRUE))
  
    # Create legend labels with only team names
    legend_labels <- average_score$team
    
    
    # Define full color mapping for teams
    full_color_mapping <- c(
      "Air Force" = "#004a7b", "Akron" = "#00285e", "Alabama" = "#9e1632",
      "Appalachian State" = "#000000", "Arizona" = "#0c234b", "Arizona State" = "#8e0c3a",
      "Arkansas" = "#a41f35", "Arkansas State" = "#e81018", "Army" = "#ce9c00",
      "Auburn" = "#002b5c", "Ball State" = "#DA0000", "Baylor" = "#154734",
      "Boise State" = "#0033a0", "Boston College" = "#8c2232", "Bowling Green" = "#2b1000",
      "Buffalo" = "#041A9B", "BYU" = "#003da5", "California" = "#031522",
      "Central Michigan" = "#6a0032", "Charlotte" = "#ffffff", "Cincinnati" = "#000000",
      "Clemson" = "#f56600", "Coastal Carolina" = "#007073", "Colorado" = "#000000",
      "Colorado State" = "#1e4d2b", "Connecticut" = "#0c2340", "Duke" = "#013088",
      "East Carolina" = "#4b1869", "Eastern Michigan" = "#00331b", "Florida" = "#0021a5",
      "Florida Atlantic" = "#00447c", "Florida International" = "#001538", "Florida State" = "#782F40",
      "Fresno State" = "#c41230", "Georgia" = "#ba0c2f", "Georgia Southern" = "#003775",
      "Georgia State" = "#1e539a", "Georgia Tech" = "#003057", "Hawai'i" = "#003420",
      "Houston" = "#c92a39", "Illinois" = "#ff5f05", "Indiana" = "#990000",
      "Iowa" = "#000000", "Iowa State" = "#822433", "Jacksonville State" = "#b50500",
      "James Madison" = "#450084", "Kansas" = "#0051ba", "Kansas State" = "#3c0969",
      "Kent State" = "#003976", "Kentucky" = "#0033a0", "Liberty" = "#071740",
      "Louisiana" = "#ce181e", "Louisiana Monroe" = "#231F20", "Louisiana Tech" = "#002d65",
      "Louisville" = "#c9001f", "LSU" = "#461d7c", "Marshall" = "#00ae42",
      "Maryland" = "#D5002B", "Memphis" = "#004991", "Miami (FL)" = "#005030",
      "Miami (OH)" = "#a4000c", "Michigan" = "#00274c", "Michigan State" = "#18453B",
      "Middle Tennessee" = "#006db6", "Minnesota" = "#5e0a2f", "Mississippi State" = "#5d1725",
      "Missouri" = "#f1b82d", "Navy" = "#00225b", "NC State" = "#cc0000",
      "Nebraska" = "#d00000", "Nevada" = "#002d62", "New Mexico" = "#000000",
      "New Mexico State" = "#891216", "North Carolina" = "#7bafd4", "Northern Illinois" = "#F1122C",
      "North Texas" = "#ffffff", "Northwestern" = "#582c83", "Notre Dame" = "#0c2340",
      "Ohio" = "#295A29", "Ohio State" = "#ce1141", "Oklahoma" = "#a32036",
      "Oklahoma State" = "#000000", "Old Dominion" = "#00507d", "Ole Miss" = "#13294b",
      "Oregon" = "#007030", "Oregon State" = "#231f20", "Penn State" = "#00265D",
      "Pittsburgh" = "#003263", "Purdue" = "#000000", "Rice" = "#d1d5d8",
      "Rutgers" = "#d21034", "Sam Houston State" = "#fe5000", "San Diego State" = "#c41230",
      "San José State" = "#005893", "SMU" = "#354ca1", "South Alabama" = "#003E7E",
      "South Carolina" = "#73000a", "Southern Mississippi" = "#FFAA3C", "South Florida" = "#004A36",
      "Stanford" = "#8c1515", "Syracuse" = "#ff6500", "TCU" = "#4d1979",
      "Temple" = "#A80532", "Tennessee" = "#ff8200", "Texas" = "#c15d26",
      "Texas A&M" = "#500000", "Texas State" = "#4e1719", "Texas Tech" = "#000000",
      "Toledo" = "#0a2240", "Troy" = "#AE0210", "Tulane" = "#006547",
      "Tulsa" = "#003595", "UAB" = "#003b28", "UCF" = "#000000",
      "UCLA" = "#2774ae", "UMass" = "#971B2F", "UNLV" = "#b10202",
      "USC" = "#9e2237", "Utah" = "#ea002a", "Utah State" = "#00263a",
      "UTEP" = "#ff8200", "UT San Antonio" = "#002A5C", "Vanderbilt" = "#000000",
      "Virginia" = "#232d4b", "Virginia Tech" = "#630031", "Wake Forest" = "#000000",
      "Washington" = "#33006f", "Washington State" = "#981e32", "Western Kentucky" = "#F32026",
      "Western Michigan" = "#532e1f", "West Virginia" = "#002855", "Wisconsin" = "#c4012f",
      "Wyoming" = "#492f24"
    )
    
    # Debugging: Print the selected colors
    selected_colors <- full_color_mapping[c(input$home_team, input$away_team)]
    print(selected_colors)
    
    # Plot the histogram with custom legend labels
    ggplot(score_data, aes(x = score, fill = team, color = team)) +
      geom_histogram(binwidth = 1, position = "identity", alpha = 0.5) +
      scale_fill_manual(
        values = selected_colors,
        labels = legend_labels
      ) +
      scale_color_manual(
        values = selected_colors,
        labels = legend_labels
      ) +
      xlab("Points Scored") +
      ylab("Frequency") +
      theme_minimal() +
      theme(legend.title = element_blank())
  })
}

# Run the application 
shinyApp(ui = ui, server = server)