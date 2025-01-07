# Libraries
library(shiny)
library(bs4Dash)
library(dplyr)
library(lubridate)
library(kableExtra)
library(plotly)

# Source of dataset
load("source/smart_patrol_data.RData")

# Define the UI----
ui <- dashboardPage(
  dashboardHeader(
    title = "SMART Patrol Dashboard",
    controlbarIcon = icon("sliders")
  ),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Instructions", tabName = "instructions", icon = icon("info-circle")),
      menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
      menuItem("Patrol Effort", tabName = "patrol_effort", icon = icon("walking")),
      menuItem("Findings", icon = icon("binoculars"),
               menuSubItem("Human Activities", tabName = "human_activities", icon = icon("users")),
               menuSubItem("Wildlife", tabName = "wildlife", icon = icon("paw"))
      )
    )
  ),
  
  dashboardBody(
    bs4DashBody(
      tabItems(
        ## Instructions tab----
        tabItem(tabName = "instructions",
                h2("Instructions"),
                p("This dashboard provides an overview of SMART patrol data, including patrol efforts, findings, and maps. Use the navigation menu to explore different sections of the dashboard."),
                tags$iframe(src="https://www.youtube.com/embed/vkfZ-lZ41w0", width="100%", height="400px", frameborder="0", allowfullscreen=T)
        ),
        
        ## Overview tab----
        tabItem(tabName = "overview",
                h2("Overview"),
                p("Summary of SMART patrol data."),
                fluidRow(
                  bs4ValueBox(
                    value = textOutput("total_patrols"),
                    subtitle = "Total Patrols",
                    icon = icon("chart-line"),
                    color = "success",
                    width = 4
                  ),
                  bs4ValueBox(
                    value = textOutput("total_days"),
                    subtitle = "Total Patrol Days",
                    icon = icon("calendar"),
                    color = "info",
                    width = 4
                  ),
                  bs4ValueBox(
                    value = textOutput("total_distance"),
                    subtitle = "Total Distance",
                    icon = icon("route"),
                    color = "primary",
                    width = 4
                  )
                ),
                fluidRow(
                  bs4Card(
                    title = "Overview Chart",
                    width = 12,
                    status = "primary",
                    solidHeader = TRUE,
                    plotlyOutput("overview_chart")
                  )
                ),
                fluidRow(
                  bs4Card(
                    title = "Data Availability",
                    width = 12,
                    status = "info",
                    solidHeader = TRUE,
                    DT::DTOutput("data_availability")
                  )
                )
        ),
        
        ## Patrol Effort tab----
        tabItem(tabName = "patrol_effort",
                h2("Patrol Effort"),
                p("Details about patrol efforts."),
                fluidRow(
                  column(3,
                         bs4Card(title = "Filters", width = 12, status = "primary", solidHeader = TRUE,
                                 collapsible = TRUE, collapsed = FALSE,
                                 selectInput("landscape", "Landscape", choices = unique(CAM$Landscape), multiple = TRUE),
                                 selectInput("site", "Site", choices = unique(CAM$Station), multiple = TRUE),
                                 dateRangeInput("dateRange", "Date", start = Sys.Date() - 365 , end = Sys.Date()),
                                 selectInput("camquerytype", "Periode", choices = c("Monthly", "Yearly", "Quarterly")),
                                 actionButton("camcalculate", "Run Query")
                         )
                  ),
                  column(9,
                         fluidRow(
                           bs4Card(title = "Effort Chart", width = 12, status = "primary", solidHeader = TRUE,
                                   collapsible = TRUE, collapsed = FALSE,
                                   plotlyOutput("effort_chart")
                           )
                         ),
                         fluidRow(
                           bs4ValueBox(
                             value = "120 Days",
                             subtitle = "Patrol Duration",
                             icon = icon("calendar"),
                             color = "success",
                             width = 6
                           ),
                           bs4ValueBox(
                             value = "50 km",
                             subtitle = "Patrol Length",
                             icon = icon("route"),
                             color = "info",
                             width = 6
                           )
                         ),
                         fluidRow(
                           bs4ValueBox(
                             value = "10 Patrols",
                             subtitle = "Patrol Frequency",
                             icon = icon("chart-line"),
                             color = "warning",
                             width = 6
                           ),
                           bs4ValueBox(
                             value = "45 People",
                             subtitle = "People Involved",
                             icon = icon("user"),
                             color = "primary",
                             width = 6
                           )
                         )
                  )
                )
        ),
        
        ## Findings - Human Activities tab----
        tabItem(tabName = "human_activities",
                h2("Human Activities"),
                p("Details about human-related findings during patrols."),
                fluidRow(
                  bs4Card(title = "Human Activities by Category", width = 6, status = "primary", solidHeader = TRUE,
                          "Placeholder for human activities charts"),
                  bs4Card(title = "Human Activities Over Time", width = 6, status = "warning", solidHeader = TRUE,
                          "Placeholder for human activities trends")
                )
        ),
        
        ## Findings - Wildlife tab----
        tabItem(tabName = "wildlife",
                h2("Wildlife"),
                p("Details about wildlife-related findings during patrols."),
                fluidRow(
                  bs4Card(title = "Wildlife by Category", width = 6, status = "primary", solidHeader = TRUE,
                          "Placeholder for wildlife charts"),
                  bs4Card(title = "Wildlife Over Time", width = 6, status = "warning", solidHeader = TRUE,
                          "Placeholder for wildlife trends")
                )
        )
      )
    )
  )
)

# Define the server----
server <- function(input, output, session) {
  
  # Function to calculate percentage change
  percentage_change <- function(latest, previous) {
    if (is.na(previous) || previous == 0) {
      return("N/A")
    }
    change <- round(((latest - previous) / previous) * 100, 2)
    if (change > 0) {
      return(paste0("+", change, "%"))
    } else {
      return(paste0(change, "%"))
    }
  }
  
  # Calculate overview metrics
  overview_metrics <- reactive({
    yearly_metrics <- CRP %>%
      mutate(
        Year = year(lubridate::mdy(Patrol_Sta)),
        Patrol_Days = as.numeric(lubridate::mdy(Patrol_End) - lubridate::mdy(Patrol_Sta)),
        Distance_km = as.numeric(sub("[m]", "", Jarak)) / 1000
      ) %>%
      group_by(Year) %>%
      summarise(
        Total_Patrols = n(),
        Total_Days = sum(Patrol_Days, na.rm = TRUE),
        Total_Distance = sum(Distance_km, na.rm = TRUE)
      )
    
    latest_year <- max(yearly_metrics$Year, na.rm = TRUE)
    previous_year <- latest_year - 1
    
    latest_data <- yearly_metrics %>% filter(Year == latest_year)
    previous_data <- yearly_metrics %>% filter(Year == previous_year)
    
    list(
      Total_Patrols = sum(yearly_metrics$Total_Patrols, na.rm = TRUE),
      Total_Days = sum(yearly_metrics$Total_Days, na.rm = TRUE),
      Total_Distance = sum(yearly_metrics$Total_Distance, na.rm = TRUE),
      Patrols_Change = percentage_change(latest_data$Total_Patrols, previous_data$Total_Patrols),
      Days_Change = percentage_change(latest_data$Total_Days, previous_data$Total_Days),
      Distance_Change = percentage_change(latest_data$Total_Distance, previous_data$Total_Distance)
    )
  })
  
  # Update value boxes
  output$total_patrols <- renderText({
    paste(
      format(overview_metrics()$Total_Patrols, big.mark = ","),
      sprintf("(%s)", overview_metrics()$Patrols_Change)
    )
  })
  
  output$total_days <- renderText({
    paste(
      format(overview_metrics()$Total_Days, big.mark = ","),
      sprintf("(%s)", overview_metrics()$Days_Change),
      "Days"
    )
  })
  
  output$total_distance <- renderText({
    paste(
      format(round(overview_metrics()$Total_Distance, 2), big.mark = ","),
      sprintf("(%s)", overview_metrics()$Distance_Change),
      "km"
    )
  })
  
  
  
  # overview chart
  output$overview_chart <- renderPlotly({
    CRP %>%
      mutate(Year = year(lubridate::mdy(Patrol_Sta)), Distance_km = as.numeric(sub("[m]", "", Jarak)) / 1000) %>%
      group_by(Year) %>%
      summarise(KM_Covered = sum(Distance_km, na.rm = TRUE)) %>%
      plot_ly(x = ~Year, y = ~KM_Covered, type = 'scatter', mode = 'lines+markers',
              line = list(color = 'blue'),
              marker = list(size = 8)) %>%
      layout(
        title = "Annual Patrol Distance Covered",
        xaxis = list(title = "Year"),
        yaxis = list(title = "Distance Covered (km)")
      )
  })
  
  # Data availability summary
  output$data_availability <- DT::renderDT({
    DAVAIL %>%
      mutate(
        `Total Patrols` = format(`Total Patrols`, big.mark = ","),
        `Start Date` = as.character(`Start Date`),
        `End Date` = as.character(`End Date`),
        `Total Patrol Days` = format(`Total Patrol Days`, big.mark = ",")
      )
  }, options = list(
    autoWidth = TRUE,
    dom = 't',
    searching = FALSE
  ))
  
  
  # Reactive data filtering
  filteredData <- eventReactive(input$camcalculate, {
    CAM_data <- CRP
    
    if (!is.null(input$landscape) && length(input$landscape) > 0) {
      CAM_data <- CAM_data %>% filter(Landscape %in% input$landscape)
    }
    
    if (!is.null(input$site) && length(input$site) > 0) {
      CAM_data <- CAM_data %>% filter(Station %in% input$site)
    }
    
    if (!is.null(input$dateRange) && length(input$dateRange) > 0) {
      CAM_data <- CAM_data %>%
        filter(as.Date(Patrol_Sta, format = "%b %d, %Y") >= input$dateRange[1] &
                 as.Date(Patrol_End, format = "%b %d, %Y") <= input$dateRange[2])
    }
    
    CAM_data %>%
      mutate(
        Patrol_Sta = as.Date(Patrol_Sta, format = "%b %d, %Y"),
        Distance_km = as.numeric(sub("[m]", "", Jarak)) / 1000,
        Period = case_when(
          input$camquerytype == "Monthly" ~ format(Patrol_Sta, "%Y-%m"),
          input$camquerytype == "Quarterly" ~ paste0(format(Patrol_Sta, "%Y"), "-Q", quarter(Patrol_Sta)),
          TRUE ~ as.character(year(Patrol_Sta))
        )
      )
  })
  
  # Patrol effort chart
  output$effort_chart <- renderPlotly({
    CAM_data <- filteredData()
    
    effort_plot <- CAM_data %>%
      group_by(Period) %>%
      summarise(Total_Distance = sum(Distance_km, na.rm = TRUE)) %>%
      ggplot(aes(x = Period, y = Total_Distance, group = 1)) +
      geom_smooth(method = "loess", se = TRUE, color = "green", size = 0.8) +
      labs(
        title = paste("Patrol Effort Over Time (", input$camquerytype, ")", sep = ""),
        x = input$camquerytype,
        y = "Total Distance (km)"
      ) +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(effort_plot)
  })
  
}

# Run the app----
shinyApp(ui, server)
