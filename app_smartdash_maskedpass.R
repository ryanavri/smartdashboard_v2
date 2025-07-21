# Libraries----
library(shiny)
library(shinymanager)
library(bs4Dash)
library(dplyr)
library(lubridate)
library(kableExtra)
library(plotly)
library(leaflet)
library(mapview)
library(sf)
library(ggpmisc)

# Source of dataset
load("source/smart_patrol_data5.RData")

# Login UI ----
credentials_ui <- fluidPage(
  textInput("user", "Username"),
  passwordInput("password", "Password"),
  actionButton("login_btn", "Log in"),
  verbatimTextOutput("login_status")
)

# User Interface ----
ui <- dashboardPage(
  dashboardHeader(
    title = "SMART Patrol Dashboard",
    titleWidth = 300  # Adjust title width for a balanced look
  ),
  
  ## Sidebar ----
  dashboardSidebar(
    sidebarMenu(
      menuItem("Instructions", 
               tabName = "instructions",
               icon = icon("info-circle")
      ),
      menuItem("Overview",
               tabName = "overview",
               icon = icon("dashboard")
      ),
      menuItem("Patrol Effort",
               tabName = "patrol_effort", 
               icon = icon("walking")
      ),
      menuItem("Findings",
               icon = icon("binoculars"),
               menuSubItem("Human Activities", 
                           tabName = "human_activities",
                           icon = icon("users")
               ),
               menuSubItem("Wildlife", 
                           tabName = "wildlife", 
                           icon = icon("paw")
               )
      )
    )
  ),
  
  ## Body ----
  dashboardBody(
    tabItems(
      
      ## Instructions tab----
      tabItem(tabName = "instructions",
              h2("Instructions"),
              p("This dashboard provides an overview of SMART patrol data, including patrol efforts, findings, and maps. Use the navigation menu to explore different sections of the dashboard.
                  The video below supposed to be overview or instruction how to use the dashboard. will added later"),
              tags$iframe(src="https://www.youtube.com/embed/5_2gbNo92kc", width="100%", height="650px", frameborder="0", allowfullscreen=T)
      ),
      
      ## Overview tab----
      tabItem(tabName = "overview",
              h2("Overview"),
              p("Summary of SMART patrol data across Indonesia Programmes."),
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
                               selectInput("camquerytype", "Period", choices = c("Monthly", "Yearly", "Quarterly")),
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
                           value = textOutput("patrol_duration"),
                           subtitle = "Patrol Duration",
                           icon = icon("calendar"),
                           color = "success",
                           width = 4
                         ),
                         bs4ValueBox(
                           value = textOutput("patrol_length"),
                           subtitle = "Patrol Length",
                           icon = icon("route"),
                           color = "info",
                           width = 4
                         ),
                         bs4ValueBox(
                           value = textOutput("patrol_frequency"),
                           subtitle = "Patrol Frequency",
                           icon = icon("chart-line"),
                           color = "warning",
                           width = 4
                         )
                       )
                )
              )
      ),
      
      ## Findings - Human Activities tab----
      tabItem(tabName = "human_activities",
              h2("Human Activities"),
              p("Details about human activities findings during patrols."),
              fluidRow(
                column(2,
                       bs4Card(title = "Filters", width = 12, status = "primary", solidHeader = TRUE,
                               collapsible = TRUE, collapsed = FALSE,
                               selectInput("landscape_human", "Landscape", choices = unique(CAM$Landscape), multiple = TRUE),
                               selectInput("site_human", "Site", choices = unique(CAM$Station), multiple = TRUE),
                               selectInput("kategori_temuan", "Category", choices = unique(CAM$Kategori_temuan), multiple = TRUE),
                               dateRangeInput("dateRange_human", "Date", start = Sys.Date() - 365 , end = Sys.Date()),
                               actionButton("human_calculate", "Run Query")
                       )
                ),
                column(10,
                       fluidRow(
                         bs4Card(title = "Donut Chart", width = 12, status = "primary", solidHeader = TRUE,
                                 plotlyOutput("human_donut_chart")
                         )),
                       fluidRow(
                         bs4Card(title = "CPUE Chart", width = 12, status = "warning", solidHeader = TRUE,
                                 plotOutput("cpue_human_chart", height = "650px")
                         )),
                       fluidRow(
                         bs4Card(title = "Map", width = 12, status = "info", solidHeader = TRUE,
                                 leafletOutput("cam_map", height = "650px")
                         )),
                       fluidRow(
                         conditionalPanel(
                           condition = "output.authenticated == true",
                           bs4Card(title = "Downloadable Table", width = 12, status = "warning", solidHeader = TRUE,
                                   DT::DTOutput("human_table"),
                                   downloadButton("download_human_data", "Download Table")
                           )
                         ),
                         conditionalPanel(
                           condition = "output.authenticated == false",
                           bs4Card(title = "Login Required", width = 12, status = "danger", solidHeader = TRUE,
                                   h5("Please login to view the data table and download."),
                                   textInput("user", "Username"),
                                   passwordInput("password", "Password"),
                                   actionButton("login_btn", "Login")
                           )
                         )
                       )
                )
              )
      ),
      
      
      ## Findings - Wildlife tab----
      tabItem(tabName = "wildlife",
              h2("Wildlife Findings"),
              p("Details about wildlife-related findings during patrols."),
              fluidRow(
                column(2,
                       bs4Card(title = "Filters", width = 12, status = "primary", solidHeader = TRUE,
                               collapsible = TRUE, collapsed = FALSE,
                               selectInput("landscape_wildlife", "Landscape", choices = unique(CSL$Landscape), multiple = TRUE),
                               selectInput("site_wildlife", "Site", choices = unique(CSL$Station), multiple = TRUE),
                               selectInput("Jenis.satwa", "Species", choices = unique(CSL$Jenis.satwa), multiple = TRUE),
                               dateRangeInput("dateRange_wildlife", "Date", start = Sys.Date() - 365 , end = Sys.Date()),
                               actionButton("wildlife_calculate", "Run Query"),
                               tags$hr(),
                               "All data presented here only show species identified at the species level. The actual number of species is higher than what is shown."
                       )
                ),
                column(10,
                       fluidRow(
                         bs4ValueBox(
                           value = HTML("<span style='font-size: 2em; font-weight: bold;'>" %>% 
                                          paste0(textOutput("total_ordo"), "</span>")),
                           subtitle = "Ordo",
                           icon = icon("paw"),
                           color = "success",
                           width = 4
                         ),
                         bs4ValueBox(
                           value = HTML("<span style='font-size: 2em; font-weight: bold;'>" %>% 
                                          paste0(textOutput("total_family"), "</span>")),
                           subtitle = "Family",
                           icon = icon("paw"),
                           color = "success",
                           width = 4
                         ),
                         bs4ValueBox(
                           value = HTML("<span style='font-size: 2em; font-weight: bold;'>" %>% 
                                          paste0(textOutput("total_species"), "</span>")),
                           subtitle = "Species",
                           icon = icon("paw"),
                           color = "success",
                           width = 4
                         ),
                         bs4ValueBox(
                           value = HTML("<span style='font-size: 2em; font-weight: bold;'>" %>% 
                                          paste0(textOutput("total_redlist"), "</span>")),
                           subtitle = "IUCN redlist",
                           icon = icon("paw"),
                           color = "danger",
                           width = 4
                         ),
                         bs4ValueBox(
                           value = HTML("<span style='font-size: 2em; font-weight: bold;'>" %>% 
                                          paste0(textOutput("total_cites"), "</span>")),
                           subtitle = "CITES",
                           icon = icon("paw"),
                           color = "danger",
                           width = 4
                         ),
                         bs4ValueBox(
                           value = HTML("<span style='font-size: 2em; font-weight: bold;'>" %>% 
                                          paste0(textOutput("total_pp"), "</span>")),
                           subtitle = "PP106",
                           icon = icon("paw"),
                           color = "danger",
                           width = 4
                         )
                       ),
                       tags$hr(),
                       fluidRow(
                         solidHeader = TRUE,
                         bs4Card(title = "Donut Chart", width = 6, status = "primary", solidHeader = TRUE,
                                 plotlyOutput("iucn_pie")),
                         bs4Card(title = "Donut Chart", width = 6, status = "primary", solidHeader = TRUE,
                                 plotlyOutput("cites_pie")),
                       ),
                       fluidRow(
                         bs4Card(title = "CPUE Chart", width = 12, status = "warning", solidHeader = TRUE,
                                 plotOutput("cpue_wildlife_chart", height = "650px")
                         )),
                       fluidRow(
                         bs4Card(title = "Map", width = 12, status = "info", solidHeader = TRUE, maximizable = TRUE,
                                 leafletOutput("CSL_map", height = "650px")
                         )),
                       fluidRow(
                         conditionalPanel(
                           condition = "output.authenticated == true",
                           bs4Card(title = "Downloadable Table", width = 12, status = "warning", solidHeader = TRUE,
                                   DT::DTOutput("wildlife_table"),
                                   downloadButton("download_wildlife_data", "Download Table")
                           )
                         ),
                         conditionalPanel(
                           condition = "output.authenticated == false",
                           bs4Card(title = "Login Required", width = 12, status = "danger", solidHeader = TRUE,
                                   credentials_ui
                           )
                         )
                       )
                )
              )
      )
    )
  )
)


# Define the server----
server <- function(input, output, session) {
  
  # Authentication logic----
  # Reactive credentials store
  credentials <- reactiveValues(authenticated = FALSE)
  
  observeEvent(input$login_btn, {
    
    # Example: Randomized usernames and passwords
    user_pass <- list(
      "user_A7X9" = "pL8#xY!q29",
      "user_K3LT" = "zR@w92Lm!",
      "user_M0PQ" = "Qe$1bn!02",
      "user_T9VX" = "Xp#8Ga29%",
      "user_Z4JW" = "Mb@12kLp*"
    )
    
    
    # Check credentials
    if (!is.null(input$user) && !is.null(input$password) &&
        input$user %in% names(user_pass) &&
        input$password == user_pass[[input$user]]) {
      credentials$authenticated <- TRUE
      credentials$user <- input$user  # store the username
    } else {
      credentials$authenticated <- FALSE
    }
  })
  
  output$authenticated <- reactive({
    credentials$authenticated
  })
  outputOptions(output, "authenticated", suspendWhenHidden = FALSE)
  
  landscape_allowed <- reactive({
    user <- credentials$user
    
    if (user == "admin") {
      unique(CAM$Landscape)
    } else if (user == "ksl_user") {
      "Kerinci-Seblat"
    } else if (user == "riau_user") {
      "Riau"
    } else if (user == "aceh_user") {
      "Aceh"
    } else if (user == "kalbar_user") {
      "Kalbar"
    } else {
      character(0)
    }
  })
  
  
  # Overview Server----
  ## Function to calculate percentage change----
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
  
  ## Calculate overview metrics----
  overview_metrics <- reactive({
    yearly_metrics <- CRP %>%
      mutate(
        Year = year(lubridate::mdy(Patrol_Sta)),
        Patrol_Days = as.numeric(lubridate::mdy(Patrol_End) - lubridate::mdy(Patrol_Sta)),
        Distance_km = as.numeric(Jarak) / 1000
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
  
  ## Update value boxes----
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
  
  ## Overview chart----
  output$overview_chart <- renderPlotly({
    ctes <- CRP %>%
      mutate(
        Year = year(lubridate::mdy(Patrol_Sta)),
        Distance_km = as.numeric(Jarak) / 1000
      ) %>%
      group_by(Year, Landscape) %>%
      summarise(KM_Covered = sum(Distance_km, na.rm = TRUE)) %>%
      ggplot(aes(x = Year, y = KM_Covered, color = Landscape, group = Landscape)) +
      geom_line(size = 1) +
      geom_point(size = 2) +
      labs(
        title = "Annual Patrol Distance Covered by Programme Site",
        x = "Year",
        y = "Distance Covered (km)",
        color = "Programme Site"
      ) +
      theme_bw()
    
    ggplotly(ctes, tooltip = c("x", "y", "color"))
  })
  
  
  ## Data availability summary----
  output$data_availability <- DT::renderDT({
    DAVAIL %>%
      mutate(
        `Total Patrols` = format(`Total Patrols`, big.mark = ","),
        `First Data` = as.character(`Start Date`),
        `Last Data` = as.character(`End Date`),
        `Total Patrol Days` = format(`Patrol Days`, big.mark = ",")
      ) %>%
      select(Landscape, `Total Patrols`,`First Data`,
             `Last Data`,`Total Patrol Days`, PIC, Link)
  }, options = list(
    autoWidth = TRUE,
    dom = 't',
    searching = FALSE
  ))
  
  # Patrol Effort----
  ## Patrol Effort Reactive Data Filtering----
  
  observeEvent(input$landscape, {
    site_choices <- CRP %>%
      filter(Landscape %in% input$landscape) %>%
      pull(Station) %>%
      unique() %>%
      sort()
    
    updateSelectInput(session, "site", choices = site_choices)
  })
  
  
  filteredData <- eventReactive(input$camcalculate, {
    CRP_data <- CRP
    
    # Apply filters sequentially
    if (!is.null(input$landscape) && length(input$landscape) > 0) {
      CRP_data <- CRP_data %>% filter(Landscape %in% input$landscape)
    }
    
    if (!is.null(input$site) && length(input$site) > 0) {
      CRP_data <- CRP_data %>% filter(Station %in% input$site)
    }
    
    if (!is.null(input$dateRange) && length(input$dateRange) > 0) {
      CRP_data <- CRP_data %>%
        filter(as.Date(Patrol_Sta, format = "%b %d, %Y") >= input$dateRange[1] &
                 as.Date(Patrol_End, format = "%b %d, %Y") <= input$dateRange[2])
    }
    
    CRP_data %>%
      mutate(
        Patrol_Sta = as.Date(Patrol_Sta, format = "%b %d, %Y"),
        Distance_km = as.numeric(Jarak) / 1000,
        Period = case_when(
          input$camquerytype == "Monthly" ~ format(Patrol_Sta, "%Y-%m"),
          input$camquerytype == "Quarterly" ~ paste0(format(Patrol_Sta, "%Y"), "-Q", quarter(Patrol_Sta)),
          TRUE ~ as.character(year(Patrol_Sta))
        )
      )
  })
  
  
  ## Patrol effort chart----
  output$effort_chart <- renderPlotly({
    
    CRP_data <- filteredData()
    
    effort_plot <- CRP_data %>%
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
  
  ## Patrol effort value boxes----
  output$patrol_duration <- renderText({
    Pd_data <- filteredData()
    paste(format(sum(as.numeric(as.Date(Pd_data$Patrol_End, "%b %d, %Y") - as.Date(Pd_data$Patrol_Sta, "%b %d, %Y")), na.rm = TRUE), big.mark = ","), "Days")
  })
  
  output$patrol_length <- renderText({
    Pl_data <- filteredData()
    paste(format(round(sum(as.numeric(Pl_data$Jarak) / 1000, na.rm = TRUE), 2), big.mark = ","), "km")
  })
  
  output$patrol_frequency <- renderText({
    Pf_data <- filteredData()
    format(n_distinct(Pf_data$Patrol_ID), big.mark = ",")
  })
  
  # Human activity----
  
  observeEvent(input$landscape_human, {
    req(input$landscape_human)
    
    filtered_sites <- CAM %>%
      filter(Landscape %in% input$landscape_human) %>%
      pull(Station) %>%
      unique() %>%
      sort()
    
    updateSelectInput(session, "site_human", choices = filtered_sites, selected = character(0))
  })
  
  ##  Human Activities Reactive Data Filtering----
  filteredHumanData <- eventReactive(input$human_calculate, {
    CAM_data <- CAM
    
    # Filter by Landscape
    if (!is.null(input$landscape_human) && length(input$landscape_human) > 0) {
      CAM_data <- CAM_data %>% filter(Landscape %in% input$landscape_human)
    }
    
    # Filter by Site
    if (!is.null(input$site_human) && length(input$site_human) > 0) {
      CAM_data <- CAM_data %>% filter(Station %in% input$site_human)
    }
    
    # Filter by Kategori Temuan
    if (!is.null(input$kategori_temuan) && length(input$kategori_temuan) > 0) {
      CAM_data <- CAM_data %>% filter(Kategori_temuan %in% input$kategori_temuan)
    }
    
    # Filter by Date Range
    if (!is.null(input$dateRange_human) && length(input$dateRange_human) > 0) {
      CAM_data <- CAM_data %>%
        filter(as.Date(Patrol.Start.Date, format = "%b %d, %Y") >= input$dateRange_human[1] &
                 as.Date(Patrol.End.Date , format = "%b %d, %Y") <= input$dateRange_human[2])
    }
    
    CAM_data
  })
  
  
  ## Human Activities Donut Chart----
  output$human_donut_chart <- renderPlotly({
    CAM_data <- filteredHumanData()
    
    donut_data <- CAM_data %>%
      count(Kategori_temuan) %>%
      mutate(percentage = n / sum(n) * 100)
    
    plot_ly(
      donut_data,
      labels = ~Kategori_temuan,
      values = ~percentage,
      type = 'pie',
      hole = 0.5,
      hoverinfo = 'label+percent+value'
    ) %>%
      layout(
        title = "Human Activities Distribution",
        showlegend = TRUE
      )
  })
  
  ## cpue human chart----
  output$cpue_human_chart <- renderPlot({
    datCPUE <- filteredHumanData()
    eff <- datEff %>% as.data.frame()
    
    # Process data
    CPUE <- datCPUE %>%
      group_by(Patrol_ID) %>%
      summarise(n = n()) %>%
      dplyr::inner_join(eff %>% select(Patrol_ID, Patrol_Sta, effort), by = "Patrol_ID") %>%
      mutate(
        dens = n / (as.numeric(effort) / 1000),  # Convert effort to numeric
        patrol.date = as.Date(Patrol_Sta, format = "%b %d, %Y")
      ) 
    
    # Generate the plot
    ggplot(CPUE, aes(x = patrol.date, y = dens)) +
      geom_point(color = "black", alpha = 0.5, size = 1) +
      geom_smooth(method = "loess", color = "blue", fill = "lightblue", size = 1.2) +
      geom_smooth(method = "lm", color = "red", se = FALSE, linetype = "dashed", size = 1.2) +
      scale_y_log10(labels = scales::comma) +
      theme_bw(base_size = 15) +
      theme(
        panel.grid.major = element_line(color = "grey80"),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12)
      ) +
      labs(
        title = "Trend of CPUE Over Time",
        x = "Date",
        y = "Log10 Density (quantity per km/CPUE)"
      )+
      stat_poly_eq(
        aes(label = paste(..eq.label.., ..adj.rr.label.., sep = "~~~~")),
        formula = y ~ x,  # Regression formula
        parse = TRUE,
        label.x.npc = "right",  # Position on the plot
        label.y.npc = "top"  # Position on the plot
      )
  })
  
  
  
  ## Map----  
  output$cam_map <- renderLeaflet({
    
    cam_sps <- filteredHumanData()
    
    CAM_sf <- st_as_sf(cam_sps, coords = c("X", "Y"), crs = 4326)
    
    CAM_sf <- CAM_sf %>% 
      select(Patrol_ID, Station, Team, Tanggal, Kategori_temuan, Tindakan, Tipe.temuan)
    
    
    m <- mapview(CAM_sf, zcol = "Kategori_temuan",
                 layer.name = "Aktivitas Manusia",
                 map.types = c("OpenStreetMap", "Esri.WorldImagery")) 
    m@map
  })
  
  ## Human Activities Table----
  output$human_table <- DT::renderDT({
    req(credentials$authenticated)
    filteredHumanData() %>%
      filter(Landscape %in% landscape_allowed()) %>%
      select(Patrol_ID, Station, Team, Tanggal, Kategori_temuan, Tindakan, Tipe.temuan)
  })
  
  
  ## Human Activities Table Download----
  output$download_human_data <- downloadHandler(
    filename = function() {
      paste("human_activities_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      
      req(credentials$authenticated)  # Only allow if logged in
      
      filtered_data <- filteredHumanData() %>%
        filter(Landscape %in% landscape_allowed())  # Restrict by user
      
      write.csv(filtered_data, file, row.names = FALSE)
    }
  )
  
  # Wildlife----
  observeEvent(input$landscape_wildlife, {
    site_choices <- CSL %>%
      filter(Landscape %in% input$landscape_wildlife) %>%
      pull(Station) %>%
      unique() %>%
      sort()
    
    updateSelectInput(session, "site_wildlife", choices = site_choices)
  })
  
  ## Widllife Reactive Data Filtering----
  filteredwildlifeData <- eventReactive(input$wildlife_calculate, {
    CSL_data <- CSL
    
    # Apply filters sequentially
    if (!is.null(input$landscape_wildlife) && length(input$landscape_wildlife) > 0) {
      CSL_data <- CSL_data %>% filter(Landscape %in% input$landscape_wildlife)
    }
    
    if (!is.null(input$site_wildlife) && length(input$site_wildlife) > 0) {
      CSL_data <- CSL_data %>% filter(Station %in% input$site_wildlife)
    }
    
    if (!is.null(input$Jenis.satwa) && length(input$Jenis.satwa) > 0) {
      CSL_data <- CSL_data %>% filter(Jenis.satwa %in% input$Jenis.satwa)
    }
    
    if (!is.null(input$dateRange_wildlife) && length(input$dateRange_wildlife) > 0) {
      CSL_data <- CSL_data %>%
        filter(as.Date(Patrol.Start.Date, format = "%b %d, %Y") >= input$dateRange_wildlife[1] &
                 as.Date(Patrol.End.Date, format = "%b %d, %Y") <= input$dateRange_wildlife[2])
    }
    
    CSL_data
  })
  
  
  ## Box Value Output----
  output$total_ordo <- renderText({
    
    CSL_dat <- filteredwildlifeData()
    value <- CSL_dat %>%
      inner_join(taxon, by = "Scientific.Name") %>%
      distinct(Order) %>%
      nrow()
    
    paste(format(value, big.mark = ",")
    )
  })
  
  output$total_family <- renderText({
    
    CSL_dat <- filteredwildlifeData()
    value <- CSL_dat %>%
      inner_join(taxon, by = "Scientific.Name") %>%
      distinct(Family) %>%
      nrow()
    
    paste(format(value, big.mark = ",")
    )
  })
  
  output$total_species <- renderText({
    
    CSL_dat <- filteredwildlifeData()
    value <- CSL_dat %>%
      inner_join(taxon, by = "Scientific.Name") %>%
      distinct(Scientific.Name) %>%
      nrow()
    
    paste(format(value, big.mark = ",")
    )
  })
  
  output$total_redlist <- renderText({
    
    CSL_dat <- filteredwildlifeData()
    
    value <- CSL_dat %>%
      inner_join(taxon, by = "Scientific.Name") %>%
      group_by(Class, Scientific.Name, Status ) %>%
      summarise(count = n()) %>%
      group_by(Status ) %>%
      mutate(iu_count = 1) %>%
      group_by(Status ) %>%
      filter(Status  %in% c("CR", "VU", "EN")) %>%
      nrow () 
    
    paste(format(value, big.mark = ",")
    )
  })
  
  output$total_cites <- renderText({
    
    CSL_dat <- filteredwildlifeData()
    
    value <- CSL_dat %>%
      inner_join(taxon, by = "Scientific.Name") %>%
      group_by(Class, Scientific.Name, Appendix) %>%
      summarise(count = n()) %>%
      group_by(Appendix) %>%
      mutate(ci_count = 1) %>%
      group_by(Appendix) %>%
      filter(Appendix %in% c("I", "II")) %>%
      nrow () 
    
    paste(format(value, big.mark = ",")
    )
  })
  
  output$total_pp <- renderText({
    
    CSL_dat <- filteredwildlifeData()
    
    value <- CSL_dat %>%
      inner_join(taxon, by = "Scientific.Name") %>%
      group_by(Class, Scientific.Name, Protected ) %>%
      summarise(count = n()) %>%
      group_by(Protected ) %>%
      mutate(Protected_count = 1) %>%
      group_by(Protected ) %>%
      filter(Protected  == "Y") %>%
      nrow () 
    
    paste(format(value, big.mark = ",")
    )
  })
  
  output$iucn_pie <- renderPlotly({
    
    CSL_dat <- filteredwildlifeData()
    filtered_iucn_pie <- CSL_dat %>% inner_join(taxon, by="Scientific.Name") %>%
      group_by(Scientific.Name, Status ) %>%
      summarise(count = n()) %>%
      filter(Status  %in% c("CR", "EN", "VU")) %>%
      group_by(Status ) %>%
      mutate(iu_count = 1) %>%
      group_by(Status ) %>%
      summarise(total_iu_count = sum(iu_count, na.rm = TRUE))
    
    plot_ly(filtered_iucn_pie, labels = ~Status , values = ~total_iu_count, type = 'pie', 
            text= ~total_iu_count,
            textinfo='label+text',
            hole=0.6) %>%
      layout(
        title = 'IUCN Red List Proportion',
        xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
        yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)
      ) 
    
  })
  
  output$cites_pie <- renderPlotly({
    
    CSL_dat <- filteredwildlifeData()
    filtered_cites_pie <- CSL_dat %>% inner_join(taxon, by="Scientific.Name") %>%
      group_by(Scientific.Name, Appendix) %>%
      summarise(count = n()) %>%
      filter(Appendix %in% c("I", "II")) %>%
      group_by(Appendix) %>%
      mutate(ci_count = 1) %>%
      group_by(Appendix) %>%
      summarise(total_ci_count = sum(ci_count, na.rm = TRUE))
    
    plot_ly(filtered_cites_pie, labels = ~Appendix, values = ~total_ci_count, type = 'pie', 
            text= ~total_ci_count,
            textinfo='label+text',
            hole=0.6) %>%
      layout(
        title = 'CITES Proportion',
        xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
        yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)
      ) 
    
  })
  
  ## cpue wildlife chart----
  output$cpue_wildlife_chart <- renderPlot({
    
    CSLdat <- filteredwildlifeData()
    
    CSLdata <- CSLdat %>%
      group_by(Scientific.Name, Patrol_ID) %>%
      summarise(n = n(), .groups = "drop") %>%
      dplyr::inner_join(datEff %>% select(Patrol_ID, Patrol_Sta, effort), by = "Patrol_ID") %>%
      mutate(
        dens = n / (as.numeric(effort) / 1000),  # Convert effort to numeric
        patrol.date = as.Date(Patrol_Sta, format = "%b %d, %Y")
      ) %>%
      group_by(Scientific.Name) %>%
      summarise(
        mean_dens = mean(dens, na.rm = TRUE),
        se_dens = sd(dens, na.rm = TRUE) / sqrt(n()),
        .groups = "drop"
      ) %>%
      arrange(desc(mean_dens)) %>%
      slice_head(n = 5)  # Select top 10 species by density
    
    ggplot(CSLdata, aes(x = reorder(Scientific.Name, mean_dens), y = mean_dens)) +
      geom_bar(stat = "identity", fill = "blue", alpha = 0.7) +
      geom_errorbar(aes(ymin = mean_dens - se_dens, ymax = mean_dens + se_dens), width = 0.2, color = "black") +
      geom_text(aes(label = round(mean_dens, 2)), hjust = -0.1, vjust = 2, size = 4) +  # Add mean density as text
      coord_flip() +
      labs(
        title = "Top 5 Species by CPUE (with SE)",
        x = "Scientific Name",
        y = "CPUE (Sighting per km)"
      ) +
      theme_bw() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text.y = element_text(size = 10, face = "italic"),  # Make y-axis (species names) italic
        plot.title = element_text(size = 16, face = "bold")
      )
    
  })
  
  ## Wildlife Table----
  output$wildlife_table <- DT::renderDT({
    req(credentials$authenticated)
    
    filteredwildlifeData() %>%
      filter(Landscape %in% landscape_allowed()) %>%
      distinct(Scientific.Name, .keep_all = TRUE) %>%  # Keep only the first row for each Scientific.Name
      inner_join(taxon, by = "Scientific.Name") %>%
      select(Class, Order, Family, Scientific.Name, Status, Trend, Appendix, Protected, Endemic, Migratory) %>%
      arrange(Class, Order, Family, Scientific.Name) %>%
      distinct(Scientific.Name, .keep_all = TRUE) 
  })
  
  ## Map----  
  output$CSL_map <- renderLeaflet({
    
    csl_sps <- filteredwildlifeData()
    
    CSL_sf <- st_as_sf(csl_sps, coords = c("X", "Y"), crs = 4326)
    
    CSL_sf <- CSL_sf %>%
      select(Patrol_ID, Station, Tanggal, Jenis.satwa ,Kategori_temuan, Tipe.temuan)
    
    m <- mapview(CSL_sf, zcol = "Jenis.satwa",
                 layer.name = "Perjumpaan Satwa",
                 map.types = c("OpenStreetMap", "Esri.WorldImagery")) 
    m@map
  })  
  
  ## Wildlife Table Download----
  output$download_wildlife_data <- downloadHandler(
    filename = function() {
      paste("wildlife_findings_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      req(credentials$authenticated)
      filtered_data <- filteredwildlifeData() %>%
        filter(Landscape %in% landscape_allowed())
      write.csv(filtered_data, file, row.names = FALSE)
    }
  )
  
}

# Run the app----
shinyApp(ui, server)