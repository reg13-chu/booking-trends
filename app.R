# =============================
#   Load Libraries
# =============================
library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)
library(plotly)

# =============================
#   Load Cleaned Data & Summaries
# =============================
source("booking_trends.R")

# =============================
#   Define custom color palette
# =============================
chart_palette <- c(
  "#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd",
  "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22", "#17becf"
)

# =============================
#   UI
# =============================
ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "📊 Booking Trends Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Event Requests", icon = icon("calendar")),
      menuItem("Partner Services", icon = icon("handshake")),
      menuItem("Weather Forecast", icon = icon("cloud")),
      menuItem("Budget Estimator", icon = icon("calculator")),
      menuItem("Booking Trends", tabName = "trends", icon = icon("chart-bar"), selected = TRUE),
      menuItem("Payment Records", icon = icon("credit-card")),
      menuItem("Feedback & Reviews", icon = icon("star")),
      menuItem("Account Settings", icon = icon("cog")),
      menuItem("Log out", icon = icon("sign-out-alt"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "trends",
              
              # Filter Row (half-width na lang)
              fluidRow(
                box(width = 6, status = "info", solidHeader = TRUE, 
                    title = "🔍 Filter Options",
                    selectInput("month_filter", "Filter by Date", 
                                choices = c("This month", "Last month", "This year", "All"),
                                selected = "All"),
                    helpText("Use the filter to view booking trends by time range.")
                )
              ),
              
              br(),
              
              # First Row: Days + Time Slots
              fluidRow(
                box(title = "📅 Most Requested Days", status = "primary", solidHeader = TRUE,
                    plotlyOutput("plot_days", height = "300px"),
                    uiOutput("insight_days"), width = 6),
                
                box(title = "⏰ Preferred Time Slots", status = "success", solidHeader = TRUE,
                    plotlyOutput("plot_timeslots", height = "300px"),
                    uiOutput("insight_timeslots"), width = 6)
              ),
              
              br(),
              
              # Second Row: Services + Packages
              fluidRow(
                box(title = "⭐ Top 5 Most Booked Services", status = "warning", solidHeader = TRUE,
                    plotOutput("plot_services", height = "300px"),
                    uiOutput("insight_services"), width = 6),
                
                box(title = "📦 Most Booked Planner Package", status = "danger", solidHeader = TRUE,
                    plotOutput("plot_packages", height = "300px"),
                    uiOutput("insight_packages"), width = 6)
              ),
              
              br(),
              
              # Third Row: Trends (full width)
              fluidRow(
                box(title = "📈 Monthly Booking Trends", status = "info", solidHeader = TRUE,
                    plotlyOutput("plot_trends", height = "400px"),
                    uiOutput("insight_trends"), width = 12)
              )
      )
    )
  )
)

# =============================
#   Server
# =============================
server <- function(input, output) {
  
  # Reactive filtered dataset
  filtered_df <- reactive({
    if (input$month_filter == "This month") {
      df %>% filter(format(EventDate, "%Y-%m") == format(Sys.Date(), "%Y-%m"))
    } else if (input$month_filter == "Last month") {
      df %>% filter(format(EventDate, "%Y-%m") == format(Sys.Date() %m-% months(1), "%Y-%m"))
    } else if (input$month_filter == "This year") {
      df %>% filter(format(EventDate, "%Y") == format(Sys.Date(), "%Y"))
    } else {
      df
    }
  })
  
  # Recompute summaries
  most_requested_days_r <- reactive({
    filtered_df() %>% filter(!is.na(DayOfWeek)) %>% count(DayOfWeek, name = "Count", sort = TRUE)
  })
  
  top_services_r <- reactive({
    filtered_df() %>% 
      separate_rows(BookedServices, sep = ",\\s*") %>%
      filter(!is.na(BookedServices)) %>%
      count(BookedServices, name = "Count", sort = TRUE) %>%
      slice_head(n = 5)
  })
  
  most_booked_packages_r <- reactive({
    filtered_df() %>% filter(!is.na(`Booked Package`)) %>% count(`Booked Package`, name = "Count", sort = TRUE)
  })
  
  preferred_time_slots_r <- reactive({
    filtered_df() %>% filter(!is.na(TimeSlot)) %>% count(TimeSlot, name = "Count", sort = TRUE)
  })
  
  monthly_trends_r <- reactive({
    filtered_df() %>% group_by(Month = floor_date(EventDate, "month")) %>%
      summarise(Bookings = n(), .groups = "drop")
  })
  
  # ================= Plots =================
  output$plot_days <- renderPlotly({
    data <- most_requested_days_r()
    if (nrow(data) > 0) {
      p <- ggplot(data, aes(x = DayOfWeek, y = Count, fill = DayOfWeek,
                            text = paste("Day:", DayOfWeek,
                                         "<br>Bookings:", Count,
                                         "<br>Share:", round(Count/sum(Count)*100, 1), "%"))) +
        geom_col() +
        scale_fill_manual(values = chart_palette) +
        theme_minimal() +
        labs(x = "Day", y = "Bookings") +
        theme(legend.position = "none")
      ggplotly(p, tooltip = "text")
    }
  })
  
  output$plot_services <- renderPlot({
    data <- top_services_r()
    ggplot(data, aes(x = "", y = Count, fill = BookedServices)) +
      geom_col(width = 1) +
      geom_text(aes(label = paste0(round(Count/sum(Count)*100), "%")), 
                position = position_stack(vjust = 0.5), color = "white") +
      coord_polar("y") +
      scale_fill_manual(values = chart_palette) +
      theme_void() +
      labs(title = "Top 5 Most Booked Services", fill = "Service")
  })
  
  output$plot_packages <- renderPlot({
    data <- most_booked_packages_r()
    ggplot(data, aes(x = "", y = Count, fill = `Booked Package`)) +
      geom_col(width = 1) +
      geom_text(aes(label = paste0(round(Count/sum(Count)*100), "%")), 
                position = position_stack(vjust = 0.5), color = "white") +
      coord_polar("y") +
      scale_fill_manual(values = chart_palette) +
      theme_void() +
      labs(title = "Most Booked Planner Package", fill = "Package")
  })
  
  output$plot_timeslots <- renderPlotly({
    data <- preferred_time_slots_r()
    if (nrow(data) > 0) {
      p <- ggplot(data, aes(x = TimeSlot, y = Count, fill = TimeSlot,
                            text = paste("Time Slot:", TimeSlot,
                                         "<br>Bookings:", Count,
                                         "<br>Share:", round(Count/sum(Count)*100, 1), "%"))) +
        geom_col() +
        scale_fill_manual(values = chart_palette) +
        theme_minimal() +
        labs(x = "Time of Day", y = "Bookings") +
        theme(legend.position = "none")
      ggplotly(p, tooltip = "text")
    }
  })
  
  output$plot_trends <- renderPlotly({
    data <- monthly_trends_r()
    if (nrow(data) > 0) {
      p <- ggplot(data, aes(x = Month, y = Bookings,
                            text = paste("Month:", format(Month, "%B %Y"),
                                         "<br>Bookings:", Bookings))) +
        geom_line(aes(group = 1), color = "#1f77b4", size = 1.2) +
        geom_point(color = "#ff7f0e", size = 3) +
        theme_minimal() +
        labs(x = "Month", y = "Number of Bookings")
      ggplotly(p, tooltip = "text") %>% layout(hovermode = "x unified")
    }
  })
  
  # ================= Insights (Detailed + Equal Height) =================
  output$insight_days <- renderUI({
    data <- most_requested_days_r()
    if (nrow(data) > 0) {
      top_day <- data$DayOfWeek[1]
      HTML(paste0(
        "<div style='min-height:150px; margin-top:10px; padding:8px; border-radius:6px;
                    background-color:#eaf4ff; border-left:5px solid #1f77b4;'>
          <span style='font-size:18px;'>💡</span> 
          <b>Insight:</b><br><br>
          <b>", top_day, "</b> is the most requested day for bookings. 
          Wednesdays are the most requested days, showing a mid-week preference among your clients. 
          This suggests you should anticipate higher demand and prepare resources accordingly.
        </div>"
      ))
    } else {"<i>No data available for days.</i>"}
  })
  
  output$insight_timeslots <- renderUI({
    data <- preferred_time_slots_r()
    if (nrow(data) > 0) {
      top_slot <- data$TimeSlot[1]
      HTML(paste0(
        "<div style='min-height:150px; margin-top:10px; padding:8px; border-radius:6px;
                    background-color:#e6f7ed; border-left:5px solid #2ca02c;'>
          <span style='font-size:18px;'>⏰</span> 
          <b>Insight:</b><br><br>
          <b>", top_slot, "</b> is the most preferred booking time, 
          indicating clients favor events scheduled later in the day. 
          This suggests allocating more staff and resources during these hours.
        </div>"
      ))
    } else {"<i>No data available for time slots.</i>"}
  })
  
  output$insight_services <- renderUI({
    data <- top_services_r()
    if (nrow(data) > 0) {
      top_service <- data$BookedServices[1]
      HTML(paste0(
        "<div style='min-height:150px; margin-top:10px; padding:8px; border-radius:6px;
                    background-color:#fff3cd; border-left:5px solid #ffae42;'>
          <span style='font-size:18px;'>⭐</span> 
          <b>Insight:</b><br><br>
          <b>", top_service, "</b> leads as the top booked service, followed by lights & sounds, and venue. 
          HMUA and entertainment are also in high demand, showing clients prioritize essentials 
          like food, ambiance, and location.
        </div>"
      ))
    } else {"<i>No data available for services.</i>"}
  })
  
  output$insight_packages <- renderUI({
    data <- most_booked_packages_r()
    if (nrow(data) > 0) {
      top_package <- data$`Booked Package`[1]
      HTML(paste0(
        "<div style='min-height:150px; margin-top:10px; padding:8px; border-radius:6px;
                    background-color:#fde2e1; border-left:5px solid #d62728;'>
          <span style='font-size:18px;'>📦</span> 
          <b>Insight:</b><br><br>
          The <b>", top_package, "</b> package is the most booked, 
          highlighting a preference for budget-friendly planning options. 
          This indicates clients value affordability while still seeking organized services.
        </div>"
      ))
    } else {"<i>No data available for packages.</i>"}
  })
  
  output$insight_trends <- renderUI({
    data <- monthly_trends_r()
    if (nrow(data) > 1) {
      latest <- tail(data, 1)$Bookings
      prev <- tail(data, 2)$Bookings[1]
      
      if (latest > prev) {
        icon <- "📈"; color <- "#d4edda"; border <- "#28a745"
        msg <- paste("Bookings increased last month (", prev, "→", latest, 
                     "), signaling growing demand. Sustaining this trend may require proactive marketing ",
                     "and readiness for higher client volume.")
      } else if (latest < prev) {
        icon <- "📉"; color <- "#f8d7da"; border <- "#dc3545"
        msg <- paste("Bookings decreased last month (", prev, "→", latest, 
                     "). This may reflect seasonal demand changes. ",
                     "Consider promotions or campaigns to recover demand.")
      } else {
        icon <- "➡️"; color <- "#fff3cd"; border <- "#ffc107"
        msg <- paste("Bookings remained stable last month with", latest, 
                     "total requests. While growth is flat, consistency shows reliability.")
      }
      
      HTML(paste0(
        "<div style='min-height:auto; margin-top:10px; padding:8px; border-radius:6px;
                    background-color:", color, "; border-left:5px solid ", border, ";'>
          <span style='font-size:18px;'>", icon, "</span> 
          <b>Insight:</b><br>", msg, "
        </div>"
      ))
    } else {
      "<i>Not enough data for monthly trend insights.</i>"
    }
  })
}

# =============================
#   Run App
# =============================
shinyApp(ui, server)
