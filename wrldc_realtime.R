# Required libraries ------------------------------------------------------

library(shinydashboard)
library(DT)
library(dplyr)
library(shinycssloaders)
library(plotly)
library(timetk)

# Dashboard components ----------------------------------------------------
header <- dashboardHeader(title = "Power Consumption")

sidebar <- dashboardSidebar(sidebarMenu(
  menuItem(
    "Dashboard",
    tabName = "dashboard",
    icon = icon("dashboard")
  ),
  menuItem("Data Table",
           tabName = "data_table",
           icon = icon("table")),
  menuItem("Visualization",
           tabName = "visual",
           icon = icon("chart-area"))
))

body <- dashboardBody(tabItems(
  tabItem(
    "dashboard",
    fluidRow(
      valueBoxOutput("state_count"),
      valueBoxOutput("max_drawal"),
      valueBoxOutput("last_update")
      
    ),
    fluidRow(
      box(
        withSpinner(dataTableOutput(outputId = "data_table")),
        title = "Data table",
        solidHeader = T,
        status = "info"
      ),
      box(
        uiOutput(outputId = "select_state"),
        withSpinner(plotlyOutput(outputId = "trend")),
        title = "Trendline",
        solidHeader = T,
        status = "info"
      )
    )
  ),
  tabItem(tabName = "data_table",
          fluidRow(box(
            dateInput(
              inputId = "data_date",
              label = "Choose date"
            ), width = 2),
            box(
            uiOutput(outputId = "select_state2"), width = 2
          )),
          fluidRow(
            box(
              withSpinner(dataTableOutput(outputId = "data_table_detail")),
              title = "Data table",
              solidHeader = T,
              status = "info",
              width = 12
              
            )
          )),
  tabItem(tabName = "visual",
          fluidRow(box(uiOutput(outputId = 'select_state3'), width = 2)),
          fluidRow(
            box(
              withSpinner(plotlyOutput(outputId = "scheVact")),
              title = "Schedule Vs Actual Generation",
              solidHeader = T,
              status = "info",
              collapsible = T,
              width = 12  
            )),
          fluidRow(
            box(
              withSpinner(plotlyOutput(outputId = "deviation")),
              title = "Deviation",
              solidHeader = T,
              status = "info",
              collapsible = T,
              width = 12  
            )),
          fluidRow(
            box(
              withSpinner(plotlyOutput(outputId = "genVdem")),
              title = "Generation Vs Demand",
              solidHeader = T,
              status = "info",
              collapsible = T,
              width = 12  
            )),
          fluidRow(
            box(
              withSpinner(plotlyOutput(outputId = "freq")),
              title = "Frequency",
              solidHeader = T,
              status = "info",
              collapsible = T,
              width = 12  
            )))
))


# UI ----------------------------------------------------------------------

ui <- dashboardPage(skin = "purple", header, sidebar, body)


# Server ------------------------------------------------------------------

server <- function(input, output) {
  # Fetch real time data ----------------------------------------------------
  
  autoInvalidate <- reactiveTimer(70000)
  generateData <- reactive({
    headers = c('User-Agent' = 'Mozilla/5.0', 'Content-Type' = 'application/json; charset=UTF-8')
    
    data = paste0("{date:'", as.character(input$data_date), "'}")
    
    r <-
      httr::POST(url = 'https://wrldc.in/OnlinestateTest1.aspx/GetRealTimeData_state_Wise',
                 httr::add_headers(.headers = headers),
                 body = data) %>% httr::content()
    autoInvalidate()
    
    current_data <- jsonlite::parse_json(r$d, simplifyVector = T)
    current_data$current_datetime <-
      format(
        as.POSIXct(current_data$current_datetime, format = '%Y-%d-%m %I:%M:%S'),
        format = "%Y-%d-%m %H:%M:%S",
        tz = Sys.timezone()
      )
    if (is.data.frame(current_data)) {
      return(current_data)
    }
    else {
      showModal(modalDialog(
        title = "Alert",
        "No current data available. Please select a past date from Data Table menu.",
        easyClose = TRUE
      ))
    }
  })
  
  
  # Dashboard tab -----------------------------------------------------------
  
  
  # Value Box ---------------------------------------------------------------
  
  output$state_count <- renderValueBox({
    valueBox(
      value = length(unique(generateData()[, 2])),
      subtitle = "Total number of states",
      icon = icon("globe-asia"),
      color = "teal"
    )
  })
  
  output$max_drawal <- renderValueBox({
    
    if(is.null(generateData())){
      max_drawal_state <- data.frame()
      max_drawal_state[1, "StateName"] <- "No data"
    }
    else{
    max_drawal_state <-
      generateData() %>% filter(Act_Drawal == max(Act_Drawal)) %>% select(StateName)
    }
    valueBox(
      value = max_drawal_state$StateName,
      subtitle = "State with maximum drawal",
      icon = icon("bolt"),
      color = "yellow"
    )
    
  })
  
  output$last_update <- renderValueBox({
    valueBox(
      value = generateData()$current_datetime[nrow(generateData())],
      subtitle = "Last updated",
      icon = icon("clock"),
      color = "green"
    )
  })
  
  # Render data table -------------------------------------------------------
  
  output$data_table <- renderDataTable({
    generateData()[, 2:6]
    
  }, options = list(
    paging = TRUE,
    pageLength =  10,
    escape = FALSE,
    searching = FALSE,
    lengthChange = FALSE
  ))
  
  # Select State ------------------------------------------------------------
  
  output$select_state <- renderUI({
    selectInput(
      inputId = "state",
      label = "Select State",
      choices = unique(generateData()[, 2]),
      width = '20%'
    )
  })
  
  # Trend line --------------------------------------------------------------
  
  output$trend <- renderPlotly({
    
    if(is.null(generateData())){
      return(NULL)
    }
    else{
    data <-
      generateData() %>% filter(StateName == ifelse(is.null(input$state), "Gujrat", input$state))
   
    plot_ly(
      x = ~ c(1:nrow(data)),
      y = ~ data$Sch_Drawal,
      type = "scatter",
      mode = "lines",
      name = "Scheduled Drawal",
      hovertext = data$current_datetime
    ) %>%
      add_trace(y = ~ data$Act_Drawal,
                name = 'Actual Drawal',
                mode = 'markers + lines') %>%
      layout(
        xaxis = list(
          title = "",
          showticklabels = FALSE,
          rangeslider = list(type = "date")
        ),
        yaxis = list(title = "Power consumption")
      )
    }
    
    
  })
  
  
  # Data Table tab ----------------------------------------------------------
  
  output$select_state2 <- renderUI({
    selectInput(
      inputId = "state2",
      label = "Select State",
      choices = unique(generateData()[, 2])
    )
  })
  
  output$data_table_detail <- renderDataTable({
    if(is.null(generateData())){
      data <- data.frame()
    }
    else{
    data <-
      generateData()[, 2:9] %>% filter(StateName == ifelse(is.null(input$state2), "Gujrat", input$state2))
    }
  }, options = list(paging = TRUE,
                    pageLength =  15))
  

# Visualization tab -------------------------------------------------------

  output$select_state3 <- renderUI({
    selectInput(
      inputId = "state3",
      label = "Select State",
      choices = unique(generateData()[, 2])
    )
  })
  
  output$scheVact <- renderPlotly({
  if(is.null(generateData())){
    return(NULL)
  }
  else{
    data <-
      generateData() %>% filter(StateName == ifelse(is.null(input$state3), "Gujrat", input$state3))
    
    plot_ly(
      x = ~ c(1:nrow(data)),
      y = ~ data$Sch_Drawal,
      type = "scatter",
      mode = "lines",
      name = "Scheduled Drawal",
      hovertext = data$current_datetime
    ) %>%
      add_trace(y = ~ data$Act_Drawal,
                name = 'Actual Drawal',
                mode = 'markers + lines') %>%
      layout(
        xaxis = list(
          title = "",
          showticklabels = FALSE
        ),
        yaxis = list(title = "Power")
      )
  }
})
  
  output$deviation <- renderPlotly({
    if(is.null(generateData())){
      return(NULL)
    }
    else{
      data <-
        generateData() %>% filter(StateName == ifelse(is.null(input$state3), "Gujrat", input$state3))
      
      plot_ly(
        x = ~ c(1:nrow(data)),
        y = ~ data$Deviation,
        type = "scatter",
        mode = "lines",
        name = "Deviation",
        hovertext = data$current_datetime
      ) %>%
        layout(
          xaxis = list(
            title = "",
            showticklabels = FALSE
          ),
          yaxis = list(title = "Power")
        )
    }
  })
  
  output$genVdem <- renderPlotly({
    if(is.null(generateData())){
      return(NULL)
    }
    else{
      data <-
        generateData() %>% filter(StateName == ifelse(is.null(input$state3), "Gujrat", input$state3))
      
      plot_ly(
        x = ~ c(1:nrow(data)),
        y = ~ data$Generation,
        name = 'Generation',
        mode = 'markers + lines',
        type = "scatter",
        hovertext = data$current_datetime
      ) %>%
        add_trace(y = ~ data$Demand,
                  name = 'Demand',
                  mode = 'markers + lines') %>%
        layout(
          xaxis = list(
            title = "",
            showticklabels = FALSE
          ),
          yaxis = list(title = "Power")
        )
    }
  })
  
  output$freq <- renderPlotly({
    if(is.null(generateData())){
      return(NULL)
    }
    else{
      data <-
        generateData() %>% filter(StateName == ifelse(is.null(input$state3), "Gujrat", input$state3))
      
      plot_ly(
        x = ~ c(1:nrow(data)),
        y = ~ data$Frequency,
        name = 'Frequency',
        mode = 'markers + lines',
        type = "scatter",
        hovertext = data$current_datetime
      ) %>%
        layout(
          xaxis = list(
            title = "",
            showticklabels = FALSE
          ),
          yaxis = list(title = "Frequency in hertz")
        )
    }
  })
  
}

shinyApp(ui, server)