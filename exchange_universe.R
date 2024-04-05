
library(tidyverse)
library(data.table)
library(httr)
library(jsonlite)
library(readr)
library(plotly)
library(echarts4r)
library(tidyr)
library(shiny)
library(shinythemes)
library(htmlwidgets)
library(reactable)
library(bslib)
library(shinydashboard)
library(shinydashboardPlus)
library(DT)
library(glue)
library(shinyjs)
library(rsconnect)


#----------------------------UI for the application----------------------------#
ui <- dashboardPage(
  skin = "yellow",
  dashboardHeader(title = "Exchange Universe",
                  dropdownMenu(
                    type = "messages",
                    badgeStatus = "info",
                    messageItem(
                      from = "Patreon",
                      message = "Get a coffee for Developer",
                      href = "https://www.patreon.com/CreativityLand"
                    ),
                    messageItem(
                      from = "linkedin",
                      message = "About Developer",
                      href = "http://www.linkedin.com/in/sadeqrezai"
                    ),
                    messageItem(
                      from = "Github",
                      message = "Check for more",
                      href = "https://github.com/Rammeshgar"
                    ),
                    messageItem(
                      from = "Mail",
                      message = "Direct Contact",
                      href = "mailto:xxxx"
                    )
                  )),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Line & Bar Charts", tabName = "line_bar_charts", 
               icon = icon("line-chart")),
      menuItem("3D and Table", tabName = "3D", icon = icon("th")),
      selectInput(
        inputId = "select_base_currency",
        label = "Choose Base Currency:",
        choices = NULL,
        selected = "EUR"
      ),
      selectInput(
        inputId = "ex_to",
        label = "Exchange to:",
        choices = NULL,
        multiple = F
      ),
      #ex_to_output
      textOutput("valueDisplay"),
      #---
      selectInput(
        inputId = "select_currencies",
        label = "Compaire Currencies:",
        choices = NULL,
        multiple = TRUE
      ),
      dateRangeInput(
        inputId = "date_range",
        label = "Select Date Range:",
        start = Sys.Date() %m-% months(1),
        end = Sys.Date()
      ))),
  dashboardBody(
    useShinyjs(),
    tabItems(
      # First tab content
      tabItem(tabName = "line_bar_charts",
              fluidRow(
                box(
                  title = "Currency Values Updating Hourly",
                  status = "primary",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  echarts4rOutput(outputId = "line_plot", height = "510px")
                ),
                box(
                  title = "Currency Values Updating Hourly",
                  status = "primary",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  echarts4rOutput(outputId = "bar_chart", height = "510px")
                ))),
      # Second tab content
      tabItem(tabName = "3D",
              fluidRow(
                box(
                  width = 8,
                  title = "3D Plot",
                  status = "warning",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  plotlyOutput(outputId = "TD", height = "510px"),
                  downloadButton("customeized_data", "Selected Data (CSV)")
                ),
                box(
                  width = 4,
                  title = "Selected Data",
                  dataTableOutput("table")
                )))))
)



#-----------------------------Define server logic------------------------------#
server <- function(input, output, session) {
 

  #-#___Collecting_online_data____________________________________________________
  online_api_data <- function(start_date, end_date, base){
    url <- 
      paste0("https://api.apilayer.com/exchangerates_data/timeseries?start_date=",
             start_date, "&end_date=", end_date, "&base=", base)
    
    # API key
    headers <- c(
      apikey = "XXXXXXXXXXXXXXXXXXX"
    )
    
    # Perform the GET request
    response <- GET(url, add_headers(headers), timeout(60))
    
    # Check if the request was successful (HTTP status code 200)
    if (status_code(response) == 200) {
      # Extract the content of the response as text
      result <- content(response, "text", encoding = "UTF-8")
      
      # Parse the JSON result into a list
      result_data <- jsonlite::fromJSON(result)
      
      # Convert the nested list to a tidy data frame
      rates <- result_data$rates
      dates <- names(rates)
      
      # Use the first date to get the currency names
      currency_names <- names(rates[[1]])
      
      # Initialize an empty list to store data frames
      list_of_dfs <- lapply(dates, function(date) {
        # Create a data frame for each date
        df <- data.frame(date = as.Date(date))
        
        # Add rate columns that exist for the current date
        existing_cols <- intersect(currency_names, names(rates[[date]]))
        df[existing_cols] <- rates[[date]][existing_cols]
        
        # Add missing columns as NA
        missing_cols <- setdiff(currency_names, existing_cols)
        df[missing_cols] <- NA
        
        # Ensure the data frame has the correct column names
        colnames(df) <- c("date", currency_names)
        
        df
      })
      
      # Bind all data frames in the list into one data frame
      result_table <- do.call(rbind, list_of_dfs)
      
      # Convert the rates to numeric (with exceptions)
      numeric_columns <- setdiff(names(result_table), "date")
      result_table[, numeric_columns] <- 
        lapply(result_table[, numeric_columns], as.numeric)
      columns_to_round <- setdiff(numeric_columns, c("BTC","XAU","XAG"))
      result_table[, columns_to_round] <- 
        lapply(result_table[, columns_to_round], round, 2)
      result_table <- tidyr::replace_na(result_table, list(rate = 0))
      
      # Gather the data into long format
      new_df <- tidyr::pivot_longer(result_table, 
                                    cols = numeric_columns, 
                                    names_to = "currency", 
                                    values_to = "rate")
      
      return(new_df)
      write_csv(new_df, "new_df.csv")
    } else {
      # Print an error message with the status code
      print(paste("Request failed with status", status_code(response)))
      return(NULL)
    }
  }
  #_____________________________________________________________________________
  
  #-#__Reactive_Mother_data_____
  new_df1 <- reactive({
    req(input$date_range)
    online_api_data(input$date_range[1], input$date_range[2], 
                    input$select_base_currency)
  })

  #-#__Reactive_selected_data___
  unique_currencies <- reactive({
    req(new_df1()) # Ensure new_df1 is available
    filtered_data <- new_df1() %>%
      filter(currency %in% input$select_currencies,
             date >= input$date_range[1],  
             date <= input$date_range[2]) %>%
      select(currency, date, rate) %>%
      group_by(currency)
    return(filtered_data)
  })
  
  #-#__Reactive_Ex_to_data______
  one_change <- reactive({
    req(new_df1())
    one_change_data <- new_df1() %>% 
      filter(currency %in% input$ex_to) %>%
      group_by(currency) %>% 
      arrange(desc(date)) %>% 
      slice_head(n = 1) %>% 
      select(currency, date, rate)
    return(one_change_data)
    })
  
  #-#__Available_data_for_inputs
  observeEvent(new_df1(), {
    if (!is.null(new_df1()) && "currency" %in% colnames(new_df1())) {
      updateSelectInput(session, "select_base_currency",
                        choices = unique(new_df1()$currency),
                        selected = input$select_base_currency)
    }
  })
  
  observeEvent(new_df1(), {
    if (!is.null(new_df1()) && "currency" %in% colnames(new_df1())) {
      updateSelectInput(session, "select_currencies",
                        choices = unique(new_df1()$currency))
    }
  })
  
  observeEvent(new_df1(), {
    if (!is.null(new_df1()) && "currency" %in% colnames(new_df1())) {
      updateSelectInput(session, "ex_to",
                        choices = unique(new_df1()$currency))
    }
  })
  
  #-#__Render_the_one_change____
  output$valueDisplay <- renderText({
    req(one_change)
    
    rate <- one_change() %>% 
      group_by(currency) %>% 
      summarise(rate = mean(rate)) %>% 
      pull(rate)
    
    return(paste(" ==>>", rate))
  })
  
  #-#__Render_the_Line_Chart____
  output$line_plot <- renderEcharts4r({
    req(unique_currencies)
    
    unique_currencies() %>%
      e_charts(date) %>%
      e_line(serie = rate, symbol = "arrow" , symbolSize = 2) %>%
      e_tooltip("axis") %>% 
      e_toolbox_feature(feature = c("saveAsImage", "dataView")) %>%
      e_datazoom(x_index = 0,type = "slider") %>%
      e_datazoom(y_index = 0,type = "slider") %>%
      e_title(glue(
        "Values by {toString(input$select_base_currency)}"
        )) %>%
      e_x_axis(name = 'Date', axisLabel = list(rotate = 30), 
               nameTextStyle = list(color = 'tomato')) %>%
      e_y_axis(name = 'Rate', nameTextStyle = list(color = 'tomato')) %>%
      e_theme("dark") %>%
      e_color(c('#E63946', '#F1FAEE', '#A8DADC', '#457B9D', 
                '#1D3557', '#2A9D8F', '#E9C46A', '#F4A261', 
                '#264653', '#E76F51')) %>%
      e_legend(show = FALSE) 
  })

  #-#__Render_the_Bar_Chart_____
  output$bar_chart <- renderEcharts4r({
    req(unique_currencies)
    
    unique_currencies() %>%
      e_charts(date) %>%
      e_bar(serie = rate) %>%
      e_tooltip("axis") %>% 
      e_toolbox_feature(feature = c("saveAsImage", "dataView")) %>%
      e_datazoom(x_index = 0, type = "slider") %>%
      e_datazoom(y_index = 0, type = "slider") %>%
      e_title(glue(
        "Values by {toString(input$select_base_currency)}"
      )) %>%
      e_x_axis(name = 'Date', axisLabel = list(rotate = 30), 
               nameTextStyle = list(color = 'tomato')) %>%
      e_y_axis(name = 'Rate', nameTextStyle = list(color = 'tomato')) %>%
      e_theme("dark") %>%
      e_color(c('#FFD700', '#FF8C00', '#FF6347', '#FF4500', 
                '#FF1493','#DB7093', '#BA55D3', '#9370DB',
                '#8A2BE2', '#9400D3')) %>%
      e_legend(show = FALSE)
  })
  
  
  #-#__Render_the_3D_plot_______
  output$TD <- plotly::renderPlotly({
    
    unique_currencies() %>%
      plot_ly(x = ~currency, y = ~date, z = ~rate,
              type = 'scatter3d', mode = 'lines',
              marker = list(opacity = 0.9, size = 3.5, showscale = FALSE)) %>%
      layout(title = glue(
        "Currency Values based on {toString(input$select_base_currency)}"),
             margin = list(t = 50),
             scene = list(
               xaxis = list(title = 'Currencies', tickangle = 30, 
                            ticks = "outside", showgrid = FALSE, color = 'tomato',
                            tickfont = list(color = 'floralwhite')),
               yaxis = list(title = 'Date', tickangle = 0, color = 'tomato',
                            tickfont = list(color = 'floralwhite')),
               zaxis = list(title = 'Value rate',color = 'tomato',
                            tickfont = list(color = 'floralwhite'))
             ),
             paper_bgcolor = 'cornflowerblue',
             colorway = c('cyan'),
             showlegend = FALSE) %>%
      hide_legend() 
        })
  
  
  #-#__Render_table_____________
  output$table <- renderDataTable({
    datatable(unique_currencies())})
  
  #-#__Render_Download_section__
  output$customeized_data <- downloadHandler(
    filename = function() {
      paste("selected_data-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(unique_currencies(), file, row.names = FALSE)
    }
  )
}



shinyApp(ui, server)

