library(shiny)
library(shinythemes)
library(shinydashboard)
library(dashboardthemes)
library(shinyWidgets)
library(reshape2)
library(tidyverse)
library(knitr)
library(lubridate)
library(heatmaply)
library(ggthemes)
library(scales)
library(plotly)
#library(gganimate) # Used for animations

#setwd("C:\\Users\\Akul\\Desktop\\DSA\\R Visualization Project Data")
# options(rsconnect.max.bundle.size= 3145728000)

# arrange flights by date
#flights <- read.csv("jan2019jun2020.csv")
#flights$FL_DATE <- ymd(mdy(flights$FL_DATE))
#flights <- flights %>% arrange(., FL_DATE)


source("https://raw.githubusercontent.com/iascchen/VisHealth/master/R/calendarHeat.R")
source("customTheme.R")

# Summing delay times for easier calculation
sum_delays <- flights %>% 
  group_by(FL_DATE, MKT_UNIQUE_CARRIER) %>% 
  summarise(count = 1/60 * sum(DEP_DELAY_NEW, na.rm = T)) %>% 
  as.data.frame()

# TOTAL STATS INFOBOX 

total_df <- flights %>%   
  summarise(total = n(), delayed = sum(DEP_DELAY_NEW != 0, na.rm = T),
            delayed_pct = percent(delayed/total, accuracy = 0.1), 
            cancelled = sum(CANCELLED == 1),
            cancelled_pct = percent(cancelled/total, accuracy = 0.01))

# INDIVIDUAL STATS INFOBOXES 

noncovid_df <- flights %>%  ## STATS FROM JAN-JUNE 2019
  filter(., FL_DATE > "2019-01-01" & FL_DATE < "2019-06-30") %>% 
  summarise(total = n(), delayed = sum(DEP_DELAY_NEW != 0, na.rm = T),
            delayed_pct = percent(delayed/total, accuracy = 0.1), 
            cancelled = sum(CANCELLED == 1),
            cancelled_pct = percent(cancelled/total, accuracy = 0.01))

covid_df <- flights %>%     ## STATS FROM JAN-JUNE 2020
  filter(., FL_DATE > "2020-01-01" & FL_DATE < "2020-06-30") %>% 
  summarise(total = n(), delayed = sum(DEP_DELAY_NEW != 0, na.rm = T),
            delayed_pct = percent(delayed/total, accuracy = 0.1), 
            cancelled = sum(CANCELLED == 1),
            cancelled_pct = percent(cancelled/total, accuracy = 0.01))

# FEATURE CORRELATION MATRIX HEATMAP

corr_df <- flights %>%
  filter(., CANCELLED == 0 & !is.na(ACTUAL_ELAPSED_TIME) & !is.na(CRS_ELAPSED_TIME)) %>%
  select(
    c(
      "DAY_OF_MONTH",
      "DAY_OF_WEEK",
      "CRS_DEP_TIME",
      "DEP_TIME",
      "DEP_DELAY_NEW",
      "TAXI_OUT",
      "WHEELS_OFF",
      "WHEELS_ON",
      "TAXI_IN",
      "CRS_ARR_TIME",
      "ARR_TIME",
      "ARR_DELAY_NEW",
      "CRS_ELAPSED_TIME",
      "ACTUAL_ELAPSED_TIME",
      "AIR_TIME",
      "DISTANCE"
    )
  )

corr_data <- melt(round(cor(corr_df), 3))

corr_mat <- ggplot(corr_data, aes(x = Var1, y = Var2, fill = value)) + 
  geom_tile() + 
  labs(x = "", y = "") + scale_fill_viridis() + theme_minimal() +
  theme(axis.text.x = element_text(hjust = 1, vjust = 1, angle = 60, color = "black", size = 12),
        axis.text.y = element_text(color = "black", size = 12))

#-------------------------------#FRONT END#-------------------------------------
#-------------------------------------------------------------------------------


ui <- dashboardPage(
    
    dashboardHeader(

        title = "COVID-19 Flights"
       
    ),
    
    dashboardSidebar(sidebarMenu(
        
        menuItem(                       ## First Menu Item in Sidebar
            "Introduction",
            tabName = "intro",
            icon = icon("dashboard")
        ),
        
        menuItem(                       ## Second Menu Item in Sidebar
            "Summary Statistics",
            tabName = "summaries",
            icon = icon("table"),
            
            menuSubItem("Overall Flight Statistics", tabName = "summary"),
            menuSubItem("More Summary Statistics", tabName = "summary2")
        ),
        
        menuItem(                       ## Third Menu Item in Sidebar
            "COVID-19 Flight Analysis", 
            tabName = "widgets", 
            icon = icon("th"),
        
            menuSubItem("COVID-19 Visualizations", tabName = "covid")
        
    ))),
    
  #----------------------#TAB CONTENTS#-----------------------------
    
    dashboardBody(customTheme,
      
      #shinyDashboardThemes(theme = "purple_gradient"),
      #tags$link(rel = "stylesheet", type = "text/css", href = "bootstrap.css"),
                  #tags$link(rel = 'stylesheet', href = '//maxcdn.bootstrapcdn.com/font-awesome/4.5.0/css/font-awesome.min.css'),
      tabItems( 
        
  # First tab content-----------------------------------
  
        tabItem(
                tabName = "intro",
                h2("COVID-19 Flight Visualization Project"),
                
                fluidRow(
                  column(6,
                  print(img(src = "intro.jpg",
                            width = "97%"))),
                  column(6,valueBoxOutput("numflights", width = 6)), 
                  column(6,valueBoxOutput("delflights", width = 6)),
                  column(6,valueBoxOutput("delpctflights", width = 6)), 
                  column(6,valueBoxOutput("cancelflights", width = 6)),
                  column(6,valueBoxOutput("cancelpctflights", width = 6))
                  ),
                
                fluidRow(column(12,
                box(h3(
                "COVID-19 has severely crippled the global airline industry with 
                air service reductions widespread throughout 2020. This dataset 
                containing 11 million flights will aid those seeking to visualize 
                the impact that the virus has had on the domestic United States 
                airline industry through detailed flight delay and cancellation data."
                    ))
                ), 
                column(12,
                box(h3("
                The United States Department of Transportation's (DOT) Bureau of
                Transportation Statistics tracks the on-time performance of domestic
                flights operated by large air carriers. The data collected is from
                January - June 2020 and contains relevant
                flight information (on-time, delayed, canceled, diverted flights)
                from the Top 10 United States flight carriers for 11 million flights.
                The compiled and cleaned data is hosted on", 
                    tags$a(
                      href="https://www.kaggle.com/akulbahl/covid19-airline-flight-delays-and-cancellations", 
                            HTML("<u>Kaggle</u>")),
                "in 47 columns with full column descriptions in the attached '.txt' file.
                The source code can be found on my", tags$a(
                  href="https://github.com/akulbahl/COVID-19FlightVisualizations", 
                  HTML("<u>Github</u>."))))
                ))
        )
                ,
        
  # Second tab content----------------------------------
  
      # Tab 2 Subtab 1--------------------------------------
  
        tabItem(tabName = "summary", # Corresponds to summary menu sub item
                
                h2("Summary Statistics"),
                
                fluidRow(
                  
                  box(title = "Flight Delay Time by Domestic US Airline (Jan 2019 - Jun 2020)",
                    width = 6, status = "primary", mainPanel(plotOutput("heatdelay"), width = 15)
                    ),
                  
                  box(title = "Elapsed Flight Time Density by Domestic US Airline (Jan 2019 - Jun 2020)",
                    width = 6, status = "warning", mainPanel(plotOutput("timedensity"), width = 15)
                  )
                ),
                
                fluidRow( 
                  column(12, align = "center", box(
                  width = 12, mainPanel(plotOutput("calendar"), width = 15), 
                  
                  box(title = "Calendar Category", width = 6, solidHeader = TRUE, 
                      status = "primary",
                      prettyRadioButtons("calbuttons", label = h3(""),
                                         status = "info",
                                         animation = "pulse",
                                         inline = TRUE,
                                         choices = list("Total Departure Delay (in Hours)" = 1, 
                                                        "Total Cancelled Flights" = 2), 
                                         selected = 1)
                  )
                  
                  )
                #box(title = "Calendar Category", width = 2, solidHeader = TRUE, status = "primary",
                #radioButtons("calbuttons", label = h3(""),
                #             choices = list("Total Departure Delay (in Hours)" = 1, 
                #                            "Total Cancelled Flights" = 2), 
                #             selected = 1)
                    )
                )
        ),
                
      # Tab 2 Subtab 2--------------------------------------

        tabItem(tabName = "summary2",
            
            h2("Additional Summary Statistics"),
            
            fluidRow(
            sidebarPanel(width = 5,
                
                selectizeInput(
                    inputId = "origin",
                    label = h2("Departure Airport"),
                    choices = sort(unique(flights$ORIGIN_CITY_NAME))
                ),
                
                selectizeInput(
                    inputId = "dest",
                    label = h2("Arrival Airport"),
                    choices = sort(unique(flights$DEST_CITY_NAME))
                ),
                
                dateRangeInput(inputId ="dates",
                               label = h2("Date range"),
                               start = "2019-01-01",
                                 end = "2020-06-30",
                               min = "2019-01-01",
                               max = "2020-06-30"),
                hr()
                #fluidRow(column(4, verbatimTextOutput("value")))
                
                          ),
            
            box(title = "Number of Flights", width = 6, status = "primary",
                mainPanel(plotOutput("barchart"), width = 15), #Plots delay bar chart
                "Airline Carrier Code:
                AA: American Airlines
                AS: Alaska Airlines
                B6: JetBlue
                DL: Delta Air Lines
                F9: Frontier Airlines
                G4: Allegiant Air
                HA: Hawaiian Airlines
                NK: Spirit Airlines
                UA: United Airlines
                WN: Southwest Airlines"),
            ),
            
            fluidRow(
            box(title = "Feature Correlation Heatmap", width = 5, status = "warning",
              mainPanel(plotOutput("corrmat", height = '700px', width = '700px'))
                ),
            box(title = "Polar Plot", width = 6, status = "primary",
                mainPanel(plotOutput("polarplot", width='800px', height='700px'))
            )),
            fluidRow(
            img(src = "delaysummary.gif", align = "left", width='800px', height='500px'),
            img(src = "delaysandcancellations.gif", align = "center", width='800px', height='500px')
            )
        ),

# Third tab content---------------------------------- 

        tabItem(tabName = "covid", 
                
                h2("COVID-19 Comparison Plots (Select 'Pandemic Button' to Change Situation)"),
                
                switchInput("switch", label = "Start a Pandemic?", size = "large", 
                    onLabel = "No", offLabel = "Yes", onStatus = "success", 
                    offStatus = "danger", value = TRUE, width = "100%"),
                
                hr(),
                fluidRow(
                         box(title = "Departure Times by Airline",
                             width = 6, 
                             status = "primary",
                             mainPanel(plotOutput("boxplot"),
                                       width = "500px")
                             ),
                         
                         column(6, box(title = "Time Density by Domestic US Airline", 
                             width = 6, 
                             status = "warning",
                             mainPanel(plotOutput("delaydensity"), 
                                       width = "500px")),
                             box(h3("For the comparison period of Jan - Jun 2019,
                         there was a significant reduction in the proportion of 
                         delayed flights during COVID-19 (Jan - Jun 2020). This is likely due
                         to the significantly decreased number of flights during this time period. 
                         There was conversely a significant increase in both the number and proportion of flight 
                         cancellations, especially around late March (as seen in the Calendar 
                         Heatmap) due to COVID-19." )
                             
                         )
                        ),
                
                 # Plotting Value Boxes
        
                fluidRow(
                
                  column(8, valueBoxOutput("numflights_valueboxes", width = 7)),
                  column(8, valueBoxOutput("delflights_valueboxes", width = 7)),
                  column(8, valueBoxOutput("delpctflights_valueboxes", width = 7)),
                  column(8, valueBoxOutput("cancelflights_valueboxes", width = 7)),
                  column(8, valueBoxOutput("cancelpctflights_valueboxes", width = 7))
                  )
                  
                        )
                )
      
              ))
)

#-------------------------------#BACK END#--------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

server <- function(input, output, session) {

##--------------------------- VALUE BOXES ## -----------------------------------
  
  output$numflights <- renderValueBox({
    
    valueBox(subtitle = HTML("<b>Total Number of Domestic Airline Flights</b>"),
            value = format(total_df[1], big.mark = ","),
            icon = icon("plane"),
            color = "blue",
            )
  })
  
  output$delflights <- renderValueBox({
    
    valueBox(subtitle = HTML("<b>Number of Delayed Airline Departures</b>"),
            value = format(total_df[2], big.mark = ","),
            icon = icon("plane-departure"),
            color = "navy",
    )
  })
  
  output$delpctflights <- renderValueBox({
    
    valueBox(subtitle = HTML("<b>Percent of Delayed Airline Departures</b>"),
            value = total_df[3],
            icon = icon("percent"),
            color = "navy",
    )
  })
  
  output$cancelflights <- renderValueBox({
    
    valueBox(subtitle = HTML("<b>Total Number of Cancelled Domestic Flights</b>"),
            value = format(total_df[4], big.mark = ","),
            icon = icon("plane-slash"),
            color = "maroon",
    )
  })
  
  output$cancelpctflights <- renderValueBox({
    
    valueBox(subtitle = HTML("<b>Percent of Cancelled Airline Flights</b>"),
            value = total_df[5],
            icon = icon("percent"),
            color = "maroon",
    )
  })
##--------------------------- SUMMARY PLOTS ## ---------------------------------
    
  ## Flight Time Density ##
    
    output$timedensity <- renderPlot(
        
        ggplot(flights, aes(x = 1/60*ACTUAL_ELAPSED_TIME, fill = MKT_UNIQUE_CARRIER)) +
        geom_density(position = 'identity',
                     alpha = 0.8,
                     adjust = 1) +
        facet_grid(MKT_UNIQUE_CARRIER ~ ., scales = "free") +
        xlim(0, 7) +
        labs(x = 'Elapsed Flight Time (Hours)', y = 'Density') +
        labs(fill = 'Airline') +
        theme_minimal() +
        theme(
          axis.title.x = element_text(size = 20),
          axis.text.x = element_text(size = 16), 
          axis.title.y = element_text(size = 20),
          axis.text.y = element_text(size = 10),
          legend.title = element_text(size = 16),
          legend.text = element_text(size = 14),
          legend.position = 'left'
        )
    )
    
    ## Flight Delay HEATMAP ##
    
    output$heatdelay <- renderPlot( 
              
                ggplot(sum_delays, aes(FL_DATE, MKT_UNIQUE_CARRIER)) + geom_tile(aes(fill = count)) +
                theme_minimal() +
                labs(x = "Flight Date", y = "Airline") +
                scale_fill_gradientn(name = "Total Delay Time (Hours)",
                                     colors = c("#2A0890", "#7404AC","#E75799", "#FDAE61", "#FFFF00"),
                                     values = scales::rescale(
                                         c(0, 1000,
                                           1000.001, 2000,
                                           2000.001, 3000,
                                           3000.001, 4000,
                                           4000.001, max(sum_delays$count)))) +
                theme(axis.title.x = element_text(size = 20),
                      axis.text.x = element_text(size = 16), 
                      axis.title.y = element_text(size = 20),
                      axis.text.y = element_text(size = 16),
                      legend.title = element_text(size = 16),
                      legend.text = element_text(size = 14)
                      )
  )
            
    ## CALENDAR HEATMAP ##
    
    cal_heat_data <- flights %>%
      group_by(FL_DATE) %>%
      summarise(dep_delay = 1/60 * sum(DEP_DELAY_NEW, na.rm = T),
                cancelled = sum(CANCELLED)) %>%
      as.data.frame()
    
            # Button and label inputs for Calendar
    
    calbuttonchoices = cbind(cal_heat_data$dep_delay, cal_heat_data$cancelled) # 1 - Departure Delay, #2 - Cancelled
    calbuttontitles = c("Departure Delay Time (in Hours)", "Cancelled Flights")
    
            # Generates Calendar
    
    output$calendar <- renderPlot(
        
        calendarHeat(
            cal_heat_data$FL_DATE,
            calbuttonchoices[, as.integer(input$calbuttons)]
            ,
            ncolors = 99,
            color = "g2r",
            varname = calbuttontitles[as.integer(input$calbuttons)]
        )
    )
    
    ## CORRELATION HEATMAP
    
    output$corrmat <- renderPlot(corr_mat)
    
    output$polarplot <- renderPlot({
      
      flights %>%
        ggplot() + 
        aes(x = MKT_UNIQUE_CARRIER, fill = MKT_UNIQUE_CARRIER) + labs(x = "", y = "Number of Flights") +
        ggtitle("Number of Domestic Flights by Carrier", subtitle = "Jan 2019 - Jun 2020") +
        geom_bar() + scale_y_continuous(labels = comma_format()) + coord_polar() +
        scale_fill_hue(aes(title = "Airline"), labels = c("American Airlines", "Alaska Airlines", 
                                                          "JetBlue", "Delta Air Lines", "Frontier Airlines", 
                                                          "Allegiant Air", "Hawaiian Airlines", "Spirit Airlines", 
                                                          "United Airlines", "Southwest Airlines")) +
      theme_minimal() + theme(plot.title = element_text(size = 20),
                              axis.title = element_text(size = 20),
                              axis.text = element_text(size = 16),
                              legend.title = element_text(size = 14),
                              legend.text = element_text(size = 14))
      
    }, height = 700, width = 870)
    
    
##---------------------------INTERACTIVE COVID PLOTS ## ------------------------
    
    ## VALUE BOXES
    
    noncovid_numflights <- 
      
      valueBox(subtitle = HTML("<b>Total Number of Domestic Airline Flights (Prior to COVID-19)</b>"),
               value = format(noncovid_df[1], big.mark = ","),
               icon = icon("plane"),
               color = "green")
    
    covid_numflights <- 
      
      valueBox(subtitle = HTML("<b>Total Number of Domestic Airline Flights (During COVID-19)</b>"),
               value = format(covid_df[1], big.mark = ","),
               icon = icon("plane"),
               color = "maroon")
    
    numflights_valueboxes <- reactive({
      
      if (input$switch == TRUE) {
        noncovid_numflights
      } else if (input$switch == FALSE) {
        covid_numflights
      }
      
    })
    
    output$numflights_valueboxes <- renderValueBox({
      numflights_valueboxes()
    })
    
    ## VALUE BOX 2
    
    noncovid_delflights <- 
      
      valueBox(subtitle = HTML("<b>Number of Delayed Airline Departures (Prior to COVID-19)</b>"),
               value = format(noncovid_df[2], big.mark = ","),
               icon = icon("plane-departure"),
               color = "green")
    
    covid_delflights <- 
      
      valueBox(subtitle = HTML("<b>Number of Delayed Airline Departures (During COVID-19)</b>"),
               value = format(covid_df[2], big.mark = ","),
               icon = icon("plane-departure"),
               color = "maroon")
    
    delflights_valueboxes <- reactive({
      
      if (input$switch == TRUE) {
        noncovid_delflights
      } else if (input$switch == FALSE) {
        covid_delflights
      }
      
    })
    
    output$delflights_valueboxes <- renderValueBox({
      delflights_valueboxes()
    })
    
    ## VALUE BOX 3
    
    noncovid_delpctflights <- 
      
      valueBox(subtitle = HTML("<b>Percent of Delayed Airline Departures (Prior to COVID-19)</b>"),
               value = format(noncovid_df[3], big.mark = ","),
               icon = icon("percent"),
               color = "green")
    
    covid_delpctflights <- 
      
      valueBox(subtitle = HTML("<b>Percent of Delayed Airline Departures (During COVID-19)</b>"),
               value = format(covid_df[3], big.mark = ","),
               icon = icon("percent"),
               color = "maroon")
    
    delpctflights_valueboxes <- reactive({
      
      if (input$switch == TRUE) {
        noncovid_delpctflights
      } else if (input$switch == FALSE) {
        covid_delpctflights
      }
      
    })
    
    output$delpctflights_valueboxes <- renderValueBox({
      delpctflights_valueboxes()
    })
    
    ## VALUE BOX 4
    
    noncovid_cancelflights <- 
      
      valueBox(subtitle = HTML("<b>Total Number of Cancelled Domestic Flights (Prior to COVID-19)</b>"),
               value = format(noncovid_df[4], big.mark = ","),
               icon = icon("plane-slash"),
               color = "green")
    
    covid_cancelflights <- 
      
      valueBox(subtitle = HTML("<b>Total Number of Cancelled Domestic Flights (During COVID-19)</b>"),
               value = format(covid_df[4], big.mark = ","),
               icon = icon("plane-slash"),
               color = "maroon")
    
    cancelflights_valueboxes <- reactive({
      
      if (input$switch == TRUE) {
        noncovid_cancelflights
      } else if (input$switch == FALSE) {
        covid_cancelflights
      }
      
    })
    
    output$cancelflights_valueboxes <- renderValueBox({
      cancelflights_valueboxes()
    })
    
    ## VALUE BOX 5
    
    noncovid_cancelpctflights <- 
      
      valueBox(subtitle = HTML("<b>Percent of Cancelled Airline Flights (Prior to COVID-19)</b>"),
               value = format(noncovid_df[5], big.mark = ","),
               icon = icon("percent"),
               color = "green")
    
    covid_cancelpctflights <- 
      
      valueBox(subtitle = HTML("<b>Percent of Cancelled Airline Flights (During COVID-19)</b>"),
               value = format(covid_df[5], big.mark = ","),
               icon = icon("percent"),
               color = "maroon")
    
    cancelpctflights_valueboxes <- reactive({
      
      if (input$switch == TRUE) {
        noncovid_cancelpctflights
      } else if (input$switch == FALSE) {
        covid_cancelpctflights
      }
      
    })
    
    output$cancelpctflights_valueboxes <- renderValueBox({
      cancelpctflights_valueboxes()
    })
    #------------------
    
    #Selects Pre/Post-COVID Dates
    
    dates <- cbind(c("2019-01-01", "2019-06-30"), c("2020-01-01", "2020-06-30"))
        
    observe({
        #Edits destination based on origin input
        dest <- sort(unique(
            flights %>% 
                filter(flights$ORIGIN_CITY_NAME == input$origin) %>%
                .$DEST_CITY_NAME 
        ))
        
        updateSelectizeInput(session,
                             "dest",
                             choices = dest,
                             selected = dest[1])
    })
    
    flights_delay <- reactive({
      
        flights %>%
            filter(ORIGIN_CITY_NAME == input$origin &
                       DEST_CITY_NAME == input$dest &
                  FL_DATE >= ymd(input$dates[1]) & FL_DATE <= ymd(input$dates[2])) %>%
                       filter(!(CANCELLATION_CODE %in% "")) %>%
               
            group_by(MKT_UNIQUE_CARRIER) %>%
            summarise(
                n = n(),
                departure = mean(DEP_DELAY),
                arrival = mean(ARR_DELAY)
            )
        
    })
    
    ## Creates Bar Chart Output
    
    output$barchart <- renderPlot(
        
      flights_delay() %>%
        
        mutate(MKT_UNIQUE_CARRIER = fct_reorder(MKT_UNIQUE_CARRIER, desc(n))) %>% 
        ggplot(aes(x = MKT_UNIQUE_CARRIER, y = n, fill = MKT_UNIQUE_CARRIER)) +
        labs(x = 'Airline', y = 'Number of Flights') +
        geom_bar(stat = "identity") +
        theme(axis.title = element_text(size = 14),
              axis.text = element_text(size = 12))
      
    )
    
    noncovidboxplot <- flights %>%
                      filter(FL_DATE >= "2019-01-01" & 
                                 FL_DATE <= "2019-06-30") %>%
                      filter(!(CANCELLATION_CODE %in% 
                                   "")) %>%
                      ggplot() +
                      aes(x = "", y = DEP_TIME, fill = MKT_UNIQUE_CARRIER) +
                      geom_boxplot() +
                      scale_fill_hue(labels = c("American Airlines", "Alaska Airlines", 
                                                "JetBlue", "Delta Air Lines", "Frontier Airlines", 
                                                "Allegiant Air", "Hawaiian Airlines", "Spirit Airlines", 
                                                "United Airlines", "Southwest Airlines")) +
                      labs(x = 'Airline', y = "Departure Time (Military)", 
                           title = "PRE-COVID (Jan 2019 - June 2019)", 
                           fill = "Airline") +
                      theme_minimal() + theme(axis.title.x = element_text(size = 20),
                                              axis.text.x = element_text(size = 16), 
                                              axis.title.y = element_text(size = 20),
                                              axis.text.y = element_text(size = 10),
                                              legend.title = element_text(size = 20),
                                              legend.text = element_text(size = 14))

    covidboxplot <- flights %>%
                      filter(FL_DATE >= "2020-01-01" & 
                               FL_DATE <= "2020-06-30") %>%
                      filter(!(CANCELLATION_CODE %in% 
                                 "")) %>%
                      ggplot() +
                      aes(x = "", y = DEP_TIME, fill = MKT_UNIQUE_CARRIER) +
                      geom_boxplot() +
                      scale_fill_hue(labels = c("American Airlines", "Alaska Airlines", 
                                                "JetBlue", "Delta Air Lines", "Frontier Airlines", 
                                                "Allegiant Air", "Hawaiian Airlines", "Spirit Airlines", 
                                                "United Airlines", "Southwest Airlines")) +
                      labs(x = 'Airline', y = "Departure Time (Military)", 
                           title = "COVID (Jan 2020 - June 2020)", 
                           fill = "Airline") +
                      theme_minimal() + theme(axis.title.x = element_text(size = 20),
                                              axis.text.x = element_text(size = 16), 
                                              axis.title.y = element_text(size = 20),
                                              axis.text.y = element_text(size = 10),
                                              legend.title = element_text(size = 20),
                                              legend.text = element_text(size = 14))

    ## Creates Reactive Boxplot Output
    
    boxplots <- reactive({
      
      if (input$switch == TRUE) {
        noncovidboxplot
      } else if (input$switch == FALSE) {
        covidboxplot
      }
      
    })
    
    output$boxplot <- renderPlot({
      boxplots()
    })
 
    
    ## Delay density plots
    
    noncovid_delaydensity <- flights %>%
      filter(FL_DATE >= "2019-01-01" & FL_DATE <= "2019-06-30") %>%
      filter(!(CANCELLATION_CODE %in% 
                 "")) %>%
      ggplot() + aes(x = DEP_DELAY_NEW, fill = MKT_UNIQUE_CARRIER) +
      geom_density(alpha = 0.8,
                   adjust = 1L) +
      facet_grid(MKT_UNIQUE_CARRIER ~ ., scales = "free") +
      labs(x = 'Flight Delay [log(Minutes)]', y = 'Density', 
           title = "PRE-COVID (Jan 2019 - June 2019)") +
      labs(fill = 'Airline') +
      scale_fill_hue(labels = c("American Airlines", "Alaska Airlines", 
                                "JetBlue", "Delta Air Lines", "Frontier Airlines", 
                                "Allegiant Air", "Hawaiian Airlines", "Spirit Airlines", 
                                "United Airlines", "Southwest Airlines")) +
      scale_x_continuous(trans = "log") +
      theme_minimal() +
      theme(
        axis.title.x = element_text(size = 20),
        axis.text.x = element_text(size = 16), 
        axis.title.y = element_text(size = 20),
        axis.text.y = element_text(size = 10),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 14),
        legend.position = 'left'
      )
    
    covid_delaydensity <- flights %>%
      filter(FL_DATE >= "2020-01-01" & FL_DATE <= "2020-06-30") %>%
      filter(!(CANCELLATION_CODE %in% 
                 "")) %>%
      ggplot() + aes(x = DEP_DELAY_NEW, fill = MKT_UNIQUE_CARRIER) +
      geom_density(alpha = 0.8,
                   adjust = 1L) +
      facet_grid(MKT_UNIQUE_CARRIER ~ ., scales = "free") +
      labs(x = '[log(Minutes)]', y = 'Density', 
           title = "COVID (Jan 2020 - June 2020)") +
      labs(fill = 'Airline') +
      scale_fill_hue(labels = c("American Airlines", "Alaska Airlines", 
                                "JetBlue", "Delta Air Lines", "Frontier Airlines", 
                                "Allegiant Air", "Hawaiian Airlines", "Spirit Airlines", 
                                "United Airlines", "Southwest Airlines")) +
      scale_x_continuous(trans = "log") +
      theme_minimal() +
      theme(
        axis.title.x = element_text(size = 20),
        axis.text.x = element_text(size = 16), 
        axis.title.y = element_text(size = 20),
        axis.text.y = element_text(size = 10),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 14),
        legend.position = 'left'
      )
    
    ## Reactive density plots
    
    delaydensities <- reactive({
      
      if (input$switch == TRUE) {
        noncovid_delaydensity
      } else if (input$switch == FALSE) {
        covid_delaydensity
      }
      
    })
    
    output$delaydensity <- renderPlot({
      delaydensities()
    })
}

shinyApp(ui, server)
