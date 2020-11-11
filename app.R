library(shinydashboard)
library(dashboardthemes)
library(shinyWidgets)
library(rsconnect)
library(reshape2)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(knitr)
library(gganimate)
library(lubridate)
library(heatmaply)
library(ggthemes)
library(esquisse)
library(plotly)

setwd("C:\\Users\\Akul\\Desktop\\DSA\\R Visualization Project Data")

# arrange flights by date
flights <- read.csv('jan2019jun2020.csv')
flights$FL_DATE <- ymd(mdy(flights$FL_DATE))
flights <- flights %>% arrange(., FL_DATE)

source("https://raw.githubusercontent.com/iascchen/VisHealth/master/R/calendarHeat.R")

#Summing delay times for easier calculation
sum_delays <- flights %>% 
  group_by(FL_DATE, MKT_UNIQUE_CARRIER) %>% 
  summarise(count = 1/60 * sum(DEP_DELAY_NEW, na.rm = T)) %>% 
  as.data.frame()

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
  theme(axis.text.x = element_text(hjust = 1, vjust = 1, angle = 60))

#-------------------------------#FRONT END#-------------------------------------

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
    
#-------------------------------#TAB CONTENTS#----------------------------------
    
    dashboardBody(shinyDashboardThemes(theme = "purple_gradient"),
      
      tabItems( 
        
  # First tab content-----------------------------------
  
        tabItem(
                tabName = "intro",
                h2("COVID-19 Flight Visualization Project"),
                fluidRow(
                print(img(src = "https://www.globaltimes.cn/Portals/0/attachment/2020/2020-10-20/84220eb8-3383-4611-8f76-4d8fcf0df7a0.jpeg",
                      width = "50%")),
                print(img(src = "https://www.transportation.gov/themes/custom/dot_cms/images/seal_dot.png",
                      width = "45%"))
                ),
                fluidRow(
                box(
                "COVID-19 has severely crippled the global airline industry with 
                air service reductions widespread throughout 2020. This dataset 
                containing 11 million flights will aid those seeking to visualize 
                the impact that the virus has had on the domestic United States 
                airline industry through detailed flight delay and cancellation data."
                ),
                box("
                The United States Department of Transportation's (DOT) Bureau of 
                Transportation Statistics tracks the on-time performance of domestic 
                flights operated by large air carriers. The data collected is from 
                January - June 2020 (will be updated soon) and contains relevant 
                flight information (on-time, delayed, canceled, diverted flights) 
                from the Top 10 United States flight carriers for 11 million flights.

                The compiled and cleaned data is hosted on github (github.com/akulbahl)
                in 47 columns with full column descriptions in the attached .txt file.")
                )
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
            sidebarPanel(
                
                selectizeInput(
                    inputId = "origin",
                    label = h3("Departure Airport"),
                    choices = sort(unique(flights$ORIGIN_CITY_NAME))
                ),
                
                selectizeInput(
                    inputId = "dest",
                    label = h3("Arrival Airport"),
                    choices = sort(unique(flights$DEST_CITY_NAME))
                ),
                
                dateRangeInput(inputId ="dates",
                               label = h3("Date range"),
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
            box(title = "Feature Correlation Heatmap", width = 9, status = "warning",
              mainPanel(plotOutput("corrmat"), width = 9)
                )
            )
        ),

# Third tab content---------------------------------- 

        tabItem(tabName = "covid", 
                
                h2("COVID-19 Comparison Plots - Select Pandemic Button to Change Situation"),
                
                switchInput("switch", label = "Start a Pandemic?", size = "large", 
                    onLabel = "No", offLabel = "Yes", onStatus = "success", 
                    offStatus = "danger", value = TRUE),
                
                hr(),
                fluidRow(
                         box(title = "Departure Times by Airline",
                             width = 9, 
                             status = "primary",
                             mainPanel(plotOutput("boxplot"),
                                       width = 9),
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
                              WN: Southwest Airlines"
                             )
                        ),
                
                 #Plots boxplot
        
                fluidRow(
                  box(title = "Time Density by Domestic US Airline", 
                             width = 10, 
                             status = "warning", 
                         mainPanel(plotOutput("delaydensity"), 
                        width = 10),
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
                              WN: Southwest Airlines"
                      
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
            axis.title = element_text(size = 12),
            axis.text = element_text(size = 8),
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
                                           4000.001, max(sum_delays$count))))
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
    
##---------------------------INTERACTIVE COVID PLOTS ## ------------------------
    
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
                      scale_fill_hue() +
                      labs(x = 'Airline', y = "Departure Time (Military)", 
                           title = "PRE-COVID (Jan 2019 - June 2019)", 
                           fill = "Airline") +
                      theme_minimal()

    covidboxplot <- flights %>%
                      filter(FL_DATE >= "2020-01-01" & 
                               FL_DATE <= "2020-06-30") %>%
                      filter(!(CANCELLATION_CODE %in% 
                                 "")) %>%
                      ggplot() +
                      aes(x = "", y = DEP_TIME, fill = MKT_UNIQUE_CARRIER) +
                      geom_boxplot() +
                      scale_fill_hue() +
                      labs(x = 'Airline', y = "Departure Time (Military)", 
                           title = "COVID (Jan 2020 - June 2020)", 
                           fill = "Airline") +
                      theme_minimal()

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
      scale_fill_hue() +
      scale_x_continuous(trans = "log") +
      theme_minimal() +
      theme(
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
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
      scale_fill_hue() +
      scale_x_continuous(trans = "log") +
      theme_minimal() +
      theme(
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
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
