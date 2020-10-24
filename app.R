library(shinydashboard)
library(dashboardthemes) # https://github.com/nik01010/dashboardthemes
library(shinyWidgets)

dates <- cbind(c("2019-01-01", "2019-06-30"), c("2020-01-01", "2020-06-30"))


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
            tabName = "placeholder",
            icon = icon("table"),
            
            menuSubItem("Overall Flight Statistics", tabName = "summary"),
            menuSubItem("General Flight Information", tabName = "summary2")
        ),
        
        menuItem(                       ## Third Menu Item in Sidebar
            "COVID-19 Flight Analysis", 
            tabName = "widgets", 
            icon = icon("th"),
        
            menuSubItem("COVID-19 Visualizations", tabName = "covid"),
            menuSubItem("More COVID-19 Visualizations", tabName = "covid2")
        
    ))),
    
#-------------------------------#TAB CONTENTS#----------------------------------
    
    dashboardBody(shinyDashboardThemes(theme = "purple_gradient"),
      
      tabItems(
        
  # First tab content-----------------------------------
  
        tabItem(
                tabName = "intro",
                h2("COVID-19 Flight Visualization Project")
                ),
        
  # Second tab content----------------------------------
  
        tabItem(tabName = "summary", # Corresponds to summary menu sub item
                
                fluidRow(
                  
                  box(title = "Flight Delay Time by Domestic US Airline (Jan 2019 - Jun 2020)",
                    width = 6, status = "primary", mainPanel(plotOutput("heatdelay"), width = 15)
                    ),
                  
                  box(title = "Elapsed Flight Time by Domestic US Airline (Jan 2019 - Jun 2020)",
                    width = 6, status = "warning", mainPanel(plotOutput("timedensity"), width = 15)
                  )
                ),
                    
                    #box(
                    #    title = "Controls",
                    #    sliderInput("slider", "Number of observations:", 1, 100, 50)
                    #     )
                
                fluidRow( 
                  column(12, align = "center", box(  # FIGURE OUT HOW TO CENTER
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
                
  # Third tab content----------------------------------
  
        tabItem(tabName = "summary2",
            
                    #radioGroupButtons(inputId = "pandemicbutton", label = "Start Pandemic?", 
                            #choices = c("NO","YES"), justified = TRUE, size = "xs",
                            #status = "success"),
            
            h2("Pre-COVID-19 and COVID-19 Flight Statistics"),
            
            sidebarPanel(
                
                selectizeInput(
                    inputId = "origin",
                    label = "Departure Airport",
                    choices = sort(unique(flights$ORIGIN_CITY_NAME))
                ),
                
                selectizeInput(
                    inputId = "dest",
                    label = "Arrival Airport",
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
            
            box(title = "Number of Flights",
                mainPanel(plotOutput("barchart")), #Plots delay bar chart
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
            
            box(
              plotOutput("corrmat")
            )
            
        ),
  
          tabItem(tabName = "covid", 
                  
                  switchInput("switch", label = "Start a Pandemic?", size = "large", 
                      onLabel = "No", offLabel = "Yes", onStatus = "success", 
                      offStatus = "danger", value = TRUE),
                  
                  #Adds buttons for boxplot
                  
                  prettyRadioButtons("boxbuttons", label = h3("Category"),
                                     status = "info",
                                     animation = "pulse", 
                                     choices = list("Pre-COVID (Jan - June 2019)" = 1, 
                                                    "COVID (Jan - June 2020)" = 2), 
                                     selected = 1),
                  hr(),
                  fluidRow(column(3, verbatimTextOutput("value"))),
                  
                  mainPanel(plotOutput("boxplot"))) #Plots boxplot
        
        
                ))
)

#-------------------------------------------------------------------------------
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
    
    #output$corrmat <- renderPlot(
                      
    #  heatmaply_cor(cor(corr_data))
      
    #  )
    
##--------------------------- iNTERACTIVE PLOTS ## -----------------------------
    
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
   
    ## Creates Boxplot Output
    
    output$boxplot <- renderPlot(
        
    flights %>%
        filter(FL_DATE >= dates[, as.integer(input$boxbuttons)][1] & 
                   FL_DATE <= dates[, as.integer(input$boxbuttons)][2]) %>%
        filter(!(CANCELLATION_CODE %in% 
                     "")) %>%
        ggplot() +
        aes(x = "", y = DEP_TIME, fill = MKT_UNIQUE_CARRIER) +
        geom_boxplot() +
        scale_fill_hue() +
        labs(x = 'Airline', y = "Departure Time (Military)", title = "Departure Times by Airline", 
             subtitle =  c("Jan 2019 - June 2019", "Jan 2020 - June 2020")[as.integer(input$boxbuttons)]
             , fill = "Airline") +
        theme_minimal()
    )
    

}

shinyApp(ui, server)
