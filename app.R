library(shinydashboard)



dates <- cbind(c("2019-01-01", "2019-06-30"), c("2020-01-01", "2020-06-30"))


ui <- dashboardPage(
    
    dashboardHeader(
        
        title = "COVID-19 Flights"
       
    ),
    
    dashboardSidebar(sidebarMenu(
        
        menuItem(
            "Introduction",
            tabName = "intro",
            icon = icon("dashboard")
        ),
        
        menuItem(
            "Summary Statistics",
            tabName = "summary",
            icon = icon("table")
        ),
        
        menuItem(
            "Impacted Flight Analysis", 
            tabName = "widgets", 
            icon = icon("th"))
        
    )),
    
#-------------------------------#TAB CONTENTS#----------------------------------
    
    dashboardBody(tabItems(
        
        # First tab content
        tabItem(tabName = "intro",
                h2("COVID-19 Flight Visualization Project")
                ),
        
        # Second tab content
        tabItem(tabName = "summary",
                
                fluidRow(
                    
                    mainPanel(plotOutput("heatdelay")),
                    
                    box(
                        title = "Controls",
                        sliderInput("slider", "Number of observations:", 1, 100, 50)
                         )
                    
                        ), # END OF FIRST FLUID ROW
                
                radioButtons("calbuttons", label = h3("Category"),
                             choices = list("Departure Delays > 15 min" = 1, 
                                            "Cancelled Flights" = 2), 
                             selected = 1),
                hr(),
                
                mainPanel(plotOutput("calendar"))
                
                ),
                
        # Third tab content
        tabItem(
            
            tabName = "widgets",
            h2("Pre-COVID-19 and COVID-19 Flight Statistics"),
            
            sidebarPanel(
                
                selectizeInput(
                    inputId = "origin",
                    label = "Departure Airport",
                    choices = unique(flights$ORIGIN_CITY_NAME)
                ),
                
                selectizeInput(
                    inputId = "dest",
                    label = "Arrival Airport",
                    choices = unique(flights$DEST_CITY_NAME)
                ),
                
                dateRangeInput(inputId ="dates",
                               label = h3("Date range")),
                hr()
                #fluidRow(column(4, verbatimTextOutput("value")))
            ),
            
            mainPanel(plotOutput("count")), #Plots delay bar chart
            
            #Adds buttons for boxplot
            radioButtons("boxbuttons", label = h3("Category"),
                         choices = list("Pre-COVID" = 1, 
                                        "COVID" = 2), 
                         selected = 1),
            hr(),
            fluidRow(column(3, verbatimTextOutput("value"))),
            
            #Plots boxplot
            mainPanel(plotOutput("boxplot"))
            
        )
        
        
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
  output$heatdelay <- renderPlot( 
              
                ggplot(sum_delays, aes(FL_DATE, MKT_UNIQUE_CARRIER)) + geom_tile(aes(fill = count)) +
                theme_minimal() +
                labs(x = "Flight Date", y = "Airline", title = "Domestic Flight Delay Time (Jan 2019 - Jun 2020)") +
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
        summarise(count = sum(DEP_DELAY_NEW, na.rm = T)) %>%
        as.data.frame()
    
    ## Button inputs for Calendar
    #choice = cbind(flights$DEP_DEL15, flights$CANCELLED) # 1 - DEP_DEL15, #2 - Cancelled
    
    output$calendar <- renderPlot( 
        
        calendarHeat(cal_heat_data$FL_DATE, cal_heat_data$count #choice[, as.integer(input$calbuttons)]
                     , ncolors = 99,
                     color = "g2r", varname="Flight Cancellations")
    )
    
    
    
    
##--------------------------- iNTERACTIVE PLOTS ## -----------------------------
    #Selects Pre/Post-COVID Dates
    
    dates <- cbind(c("2019-01-01", "2019-06-30"), c("2020-01-01", "2020-06-30"))
        
    observe({
        #Edits destination based on origin input
        dest <- unique(
            flights %>%
                filter(flights$ORIGIN_CITY_NAME == input$origin) %>%
                .$DEST_CITY_NAME
        )
        updateSelectizeInput(session,
                             "dest",
                             choices = dest,
                             selected = dest[1])
    })
    
    # Future edit to restrict dates
    
    flights_delay <- reactive({
        flights %>%
            filter(ORIGIN_CITY_NAME == input$origin &
                       DEST_CITY_NAME == input$dest &
                  FL_DATE >= ymd(input$dates[1]) & FL_DATE <= ymd(input$dates[2])) %>%
                       filter(!(CANCELLATION_CODE %in% 
                                    "")) %>%
               
                 
            group_by(MKT_UNIQUE_CARRIER) %>%
            summarise(
                n = n(),
                departure = mean(DEP_DELAY),
                arrival = mean(ARR_DELAY)
            )
        
    })
    
    output$count <- renderPlot(
        
        flights_delay() %>%
            ggplot(aes(x = MKT_UNIQUE_CARRIER, y = n, fill = MKT_UNIQUE_CARRIER)) +
            geom_col(fill = "lightblue") +
            ggtitle("Number of flights")
    )
   
    
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
        labs(x = 'Airlines', y = "Departure Time (Military)", title = "Departure Times by Airline", 
             subtitle =  c("Jan 2019 - June 2019", "Jan 2020 - June 2020")[as.integer(input$boxbuttons)]
             , fill = "Airline") +
        theme_minimal()
    )
    

}

shinyApp(ui, server)
