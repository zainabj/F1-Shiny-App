
ui <- shinyUI(
  dashboardPage(skin = "red",
                dashboardHeader(title = "FIA Formula 1"),
                
                dashboardSidebar(width = 200,
                                 sidebarUserPanel("Team 10", 
                                                  image = "https://images-na.ssl-images-amazon.com/images/I/61Ysu6NeEFL._SY355_.jpg"),
                                 sidebarMenu(
                                   menuItem("About", tabName = "about", icon = icon("book")),
                                   menuItem("Drivers", tabName = "drivers", icon = icon("id-card")),
                                   menuItem("Constructors", tabName = "constructor", icon = icon("trophy")),
                                   menuItem("Standings", tabName = "standings", icon = icon("line-chart")),
                                   menuItem("Circuits", tabName = "pop_circ", icon = icon("flag-checkered")),
                                   menuItem("Races", tabName = "races", icon = icon("globe")),
                                   menuItem("2021 Predictions", tabName = "prediction", icon = icon("line-chart"))

                                 )),
                
                dashboardBody(
                  tags$head(
                    tags$style(HTML("
                      .content-wrapper {
                      background-color: white !important;
                      }
                      .main-sidebar {
                      background-color: black !important;
                      }
                      "))
                  ),
                  tabItems(
                    tabItem(tabName = "about",
                            h2('Welcome to Formula 1 Dashboard', align = 'center'),
                            tags$p('This dashboard helps to compare drivers and constructors participating in 
                                       Formula 1 races across different countries and circuits. This dashboard also predicts the winning
                                       driver and championship for 2021 across different circuits based on number of parameters.', 
                                   style = 'font-size: 120%;margin-left:2.5em;'),
                            br(),
                            iframe(width="1120", height="630", url_link = "https://www.youtube.com/embed/k4Pegt-HcI8"),
                            br(),
                            h2('Racing Teams', align = 'center'),
                            br(),
                            fluidRow(
                              shinydashboard::box(width = 12, background = 'black',
                                                  valueBox(tags$p('Mercedes', style = 'font-size: 50%;text-align: center;'), 
                                                           div(img(src = 'mercedes.jpg', height = '160', width = '160'),style='text-align: center;'), 
                                                           icon = NULL, width = 2, color = 'black'),
                                                  valueBox(tags$p('Red Bull Racing', style = 'font-size: 50%;text-align: center;'), 
                                                           div(img(src = 'redbull.jpg', height = '160', width = '160'),style='text-align: center;'), 
                                                           icon = NULL, width = 2, color = 'black'),
                                                  valueBox(tags$p('Ferrari', style = 'font-size: 50%;text-align: center;'), 
                                                           div(img(src = 'ferrari.jpg', height = '160', width = '160'),style='text-align: center;'), 
                                                           icon = NULL, width = 2, color = 'black'),
                                                  valueBox(tags$p('McLaren', style = 'font-size: 50%;text-align: center;'), 
                                                           div(img(src = 'mclaren.jpg', height = '160', width = '160'),style='text-align: center;'), 
                                                           icon = NULL, width = 2, color = 'black'),
                                                  valueBox(tags$p('Alpine', style = 'font-size: 50%;text-align: center;'), 
                                                           div(img(src = 'alpine.jpg', height = '160', width = '160'),style='text-align: center;'), 
                                                           icon = NULL, width = 2, color = 'black'),
                                                  valueBox(tags$p('Alpha Tauri', style = 'font-size: 50%;text-align: center;'), 
                                                           div(img(src = 'alpha.png', height = '160', width = '160'),style='text-align: center;'), 
                                                           icon = NULL, width = 2, color = 'black'),
                                                  valueBox(tags$p(' ', style = 'font-size: 50%;text-align: center;'),
                                                           div(img(src = 'black.png', height = '160', width = '160'),style='text-align: center;'), 
                                                           icon = NULL, width = 2, color = 'black'),
                                                  valueBox(tags$p('Aston Martin', style = 'font-size: 50%;text-align: center;'), 
                                                           div(img(src = 'aston.jpg', height = '160', width = '160'),style='text-align: center;'), 
                                                           icon = NULL, width = 2, color = 'black'),
                                                  valueBox(tags$p('Williams', style = 'font-size: 50%;text-align: center;'), 
                                                           div(img(src = 'williams.jpg', height = '160', width = '160'),style='text-align: center;'), 
                                                           icon = NULL, width = 2, color = 'black'),
                                                  valueBox(tags$p('Alpha Romeo Racing', style = 'font-size: 50%;text-align: center;'), 
                                                           div(img(src = 'romeo.jpg', height = '160', width = '160'),style='text-align: center;'), 
                                                           icon = NULL, width = 2, color = 'black'),
                                                  valueBox(tags$p('Haas F1 Racing', style = 'font-size: 50%;text-align: center;'), 
                                                           div(img(src = 'haas.jpg', height = '160', width = '160'),style='text-align: center;'), 
                                                           icon = NULL, width = 2, color = 'black'),
                                                  valueBox(tags$p(' ', style = 'font-size: 50%;text-align: center;'),
                                                           div(img(src = 'black.png', height = '160', width = '160'),style='text-align: center;'), 
                                                           icon = NULL, width = 2, color = 'black'),
                              ))
                            
                            ),
                    
                    
                     tabItem(tabName = "drivers",
                            fluidPage(
                              mainPanel(
                                fluidRow(
                                  
                                  tabsetPanel(type = "tabs",
                                              
                                              tabPanel("2021 Driver's info",fluidRow(
                                                
                                                h3("Driver info"),
                                                fluidRow(column(width = 4, selectizeInput('driver_choices', label =  "Drivers", choices  = driver_vector, selected = 'Hamilton Lewis'))
                                                         ),
                                                fluidRow(box(title="Driver",
                                                             span(textOutput("driver_name"),
                                                                  style="font-size: 25px"),
                                                             background = "light-blue"
                                                             
                                                             
                                                )),
                                                fluidRow(
                                                  column(width=6,
                                                         box(title="Nation",
                                                             span(textOutput("driver_nation"),
                                                                  style="font-size: 30px;")),
                                                         box(title="Wins",
                                                             span(textOutput("driver_win"),
                                                                  style="font-size: 30px;")),    
                                                         
                                                         box(title="Pole",
                                                             span(textOutput("driver_pole"),
                                                                  style="font-size: 30px;")),
                                                         box(title="Races",
                                                             span(textOutput("driver_race"),
                                                                  style="font-size: 30px;"))  ), 
                                                  column(width=6,
                                                         
                                                         box(title="Points",
                                                             span(textOutput("driver_points"),
                                                                  style="font-size: 30px;")),
                                                         
                                                         
                                                         box(title="Fatest Lap",
                                                             span(textOutput("driver_fatestlap"),
                                                                  style="font-size: 30px;")),
                                                         box(title="Titles",
                                                             span(textOutput("driver_title"),
                                                                  style="font-size: 30px;")), 
                                                         box(title="Laps",
                                                             span(textOutput("driver_lap"),
                                                                  style="font-size: 30px;"))     )
                                                  
                                                ),
                                                fluidRow(
                                                  column(width = 4, uiOutput("dynamic_widget_2")),
                                                  br(),
                                                  br(),
                                                  plotlyOutput("driver_lap_time"),
                                                  plotlyOutput("driver_perf_overyear")
                                                )
                                              )),
                                              
                                              tabPanel("Summary", 
                                                       
                                                       fluidRow(
                                                         h3("Champions"),
                                                         column(width = 10, plotlyOutput("champ_counts")),
                                                         ),
                                                       fluidRow(
                                                         h3("Championship by Nationality"),
                                                         br(),
                                                         column(width = 10, plotlyOutput("country_wins"))
                                                       ),
                                                       fluidRow(DT::dataTableOutput("drivers_table"))))
                                  
                                ))), class = "span12"),
                    
                    tabItem(tabName = "constructor",
                            fluidPage(
                              mainPanel(
                                fluidRow(
                                  
                                  tabsetPanel(type = "tabs",
                                              tabPanel("Summary", 
                                                       fluidRow(
                                                         h3("Contructors Info"),
                                                         column(width = 10, plotlyOutput("const_counts")),
                                                       ),
                                                       fluidRow(
                                                         h3("Championship by Nationality"),
                                                         br(),
                                                         column(width = 10, plotlyOutput("country_constructor_wins"))
                                                       ),
                                                       fluidRow(DT::dataTableOutput("constructors_table"))),
                                              tabPanel("Constructor's issue",
                                                       fluidRow(
                                                         h3("Constructors with most issues!", align = "center")
                                                       ),
                                                       fluidRow(
                                                         column(width = 10, plotlyOutput("const_issues_chart"))
                                                       )

                                                       )
                                              
                                              )
                                  
                                ))), class = "span12"),
                    
                    tabItem(tabName = "pop_circ",
                            fluidPage(
                              mainPanel(
                                fluidRow(
                                  
                                  tabsetPanel(type = "tabs",
                                              
                                              tabPanel("Summary",
                                              fluidRow(
                                                h3("Most Popular Circuits"),
                                                column(width = 12,
                                                       plotlyOutput("pop_cir_chart"), height = 100)
                                              )),
                                              tabPanel("Repeated Winners", fluidRow(
                                                h4("Please select a circuit"),
                                                selectInput('circuit_choices', label = "Circuit", choices  = circuit_vector, selected = 'Australian Grand Prix'),
                                                h3("Repeated Winner - Constructor"),
                                                plotlyOutput("repeatwins_constructor"),
                                                h3("Repeated Winner - Driver"),
                                                plotlyOutput("repeatwins_driver"))),
                                              tabPanel("Issue by Circuit", fluidRow(
                                                h4("Please select an issue"),
                                                selectInput('status_choices', label='Status',choices=status_vector,selected='Collision'),
                                                plotlyOutput("status"))),
                                              
                                              tabPanel("Circuit Lineup", fluidRow(
                                                h3("Circuit Metrics"),
                                                valueBoxOutput('valuebox1'),
                                                valueBoxOutput("valuebox2"),
                                                valueBoxOutput("valuebox3")
                                              ),fluidRow(
                                                div(img(src = 'circuits.png'),style='text-align: center;')
                                                ))
                                              
                                  )
                                ))), class = "span12"),
                    
                    
                    # tabItem(tabName = "race_animation", 
                    #         fluidPage(
                    #           h4("Please Select a F1 Season & Grand Prix Name"),
                    #           # fluidRow(column(width = 4, selectizeInput('year_animation', label = "F1 Season", choices = year_vector_animation)),
                    #           #           column(width = 4, uiOutput("gp_name_animation"))),
                    #           h5("Even if you see an error, please be patient, animation is loading!"),
                    #           plotlyOutput("race_animation")
                    #           
                    #         )),
                    

                    tabItem(tabName = "standings", 
                            fluidPage(
                              h1("Grand Prix Standings", align = "center"),
                              br(),
                              h6("Please Select a Season & Grand Prix", align = "center"),
                              br(),
                              fluidRow(column(width = 4, selectizeInput('year_table1', label = "F1 Season", choices = year_vector)),
                                       column(width = 4, uiOutput("dynamic_widget"))),
                              br(),
                              fluidRow(
                                column(4,
                                       uiOutput("max_speed")
                                ),
                                
                                column(4,
                                       uiOutput("gps_held")
                                ),
                                
                                column(4, 
                                       uiOutput("fastest_lap_time")
                                )),
                              
                              
                              fluidRow(DT::dataTableOutput("race_standings_table"))
                              
                            )),
                    
                    tabItem(tabName = "races",
                            h2('Formula 1 Races Schedule Throughout the History'),
                            fluidRow(column(width = 1),
                                     column(width = 2, title = "Select a Formula 1 Season", solidHeader = TRUE, status = "primary",
                                            selectInput(inputId = "year_races", label = '', choices = sort(unique(for_map2$year)),
                                                        selected = NULL, multiple = FALSE),
                                            DT::dataTableOutput("mini_table")),
                                     column(width = 9, plotlyOutput("race_map", height = 700), solidHeader = TRUE, status = "primary")
                            )
                    ),
                    tabItem(tabName = "prediction",
                            fluidPage(
                              h1("Grand Prix Prediction", align = "center"),
                              br(),
                              h3("Please Select Grand Prix", align = "center"),
                              br(),
                              fluidRow(column(width = 4, selectizeInput('track_name_prediction', label = "Grand Prix", choices = as.character(race_list))),
                                       column(width = 4, uiOutput("track_name_prediction"))),
                              br(),
                              fluidRow(DT::dataTableOutput("result_prediction"))
                              
                            ))
                    
                  )       
                )
  ))