
server <- function(input, output) {
  
  ##### WORLD MAP
  output$race_map <- renderPlotly({
    for_map2 <- for_map2 %>%
      dplyr::filter(., year == input$year_races)
    for_map2<-for_map2 %>%
      plot_ly(
        lat = ~lat,
        lon = ~lng,
        marker = list(color = "blue"),
        type = 'scattermapbox',
        mode = 'markers',
        hovertext = ~name.x)
    for_map2<-for_map2 %>%
      layout(
        mapbox = list(
          style = 'open-street-map',
          zoom = 1.6,
          center = list(lon = -5, lat = 53)))
    
    
  })
  
  output$mini_table = DT::renderDataTable({datatable((
    unique(race_stats_final %>% filter(., year == input$year_races) %>% select('race'))),
    rownames = FALSE, options = list(searching = FALSE))
    
  })
  ##### RACE STANDINGS TABLE
  output$race_standings_table = DT::renderDataTable({unique(race_stats_final %>% 
                                                              filter(year == input$year_table1, race_stats_final['race'] == .env$input$gp_name_table1) %>%
                                                              select('grid', 'position', 'points', 'laps','fastestLapTime', 'fastestLapSpeed', 'forename','surname', 'driver_nationality', 'car_name', 'status') %>% 
                                                              mutate(fastestLapTime=round(fastestLapTime/60000,2),fastestLapSpeed=round(fastestLapSpeed))%>%
                                                              rename(c(Grid=grid,Position=position,Points=points,Laps=laps,'Fastest Lap Time (mins)'=fastestLapTime,'Fastest Lap Speed (mph)'=fastestLapSpeed,'First Name'=forename,'Last Name'=surname, Nationality=driver_nationality, Constructor=car_name, Status=status)))
  })
  
  output$max_speed <- renderUI({
    
    m_speed = max((race_stats_final %>% filter(year == input$year_table1, race_stats_final['race'] ==input$gp_name_table1) %>% select('fastestLapSpeed')), na.rm=TRUE)
    
    infoBox(paste("Max Speed "),  m_speed,
            icon = icon("angle-double-up"), color = 'black', width = 8)
    
  })
  
  
  output$fastest_lap_time <- renderUI({
    fast_lap = race_stats_final %>% filter(., year == input$year_table1, race_stats_final['race'] == input$gp_name_table1, na.rm=TRUE) %>% select('fastestLapTime') %>%
      top_n(-1, fastestLapTime)
    
    infoBox(paste("Fastest Lap (ms)"), fast_lap$fastestLapTime[1],
            icon = icon("angle-double-up"), color = 'black', width = 8)
    
  })
  
  output$gps_held <- renderUI({
    infoBox(paste("GPs Held"),
            count(unique(race_stats_final %>% filter(race_stats_final['race'] == input$gp_name_table1) %>% select("year"))), # IF 2018 - count < 0, output 0, else count
            icon = icon("angle-double-up"), color = 'blue', width = 8)
    
  })
  
  
  output$dynamic_widget = renderUI({
    selectizeInput('gp_name_table1', label = "Grand Prix Name", choices = gp_name_table1_choices())
  })
  
  gp_name_table1_choices = reactive({
    sort(unique(filter(race_stats_final, year == input$year_table1)$race))
  })
  
  ##########################
  output$gp_name_animation = renderUI({
    selectizeInput('gp_name_animation', label = "Grand Prix Name", choices = gp_name_animation_choices())
  })
  
  gp_name_animation_choices = reactive({
    sort(unique(filter(race_and_laps, year == input$year_animation)$gp_name))
  })
  
  output$race_animation <- renderPlotly({
    gg <- ggplot(animation_data(), aes(x = lap, y = position, color = driver_name)) +
      geom_point(aes(frame = lap))
    ggplotly(gg, height = 600, width=1000, tooltip = "driver_name")
  })
  
  # Driver's infor -Driver
  output$driver_name=renderText({ input$driver_choices })
  
  driver_info=reactive({ 
    champions%>%filter(Driver.Name == input$driver_choices)
  })
  
  output$dynamic_widget_2 = renderUI({
    selectizeInput('gp_name_table2', label = "Grand Prix Name", choices = gp_name_table2_choices())
  })
  
  gp_name_table2_choices = reactive({
    # sort(unique((race_stats_final%>%filter(driver_name == input$driver_choices))$race))
    sort(unique((race_stats_final%>%filter(driver_name==input$driver_choices & race!='70th Anniversary Grand Prix'))$race))
  })
  

  output$driver_win=renderText({ 
    driver_info()$Win 
  })
  output$driver_pole=renderText({ 
    driver_info()$Pole
  })
  output$driver_nation=renderText({ 
    driver_info()$Nationality
  })
  output$driver_title=renderText({ 
    driver_info()$Titles 
  })
  output$driver_fatestlap=renderText({ 
    driver_info()$Fastest.Lap
  })
  output$driver_points=renderText({ 
    driver_info()$Points
  })
  output$driver_race=renderText({ 
    driver_info()$Races
  })
  output$driver_lap=renderText({ 
    driver_info()$Laps
  })
  
  
  #driver lap time -Driver
  output$driver_lap_time=renderPlotly({
    
    champions_driver=driver_2021%>%filter(driver_name==driver_info()$Driver.Name)
    lap_times_driver=lap_times%>%select(raceId,driverId,lap,milliseconds)
    races_driver=races%>%select(raceId,circuitId,name)
    lap_times_driver=lap_times_driver%>%left_join(races_driver)%>%
      filter(name == .env$input$gp_name_table2 & driverId==champions_driver$driverId)%>%
      filter(!milliseconds %in% boxplot.stats(milliseconds)$out)%>%
      group_by(driverId,lap)%>%
      summarise(time=mean(milliseconds))
    
    gg<-ggplot(lap_times_driver,aes(x=lap,y=time))+
      geom_line(show.legend = FALSE,color="blue", size=1)+
      xlab('Lap') +ylab('Qualifying Time (MS)')+ theme_classic()
    
    
  })
  
  #driver total -Driver
  output$driver_perf_overyear=renderPlotly({
    
    champions_driver=driver_2021%>%filter(driver_name==driver_info()$Driver.Name)
    driver_perf_overyear<-driver_standings%>% left_join(races)
    driver_perf_overyear=driver_perf_overyear%>%group_by(driverId,name,year)%>%
      summarise(totalpoints=sum(points))%>%
      filter(name == input$gp_name_table2 & driverId==unique(champions_driver$driverId)) 
      a=ggplot(driver_perf_overyear, aes(x=factor(year), y=totalpoints, group = 1, color="red2"))+
        geom_point()+
        geom_line()+
        xlab('Season')+
        ylab('Total Points')+
        expand_limits(y=0)+
        theme_classic()+
        theme(legend.position = "none")
    
  })
  
  # Drivers page, data table
  output$drivers_table = DT::renderDataTable({champions_d
  })
  
  # Championships chart - Driver
  output$champ_counts = renderPlotly(champ_counts
  )
  # Country wins chart - Driver
  output$country_wins = renderPlotly(country_wins
  )
  
  
  # Constructors page, data table
  output$constructors_table = DT::renderDataTable({const_champs_name
  })
  
  # Championships chart - Constructor
  output$const_counts = renderPlotly(const_counts
  )
  # Country wins chart - Constructor
  output$country_constructor_wins = renderPlotly(country_constructor_wins
  )
  # Constructor Issues chart
  output$const_issues_chart = renderPlotly({
    gg <- ggplot(car_prob_per, aes(x=percent, y=reorder(car_name, percent), colour=car_name, fill=car_name)) +
      geom_bar(stat = "identity", show.legend = FALSE,width =0.5) +
      xlab('Failure Percentage') +
      ylab('Constructor') +
      theme_classic() 
    ggplotly(gg, height = 550, width=900, tooltip = NULL)
  })
  
  # Popular Circuits chart
  output$pop_cir_chart = renderPlotly({
    gg <- ggplot(circuit_data, aes(x=year, y=reorder(circuitRef,n),colour=factor(circuitRef))) +
          geom_point(shape='|',show.legend = FALSE,size=1) +
          xlab('Season') +
          theme_classic() +
          theme(legend.position = "none", axis.title.y = element_blank())
    ggplotly(gg, height = 550, width=900, tooltip = NULL)
  })
  
  # repeat wins driver bar chart - Circuit
  output$repeatwins_driver = renderPlotly({ 
    circuit_repeatwins_drivers<-race_stats_final%>% 
      select (raceId,year,circuitId,statusId,driverId,driver_name,fastestLapTime,fastestLapSpeed,status,rank,race) %>%
      filter( race==input$circuit_choices & rank ==1 & statusId==1) 
    
    
    circuit_repeatwins_drivers<-circuit_repeatwins_drivers %>% distinct()%>%
      group_by(driverId,driver_name)%>%
      summarize(count=n(),.groups=NULL)%>%
      filter(count>=1)%>%
      arrange(desc(count))
    
    circuit_repeatwins_drivers<-
      ggplot(circuit_repeatwins_drivers,mapping=aes(x=reorder(driver_name,count),y=count,fill=driver_name))+
      geom_bar(stat='identity',width = 0.3)+
      coord_flip()+
      labs(y= "Wins", x = "Driver")+ 
      theme_classic()+
      theme(legend.position = "none", axis.title.y = element_blank())
    ggplotly(circuit_repeatwins_drivers, tooltip = NULL)
  })
  
  
  # repeat wins constructor bar chart - Circuit
  output$repeatwins_constructor = renderPlotly({ 
    circuit_repeatwins_constructors<-race_stats_final%>% 
      select (raceId,year,constructorId,constructorRef,circuitId,statusId,driverId,forename,fastestLapTime,fastestLapSpeed,status,rank,race) %>%
      filter( race==input$circuit_choices & rank ==1 & statusId==1) 
    
    
    circuit_repeatwins_constructors <-circuit_repeatwins_constructors %>% distinct()%>%
      group_by(constructorId,constructorRef)%>%
      summarize(count=n())%>%
      filter(count>=1)%>%
      arrange(desc(count))
    
    
    circuit_repeatwins_constructorsr<-
      ggplot(circuit_repeatwins_constructors,mapping=aes(x=reorder(constructorRef,count),y=count,fill=constructorRef))+
      geom_bar(stat='identity',width = 0.3)+
      coord_flip()+
      labs(y= "Wins", x = "Constructor")+
      theme_classic()+
      theme(legend.position = "none", axis.title.y = element_blank())
    ggplotly(circuit_repeatwins_constructorsr, tooltip = NULL)
  })
  
  # issue tree map - Circuit
  output$status = renderPlotly({ 
    data<-race_stats_final %>% 
      select (circuitId,statusId,status,race) %>%
      filter( status==input$status_choices) %>%
      group_by(race)%>%
      summarise(n=n())%>%
      arrange(desc(n))
    #if(nrow(data) > 1){
      ramp4 <- colorRamp(c("red3", "darkblue"))
      ramp.list4 <- rgb( ramp4(seq(0, 1, length = 50)), max = 255)
      
      fig <- data %>% plot_ly(
        type='treemap',
        parents=~NA,
        values=~n,
        labels=data$race,
        domain=list(column=0))
      
      status <- fig %>% layout(title=input$status_choices,colorway=ramp.list4)
    #}else{}
    
  })
  
  
  #circuit info card -circuit
  output$valuebox1=renderValueBox({
    valueBox(
      subtitle = tags$p("73", style = "font-size: 100%;"),
      value = tags$p("No of Circuits", style = "font-size: 45%;"),
      color = 'red'
    ) 
  })
  
  output$valuebox2=renderValueBox({
    valueBox(
      subtitle = tags$p("Spa-Francorchamps", style = "font-size: 100%;"),
      value = tags$p("Circuit with most Turns", style = "font-size: 45%;"),
      color = 'red'
    ) 
  })
  
  output$valuebox3=renderValueBox({
    valueBox(
      subtitle = tags$p("Pescara", style = "font-size: 100%;"),
      value = tags$p("Longest Circuit", style = "font-size: 45%;"),
      color = 'red'
    ) 
  })
  # for prediction
  output$result_prediction = DT::renderDataTable({datatable(
    prediction[,c(input$track_name_prediction,"Constructor",name)] %>% arrange(prediction[,input$track_name_prediction]))
  })
}