library(plotly)
library(dplyr)
library(shiny)
library(shinydashboard)
library(DT)
library(shinyWidgets)
library(RSQLite)
library(shinyLP)

#setwd("C:/Users/zaina/OneDrive/Desktop/R Final Project")

circuits<- read.csv(file = 'circuits.csv')
constructor_results<- read.csv(file = 'constructor_results.csv')
constructor_standings<- read.csv(file = 'constructor_standings.csv')
constructors<- read.csv(file = 'constructors.csv')
driver_standings<- read.csv(file = 'driver_standings.csv')
drivers<- read.csv(file = 'drivers.csv')
lap_times<- read.csv(file = 'lap_times.csv')
pit_stops<- read.csv(file = 'pit_stops.csv')
qualifying<- read.csv(file = 'qualifying.csv')
races<- read.csv(file = 'races.csv')
results<- read.csv(file = 'results.csv')
seasons<- read.csv(file = 'seasons.csv')
status<- read.csv(file = 'status.csv')

#conn = dbConnect(drv = SQLite(), dbname = "./database.sqlite")

#dbListTables(conn)

##########################################################################
# standings = dbGetQuery(conn, "SELECT * FROM standings")
# races = dbGetQuery(conn, "SELECT * FROM races")
# circuits = dbGetQuery(conn, "SELECT * FROM circuits")
# for_map = dbGetQuery(conn, "SELECT * FROM races LEFT JOIN circuits ON circuits.Circuit_id = races.circuit_id" )

for_map <- races %>%
  left_join(circuits, by = "circuitId")

# constructors = dbGetQuery(conn, "SELECT * FROM constructors")
# constructor_standings = dbGetQuery(conn, "SELECT * FROM constructor_standings")
# constructors_new = dbGetQuery(conn, "SELECT * FROM constructor_standings LEFT JOIN constructors ON constructor_standings.constructor_id = constructors.constructor_id")

constructors_new <- constructor_standings %>%
  left_join(constructors, by = "constructorId")

# results = dbGetQuery(conn, "SELECT * FROM results")
# status = dbGetQuery(conn, "SELECT * FROM status")
# results_new = dbGetQuery(conn, "SELECT * FROM results LEFT JOIN status on results.status_id = status.status_id")

results_new <- results %>%
  left_join(status, by = "statusId")

# pit_stops = dbGetQuery(conn, "SELECT * FROM pit_stops")
##########################################################################

# map projection
for_map2 = for_map %>% group_by(year) %>% mutate(EndLat = lead(lat), EndLong=lead(lng))

# for standings page
# drivers = dbGetQuery(conn, "SELECT * FROM driver")
# standings_v1 = dbGetQuery(conn, "SELECT * FROM results LEFT JOIN driver ON results.driver_id = driver.driver_id")

standings_v1 <- left_join(results,drivers, by = "driverId")

# dbWriteTable(conn, name = "standings_v1", value = standings_v1)
# standings = dbGetQuery(conn, "SELECT * FROM standings_v1 LEFT JOIN constructors ON standings_v1.constructor_id = constructors.constructor_id")

standings <- standings_v1 %>% 
  left_join(constructors, by = "constructorId")

# dbWriteTable(conn, name = "standings", value = standings)
# standings = dbGetQuery(conn, "SELECT * FROM standings LEFT JOIN races ON standings.race_id = races.race_id")

standings <- standings %>%
  left_join(races, by = "raceId")

# standings
# colnames(standings)
standings <- standings[, -c(1,2,3,4,5,8,9,11,12,13,14,18,19,20,21,22,25,27,28,30,31,33,34,36)]
standings$driver_name <- paste(standings$forename, standings$surname, sep = " ")
standings <- standings[, -c(7,8)]
standings <- standings[, -c(4)]
# dbWriteTable(conn, name = "standings", value = standings, overwrite = TRUE)


#standings = dbGetQuery(conn, "SELECT * FROM standings")
#standings

####### DO NOT LOAD THESE #################
# race_stats = dbGetQuery(conn, "SELECT * FROM results LEFT JOIN driver ON results.driver_id = driver.driver_id") #delete when done
race_stats = results %>%
  left_join(drivers, by = "driverId")

# dbWriteTable(conn, name = "race_stats", value = race_stats)
# race_stats1 = dbGetQuery(conn, "SELECT * FROM race_stats LEFT JOIN constructors ON race_stats.constructor_id = constructors.constructor_id")

race_stats1 = race_stats %>%
  left_join(constructors, by = "constructorId")

# dbWriteTable(conn, name = "race_stats1", value = race_stats1)
# race_stats2 = dbGetQuery(conn, "SELECT * FROM race_stats1 LEFT JOIN races ON race_stats1.race_id = races.race_id")

race_stats2 = race_stats1 %>%
  left_join(races, by = "raceId")

# dbWriteTable(conn, name = "race_stats2", value = race_stats2)
# race_stats3 = dbGetQuery(conn, "SELECT * FROM race_stats2 LEFT JOIN status ON race_stats2.status_id = status.status_id")

race_stats3 = race_stats2 %>%
  left_join(status, by = "statusId")

# dbWriteTable(conn, name = "race_stats3", value = race_stats3)
# race_stats_final = dbGetQuery(conn, "SELECT * FROM race_stats3 LEFT JOIN pit_stops ON race_stats3.race_id = pit_stops.race_id") #THIS IS THE FINAL NEEDED

race_stats_final =  race_stats3 %>%
  left_join(pit_stops, by = "raceId")
#intersect(race_stats3$raceId,pit_stops$raceId)
write.csv(race_stats_final, file = "race_stats_final.csv")

race_stats_final = read.csv(file = "race_stats_final.csv", header = TRUE)

year_vector = sort(unique(race_stats_final$year), decreasing = TRUE)
gp_vector = sort(unique(race_stats_final$name.y))

#year_vector_animation = sort(unique(race_and_laps$year))


race_stats_final$fastestLapSpeed = as.numeric(race_stats_final$fastestLapSpeed)

#race_and_laps$driver_name <- paste(race_and_laps$name, race_and_laps$last_name, sep = " ")

# merge driver name & last name

#race_stats_final$driver_name <- paste(race_stats_final$name, race_stats_final$last_name, sep=" ")


#t = "2:30.5"
#t2="1:17.450"
#t=time_calc(t)
time_calc <- function(t) {
  
  t = as.character(t)
  minutes = as.integer(substr(t, 1, 1))
  seconds = as.integer(substr(t, 4, 5))
  milliseconds = as.integer(substr(t, 7, 10))
  new_fastest_time = minutes * 60000 + seconds * 1000 + milliseconds * 1
  return(new_fastest_time)
}

race_stats_final$fastestLapTime =time_calc(race_stats_final$fastestLapTime)

time_calc_to <- function(new_time) {
  minutes = as.integer(new_time %/% 60000)
  seconds = (new_time-minutes*60000)%/%1000
  milliseconds = (new_time-minutes*60000)-seconds*1000
  
  minutes = ifelse(minutes<10, paste0("0", as.character(minutes)), paste0(as.character(minutes)))
  seconds = ifelse(seconds<10, paste0("0", as.character(seconds)), paste0(as.character(seconds)))
  
  paste0(minutes,":",seconds,".",milliseconds)
  
}

####################### FOR DRIVERS TAB

# drivers = read.csv("drivers.csv", header = TRUE)

#library(reticulate)

#py_run_file("test.py")

champions = read.csv("championship.csv", header = TRUE)
champions_d = champions[champions$Titles != 0,]
names(champions_d) <- gsub("\\.", " ", names(champions_d))

champ_counts = plot_ly(data = champions_d,
                       x = reorder(champions_d$`Driver Name`, champions_d$Titles),
                       y = ~Titles,
                       type = "bar",
                       color = champions_d$`Driver Name`,
                       showlegend = FALSE) %>%
  layout(title = "World Champions",
         xaxis = list(title = ""),
         yaxis = list(title = ""))

####################################################

cntr = champions_d %>% group_by(Nationality) %>% summarise(wins=sum(Win))
cntr = cntr[cntr$wins != 0,]

country_wins = plot_ly(data = cntr,
                       x = reorder(cntr$Nationality,-cntr$wins),
                       y = cntr$wins,
                       type = "bar",
                       color = cntr$Nationality,
                       showlegend = FALSE) %>%
  layout(title = "Most Successful Countries in Producing Winners",
         xaxis = list(title = ""),
         yaxis = list(title = ""))

##################################################

####################### FOR Constructors TAB

const_champs = read.csv("constructor_championship.csv", header = TRUE)
const_champs_name = const_champs[const_champs$Titles != 0,]
names(const_champs_name) <- gsub("\\.", " ", names(const_champs_name))



const_counts = plot_ly(data = const_champs_name,
                       x = reorder(const_champs_name$`Constructor Name`, const_champs_name$Titles),
                       y = ~Titles,
                       type = "bar",
                       color = const_champs_name$`Constructor Name`,
                       showlegend = FALSE) %>%
  layout(title = "World Champions",
         xaxis = list(title = ""),
         yaxis = list(title = ""))

####################################################
####################################################

cntr_const = const_champs_name %>% group_by(Nationality) %>% summarise(wins=sum(Wins))
cntr_const = cntr_const[cntr_const$wins != 0,]

country_constructor_wins = plot_ly(data = cntr_const,
                                   x = reorder(cntr_const$Nationality,-cntr_const$wins),
                                   y = cntr_const$wins,
                                   type = "bar",
                                   color = cntr_const$Nationality,
                                   width = 0.3,
                                   showlegend = FALSE) %>%
  layout(title = "Most Successful Countries in Producing Winners",
         xaxis = list(title = ""),
         yaxis = list(title = ""))

##################################################

# For circuits TAB
#

rank_df = race_stats_final %>% filter(rank != "\\N")

circuit_vector = sort(unique(rank_df$name.y))
#circuit_vector = circuit_vector[! circuit_vector %in% "United States Grand Prix West"]
status_vector = sort(unique(race_stats_final$status))
status_vector=status_vector[!status_vector %in% c("+1 Lap","+2 Laps","+3 Laps","+4 Laps","+5 Laps",
                                                  "+6 Laps","+7 Laps","+8 Laps","+9 Laps","+10 Laps",
                                                  "+11 Laps","+12 Laps","+13 Laps","+14 Laps","+15 Laps",
                                                  "+16 Laps","+17 Laps","+18 Laps","+19 Laps","+20 Laps",
                                                  "+21 Laps","+22 Laps","+23 Laps","+24 Laps","+25 Laps",
                                                  "+26 Laps","+27 Laps","+28 Laps","+29 Laps","+30 Laps",
                                                  "+31 Laps","+32 Laps","+33 Laps","+34 Laps","+35 Laps",
                                                  "+36 Laps","+37 Laps","+38 Laps","+39 Laps","+40 Laps",
                                                  "+41 Laps","+42 Laps","+43 Laps","+44 Laps","+45 Laps",
                                                  "+46 Laps","+47 Laps","+48 Laps","+49 Laps","+50 Laps",
                                                  "+51 Laps","+52 Laps","+53 Laps","+54 Laps","+55 Laps",
                                                  "+56 Laps","+57 Laps","+58 Laps","+59 Laps","+60 Laps",
                                                  "+61 Laps","+62 Laps","+63 Laps","+64 Laps","+65 Laps",
                                                  "+66 Laps","+67 Laps","+68 Laps","+69 Laps","+70 Laps"
                                                  )]

##################################################
#drivers_table = datatable(drivers, class = "display")
#drivers_table

names(race_stats_final)
names(race_stats_final)[1]<-paste("index")
names(race_stats_final)[4]<-paste("driverId")
names(race_stats_final)[6]<-paste("car_number")
names(race_stats_final)[13]<-paste("time")
names(race_stats_final)[14]<-paste("time_milliseconds")
names(race_stats_final)[21]<-paste("common_car_number")
names(race_stats_final)[26]<-paste("driver_nationality")
names(race_stats_final)[30]<-paste("constructor_nationality")
names(race_stats_final)[35]<-paste("race")
names(race_stats_final)[27]<-paste("url_driver")
names(race_stats_final)[31]<-paste("url_constructor")
names(race_stats_final)[29]<-paste("car_name")
names(race_stats_final)[40]<-paste("lap_driverId")
names(race_stats_final)[43]<-paste("lap_time")
names(race_stats_final)[45]<-paste("lap_time_milliseconds")

race_stats_final$driver_name <- paste(race_stats_final$forename, race_stats_final$surname, sep=" ")

#write.csv(race_stats_final, file = "race_stats_final.csv")

##################################################
# Constructor Issues Tab
# 
car_prob <- read.csv(file = 'car_prob.csv')
 
const_stats_prob = subset(race_stats_final, status %in% car_prob$status)

total = nrow(const_stats_prob)

car_prob_per = const_stats_prob %>%
                  group_by(car_name) %>%
                  count(car_name) %>%
                  mutate(percent = (n / total) * 100) %>%
                  arrange(desc(percent)) %>%
                  filter(percent>3)


##################################################
# Drivers Tab
# 
driver_2021=race_stats_final%>%filter(year==2021)
driver_vector = sort(unique(paste0(driver_2021$forename,' ',driver_2021$surname)))

##################################################
# Popular Circuits Tab
# 
circuit_mod= races %>% left_join(circuits,by='circuitId') %>% select('circuitRef','year') %>% mutate_if(is.character,as.factor)
circuit_data = circuit_mod %>% count(circuitRef)%>% arrange(desc(n))%>% right_join(circuit_mod,by='circuitRef') %>% select('circuitRef','year','n')%>%filter(n>30)

####################################################################
# For predictions tab
#
prediction <- read.csv("prediction_results.csv", header = TRUE)
name = "Drivers"
names(prediction) <- gsub("\\.", " ", names(prediction))
names(prediction)[1]="Drivers"


race_list <- names(prediction)[2:(length(names(prediction))-1)]
