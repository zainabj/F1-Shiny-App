library(rsconnect)

source("C:/Users/zaina/OneDrive/Desktop/R Final Project/global.R")
source("C:/Users/zaina/OneDrive/Desktop/R Final Project/server.R")
source("C:/Users/zaina/OneDrive/Desktop/R Final Project/ui.R")

shinyApp(ui=ui, server=server)

rsconnect::deployApp('C:/Users/zaina/OneDrive/Desktop/R Final Project')
