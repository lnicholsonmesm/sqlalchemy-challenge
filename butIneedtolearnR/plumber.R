#plumber
#setwd("/Users/lauranicholson/Desktop/data_analysis/bootcamp/Homework/sqlalchemy-challenge/butineedtolearnR/plumber.R")
#plumb('/Users/lauranicholson/Desktop/data_analysis/bootcamp/Homework/sqlalchemy-challenge/butineedtolearnR/plumber.R')
#install.packages("plumber")
#packageme = function()
library(tidyverse)
library(dbplyr)
library(RSQLite)
library(plumber)
library(jsonlite)
library(lubridate)
library(janitor)
library(jsonify)

## Set-up

#___________________________________________________________________________________________
#         *         *         *         *MAKE APP *         *         *         *          #
#___________________________________________________________________________________________
homepage <- '/'
precip <- 'api/v1.0/precipitation'
stati <- 'api/v1.0/stations'
tobs <- 'api/v1.0/tobs'
summarytstart <- 'api/v1.0/<start>'
summarytend <- 'api/v1.0/<start>/<end>'

##################################### HOME PAGE ######################
#       1) Home Page:   host.place/
#           - List All Routes
#* List All API Routes
#* @get /
function(hi){
  tibble("HelpNote" = "don't forget to delete '__swagger__' to get to the api route", 
         "Test Route" = 'http://localhost:8000/__swagger__/',
         "Precipitation" = precip, "Station Info"= stati, 
         "1 Year Temperature Observations, 1 Station"= tobs,  
         "Temperature Summary, All Stations-Select Start Date (YYYY-MM-DD)" = summarytstart, 
         "Temp Summary, All Stations-Start and End Dates (YYYY-MM-DD)" = summarytend)}



##################################### PRECIP API #####################

precip <- 'api/v1.0/precipitation'
#       2) /api/v1.0/precipitation
#           - Convert query results to dictionary using key = date and value = prcp
#           - Have it return json
#* @get /api/v1.0/precipitation
function(precipitation){
  con <- dbConnect(RSQLite::SQLite(), "hawaii.sqlite")
  precipitation <- collect(con%>%tbl(sql("SELECT date, prcp FROM measurement WHERE date >= '2016-08-23'")))
  test <- precipitation %>% filter(!is.na(prcp))
  test <- pivot_wider(test, names_from = 'date', values_from='prcp')
  dbDisconnect(con)
  list(test)
}
##################################### STATIONS ######################
stations <- 'api/v1.0/stations'
#* Get a JSON file with list of stations
#* @get /api/v1.0/stations
function(stations){
  con <- dbConnect(RSQLite::SQLite(), "hawaii.sqlite")
  stations <- collect(con%>%tbl(sql("SELECT distinct station,name FROM station")))
  dbDisconnect(con)
  list(stations)
}
##################################### TOBS API #####################
tobs <- 'api/v1.0/tobs'
#* Get JSON list of temps for previous year from most active station
#* @get /api/v1.0/tobs
function(tobs){
  con <- dbConnect(RSQLite::SQLite(), "hawaii.sqlite")
  
  tobs12mo <- collect(con%>%tbl(sql("SELECT station, date, tobs FROM measurement WHERE date >= '2016-08-23'")))
  stationcount <- tobs12mo %>% add_count(station)
  maxobs <- stationcount %>% filter(n == max(n))
  output <- maxobs %>% select(-n)
  dbDisconnect(con)
  
  list(out_data)
}

##################################### TEMPS API Start&END #####################
summarytstartend <- 'api/v1.0/range/<start>/<end>'
#* Return JSON list of min T, avg T, max T for given start or start-end range
#* @get /api/v1.0/range/<start>/<end>
function(start = '2010-01-01', end='2017-08-23'){
  con <- dbConnect(RSQLite::SQLite(), "hawaii.sqlite")
  query = paste0("SELECT date, tobs FROM measurement WHERE date >= '", 
                 start, "' AND date <= '", end, "'")
  output <- dbGetQuery(con, query)
  output <- output %>% filter(!is.na(tobs))
  minT <- min(output$tobs) 
  maxT <- max(output$tobs)
  avgT <- mean(output$tobs)
  numDays <- nrow(output %>% distinct(date))
  numRow <- nrow(output)
  NumRepeatDays <- numRow - nrow(output %>% distinct(date))
  final_table <- tibble('Minimum Temp' = minT, 
                        'Maximum Temp' = maxT, 
                        'Average Temp' = avgT, 
                        'Number of Days' = numDays,
                        'Number of Rows' = numRow,
                        'Num values representing 2nd+ obs from day' = NumRepeatDays)
  dbDisconnect(con)
  final_table
}


##################################### TEMPS API START ONLY #####################

#* Return JSON list of min T, avg T, max T for given start or start-end range
#* @get /api/v1.0/range/<start>
function(start = '2010-01-01', end='2017-08-23'){
  list(start)
  #print(c(start, end))
  con <- dbConnect(RSQLite::SQLite(), "hawaii.sqlite")
  query = paste0("SELECT date, tobs FROM measurement WHERE date >= '", start, "' AND date <= '", end, "'")
  output <- dbGetQuery(con, query)
  output <- output %>% filter(!is.na(tobs))
  minT <- min(output$tobs) 
  maxT <- max(output$tobs)
  avgT <- mean(output$tobs)
  numDays <- nrow(output %>% distinct(date))
  numRow <- nrow(output)
  NumRepeatDays <- numRow - nrow(output %>% distinct(date))
  final_table <- tibble('Minimum Temp' = minT, 
                        'Maximum Temp' = maxT, 
                        'Average Temp' = avgT, 
                        'Number of Days' = numDays,
                        'Number of Rows' = numRow,
                        'Num values representing 2nd+ obs from day' = NumRepeatDays)
  dbDisconnect(con)
  final_table
}
#r <- plumb('plumber.R')  # Where 'plumber.R' is the location of the file shown above
#r$run(port=8000)