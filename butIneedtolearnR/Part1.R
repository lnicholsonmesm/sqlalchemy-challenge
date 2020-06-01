## SQL Alchemy, but in R
# Laura Nicholson
# May 2020
#setdiff(definitive, complete)
# Dependencies
# ******************************************************************************

#library(RPostgreSQL) #not using PostgreSQL
#if(!'RSQLite' %in% installed.packages()){
#  install.packages('RSQLite')
#}
library(tidyverse)
library(dbplyr)
library(RSQLite)
library(jsonlite)


# Code I need
# ******************************************************************************
path = "hawaii.sqlite"
#path = "/Users/lauranicholson/Desktop/data_analysis/bootcamp/Homework/sqlalchemy-challenge/butIneedtolearnR/Resources/hawaii.sqlite"
con <- dbConnect(RSQLite::SQLite(), path)
con
# ******************************************************************************

# GET TABLE NAMES/INSPECT DB
db_desc(con) #this told me it was sqlite 3.30.1 aaand showed me the path
metadata <- dbGetQuery(con, "SELECT * FROM sqlite_master") #note: dbSendQuery doesn't work like I thought it would. need to use "GetQuery"
table_name_query = "SELECT name FROM sqlite_master WHERE type='table' ORDER BY name"
tbl_name_query2 = "SELECT name from sqlite_master WHERE type =  'table' AND name NOT LIKE 'sqlite_%';"
tablenames_maybe <- dbGetQuery(con, table_name_query)
tablenames_maybe
# ******************************************************************************

Station_v1 <- dbGetQuery(con, "SELECT * FROM station")
Measurement_v1 <- dbGetQuery(con, "SELECT * FROM measurement")

Station_v2 <- con %>% tbl("station") 
Measurement_v2 <- con %>% tbl("measurement")

# Explore the data
# ******************************************************************************
maxdate <- dbGetQuery(con, "SELECT max(date) FROM measurement")
maxdate
class(maxdate) #data.frame makes dealing with this easy, I think.
# type(maxdate) this is not a way to get more information
dbGetQuery(con, "SELECT min(date) FROM measurement")
#max date is 2017-08-23, so the last 12 months of precipitation data will go back to 2016-08-23ish
Station_v1_columns <- names(Station_v1)
Measurement_v1_columns <- names(Measurement_v1)

Station_v1_columns
Measurement_v1_columns #dates are in measurements table
#these two match on station, presumably


#Get last year of data, retrieving date and precipitation (date, prcp) values only
# ******************************************************************************
precipitation <- con%>%tbl(sql("SELECT date, prcp FROM measurement WHERE date >= '2016-08-23'"))
precipitation
#date is a character column--going to make it a date with lubridate package
library(lubridate)
precipitation$date <- ymd(precipitation$date)
precipitation #interesting. it's still a "chr". I wonder if that's an R thing, because I know it has a class...
class(precipitation$date) #yep, date. ok then:

#pull into pandas df and set index to date. My first idea was that it was more or less already saved in a tibble, but maybe it wasn't a real one, so I could stick it there. That didn't work, since it's a "tbl_SQLiteConnection" instead of a 1d or 2d object.
#ah yes, collect() was the function
precip_tibble <- collect(precipitation)
precip_tibble #and yes, it's a tibble--don't need to convert it.
precip_df <- tibble(precip_tibble)
glimpse(precip_tibble) # this will be super useful for my 400-column table later
#also, a tribble() will create a table by rows, ie you can put all column names first

# install.packages('jsonify')
library(jsonify)
precip_df
to_json(precipitation)
#barplot of precipitation stuff
# ******************************************************************************
x = precip_tibble %>% select("date")
x$date = parse_date(x$date)
x= x$date
xvec = precip_tibble$date
y = precip_tibble %>% select(prcp)
min_x = ymd(min(x$date)) #had to make this a date with lubridates ymd
max_x = ymd(max(x$date)) #had to make this a date with lubridates ymd

library(scales)

prcp_test <- precip_tibble %>% ggplot() +
  geom_bar(mapping = aes(x = date))
#that's horrible
prcp_test


prcp_graph <- ggplot(precip_tibble, 
                     aes(x=as.Date(`date`), y=prcp, fill = prcp)) +
  geom_bar(stat = 'identity', width = 1) +
  scale_x_date(labels = date_format("%Y-%m-%d"), breaks = date_breaks("months"), limits = c(min_x, max_x)) + 
  labs(title = "Measured Rainfall, August 2016 - August 2017",
    x = "Date",
    y = "Precipitation (in)"
    ) + 
  theme_bw() +
  theme(plot.title = element_text(),
    legend.background = element_rect(fill = "white", size = 4, color = 'white'),
    axis.text.x = element_text(angle = 45, hjust=1),
    panel.grid.major = element_line(color = "grey70", size = 0.2),
    panel.grid.minor = element_blank()
    ) + 
  scale_fill_gradient(low = "light blue", high = "dark blue")
  #scale_x_date()

prcp_graph
library(janitor)
summary_table <- summary(precip_tibble)
summary_table
#precip_tibble %>% tabyl(prcp)


  
# STATION ANALYSIS
# ******************************************************************************
# 1 design query to calculate total number of stations
Stations <- dbGetQuery(con, "SELECT * FROM station")
num_stations <- con%>%tbl(sql("SELECT count(distinct station) as StationCount FROM station"))
num_stations

Stations
station_activity <- dbGetQuery(con,"SELECT measurement.station, count(measurement.station) AS ObservationCounts, station.name FROM measurement LEFT JOIN station on station.station = measurement.station GROUP BY measurement.station ORDER BY ObservationCounts DESC")
  
station_activity
#Which station has the highest number of observations?
# ******************************************************************************
highest_obs_station <- station_activity %>% filter(ObservationCounts == max(ObservationCounts))

print(paste("The highest number of observations was ", highest_obs_station$ObservationCounts, " at ", highest_obs_station$name, sep=''))

dbGetQuery(con, "SELECT min(date) FROM measurement")
dbGetQuery(con, "SELECT max(date) FROM measurement")
#is #2017-08-23, so 1yr back is 2016-08-23

tobs12mo <- dbGetQuery(con, "SELECT * FROM measurement WHERE date >= '2016-08-23'")

stationcount <- tobs12mo %>% add_count(station)
maxobs <- stationcount %>% filter(n == max(n))

maxobs

# HISTOGRAM PLOT
bins = 12
histplt <- ggplot(maxobs,
                     aes(x=maxobs$tobs, fill=maxobs$tobs)) +
  geom_histogram(bins=bins, fill='dark blue'
                 #, fill = 'dark orange'
                 ) + 
  labs(title = "Measured Temperature Observations, \nWaihee Station, August 2016 - August 2017",
       x = "Temperature",
       y = "Number of Observations" #technically it's a year anda  day
  ) + 
  theme_bw() +
  theme(plot.title = element_text(),
       # legend.background = element_rect(fill = "white", size = 4, color = 'white'),
     #   legend.position = c(0,1),
        axis.text.x = element_text(angle = 0, hjust=1),
        panel.grid.major = element_line(color = "grey70", size = 0.2),
        panel.grid.minor = element_blank()
  ) + 
  scale_fill_gradient(low = "light blue", high = "dark blue")

histplt


view(Stations)

data <- dbSendQuery(con, "SELECT * FROM Measurements")
remote_name(con)
remote_src(con, "sqlite_master")

tbl(con, "measurements")
remote_query_plan(con)
#getTableInformation(con)
dbDisconnect(con)

### RUN THE API
r <- plumb('plumber.R')  # Where 'plumber.R' is the location of the file shown above
r$run(port=8000)














######## EVERTHING BELOW ARE NOTES/EXAMPLE CODE FROM THE INTERNET ******
# 
# dbManager(con)
# rs <- dbSendQuery(con, "select * from seattlecrimeincidents limit 100");
# df <- fetch(rs)
# 
# 
# df = tbl(con, from = 'BGBUref')
# df = tbl(con, from = in_schema('dbo', 'BGBUref'))
# If your connection (con) is not to the QnRStore database directly then this may work:
# 
#   df = tbl(con, from = in_schema('QnRStore.dbo', 'BGBUref'))
# 
# #I don't think RODBC really solves this problem for you. It does allow you to talk to different products, but so do the various DBI drivers.
# 
# 
# #' Get the table information for a postgres database
# #' @param config the configuration list
# #' @return the table names, columns, and column types of all columns in the database
# getTableInformation <- function(config = config.gp) {
#   tables <- dbSendQuery(con,
#     "SELECT table_name, column_name, data_type
#     FROM information_schema.columns
#     WHERE table_name NOT LIKE '%prt%'
#       AND table_name NOT LIKE '%ext%'
#       AND table_name NOT LIKE '%tmp%'
#     ORDER BY 1, 2"#,
#     #config
#   )
# }
# 
# #' Replacement of the normal update function, you don't need to call this.
# update <- function(object, ...) {
#   args <- list(...)
#   for (nm in names(args)) {
#     object[[nm]] <- args[[nm]]
#   }
#   if (is.null(object$select)) {
#     if (is.ident(object$from)) {
#       var_names <- object$select
#     }
#     else {
#       var_names <- qry_fields(object$src$con, object$from)
#     }
#     vars <- lapply(var_names, as.name)
#     object$select <- vars
#   }
#   object$query <- dplyr:::build_query(object)
#   object
# 
#   To use this function, you can simply call
#   1
#   reflectDatabase()
#   and if you’re using a Postgres database, that should be it!
# 
#     The fun part now, is that I can do things like
# 
#   1
#   res <- inner_join(my_table_1, my_table_2)
#   where my_table_1 and my_table_2 are simply names of tables in my database. This provides me with auto-complete of table names, search-able table names and columns, etc.
# 
#   For example:
#     1
#   searchTables('user')
# #' Function to reflect a database, generalizable to others beyond postgres
# #' by simply changing getTableInformation appropriately
# reflectDatabase <- function(config, envir.name = "tables",
#                             subclass = "SQlite") {
#   if (!(envir.name %in% search())) {
#     envir <- new.env(parent = .GlobalEnv)
#   } else {
#     envir <- as.environment(envir.name)
#     detach(envir.name, character.only = TRUE)
#   }
#   src <- do.call(src_sqlite, config)
#   tables <- getTableInformation(config)
#   tables <- split(tables, tables$table_name)
#   lapply(tables, function(i) {
#     nm <- ident(i$table_name[1])
#     vars <- lapply(i$column_name, as.name)
#     tbl <- dplyr::make_tbl(c(subclass, "sql"), src = src, from = nm,
#                            select = vars, summarise = FALSE, mutate = FALSE,
#                            where = NULL, group_by = NULL, order_by = NULL)
#     tbl <- update(tbl)
#     assign(
#       nm,
#       tbl,
#       envir = envir
#     )
#   })
#   attach(envir, name = envir.name)
# }
# 
# searchTables <- function(str, env = "tables") {
#   all.tbls <- ls(env)
#   all.tbls[grep(str, all.tbls)]
# }
# To use this function, you can simply call
# 
# 1
# reflectDatabase(con, env="tables")
# 
# 
# Database connection
# in_schema()
# 
# Refer to a table in a schema
# 
# memdb_frame() tbl_memdb() src_memdb()
# 
# Create a database table in temporary in-memory database.
# 
# remote_name() remote_src() remote_con() remote_query() remote_query_plan()
# 
# Metadata about a remote table
# 
# SQL generation
# build_sql()
# 
# Build a SQL string.
# 
# escape() escape_ansi() sql_vector()
# 
# Escape/quote a string.
# 
# ident() ident_q() is.ident()
# 
# Flag a character vector as SQL identifiers
# 
# partial_eval()
# 
# Partially evaluate an expression.
# 
# sql() is.sql() as.sql()
# 
# SQL escaping.
# 
# sql_expr() sql_call2()
# 
# Generate SQL from R expressions
# 
# translate_sql() translate_sql_()
# 
# Translate an expression to sql.

# window_order() window_frame()
# 
# Override window order and frame
