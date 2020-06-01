#Bonus
#Laura Nicholson

#dependencies
library(tidyverse)
library(dbplyr)
library(RSQLite)
library(lubridate)
library(janitor)

# Database Query
get = function(query){
  path = "hawaii.sqlite"
  con <- dbConnect(RSQLite::SQLite(), path)
  r <- dbGetQuery(con, query)
  dbDisconnect(con)
  r
  }

query = "SELECT measurement.*, station.name, station.latitude, station.longitude 
         FROM measurement 
         LEFT JOIN station ON station.station = measurement.station
         WHERE date like '____-06-__' OR date like '____-12-__' "

temp_anal_1 <- get(query) %>% select(-id)
glimpse(temp_anal_1)

datesplit <- temp_anal_1 %>% 
  separate(`date`, into = c("year", "month", "day"), remove=FALSE, sep='-') %>%
  mutate(month = ifelse(month =='06', 'June', 'December'))

summarytable <- datesplit %>% select(-id, -date, -day, -year)  %>% group_by(month, station) %>% mutate(min_tobs = min(tobs), first_quart_tobs = quantile(tobs)[1], median_tobs = median(tobs), mean_tobs = mean(tobs), third_quart_tobs = quantile(tobs)[2], max_tobs=max(tobs), min_prcp = min(prcp, na.rm = TRUE), first_quart_prcp = quantile(prcp, na.rm=TRUE)[1], median_prcp = median(prcp, na.rm=TRUE), mean_prcp = mean(prcp, na.rm=TRUE), third_quart_prcp = quantile(prcp, na.rm=TRUE)[2], max_prcp=max(prcp, na.rm=TRUE)) %>% select(-prcp, -tobs) %>% distinct()
glimpse(summarytable)
view(summarytable)
stationnames <- summarytable %>% distinct(station)
stationnames
summarytable
## Station 1: USC00519397

x1 <- datesplit %>% 
  select(station, tobs, prcp, month) %>% 
  filter(station=='USC00519397', month=='June')

y1 <-datesplit %>% 
  select(station, tobs, prcp, month) %>%  
  filter(station=='USC00519397', month=='December')

## Station 2: USC00513117
x2 <- datesplit %>% 
  select(station, tobs, prcp, month) %>% 
  filter(station=='USC00513117', month=='June')
y2 <-datesplit %>% 
  select(station, tobs, prcp, month) %>% 
  filter(station=='USC00513117', month=='December')

## Station 3: USC00514830
x3 <- datesplit %>% 
  select(station, tobs, prcp, month) %>% 
  filter(station=='USC00514830', month=='June')
y3 <- datesplit %>% 
  select(station, tobs, prcp, month) %>% 
  filter(station=='USC00514830', month=='December')

## Station 4: USC00517948
x4<- datesplit %>% 
  select(station, tobs, prcp, month) %>% 
  filter(station=='USC00517948', month=='June')
y4 <-datesplit %>% 
  select(station, tobs, prcp, month) %>% 
  filter(station=='USC00517948', month=='December')

## Station 5: USC00518838
x5<- datesplit %>% 
  select(station, tobs, prcp, month) %>% 
  filter(station=='USC00518838', month=='June')
y5<-datesplit %>% 
  select(station, tobs, prcp, month) %>% 
  filter(station=='USC00518838', month=='December')

## Station 6: USC00519523
x6<- datesplit %>% 
  select(station, tobs, prcp, month) %>%  
  filter(station=='USC00519523', month=='June')
y6<-datesplit %>% 
  select(station, tobs, prcp, month) %>% 
  filter(station=='USC00519523', month=='December')

## Station 7: USC00519281
x7<- datesplit %>% 
  select(station, tobs, prcp, month) %>% 
  filter(station=='USC00519281', month=='June')
y7 <- datesplit %>% 
  select(station, tobs, prcp, month) %>% 
  filter(station=='USC00519281', month=='December')

## Station 8: USC00511918
x8 <- datesplit %>% 
  select(station, tobs, prcp, month) %>% 
  filter(station=='USC00511918', month=='June')
y8 <-datesplit %>% 
  select(station, tobs, prcp, month) %>% 
  filter(station=='USC00511918', month=='December')

## Station 9: USC00516128
x9 <- datesplit %>% 
  select(station, tobs, prcp, month) %>% 
  filter(station=='USC00516128', month=='June')
y9 <-datesplit %>% 
  select(station, tobs, prcp, month) %>% 
  filter(station=='USC00516128', month=='December')

############################ T-TESTS ###########################
library(broom)
t1 <- t.test(x = x1$tobs, y = y1$tobs, paired = FALSE, alternative = "two.sided")
#datesplit %>% select(station, tobs, prcp, month) %>% group_by(station, month) %>% do(tidy(t.test(mtcars$mpg)))
tt1 <- t.test(x = x1$prcp, y = y1$prcp, paired = FALSE,  alternative = "two.sided")

t2  <- t.test(x = x2$tobs, y = y2$tobs, paired = FALSE, alternative = "two.sided")
tt2 <- t.test(x = x2$prcp, y = y2$prcp, paired = FALSE,  alternative = "two.sided")

t3  <- t.test(x = x3$tobs, y = y3$tobs, paired = FALSE, alternative = "two.sided")
tt3 <- t.test(x = x3$prcp, y = y3$prcp, paired = FALSE,  alternative = "two.sided")

t4  <- t.test(x = x4$tobs, y = y4$tobs, paired = FALSE, alternative = "two.sided")
tt4 <- t.test(x = x4$prcp, y = y4$prcp, paired = FALSE,  alternative = "two.sided")

t5  <- t.test(x = x5$tobs, y = y5$tobs, paired = FALSE, alternative = "two.sided")
tt5 <- t.test(x = x5$prcp, y = y5$prcp, paired = FALSE,  alternative = "two.sided")

t6  <- t.test(x = x6$tobs, y = y6$tobs, paired = FALSE, alternative = "two.sided")
tt6 <- t.test(x = x6$prcp, y = y6$prcp, paired = FALSE,  alternative = "two.sided")

t7  <- t.test(x = x7$tobs, y = y7$tobs, paired = FALSE, alternative = "two.sided")
tt7 <- t.test(x = x7$prcp, y = y7$prcp, paired = FALSE,  alternative = "two.sided")

t8  <- t.test(x = x8$tobs, y = y8$tobs, paired = FALSE, alternative = "two.sided")
tt8 <- t.test(x = x8$prcp, y = y8$prcp, paired = FALSE,  alternative = "two.sided")

t9  <- t.test(x = x9$tobs, y = y9$tobs, paired = FALSE, alternative = "two.sided")
tt9 <- t.test(x = x9$prcp, y = y9$prcp, paired = FALSE,  alternative = "two.sided")

glimpse(tidy(t1))
glimpse(tidy(t2))
glimpse(tidy(t3))
glimpse(tidy(t4))
glimpse(tidy(t5))
glimpse(tidy(t6))
glimpse(tidy(t7))
glimpse(tidy(t8))
glimpse(tidy(t9))

t_test_tobs <- bind_rows(tidy(t1), tidy(t2), tidy(t3), tidy(t4), tidy(t5), tidy(t6), tidy(t7), tidy(t8), tidy(t9)) %>% select(diff_in_means = estimate, mean_of_June=estimate1, mean_of_Dec=estimate2, statistic_t=statistic, df_parameter=parameter, everything())
t_test_prcp <-bind_rows(tidy(tt1), tidy(tt2), tidy(tt3), tidy(tt4), tidy(tt5), tidy(tt6), tidy(tt7), tidy(tt8), tidy(tt9))%>% select(diff_in_means = estimate, mean_of_June=estimate1, mean_of_Dec=estimate2, statistic_t=statistic, df_parameter=parameter, everything())

#getstations and station names. Yes, this would be easier by getting it from the orig df
stationname <- datesplit %>% distinct(station) 
stationname
stationcity <- datesplit %>% distinct(name) 
stationnamecity <- datesplit %>% distinct(station, name) %>%   separate(col=name, into= 'city', sep = " ", extra="drop") %>% select(station, city)
stationcity
stationname
stationnamecity$city <- str_replace(stationnamecity$city, pattern="UPPER", replacement="UPPER WAHIAWA")
stationnamecity
stationcolumn <- c(x1$station[1], x2$station[1],x3$station[1],x4$station[1],x5$station[1],x6$station[1],x7$station[1],x8$station[1],x9$station[1])
stationcolumn

#build summary dfs
t_test_temp_df <- bind_cols(station = stationcolumn, stationnamecity, t_test_tobs)
glimpse(t_test_temp_df)
t_test_temp_df <- t_test_temp_df %>% select(-station1)
t_test_rain_df <-bind_cols(station = stationcolumn, stationnamecity, t_test_tobs) 
t_test_rain_df <- t_test_rain_df %>% select(-station1)

glimpse(t_test_rain_df)
view(t_test_temp_df %>% arrange(p.value))
view(t_test_rain_df)
t1
print("For Upper Wahiawa, we failed to reject the null hypothesis that there was no difference in mean temperatures between June and December. For all other observations sites in our Hawaii database, we reject the null hypothesis that there is no difference in temperature in June versus December, and therefore accept the alternate hypothesis that Hawaii is not, in fact, a state with constant year-round temperatures. In most places. The same is true of precipitation, which makes me wonder what the elevation of upper Wahiawa is and if it's just cold and rainy all the time there, or if there is some issue with the data. Either one seems plausible to me.")
#average of average of months = ok to do for data analysis because we're weighting them equally
#if there were more observations in a given year, they may drag the summary stats one direction or another.



######## VACATION TEMPERATURE ANALYSIS ##########
#to get all years rather than "last" year, like the homework asks, pull all oct/nov months and then filter in R
#vacation: 10/25 - 11/15
query = "SELECT measurement.*, station.name, station.latitude, station.longitude 
         FROM measurement
         LEFT JOIN station ON station.station = measurement.station
         WHERE date LIKE '____-10-__' OR date LIKE '____-11-__'"
query
temp_anal_2 <- get(query) %>%  select(-id)
glimpse(temp_anal_2)

#want min, avg and max

temp_groups <- temp_anal_2 %>%   separate(`date`, into = c("year", "month", "day"), remove=FALSE, sep='-') %>%
  mutate(month = case_when(month =='10'~'October', month =='11'~'November', TRUE ~"Error"))
max(temp_groups$year)
tstats = function(stationnum, yearsel){
  if(max(yearsel)>2016){print("Please choose an earlier year")} else {
  if(missing(yearsel)){yearsel=2016}
  filter = stationname[stationnum,1]
  print(filter)
  print(yearsel)
  df <- temp_groups %>% filter(station == filter, year %in% yearsel)
  glimpse(df)
  numyears = length(yearsel)
  minyear = as.character(yearsel[1])
  maxyear=as.character(yearsel[numyears])
  yearsofdata = ifelse(numyears==1, minyear, paste(minyear,"-", maxyear, sep=""))
  print(yearsofdata)
  dftobs <- df %>% filter(!is.na(tobs))
  sum_df <-tibble(station = stationnamecity[stationnum,1],
                  city = stationnamecity[stationnum, 2],
                  years = yearsofdata,
                  min_temp = min(dftobs$tobs),
                  avg_temp = mean(dftobs$tobs),
                  max_temp = max(dftobs$tobs))
  sum_df}
}
stationnamecity
yearsel <- c(2010, 2011, 2012, 2013, 2014, 2015, 2016)
as.character(years)
length(as.character(years))
data1 <- tstats(1, c(2010, 2011, 2012, 2013, 2014, 2015, 2016))
data2 <- tstats(2, c(2010, 2011, 2012, 2013, 2014, 2015, 2016))
data3 <- tstats(3, c(2010, 2011, 2012, 2013, 2014, 2015, 2016))
data4 <- tstats(4, c(2010, 2011, 2012, 2013, 2014, 2015, 2016))
data5 <- tstats(5, c(2010, 2011, 2012, 2013, 2014, 2015, 2016))
data6 <- tstats(6, c(2010, 2011, 2012, 2013, 2014, 2015, 2016))
data7 <- tstats(7, c(2010, 2011, 2012, 2013, 2014, 2015, 2016))
data8 <- tstats(8, c(2010, 2011, 2012, 2013, 2014, 2015, 2016))
data9 <- tstats(9, c(2010, 2011, 2012, 2013, 2014, 2015, 2016))
#data goes to june 30 2017, so for my trip the latest data would be 2016

yr1 <- tstats(1, 2016)
yr2 <- tstats(2, 2016)
yr3 <- tstats(3, 2016)
yr4 <- tstats(4, 2016)
yr5 <- tstats(5, 2016)
yr6 <- tstats(6, 2016)
yr7 <- tstats(7, 2016)
yr8 <- tstats(8, 2016)
yr9 <- tstats(9, 2016)

trip_dataframe <- bind_rows(data1, data2, data3, data4, data5, data6,data7,data8,data9, yr1, yr2,yr3, yr4,yr5,yr6,yr7,yr8,yr9)
trip_dataframe

allyrs <- trip_dataframe %>% filter(years=='2010-2016')
yr2016 <- trip_dataframe %>% filter(years=='2016')
trip_dataframe
view(yr2016)
temp_plot <- ggplot(yr2016) +
  geom_bar(stat='identity', mapping = aes(city, avg_temp, fill=city)) +
  labs(title="Expected Avg Temp for Trip (2016 Data)") +
  theme_set(theme_bw()) +
  scale_fill_brewer(palette="Accent") +
  theme(axis.text.x = element_text(angle=45, vjust=0.6)) +
  geom_linerange(data=yr2016, mapping = aes(x=city, ymin=min_temp, ymax=max_temp), na.rm=TRUE, size=0.5, color = 'brown')

temp_plot

stationnamecity

