##########################################################################
# Jose Cajide - @jrcajide
# Master Big data: Reshaping and Transforming data with tidyr and dplyr
##########################################################################

list.of.packages <- c("R.utils", "tidyverse", "sqldf", "broom", "DBI", "ggplot2", "tidyr", "lubridate")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)


flights <- readr::read_csv('data/2008.csv')



# DPLYR -------------------------------------------------------------------

# Identify the most important data manipulation tools needed for data analysis and make them easy to use in R.
# Provide blazing fast performance for in-memory data by writing key pieces of code in C++.
# Use the same code interface to work with data no matter where it’s stored, whether in a data frame or database.

# The 5 verbs of dplyr
# select – removes columns from a dataset
# filter – removes rows from a dataset
# arrange – reorders rows in a dataset
# mutate – uses the data to build new columns and values
# summarize – calculates summary statistics

library(dplyr)

# SELECT() ----------------------------------------------------------------------------


select(flights, ActualElapsedTime, ArrDelay, DepDelay)

# Funciones de ayuda

# starts_with(“X”): every name that starts with “X”
# ends_with(“X”): every name that ends with “X”
# contains(“X”): every name that contains “X”
# matches(“X”): every name that matches “X”, where “X” can be a regular expression
# num_range(“x”, 1:5): the variables named x01, x02, x03, x04 and x05
# one_of(x): every name that appears in x, which should be a character vector

select(flights, Origin:Cancelled)
select(flights, -(DepTime:AirTime))
select(flights, UniqueCarrier, FlightNum, contains("Tail"), ends_with("Delay"))

# MUTATE() ----------------------------------------------------------------------------

foo <- mutate(flights, ActualGroundTime = ActualElapsedTime - AirTime)
foo <- mutate(foo, GroundTime = TaxiIn + TaxiOut)
select(foo, ActualGroundTime, GroundTime)

# Varias operaciones

foo <- mutate(flights, 
              loss = ArrDelay - DepDelay, 
              loss_percent = (loss/DepDelay) * 100 )



# FILTER() --------------------------------------------------------------------------

# x < y, TRUE if x is less than y
# x <= y, TRUE if x is less than or equal to y
# x == y, TRUE if x equals y
# x != y, TRUE if x does not equal y
# x >= y, TRUE if x is greater than or equal to y
# x > y, TRUE if x is greater than y
# x %in% c(a, b, c), TRUE if x is in the vector c(a, b, c)

# Print out all flights in hflights that traveled 3000 or more miles
filter(flights, Distance > 3000)

# All flights flown by one of AA or UA
filter(flights, UniqueCarrier %in% c('AA', 'UA'))

# All flights where taxiing took longer than flying
# Taxi-Out Time: The time elapsed between departure from the origin airport gate and wheels off.
# Taxi-In Time: The time elapsed between wheels-on and gate arrival at the destination airport.
filter(flights, TaxiIn + TaxiOut > AirTime)

# Combining tests using boolean operators

# All flights that departed late but arrived ahead of schedule
filter(flights, DepDelay > 0 & ArrDelay < 0)

# All flights that were cancelled after being delayed
filter(flights, Cancelled == 1, DepDelay > 0)

##########################################################################
# Exercise: 
# How many weekend flights to JFK airport flew a distance of more than 1000 miles 
# but had a total taxiing time below 15 minutes?
# 1) Select the flights that had JFK as their destination and assign the result to jfk
jfk <- filter(flights, Dest == 'JFK')

# 2) Combine the Year, Month and DayofMonth variables to create a Date column
jfk <- mutate(jfk, Date = as.Date(paste(Year, Month, DayofMonth, sep = "-")))

# 3) Result:
nrow(filter(jfk, DayOfWeek %in% c(6,7), Distance > 1000, TaxiIn + TaxiOut < 15)) 

# 4) Delete jfk object to free resources 
rm(jfk)

# ARRANGE() --------------------------------------------------------------------------

# Cancelled
( cancelled <- select(flights, UniqueCarrier, Dest, Cancelled, CancellationCode, DepDelay, ArrDelay) )

( cancelled <- filter(cancelled, Cancelled == 1, !is.na(DepDelay)) )

# Arrange cancelled by departure delays
arrange(cancelled, DepDelay)

# Arrange cancelled so that cancellation reasons are grouped
arrange(cancelled, CancellationCode)

# Arrange cancelled according to carrier and departure delays
arrange(cancelled, UniqueCarrier, DepDelay)

# Arrange cancelled according to carrier and decreasing departure delays
arrange(cancelled, UniqueCarrier, desc(DepDelay))

rm(cancelled)

# Arrange flights by total delay (normal order).
arrange(flights, DepDelay + ArrDelay)

# Keep flights leaving to DFW and arrange according to decreasing AirTime 
arrange(filter(flights, Dest == 'JFK'), desc(AirTime))



# SUMMARISE() -----------------------------------------------------------------------

# min(x) – minimum value of vector x.
# max(x) – maximum value of vector x.
# mean(x) – mean value of vector x.
# median(x) – median value of vector x.
# quantile(x, p) – pth quantile of vector x.
# sd(x) – standard deviation of vector x.
# var(x) – variance of vector x.
# IQR(x) – Inter Quartile Range (IQR) of vector x.

# Print out a summary with variables min_dist and max_dist
summarize(flights, min_dist = min(Distance), max_dist = max(Distance))

# Remove rows that have NA ArrDelay: temp1
na_array_delay <- filter(flights, !is.na(ArrDelay))

# Generate summary about ArrDelay column of temp1
summarise(na_array_delay, 
          earliest = min(ArrDelay), 
          average = mean(ArrDelay), 
          latest = max(ArrDelay), 
          sd = sd(ArrDelay))

hist(na_array_delay$ArrDelay)

rm(na_array_delay)

# Keep rows that have no NA TaxiIn and no NA TaxiOut: temp2
taxi <- filter(flights, !is.na(TaxiIn), !is.na(TaxiOut))

##########################################################################
# Exercise: 
# Print the maximum taxiing difference of taxi with summarise()
summarise(taxi, max_taxi_diff = max(abs(TaxiIn - TaxiOut)))

rm(taxi)


# dplyr provides several helpful aggregate functions of its own, in addition to the ones that are already defined in R. These include:
# first(x) - The first element of vector x.
# last(x) - The last element of vector x.
# nth(x, n) - The nth element of vector x.
# n() - The number of rows in the data.frame or group of observations that summarise() describes.
# n_distinct(x) - The number of unique values in vector x.




# Filter flights to keep all American Airline flights: aa
aa <- filter(flights, UniqueCarrier == "AA")


##########################################################################
# Exercise: 
# Print out a summary of aa with the following variables:
# n_flights: the total number of flights,
# n_canc: the total number of cancelled flights,
# p_canc: the percentage of cancelled flights,
# avg_delay: the average arrival delay of flights whose delay is not NA.

summarise(
  aa,
  n_flights = n(),
  n_canc = sum(Cancelled),
  p_canc = 100 * (n_canc / n_flights),
  avg_delay = mean(ArrDelay, na.rm = TRUE)
)

rm(aa)



# %>% OPERATOR ----------------------------------------------------------------------

# Piping

mean(c(1, 2, 3, NA), na.rm = TRUE)

# Vs

c(1, 2, 3, NA) %>% mean(na.rm = TRUE)


summarize(filter(mutate(flights, diff = TaxiOut - TaxiIn),!is.na(diff)), avg = mean(diff))

# Vs

flights %>%
  mutate(diff=(TaxiOut-TaxiIn)) %>%
  filter(!is.na(diff)) %>%
  summarise(avg=mean(diff))


flights %>%
  filter(Month == 5, DayofMonth == 17, UniqueCarrier %in% c('UA', 'WN', 'AA', 'DL')) %>%
  select(UniqueCarrier, DepDelay, AirTime, Distance) %>%
  arrange(UniqueCarrier) %>%
  mutate(air_time_hours = AirTime / 60)


# GROUP_BY() -------------------------------------------------------------------------

flights %>% 
  group_by(UniqueCarrier) %>% 
  summarise(n_flights = n(), 
            n_canc = sum(Cancelled), 
            p_canc = 100*n_canc/n_flights, 
            avg_delay = mean(ArrDelay, na.rm = TRUE)) %>% 
  arrange(avg_delay)


flights %>% 
  group_by(DayOfWeek) %>% 
  summarize(avg_taxi = mean(TaxiIn + TaxiOut, na.rm = TRUE)) %>% 
  arrange(desc(avg_taxi))


# Combine group_by with mutate
rank(c(21, 22, 24, 23))

flights %>% 
  filter(!is.na(ArrDelay)) %>% 
  group_by(UniqueCarrier) %>% 
  summarise(p_delay = sum(ArrDelay >0)/n()) %>% 
  mutate(rank = rank(p_delay)) %>% 
  arrange(rank) 




# Other dplyr functions ---------------------------------------------------

# top_n()

flights %>% 
  group_by(UniqueCarrier) %>% 
  top_n(2, ArrDelay) %>% 
  select(UniqueCarrier,Dest, ArrDelay) %>% 
  arrange(desc(UniqueCarrier))




# Dealing with outliers ---------------------------------------------------

# ActualElapsedTime: Elapsed Time of Flight, in Minutes
summary(flights$ActualElapsedTime)

hist(flights$ActualElapsedTime)

library(ggplot2)
ggplot(flights) + 
  geom_histogram(aes(x = ActualElapsedTime))

boxplot(flights$ActualElapsedTime,horizontal = TRUE)

outliers <- boxplot.stats(flights$ActualElapsedTime)$out
length(outliers)
outliers

no_outliers <- flights %>% 
  filter(!ActualElapsedTime %in% outliers) 

boxplot(no_outliers$ActualElapsedTime,horizontal = TRUE)

mean(no_outliers$ActualElapsedTime, na.rm = T)
hist(no_outliers$ActualElapsedTime)

rm(outliers)
rm(no_outliers)


barplot(table(flights$UniqueCarrier))



# Tidy Data ---------------------------------------------------------------

library(tidyr)

# Wide Vs Long 

# spread
# gather

flights %>% 
  group_by(Origin, Dest) %>% 
  summarise(n = n()) %>% 
  arrange(-n) %>% 
  spread(Origin, n) %>% 
  gather("Origin", "n", 2:ncol(.)) %>% 
  arrange(-n) 


##########################################################################
# Run the follow statements step by step and trying to understand what they do

flights %>% 
  group_by(UniqueCarrier, Dest) %>% 
  summarise(n = n()) %>%
  ungroup() %>% 
  group_by(Dest) %>% 
  mutate(total= sum(n), pct=n/total, pct= round(pct,4)) %>% 
  ungroup() %>% 
  select(UniqueCarrier, Dest, pct) %>% 
  spread(UniqueCarrier, pct) %>% 
  replace(is.na(.), 0) %>% 
  mutate(total = rowSums(select(., -1))) 







# Dplyr: Joins ------------------------------------------------------------

# inner_join(x, y)  SELECT * FROM x INNER JOIN y USING (z)
# left_join(x, y) SELECT * FROM x LEFT OUTER JOIN y USING (z)
# right_join(x, y, by = "z") SELECT * FROM x RIGHT OUTER JOIN y USING (z)
# full_join(x, y, by = "z") SELECT * FROM x FULL OUTER JOIN y USING (z)

# semi_join(x, y)
# anti_join(x, y)


airlines <- readr::read_csv('data/airlines.csv')
airlines

airports <- readr::read_csv('data/airports.csv')
airports

# Before joing dataframes, check for unique keys
airports %>% 
  count(iata) %>% 
  filter(n > 1)


flights2 <- flights %>% 
  select(Origin, Dest, TailNum, UniqueCarrier, DepDelay)

# Top delayed flight by airline
flights2 %>% 
  group_by(UniqueCarrier) %>%
  top_n(1, DepDelay) %>% 
  left_join(airlines, by = c("UniqueCarrier" = "Code"))



# Exporting data ----------------------------------------------------------

flights %>% 
  sample_n(1000) %>% 
  write_csv(., file.path("exports", "flights.csv"))


flights %>% 
  group_by(Month) %>% 
  do(tail(., 2))

dir.create('exports')

flights %>% 
  sample_n(1000) %>% 
  group_by(Year, Month) %>%
  do(write_csv(., file.path("exports", paste0(unique(.$Year),"_",unique(.$Month), "_flights.csv"))))