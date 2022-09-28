library(dplyr)
library(lubridate)
library(tidyverse)
library(visdat)
library(ggplot2)
library(leaflet)
library(forcats)

#question 2
police_data <- read.csv('/Users/jazzopardi/Desktop/NOPD_Service_Calls_2021.csv')
str(police_data) #overview of data set and columns/rows of which there are 
#21 and 428315 respectively

#question 3
filter_data <- filter(police_data, Zip == 70119)
nrow(filter_data) #56796 rows

#question 4
na_values <- is.na(filter_data) #no NA values
anyNA(filter_data) # just double checking 
filter_data[filter_data==""] <- NA # converting empty spaces to NA
na_now <- which(is.na(filter_data))
length(na_now) # 22499
vis_miss(filter_data, warn_large_data = FALSE) # why is this data missing? 
# perhaps some discrepancies on when they were dispatched or they were never
# dispatched at all


#question 5
str(police_data) # four time related variables listed as characters

#converting from character to POSIXct format.

filter_data$TimeCreate <- parse_date_time(filter_data$TimeCreate, 'mdy HMSp')

filter_data$TimeDispatch <- parse_date_time(filter_data$TimeDispatch, 'mdy HMSp')

filter_data$TimeArrive <- parse_date_time(filter_data$TimeArrive, 'mdy HMSp')

filter_data$TimeClosed <- parse_date_time(filter_data$TimeClosed, 'mdy HMSp')

class(filter_data$TimeCreate) # returns a POSIXct format.

#birthday

birthday <- filter(filter_data, TimeCreate >= as_datetime("2021-04-02 00:00:00"),
                   TimeCreate <= as_datetime("2021-04-02 23:59:59"))

nrow(birthday) # 164 calls on my birthday

#most common disposition.text
common_dis <- tail(names(sort(table(filter_data$DispositionText))), 1) 


nrow(birthday) # 164 calls

# finish birthday


# adding delay column
filter_data['Delay'] = c(filter_data$TimeArrive - filter_data$TimeDispatch)

# adding handled column
filter_data['Handled'] = c(filter_data$TimeClosed - filter_data$TimeDispatch)


# Question 6

# ZIP Code should be considered categorical because it doesn't have a quantitative
# value, it is a representation of something else .
View(filter_data)
self_initiated <- filter_data[filter_data$SelfInitiated == 'Y', ]
perc_self_initiated <- (nrow(self_initiated) / nrow(filter_data))*100
perc_self_initiated # percentage that were self initiated 
gone_on_arrival <- filter_data[filter_data$DispositionText == "GONE ON ARRIVAL", ]
perc_gone_on_arrival <- (nrow(gone_on_arrival) / nrow(filter_data))*100
perc_gone_on_arrival # percentage of gone on arrival
disposition_text <- filter_data[filter_data$DispositionText == 'FALSE ALARM', ]
nrow(disposition_text) # actually no accounts of false alarm...
perc_disposition_text <- (nrow(disposition_text)/ nrow(filter_data))*100
perc_disposition_text # ...so percentage would be 0%

# Question 7

filter_data$Beat <- NULL 
ncol(filter_data) #20 columns

# Question 8

season <- quarter(filter_data$TimeDispatch, type = "quarter")
filter_data['Season'] = season
filter_data$Season <- gsub("1", "Winter", filter_data$Season)
filter_data$Season <- gsub("2", "Spring", filter_data$Season)
filter_data$Season <- gsub("3", "Summer", filter_data$Season)
filter_data$Season <- gsub("4", "Fall", filter_data$Season)


# Question 9

counts <- table(filter_data$Season)
ggplot(filter_data, aes(x= Season)) + geom_bar(fill = "blue")

#Most calls were made in Spring and Winter -  there's an NA column because
# we're using TimeDispatch - and there are some missing values there.
# More crime in winter because the sun sets earlier so chances of crime increasing
# at night are likely.

# Question 10 

#filtering data
filter_data_two <- filter(filter_data, TypeText == 
                            tail(names(sort(table(filter_data$TypeText))), 6))

filter_data_two

unique(filter_data_two$TypeText)      

# most common values =  "RETURN FOR ADDITIONAL INFO" "BURGLAR ALARM, SILENT"  
# "DISTURBANCE (OTHER)" "BUSINESS CHECK"  "COMPLAINT OTHER"  "AREA CHECK"  


nrow(filter_data_two) # 6255 rows

plot <- ggplot(filter_data_two, aes(fct_infreq(TypeText))) + geom_bar(fill = "blue")

plot + theme(axis.text.x=element_text(angle=50, size=7, vjust=0.5)) +geom_bar(
  fill = "salmon")

# the bar plot shows that area checks were the most popular police checks
# whilst a lot of data was sent back for additional info
# disturbance and burglar alarm incidents are somewhat as frequent as the other

# Question 11 - faceted histogram

ggplot(filter_data_two, aes(x = Delay)) + geom_histogram() + facet_wrap(~TypeText) + 
  theme(axis.text.x=element_text(angle=50, size=7, vjust=0.5)) + xlim(0,4000) + ylim(0, 160)

#  The faceted  histogoram shows the frequency of police responses according
# to the time of receiving the complaint to arriving.
#Police responded quickly to calls of burglar alarms and distrubances in
# this particular zip code.

# Question 12

filter_data_three <- filter(filter_data_two, Disposition == 
                              tail(names(sort(table(filter_data_two$Disposition))), 6))


ggplot(filter_data_two, aes(x = Disposition, fill = TypeText)) +
  geom_bar(position = "fill")

# I definitely think I did this wrong because I'm getting no proportionality
# fill. 

# Question 13


vec <- c(filter_data$Location)
df <- strcapture("\\(([-0-9.]+)\\s+([-0-9.]+)", vec, proto = list(lon = 1,lat = 1))
                                                      # proto to define class type
                                                      # values are arbitrary

filter_data['longitude'] = df[1]
filter_data['latitude'] = df[2]



map <- leaflet() %>% addTiles() %>%  addCircles(lng= filter_data$longitude ,
                                              lat= filter_data$latitude)

map <- leaflet() %>% addTiles() %>%  addCircles(lng= filter_data$longitude , 
                                              lat= filter_data$latitude ) %>%
  addProviderTiles(providers$Esri.WorldShadedRelief)

