setwd("~/Documents/PRA2/")
library(dplyr)

data = read.csv("bergen/bergen_merged.csv")
elev = read.csv("elevation.csv", sep= ";")
estacions = data[,c(4,5,6,7)]
est_dest = data[,c(8,9,10,11)]

names(estacions) <- c("id","name","lat","lon")
names(est_dest) <- c("id","name","lat","lon")

estacions = rbind(estacions, est_dest)

est_uniques <- unique(estacions)

group1 <- est_uniques %>% group_by(id) %>% 
  summarise(total=n(),id = id, name = name) %>% filter(total>1)

group1

count1 <- estacions[estacions$id %in% unique(group1$id),] %>% group_by(id, name, lat, lon) %>% 
  summarise(total=n(),id = id, name = name, lat= lat, lon = lon) %>% unique()

count1

estacions[estacions$id == 132,]$lat <- 60.39222
estacions[estacions$id == 132,]$lon <- 5.314881
estacions[estacions$id == 803,]$name = "Permanenten"
estacions[estacions$id == 804,]$name = "Stenersen"
estacions[estacions$id == 1889,]$lat <- 60.39923
estacions[estacions$id == 1889,]$lon <- 5.302345
estacions[estacions$id == 3397,]$lat <- 60.35780
estacions[estacions$id == 3397,]$lon <- 5.342070
estacions[estacions$id == 3438,]$lat <- 60.34930
estacions[estacions$id == 3438,]$lon <- 5.288490

group2 <- unique(estacions) %>% group_by(lat, lon) %>% 
  summarise(total=n(), id = id, name = name, lat= lat, lon = lon) %>% filter(total>1)

group2

est_uniques <- unique(estacions)

data[data$start_station_id == 132,]$start_station_latitude <- 60.39222
data[data$start_station_id == 132,]$start_station_longitude <- 5.314881
data[data$start_station_id == 803,]$start_station_name = "Permanenten"
data[data$start_station_id == 804,]$start_station_name = "Stenersen"
data[data$start_station_id == 1889,]$start_station_latitude <- 60.39923
data[data$start_station_id == 1889,]$start_station_longitude <- 5.302345
data[data$start_station_id == 3397,]$start_station_latitude <- 60.35780
data[data$start_station_id == 3397,]$start_station_longitude <- 5.342070
data[data$start_station_id == 3438,]$start_station_latitude <- 60.34930
data[data$start_station_id == 3438,]$start_station_longitude <- 5.288490

data[data$end_station_id == 132,]$end_station_latitude <- 60.39222
data[data$end_station_id == 132,]$end_station_longitude <- 5.314881
data[data$end_station_id == 803,]$end_station_name = "Permanenten"
data[data$end_station_id == 804,]$end_station_name = "Stenersen"
data[data$end_station_id == 1889,]$end_station_latitude <- 60.39923
data[data$end_station_id == 1889,]$end_station_longitude <- 5.302345
data[data$end_station_id == 3397,]$end_station_latitude <- 60.35780
data[data$end_station_id == 3397,]$end_station_longitude <- 5.342070
data[data$end_station_id == 3438,]$end_station_latitude <- 60.34930
data[data$end_station_id == 3438,]$end_station_longitude <- 5.288490

count_start <- data %>% group_by(start_station_id, start_station_name, start_station_latitude, start_station_longitude) %>% 
  tally() %>% rename(station_id = start_station_id, name = start_station_name, latitude = start_station_latitude, 
                     longitude = start_station_longitude, total=n)
#  mutate(start_total=n(), id = start_station_id, name = start_station_name, lat= start_station_latitude, lon = start_station_longitude) 

count_end <- data %>% group_by(end_station_id, end_station_name, end_station_latitude, end_station_longitude) %>% 
 # summarise(end_total=n(), end_id = end_station_id, end_name = end_station_name, end_lat= end_station_latitude, end_lon = end_station_longitude) %>% 
  tally() %>% rename(station_id = end_station_id, name = end_station_name, latitude = end_station_latitude, 
                     longitude = end_station_longitude, total=n)

# GRAFIC DE CERCLES

count_start$class <- 1
count_end$class <- 2

count_total <- count_start[count_start$total>2000,]

count_total <- rbind(count_total, count_end[count_end$station_id %in% count_total$station_id,])

write.csv(count_total,"count_bergen.csv")

# GRAFIC DE DIFERENCIES

count_start2 <- data %>% group_by(start_station_id, start_station_name, start_station_latitude, start_station_longitude) %>% 
  tally() %>% rename(start_total=n)

count_end2 <- data %>% group_by(end_station_id, end_station_name, end_station_latitude, end_station_longitude) %>% 
 tally() %>% rename(end_total=n)

diff <-full_join(count_start2, count_end2, by = join_by( start_station_id == end_station_id), keep = TRUE) %>% 
  select ( start_station_id, start_station_name, start_station_latitude, start_station_longitude, start_total, end_total)  
diff$total <- diff$start_total+diff$end_total
diff$diff <- diff$start_total-diff$end_total

write.csv(diff,"diff_bergen.csv")

#CLASSFICACIÓ ELEVACIÓ

elev$alt <- 1
elev$alt[elev$elevation>9] <- 2
elev$alt[elev$elevation>27] <- 3

elev <- elev[,-c(1)]

hist(elev$alt)
write.csv(elev,"elevation_2.csv")

# AFEGIM DADES D'ELEVACIÓ AL DATASET

elev[,c(1,3,6)]

data <-data %>% left_join(elev[,c(1,3,6)], join_by( start_station_id == station_id), keep = TRUE ) %>% 
  rename(start_elev = elevation, start_alt = alt) 

data <-data %>% left_join(elev[,c(1,3,6)], join_by( end_station_id == station_id), keep = TRUE ) %>% 
  rename(end_elev = elevation, end_alt = alt)  

data$elev_gain <-data$end_elev - data$start_elev
data$temp <- 1 
# "Above 10 degrees"
data$temp[data$temperature<10] <- 0
# "Under 10 degrees"

str(data)

p_num <- c(3,12,13,14,15,16,17,18,19,20,24,25,27,28,29,30)
head(data[, p_num])
cor(data[, p_num])
# install.packages("corrgram")
#install.packages("ggplot2")
#library(corrgram)
#library(ggplot2)
#corrgram(data[, p_num])
data$season <- factor(data$season, levels = c(0,1,2,3), labels = c("Spring","Summer","Fall","Winter"))

ggplot(data, aes(start_alt,fill=season)) + 
  geom_bar() + 
  theme_light()

# DATES
require(lubridate)
data$start_day <- ymd_hms(data$start_time, tz = "Europe/Oslo")

data$weekday = wday(data$start_day, week_start = 1)
table(data$weekday)
# Es sustitueix per dia de la setmana
data$weekend <-0
data$weekend[wday(data$start_day, week_start = 1) %in% c(6,7)] <-1 

data$period_of_day <- 4  
data$period_of_day[hour(data$start_day) <= 17] <- 3
data$period_of_day[hour(data$start_day) <= 11] <- 2
data$period_of_day[hour(data$start_day) <= 5] <- 1

table(data$period_of_day)
data$period_of_day <- factor(data$period_of_day, levels = c(1,2,3,4), labels = c("Night","Morning","Afternoon","Evening"))

# DE NOU DIFFERENCIES AMB ELS NOUS FACTORS

count_start3 <- data %>% group_by(start_station_id, start_station_name, start_station_latitude, start_station_longitude, weekday, period_of_day) %>% 
  tally() %>% rename(start_total=n)

count_end3 <- data %>% group_by(end_station_id, end_station_name, end_station_latitude, end_station_longitude, weekday, period_of_day) %>% 
  tally() %>% rename(end_total=n)

diff <-left_join(count_start3, count_end3, by = join_by( start_station_id == end_station_id, weekday == weekday,  period_of_day == period_of_day )) %>% 
  select ( start_station_id, start_station_name, start_station_latitude, start_station_longitude, weekday, period_of_day, start_total, end_total) 

diff$end_total[is.na(diff$end_total)] <-0

diff_right <-left_join(count_end3, count_start3, by = join_by( end_station_id == start_station_id, weekday == weekday,  period_of_day == period_of_day )) %>% 
  select ( end_station_id, end_station_name, end_station_latitude, end_station_longitude, weekday, period_of_day, start_total, end_total) %>% 
  rename(start_station_id = end_station_id, start_station_name = end_station_name, start_station_latitude = end_station_latitude, start_station_longitude = end_station_longitude)
diff_right <- diff_right[is.na(diff_right$start_total),]
diff_right$start_total = 0

diff <- rbind(diff, diff_right)

diff$total <- diff$start_total+diff$end_total
diff$diff <- diff$start_total-diff$end_total

write.csv(diff,"diff_more_bergen.csv")
