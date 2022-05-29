require(readr)
library(ggplot2)
library(dplyr)
library(usmap)
library(ggpubr)

# Load data
airports = read.csv(file = "./airports.csv")
planeData = read.csv(file = "./plane-data.csv")
carriers = read.csv(file = "./carriers.csv")

flights88 = read_csv("./1988.csv.bz2", col_types = cols(.default = 'i', UniqueCarrier = 'c', TailNum = 'c', Origin = 'c',Dest = 'c', CancellationCode = 'c'))
flights89 = read_csv("./1989.csv.bz2", col_types = cols(.default = 'i', UniqueCarrier = 'c', TailNum = 'c', Origin = 'c',Dest = 'c', CancellationCode = 'c'))

# Find NAs
naCol89 = names(which(colSums(is.na(flights89))>0))
naCol88 = names(which(colSums(is.na(flights88))>0))

# Fill up NAs by category - All delays make 0. 
flights89$ArrDelay[is.na(flights89$ArrDelay)] = 0
flights89$DepDelay[is.na(flights89$DepDelay)] = 0
flights89$CarrierDelay[is.na(flights89$CarrierDelay)] = 0
flights89$WeatherDelay[is.na(flights89$WeatherDelay)] = 0
flights89$NASDelay[is.na(flights89$NASDelay)] = 0
flights89$SecurityDelay[is.na(flights89$SecurityDelay)] = 0
flights89$LateAircraftDelay[is.na(flights89$LateAircraftDelay)] = 0

flights88$ArrDelay[is.na(flights88$ArrDelay)] = 0
flights88$DepDelay[is.na(flights88$DepDelay)] = 0
flights88$CarrierDelay[is.na(flights88$CarrierDelay)] = 0
flights88$WeatherDelay[is.na(flights88$WeatherDelay)] = 0
flights88$NASDelay[is.na(flights88$NASDelay)] = 0
flights88$SecurityDelay[is.na(flights88$SecurityDelay)] = 0
flights88$LateAircraftDelay[is.na(flights88$LateAircraftDelay)] = 0

# Figuring out that ElapsedTime NAs have Origin & Dest values
sum(is.na(flights88$ActualElapsedTime) & !is.na(flights88$Origin) & !is.na(flights88$Dest)) # Originally: 64599

# Just in case, calc avg Dataset times and have them as backup
avg88 = mean(flights88$ActualElapsedTime[!is.na(flights88$ActualElapsedTime)])
avg89 = mean(flights89$ActualElapsedTime[!is.na(flights89$ActualElapsedTime)])

# Finds bidirectional Avg Flight Time
findAvgTimeFromTo = function(DF, Frm, To, avrg){
  if(is.na(Frm) | is.na(To)){
    print("Found NA in FRM & To")
    return(avrg)
  }
  times = DF$ActualElapsedTime[!is.na(DF$ActualElapsedTime) & ((DF$Origin == Frm & DF$Dest == To) |  (DF$Origin == To & DF$Dest == Frm))]
  return(mean(times))
}

# Iterates DF and tries to find avg time of flights
for(i in 1:nrow(flights88)){
  if(is.na(flights88$ActualElapsedTime[i])){
    flights88$ActualElapsedTime[i] = findAvgTimeFromTo(flights88, flights88$Origin[i], flights88$Dest[i], avg88)  
  }
}

# Iterates DF and tries to find avg time of flights
for(i in 1:nrow(flights89)){
  if(is.na(flights89$ActualElapsedTime[i])){
    flights89$ActualElapsedTime[i] = findAvgTimeFromTo(flights89, flights89$Origin[i], flights89$Dest[i], avg89)  
  }
}

# After running
sum(is.na(flights88$ActualElapsedTime) & !is.na(flights88$Origin) & !is.na(flights88$Dest)) # 16

# Fill up ActualElapsedTime NAs with AirTime - For those 16 left, have avg time
flights88$ActualElapsedTime[is.na(flights88$ActualElapsedTime)] = avg88
flights89$ActualElapsedTime[is.na(flights89$ActualElapsedTime)] = avg89


# DeptTime & ArrTime

# Have at least one of two destinations filled - All missing

flights88$DeptTimeHours = as.integer(flights88$DepTime / 100)
flights88$ArrTimeHours = as.integer(flights88$ArrTime / 100)
flights88$DeptTimeMin = as.integer(flights88$DepTime %% 100)
flights88$ArrTimeMin = as.integer(flights88$ArrTime %% 100)

flights89$DeptTimeHours = as.integer(flights89$DepTime / 100)
flights89$ArrTimeHours = as.integer(flights89$ArrTime / 100)
flights89$DeptTimeMin = as.integer(flights89$DepTime %% 100)
flights89$ArrTimeMin = as.integer(flights89$ArrTime %% 100)

for(i in 1:nrow(flights88)){
  if(is.na(flights88$DepTime[i])){
    flights88$DepTime[i] = flights88$CRSDepTime[i]
    flights88$DeptTimeMin[i] = as.integer(flights88$DepTime[i] %% 100)
    flights88$DeptTimeHours[i] = as.integer(flights88$DepTime[i] / 100)
  }
  if(is.na(flights88$ArrTime[i])){
    hours = as.integer(flights88$ActualElapsedTime[i] / 60)
    
    # Calc how many minutes spent on aircraft and if hour changed (started at 3.50 and arrived 4.30)
    mins = flights88$ActualElapsedTime[i] %% 60 + flights88$DeptTimeMin[i]
    
    hours = (hours + as.integer(mins/60) + flights88$DeptTimeHours[i]) %% 24
    mins = mins %% 60
    
    flights88$ArrTime[i] = hours * 100 + mins
    flights88$ArrTimeMin[i] = mins
    flights88$ArrTimeHours[i] = hours
  }
}

for(i in 1:nrow(flights89)){
  if(is.na(flights89$DepTime[i])){
    flights89$DepTime[i] = flights89$CRSDepTime[i]
    flights89$DeptTimeMin[i] = as.integer(flights89$DepTime[i] %% 100)
    flights89$DeptTimeHours[i] = as.integer(flights89$DepTime[i] / 100)
  }
  
  if(is.na(flights89$ArrTime[i])){
    hours = as.integer(flights89$ActualElapsedTime[i] / 60)
    
    # Calc how many minutes spent on aircraft and if hour changed (started at 3.50 and arrived 4.30)
    mins = flights89$ActualElapsedTime[i] %% 60 + flights89$DeptTimeMin[i]
    
    hours = (hours + as.integer(mins/60) + flights89$DeptTimeHours[i]) %% 24
    mins = mins %% 60
    
    flights89$ArrTime[i] = hours * 100 + mins
    flights89$ArrTimeMin[i] = mins
    flights89$ArrTimeHours[i] = hours
  }
}

flights88$AirTime = (flights88$ArrTimeHours - flights88$DeptTimeHours )*60 + (flights88$ArrTimeMin -flights88$DeptTimeMin)
flights89$AirTime = (flights89$ArrTimeHours - flights89$DeptTimeHours )*60 + (flights89$ArrTimeMin -flights89$DeptTimeMin)

# Find flights covered distances


avg88 =  mean(flights88$Distance[!is.na(flights88$Distance)])
avg89 =  mean(flights89$Distance[!is.na(flights89$Distance)])

# Finds bidirectional Avg Distance
findAvgDistFromTo = function(DF, Frm, To, avrg){
  if(is.na(Frm) | is.na(To)){
    print("Found NA in FRM & To")
    return(avrg)
  }
  times = DF$Distance[!is.na(DF$Distance) & ((DF$Origin == Frm & DF$Dest == To) |  (DF$Origin == To & DF$Dest == Frm))]
  return(mean(times))
}

# Iterates DF and tries to find avg time of flights
for(i in 1:nrow(flights88)){
  if(is.na(flights88$Distance[i])){
    flights88$Distance[i] = findAvgDistFromTo(flights88, flights88$Origin[i], flights88$Dest[i], avg88)  
  }
}


# Iterates DF and tries to find avg time of flights
for(i in 1:nrow(flights89)){
  if(is.na(flights89$Distance[i])){
    flights89$Distance[i] = findAvgDistFromTo(flights89, flights89$Origin[i], flights89$Dest[i], avg89)  
  }
}


# Remove Non related Columns

flights88$TailNum = NULL
flights88$AirTime = NULL
flights88$TaxiIn = NULL
flights88$TaxiOut = NULL
flights88$CancellationCode = NULL
flights88$CarrierDelay = NULL
flights88$NASDelay = NULL
flights88$SecurityDelay = NULL
flights88$LateAircraftDelay = NULL
flights88$WeatherDelay = NULL


flights89$TailNum = NULL
flights89$AirTime = NULL
flights89$TaxiIn = NULL
flights89$TaxiOut = NULL
flights89$CancellationCode = NULL
flights89$CarrierDelay = NULL
flights89$NASDelay = NULL
flights89$SecurityDelay = NULL
flights89$LateAircraftDelay = NULL
flights89$WeatherDelay = NULL


# Since there is no tailnum, assign randomly values from the table. First clean it up

planeData = planeData[!is.na(planeData$model) & planeData$model!= '', ]
tester8 = array(sample(1:nrow(planeData), nrow(flights88), replace=T), c(nrow(flights88)))
tester = array(sample(1:nrow(planeData), nrow(flights89), replace=T), c(nrow(flights89)))
retRand = function(i){
  return(planeData$tailnum[i])
}
test1 = lapply(tester, retRand)
test2 = lapply(tester8, retRand)
flights88$TailNum = unlist(test2)
flights89$TailNum = unlist(test1)

rm(test1)
rm(test2)
rm(tester8)
rm(tester)


# Combine DFs
flightsAll = rbind(flights88,flights89)
flightsAll$YearF = as.factor(flightsAll$Year)

# Generate Visuals
# Differences between the two years in: 

# Total Flights per Month

ggplot(flightsAll, aes(x=Month, fill=YearF)) + 
  ggtitle("Total flights per month") + ylab("Total flights (in thousands)")+
  geom_histogram(binwidth = 0.5, position="dodge") +
  expand_limits(x = c(1, NA), y = c(0, NA)) +
  scale_x_discrete(limits=month.abb)  +
  scale_y_continuous(labels = scales::label_number_si()) 

# Total Distance per Month
groupedMonYr = flightsAll %>% 
  select(Distance, Year, Month) %>%
  group_by(Year, Month) %>%
  summarise_all(sum)
groupedMonYr$Year = groupedMonYr$Year + 1997 
groupedMonYr$YearF = as.factor(groupedMonYr$Year)

ggplot(groupedMonYr, aes(x=Month,y=Distance,fill=YearF)) + 
  ggtitle("Total distance covered per month") + ylab("Total Distance (in mil.)")+
  geom_bar(stat='identity') +
  scale_x_discrete(limits=month.abb)  +
  scale_y_continuous(labels = scales::label_number_si()) 

# Most common destinations per year (To) - Airport Cities

groupedCitiesMonYr88 = flights88 %>% 
  select(Dest, Year) %>%
  group_by(Year, Dest) %>%
  summarise(count=n())
groupedCitiesMonYr89 = flights89 %>% 
  select(Dest, Year) %>%
  group_by(Year, Dest) %>%
  summarise(count=n())

groupedTop10CitiesMonYr88 = top_n(groupedCitiesMonYr88, n=10, count)
groupedTop10CitiesMonYr88$iata = groupedTop10CitiesMonYr88$Dest
groupedTop10CitiesMonYr89 = top_n(groupedCitiesMonYr89, n=10, count)
groupedTop10CitiesMonYr89$iata = groupedTop10CitiesMonYr89$Dest

groupedTop10CitiesMonYr88 = merge(groupedTop10CitiesMonYr88, airports, by="iata")
groupedTop10CitiesMonYr89 = merge(groupedTop10CitiesMonYr89, airports, by="iata")

groupedTop10CitiesMonYr88$Year = 1998
groupedTop10CitiesMonYr88$YearF = as.factor(groupedTop10CitiesMonYr88$Year)
groupedTop10CitiesMonYr89$Year = 1999
groupedTop10CitiesMonYr89$YearF = as.factor(groupedTop10CitiesMonYr89$Year)
top10Cities = left_join(airports, groupedTop10CitiesMonYr88, by="iata")

world = map_data("world")
us_states = worldmap[worldmap$continent == 'America',]
us_counties = map_data("county")


# Global map with spots
ggplot() +
  geom_map(
    data = us_states, map = us_states,
    aes(long, lat, map_id = region),
    color = "black", fill = "lightgray", size = 0.1
  ) +
  geom_point(
    data = groupedTop10CitiesMonYr88,
    aes(long, lat, color = YearF, 
        size=count, label=Dest),
    alpha = 0.7
  ) +
  geom_point(
    data = groupedTop10CitiesMonYr89,
    aes(long, lat, color = YearF, 
        size=count,label=Dest),
    alpha = 0.7
  )



ggplot(groupedTop10CitiesMonYr88, aes(x=city,y=count,fill=YearF)) + 
  ggtitle("Top Destinations 1988") + ylab("Times Visted")+ xlab("Cities")+
  geom_bar(stat='identity') +
  scale_y_continuous(labels = scales::label_number_si()) 

ggplot(groupedTop10CitiesMonYr89, aes(x=city,y=count,fill=YearF)) + 
  ggtitle("Top Destinations 1989") + ylab("Times Visted")+ xlab("Cities")+
  geom_bar(stat='identity') +
  scale_y_continuous(labels = scales::label_number_si()) 


# Top 10 Carrier per Year (Total Flights)

groupedCitiesMonYr88 = flights88 %>% 
  select(UniqueCarrier, Year) %>%
  group_by(Year, UniqueCarrier) %>%
  summarise(count=n())
groupedCitiesMonYr89 = flights89 %>% 
  select(UniqueCarrier, Year) %>%
  group_by(Year, UniqueCarrier) %>%
  summarise(count=n())

groupedTop10CitiesMonYr88 = top_n(groupedCitiesMonYr88, n=10, count)
groupedTop10CitiesMonYr88$Code = groupedTop10CitiesMonYr88$UniqueCarrier
groupedTop10CitiesMonYr89 = top_n(groupedCitiesMonYr89, n=10, count)
groupedTop10CitiesMonYr89$Code = groupedTop10CitiesMonYr89$UniqueCarrier

groupedTop10CitiesMonYr88 = merge(groupedTop10CitiesMonYr88, carriers, by="Code")
groupedTop10CitiesMonYr89 = merge(groupedTop10CitiesMonYr89, carriers, by="Code")

groupedTop10CitiesMonYr88$Year = 1998
groupedTop10CitiesMonYr88$YearF = as.factor(groupedTop10CitiesMonYr88$Year)
groupedTop10CitiesMonYr89$Year = 1999
groupedTop10CitiesMonYr89$YearF = as.factor(groupedTop10CitiesMonYr89$Year)

groupedTop10CitiesMonYr88$ShortDesc = substr(groupedTop10CitiesMonYr88$Description, 0, 10)
groupedTop10CitiesMonYr89$ShortDesc = substr(groupedTop10CitiesMonYr89$Description, 0, 10)

ggplot(groupedTop10CitiesMonYr88, aes(x=ShortDesc,y=count,fill=Year)) + 
  ggtitle("Top Companies 1988") + ylab("Flights #")+ xlab("Company")+
  geom_bar(stat='identity') +
  scale_y_continuous(labels = scales::label_number_si()) +
  theme(axis.text.x = element_text(angle = 45))

ggplot(groupedTop10CitiesMonYr89, aes(x=ShortDesc,y=count,fill=Year)) + 
  ggtitle("Top Companies 1989") + ylab("Flights #")+ xlab("Company")+
  geom_bar(stat='identity') +
  scale_y_continuous(labels = scales::label_number_si()) +
  theme(axis.text.x = element_text(angle = 45))


# These Top 10 in 88 what did they do in 89
# The 89 Top 10, what did they do in 88 
# .... 

topCompanies = unique(groupedTop10CitiesMonYr88$UniqueCarrier)
flightsTop = flightsAll[flightsAll$UniqueCarrier %in% topCompanies, ]
flightsTop$YearF = as.factor(flightsTop$Year)
flightsTop$Code = flightsTop$UniqueCarrier
flightsTop$iata = flightsTop$Dest
flightsTop = merge(flightsTop, carriers, by="Code")
flightsTop = merge(flightsTop, airports, by="iata")
flightsTop$ShortDesc = substr(flightsTop$Description, 0, 10)


# in terms of totalFlights (Top)

groupedTopCitiesprComp = flightsTop %>% 
  select(Dest, Code ) %>%
  group_by(Code, Dest) %>%
  summarise(count=n())

groupedTopCitiesprComp = merge(groupedTopCitiesprComp, carriers, by ='Code' )
groupedTopCitiesprComp$iata = groupedTopCitiesprComp$Dest
groupedTopCitiesprComp = merge(groupedTopCitiesprComp, airports, by="iata")

splitDF = split(groupedTopCitiesprComp, with(groupedTopCitiesprComp, interaction(Code)),drop = T)


pltArr = c()
labArr = c()
for(i in 1:3){
  splitDF[[i]] = top_n(splitDF[[i]], n=10, count)
  ggplot() +
    geom_map(
      data = us_states, map = us_states,
      aes(long, lat, map_id = region),
      color = "black", fill = "lightgray", size = 0.1
    ) +
    geom_point(
      data = splitDF[[i]],
      aes(long, lat, color = Code, 
          size=count),
      alpha = 0.7
    )

}
binded = rbind(splitDF[[1]], splitDF[[2]], splitDF[[3]])
ggplot() +
  ggtitle("Top destinations of top 3 companies ") + 
  geom_map(
    data = us_states, map = us_states,
    aes(long, lat, map_id = region),
    color = "black", fill = "lightgray", size = 0.1
  ) +
  geom_point(
    data = binded,
    aes(long, lat, color = Description, 
        size=count),
    alpha = 0.7
  ) + 
  geom_text(data= binded, aes(label = state, x = long, y = lat-2), position = "identity") 

# in terms of total distance covered (by Month)

groupedDistComp = flightsTop %>% 
  select(Distance, Description ) %>%
  group_by(Description) %>%
  summarise_all(sum)

groupedDistComp$ShortDesc = substr(groupedDistComp$Description, 0, 10)

ggplot(groupedDistComp, aes(x=ShortDesc,y=Distance,fill=ShortDesc)) + 
  ggtitle("Total miles of top companies") + ylab("Distance #")+ xlab("Company")+
  geom_bar(stat='identity') +
  scale_y_continuous(labels = scales::label_number_si()) +
  theme(axis.text.x = element_text(angle = 45))

# in terms of TotalDelay (DeptDelay + ArrDelay) over Months / Year

flightsTop$ArrDelayAbove10 = flightsTop$ArrDelay > 30 

groupTimesVeryLate = flightsTop[flightsTop$ArrDelayAbove10 == TRUE, ] %>% 
  select(ArrDelayAbove10, Description ) %>%
  group_by(Description) %>%
  summarise(count=n())

groupTimesVeryLate$ShortDesc = substr(groupTimesVeryLate$Description, 0, 10)

ggplot(groupTimesVeryLate, aes(x=ShortDesc,y=count,fill=ShortDesc)) + 
  ggtitle("Total number of times top companies delay") + ylab("# of Delayed flights")+ xlab("Company")+
  geom_bar(stat='identity') +
  scale_y_continuous(labels = scales::label_number_si()) +
  theme(axis.text.x = element_text(angle = 45))


# Kept Promise% (Arr time vs CRS Arrtime) over Months / Year
flightsTop$promiseNotKept = flightsTop$CRSArrTime > flightsTop$ArrDelay

promNotKept = flightsTop[flightsTop$promiseNotKept == FALSE, ] %>% 
  select(promiseNotKept, Description ) %>%
  group_by(Description) %>%
  summarise(count=n())

promNotKept$ShortDesc = substr(promNotKept$Description, 0, 10)

totalNum = sum(promNotKept$count)

ggplot(promNotKept,aes(x=1, y=count, fill=ShortDesc)) +
  geom_col() +
  ggtitle("Distribution of non kept promises (Delayed") + 
  geom_text(aes(x= 1.5, label = paste(as.integer(count/totalNum * 100), "%")), position = position_stack(vjust = 0.5))+
  coord_polar(theta = "y") + 
  theme_void() +
  scale_fill_brewer(palette = "Pastel1")

# Temporarily save csv
# write_csv(flights88, "./1988_upd.csv.bz2")
# write_csv(flights89, "./1989_upd.csv.bz2")
