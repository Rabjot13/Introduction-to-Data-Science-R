#MODULE 1 - 7 january
### INVOKE MAGRITTR PACKAGE
#DPLYR PACKAGE - useful for data visualization and gives the best results for readable data
library("dplyr")

##Loading dplyr and an example dataset 
##Hflights is a dataset for flights departing from 2 Houston airports in 2011

#Load packages
supressMessages(library(dplyr))
library(hflights)
install.packages("hflights")
library(hflights)

##EXPLORING DATA 
data("hflights")
head(hflights)
summary(hflights)
nrow(hflights)
ncol(hflights)
dim(hflights)

#'tbl_df' creates a local dataframe(wrapper for a dataframe that prints nicely)
#'convert to local dataframe
flights = tbl_df(hflights)
flights
print(flights,n=20)

##converting to a normal data frame to see all columns
data.frame(head(flights))

##base R approach to view all flights on January 1 (traditional approach)
flights[flights$Month==1 & flights$DayofMonth==1,]

###DPLYR APPROACH
filter(flights,Month==1,DayofMonth==1) ## comma is being used as and
##in this function we used all those parameters which were needed

#Use pipe for OR condition
filter(flights,UniqueCarrier=="AA"| UniqueCarrier == "UA") ##substitute of OR in dplyr

##In operator
filter(flights,UniqueCarrier %in% c("AA","UA"))

##traditional approach for selecting DepTime,ArrTime, and FlightNum Columns
flights[,c("DepTime","ArrTime","FlightNum")]
##DPLYR APPROACH - for selecting any row or column
select(flights,DepTime,ArrTime,FlightNum)


library(magrittr)
##use colon to select multiple contiguous columns, and use 'contains' to match columns by their name 
##  starts_with, ends_with , matches can also be used to match columns

select(flights,Year:DayofMonth,contains("Taxi"),contains("Delay"))

##CHAINING OR PIPELINE OPERATOR - helps in performing multiple operations in one line by nesting
##helps in uderstanding the flow from arrows

##Traditional Approach
filter(select(flights,UniqueCarrier,DepDelay),DepDelay>60)

##CHAINING METHOD
flights %>%
  select(UniqueCarrier,DepDelay)%>%
  filter(DepDelay > 60)

flights %>%
  select(UniqueCarrier,DepDelay)

flights %<>%
  select(UniqueCarrier,DepDelay)%<>%
  filter(DepDelay > 60)


##CREATE TWO VECTORS AND CALCULATE EUCLIDIEAN DISTANCE BETWEEN THEM
##Traditional Approach
x1 = 1:5;x2 = 2:6  
sqrt(sum((x1-x2)^2))

##DPLYR APPROACH
(x1-x2)^2 %>% sum() %>% sqrt()

##Arranging 
##Traditional approach
flights[order(flights$DepDelay),c("UniqueCarrier","DepDelay")]

##Dplyr Approach
flights %>%
  select(UniqueCarrier,DepDelay) %>%
  arrange(DepDelay)


flights %>%
  select(UniqueCarrier,DepDelay) %>%
  arrange(desc(DepDelay))


##CREATING NEW COLUMNS
##Traditional approach
flights$Speed = flights$Distance/flights$AirTime*60
flights[,c("Distance","AirTime","Speed")]

##DPLYR APPROACH
flights %>%
  select(Distance,AirTime) %>%
  mutate(Speed = Distance/AirTime*60)
#Storing new variable
flights = flights %>% mutate(Speed = Distance/AirTime*60)

##SUMMARISE : REDUCE VARIABLES TO VALUES
## traditional approach : calculate mean of arrival delay based on destination
head(with(flights,tapply(ArrDelay,Dest,mean,na.rm=T)))
head(aggregate(ArrDelay~Dest,flights,mean))

#Dplyr Approach
flights %>%
  group_by(Dest) %>%
  summarise(avg_delay = mean(ArrDelay,na.rm = T))