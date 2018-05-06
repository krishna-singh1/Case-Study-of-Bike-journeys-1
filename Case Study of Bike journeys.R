library(dplyr)

#Loading the data set and create a data_dplyr
data<-read.csv("C:/Users/KrRish/Desktop/case study/BikeJourneys_data.csv")
data_dplyr <- tbl_df(data)
View(data_dplyr)
names(data_dplyr)

#Loading the data set and create a data_base data frame
data_base<-as.data.frame(data)

#Format trip_id, user_id and location to be factor variables.

#using dplyr
data_dplyr<-mutate_each(data_dplyr,funs(as.factor),c(trip_id,user_id,location))
sapply(data_dplyr,class)

#using base R
for(i in 1:(ncol(data_base)-1)) data_base[,i]=as.factor(data_base[,i])
sapply(data_base,class)


#3. What is the longest trip taken?

#dplyr
long<-filter(data_dplyr,data_dplyr$time==max(data_dplyr$time))
long

#base
longestTrip<-which(data_base$time==max(data_base$time))
data_base[longestTrip,]


#3. What is the longest trip taken by location?
#base R
aggregate(.~ location,data=data_base, FUN= "max")

#dplyr
data_dplyr%>% group_by(location) %>% filter(time==max(time)) %>% arrange(location) %>% distinct(location,time)



#5. Add a column with the average time for each location. 
#Try to complete using dplyr and base R and time how long it takes to run each version

#dplyr
time1=Sys.time()
total<-data_dplyr%>% group_by(location) %>% summarise(mean(time))
data_dplyr<-data_dplyr%>%inner_join(total, by='location')
names(data_dplyr$`mean(time)`)<-c("Avg_time")
Sys.time()-time1

#base R
time2=Sys.time()
totalbase<-aggregate(data_base$time,list(data_base$location), mean)
names(totalbase)<-c("location","Avg_time")
data_base<-inner_join(data_base,totalbase, by='location')
Sys.time()-time2

#6. Create a data frame with only riders who have taken over 100 journeys. Do this just with dplyr
#dplyr
summary(data_base$user_id)
df<- data.frame(data_dplyr %>% group_by(user_id) %>% summarise(total_no_trip=n()) %>% filter(total_no_trip>100))
df

#7. For riders who have taken over 100 journeys what is the most popular location? Do this just with dplyr
 newData<-data_dplyr %>% filter(user_id==df[,1])
newData%>% group_by(location) %>% summarise(Total_no_location=n()) %>% filter(Total_no_location==max(Total_no_location))
