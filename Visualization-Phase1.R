
Data=read.csv("Data.csv")
str(Data)
unique(Data$RequestSource)
library(dplyr)
library(ggplot2)
library(lubridate)
library(stringr)

Data$CreatedDate=ymd_hms(Data$CreatedDate)



## 1. Distribution of requests, distribution departments referred to

# Create the date range for the dataset: 2015/11/01 - 2016/11/01

Data$Date=str_extract_all(Data$CreatedDate,"[0-9]{4}[-][0-9]{2}[-][0-9]{2}")
Data$Date = ymd(Data$Date)

Data_1Y = Data%>%
  filter(Date>"2015-11-01" & Date< "2016-10-31")

# Distribution of requests by month
Data_1Y$Created_Month = factor(Data_1Y$Created_Month,levels = levels(Data_1Y$Created_Month)[c(10,3,5,4,8,1,9,7,6,2,12,11)])

ggplot(Data_1Y,aes(Created_Month)) +
  geom_bar(fill="#1AB58A") +
  xlab("") +
  ylab("") +
  ggtitle("Distribution of Requests by Month") +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 30, family="Helvetica"),
    axis.text.y = element_text(family="Helvetica"),
    plot.title = element_text(face="bold",color="darkgrey")
  )

# Distribution of requests by Day_of_Week
Data_1Y$Created_Day = factor(Data_1Y$Created_Day,levels = levels(Data_1Y$Created_Day)[c(2,6,7,5,1,3,4)])

ggplot(Data_1Y,aes(Created_Day)) +
  geom_bar(fill="#FFAD29") +
  xlab("") +
  ylab("") +
  ggtitle("Distribution of Requests by Day of Week") +
  theme_classic() +
  theme(
    axis.text.x = element_text(family="Helvetica"),
    axis.text.y = element_text(family="Helvetica"),
    plot.title = element_text(face="bold",color="darkgrey")
  )

# HeatMap of requests by Hour and Dayofweek
Data1 = Data_1Y%>%
  group_by(Created_Day,Created_Hour)%>%
  summarise(Count=n())

ggplot(Data1,aes(Created_Day,factor(Created_Hour),fill=Count)) +
  geom_tile() +
  scale_fill_gradient(low="#1AB58A", high = "#FF2151") +
  xlab("Day of Week") +
  ylab("Hour") +
  ggtitle("Heat Map of Requests By Hour and Day_of_Week") +
  theme_classic() +
  theme(
    axis.text.x = element_text(family="Helvetica",angle = 30),
    axis.text.y = element_text(family="Helvetica"),
    plot.title = element_text(face="bold",color="darkgrey")
  )

# Distribution of departments reffered to
ggplot(Data_1Y,aes(Owner_Full)) +
  geom_bar(fill="#FFAD29") +
  xlab("Department") +
  ylab("") +
  ggtitle("Distribution of Departments reffered to") +
  theme_linedraw() +
  theme(
    axis.text.x = element_text(family="Helvetica",angle = 20, h=1),
    axis.text.y = element_text(family="Helvetica"),
    plot.title = element_text(face="bold",color="darkgrey")
  )


## 2.App vs call vs Self_Service referrals, service type question for each input channel

# Create the subset contains the RequestSource of App,call,Self-serice

Data3 = Data_1Y%>%
  filter(RequestSource %in% c("Call","Mobile App","Self Service"))

# General distribution of requests through 3 major Request Sources
ggplot(Data3,aes(RequestSource)) +
  geom_bar(fill="#1AB58A",) +
  xlab("Major Request Soucr") +
  ylab("") +
  ggtitle("Distribution of Requests by Major Request Source") +
  theme_classic() +
  theme(
    axis.text.x = element_text(family="Helvetica"),
    axis.text.y = element_text(family="Helvetica"),
    plot.title = element_text(face="bold",color="darkgrey")
  )


# General distribution of Service Questions through 3 major Request Sources
ggplot(Data3,aes(RequestType,fill=RequestType)) +
  geom_bar() +
  xlab("Major Request Source") +
  ylab("") +
  ggtitle("General distribution of Service Questions through 3 major Request Sources") +
  theme_classic() +
  theme(
    axis.text.x = element_text(family="Helvetica",angle = 30, h=1, size=8),
    axis.text.y = element_text(family="Helvetica"),
    plot.title = element_text(face="bold",color="darkgrey")
  ) +
  facet_grid(RequestSource~., scales = "free")

## Distribution of Request Source on Different Hour
Data4 = Data_1Y%>%
  filter(RequestSource%in% c("Call","Mobile App","Self Service"))%>%
  group_by(Created_Hour,RequestSource)%>%
  summarise(Count=n())

ggplot(Data4,aes(factor(Created_Hour),Count,fill=RequestSource)) +
  geom_bar(stat = "identity") +
  xlab("Hour") +
  ylab("") +
  ggtitle("Distribution of Request Source on Different Hour") +
  theme_classic() +
  theme(
    axis.text.x = element_text(family="Helvetica",angle = 30),
    axis.text.y = element_text(family="Helvetica"),
    plot.title = element_text(face="bold",color="darkgrey")
  ) +facet_grid(RequestSource~., scales = "free") +
  scale_fill_manual(labels=letters[1:3], values=c("#FF2151","#FF7729","#FFAD29"),guid=F) 




##3

Data2=Data%>%
  mutate(CreatedDate_1=str_sub(CreatedDate,start=1,end=10))%>%
  mutate(Created_Year=year(CreatedDate))%>%
  mutate(CreatedDate_2=str_sub(CreatedDate,start=1,end=7))

Data2$CreatedDate_1=ymd(Data2$CreatedDate_1)

str(Data2)

#(1)Trend by Date
Data2%>%
  filter(RequestSource=="Call")%>%
  group_by(CreatedDate_1)%>%
  summarise(Count_source=n())%>%
  ggplot(aes(x=CreatedDate_1,y=Count_source))+
  geom_line(stat = "identity")


#(2)Trend by year
lev=levels(Data2$Created_Month)
lev=lev[c(5,4,8,1,9,7,6,2,12,11,10,3)]
Data2$Created_Month=factor(Data2$Created_Month,levels = lev)


Data2%>%
  filter(RequestSource=="Call")%>%
  group_by(Created_Month,Created_Year)%>%
  summarise(Count_Source2=n())%>%
  ggplot(aes(x=Created_Month,y=Count_Source2,group=1))+
  geom_point(stat = "identity")+
  geom_line() +
  facet_wrap(~Created_Year)

#(3)Heatmap by Day and Hour
Data2$Created_Day=factor(Data2$Created_Day,levels=levels(Data2$Created_Day)[c(2,6,7,5,1,3,4)])

Data2%>%
  filter(RequestSource=="Call")%>%
  group_by(Created_Hour,Created_Day)%>%
  summarise(Count_Source3=n())%>%
  ggplot(aes(Created_Day,Created_Hour,fill=Count_Source3))+
  geom_tile()+
  scale_fill_gradient(low = "white",high = "darkgreen")

##5.Service Type Breakdowns
unique(Data2$RequestType)

#(1)Compare request types in general
ggplot(Data2,aes(x=RequestType,fill=RequestType))+
  geom_bar()

#(2)Request types separated by year
Data2%>%
  group_by(RequestType,Created_Year)%>%
  summarise(Count_Type=n())%>%
  ggplot(aes(x=RequestType,y=Count_Type,fill=RequestType))+
  geom_bar(stat = "identity")+
  facet_wrap(~Created_Year)

#(3)Request types compared by year
Data2%>%
  group_by(Created_Month,Created_Year,RequestType)%>%
  summarise(Count_Type2=n())%>%
  ggplot(aes(Created_Month,Count_Type2,fill=RequestType))+
  geom_bar(stat = "identity",position = position_dodge())+
  facet_wrap(~Created_Year)


#(4)Map
library(ggmap)
LA="Los Angeles"
LAmap=qmap(LA,zoom=10,color="bw",maptype="roadmap")
LAmap+
  stat_bin2d(data = Data2,aes(x=Longitude,y=Latitude,fill=RequestType),bins = 30,alpha=0.2)

LAmap+
  geom_point(data = Data2,aes(x=Longitude,y=Latitude,color=RequestType))


##6.Change Volume

#(1)Change Volume of Requests Overtime
Data2%>%
  group_by(CreatedDate_1)%>%
  summarise(Count_Requests=n())%>%
  ggplot(aes(x=CreatedDate_1,y=Count_Requests))+
  geom_line(stat = "identity")



## 7. Geographic + Service Type breakdown

# Request Distributions on Geo
Data5 = Data_1Y%>%
  filter(RequestSource %in% c("Call","Mobile App","Self Service"))
library(ggmap)
LA = qmap("Los Angeles", color = "bw", zoom=10)

qmap("Los Angeles", color="bw",zoom=12) +
  stat_density2d(data=Data5,aes(Longitude,Latitude,fill=..level..,alpha=..level..),geom="polygon") +
  scale_fill_gradient(low = "#1AB58A", high = "#FF7729") +
  facet_wrap(~Created_Day, nrow = 3) +
  theme(legend.position = "none")



# Request Source Distributions on Geo
LA = qmap("Los Angeles", color = "bw", zoom=10)

qmap("Los Angeles", color = "bw", zoom=12)+
  stat_density2d(data=Data5,aes(Longitude,Latitude,fill=..level..,alpha=..level..),geom="polygon") +
  scale_fill_gradient(low = "#1AB58A", high = "#FF7729") +
  facet_wrap(~RequestSource, nrow = 3) +
  theme(legend.position = "none")



# Request Type Distributions on Geo
table(RequestType)

Data6 = Data_1Y%>%
  filter(RequestType %in% c("Bulky Items","Dead Animal Removalp","Electronic Waste","Graffiti Removal","Illegal Dumping Pickup","Metal/Household Appliances","Homeless Encampment","Single Streetlight Issue"))

qmap("Los Angeles", color = "bw", zoom=12)+
  stat_density2d(data=Data6,aes(Longitude,Latitude,fill=..level..,alpha=..level..),geom="polygon") +
  scale_fill_gradient(low = "#1AB58A", high = "#FF7729") +
  facet_wrap(~RequestType, nrow = 3) +
  theme(legend.position = "none")











