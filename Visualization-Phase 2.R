library(ggplot2)
library(dplyr)
library(lubridate)
library(stringr)


Data_merged = read.csv("Data_merged.csv",header = T)

str(Data_merged)

Data_merged$CRDate=str_extract_all(Data_merged$CreatedDate,"[0-9]{4}[-][0-9]{2}[-][0-9]{2}")
Data_merged$CRDate = ymd(Data_merged$CRDate)

Data_merged=Data_merged%>%
  mutate(SRDate=str_sub(ServiceDate,start=1,end=10))%>%
  mutate(UPDate=str_sub(UpdatedDate,start=1,end=10))

Data_merged$SRDate=mdy(Data_merged$SRDate)
Data_merged$UPDate=mdy(Data_merged$UPDate)

##Narrow down to 1 year period
Data_1Y = Data_merged%>%
  filter(CRDate>"2015-11-01" & CRDate< "2016-10-31")

Data_1Y%>%
  group_by(RequestSource)%>%
  summarise(Count=n())%>%
  arrange(-Count)

##Call, Driver Self Report, Mobile App, and Self Service are four major request source.

##Check the processing time for each source:



Data_1Y%>%
  mutate(Duration=UPDate-CRDate)%>%
  filter(RequestSource%in%c("Call","Driver Self Report","Mobile App","Self Service"))%>%
  group_by(RequestSource)%>%
  summarise(Ave_Duration=mean(Duration))%>%
  ggplot(aes(reorder(RequestSource,Ave_Duration),Ave_Duration,fill=RequestSource))+
  geom_bar(stat = "identity")+
  xlab("Request Source")+
  ylab("Average Processing Time")+
  ggtitle("Processing Time of Major Request Sources")+
  scale_fill_manual(labels=letters[1:4], values=c("#FFAD29","#FCF2D7","#FF7729","#E65848"),guid=F)+
  theme_classic()+
  theme(
    axis.title=element_text(face = "bold",colour = "darkgray"),
    axis.text.x = element_text(family="Helvetica",angle = 30),
    axis.text.y = element_text(family="Helvetica"),
    plot.title = element_text(face="bold",color="darkgrey")
  )


Data_1Y%>%
  group_by(RequestType)%>%
  summarise(Count_Type=n())%>%
  arrange(-Count_Type)  

##Bulky Items, Graffiti Removal, Metal/Household Appliances, Illegal Dumping Pickup, and Electronic Waste are Five major request source.

##Check the duration of service for each type:


Data_1Y%>%
  mutate(Duration=UPDate-CRDate)%>%
  group_by(RequestType)%>%
  summarise(Ave_Duration=mean(Duration))%>%
  ggplot(aes(reorder(RequestType,Ave_Duration),Ave_Duration,fill=RequestType))+
  geom_bar(stat = "identity")+
  xlab("Request Type")+
  ylab("Average Processing Time")+
  ggtitle("Processing Time of Request Types")+
  theme_classic()+
  theme(
    axis.title=element_text(face = "bold",colour = "darkgray"),
    axis.text.x = element_text(family="Helvetica",angle = 30),
    axis.text.y = element_text(family="Helvetica"),
    plot.title = element_text(face="bold",color="darkgrey")
  )+
  scale_fill_manual(labels=letters[1:12], values=c("#E65848","#2AB69D","#2AB69D","#2AB69D","#E65848","#2AB69D","#FFAD29","#FFAD29","#2AB69D","#2AB69D","#2AB69D","#2AB69D"),guid=F)


Data_1Y%>%
  filter(Created_Month%in%c("May","Jun","Jul","Aug","Sep"))%>%
  group_by(RequestType,Created_Month)%>%
  summarise(Count=n())%>%
  ggplot(aes(Created_Month,Count,fill=RequestType))+
  geom_bar(stat = "identity")

## Heatmap of Call by Hour and Day of Week
Data_1Y$Created_Day=factor(Data_1Y$Created_Day,levels=levels(Data_1Y$Created_Day)[c(2,6,7,5,1,3,4)])

Data_1Y%>%
  filter(RequestSource=="Call")%>%
  group_by(Created_Hour,Created_Day)%>%
  summarise(Count_Source=n())%>%
  ggplot(aes(Created_Day,Created_Hour,fill=Count_Source))+
  geom_tile()+
  scale_fill_gradient(low = "white",high = "#2AB69D",guide = F)+
  theme_classic()+
  xlab("")+
  ylab("Hour")+
  ggtitle("Heatmap of Call by Hour and Day of Week")+
  theme(
    axis.title=element_text(face = "bold",colour = "darkgray"),
    plot.title = element_text(face="bold",color="darkgrey")
  )


##Map by Major Request Sources

library(ggmap)

Data1=Data_1Y%>%
  filter(RequestSource%in%c("Call","Driver Self Report","Mobile App","Self Service"))

LA="Los Angeles"
LAmap=qmap(LA,zoom=12,color="bw",maptype="terrain")
LAmap+
  stat_density2d(data = Data1,aes(x=Longitude,y=Latitude,fill=..level..,alpha=..level..),geom = "polygon")+
  facet_wrap(~RequestSource,nrow = 2)+
  scale_fill_gradient(low = "#1AB58A", high = "#FF7729") +
  theme(legend.position = "none")



##Percentage of Requests by Request Source by Major Owner
Data_1Y%>%
  filter(Owner_Full%in%c("Bureau of Sanitation","Office of Community Beautification"),RequestSource%in%c("Call","Driver Self Report","Mobile App","Self Service"))%>%
  group_by(Owner_Full,RequestSource)%>%
  summarise(Count=n())%>%
  ggplot(aes(Owner_Full,Count,fill=RequestSource))+
  geom_bar(stat = "identity",position = "fill")+
  xlab("")+
  ylab("Percentage")+
  theme_classic()+
  scale_fill_manual(values=c("#4D95DE","#2AB96D","#FDC536","#E65848"))+
  coord_flip()+
  ggtitle("Percentage of Requests by Request Source by Major Owner")+
  theme(
    axis.title=element_text(face = "bold",colour = "darkgray"),
    axis.text.x = element_text(family="Helvetica"),
    axis.text.y = element_text(family="Helvetica"),
    plot.title = element_text(face="bold",color="darkgrey")
  )



##Driver Report vs Other Request Sources of Office of Community Beautification
Data2 = Data_1Y%>%
  filter(RequestSource%in% c("Call","Mobile App","Self Service","Driver Self Report"))%>%
  filter(Owner_Full=="Office of Community Beautification")%>%
  mutate(SD= ifelse(RequestSource=="Driver Self Report","Driver","Self"))



qmap("Los Angeles", zoom=12, color="bw") +
  stat_density2d(data=Data2,aes(Longitude,Latitude,fill=SD),
                 geom ="polygon",alpha=0.2) +
  scale_fill_manual(values = c("#FDC536","#E65848"),
                    name="Request Source Type",
                    labels=c("Driver Self Report","Other Request Sources"))+
  ggtitle("Driver Report vs Other Request Sources of Office of Community Beautification")+
  theme(
    plot.title = element_text(face="bold",color="darkgrey")
  ) 





#Distribution of Bulky Items and Graffiti Removal in Koreatown by Day of Week
Data_1Y%>%
  filter(ZipCode%in%c("90010","90005","90006","90020"),
         RequestType%in%c("Bulky Items","Graffiti Removal"))%>%
  group_by(Created_Day,RequestType)%>%
  summarise(Count=n())%>%
  ggplot(aes(Created_Day,Count,fill=RequestType))+
  geom_bar(stat = "identity")+
  xlab("")+
  ylab("")+
  ggtitle("Distribution of Bulky Items and Graffiti Removal in Koreatown by Day of Week")+
  facet_grid(RequestType~.)+
  theme_classic()+
  scale_fill_manual(values = c("#2AB96D","#FDC536"),guide=F)+
  theme(
    plot.title = element_text(face="bold",color="darkgrey")
  )
  


##Processing Time of Bulky Items for Different Assign To
Data_1Y%>%
  mutate(Duration=UPDate-CRDate)%>%
  filter(Owner_Full=="Bureau of Sanitation",
         RequestType=="Bulky Items",
         AssignTo!="")%>%
  group_by(AssignTo)%>%
  summarise(Ave_Duration=mean(Duration))%>%
  ggplot(aes(reorder(AssignTo,Ave_Duration),Ave_Duration))+
  geom_bar(stat = "identity",fill="#4D95DE")+
  xlab("Assign To")+
  ylab("Average Processing Time")+
  ggtitle("Processing Time of Bulky Items for Different Assign To")+
  ylim(0,10)+
  theme_classic()+
  theme(
    axis.title=element_text(face = "bold",colour = "darkgray"),
    axis.text.x = element_text(family="Helvetica"),
    axis.text.y = element_text(family="Helvetica"),
    plot.title = element_text(face="bold",color="darkgrey")
  )


  
##Processing Time of Graffiti Removal for Different Assign To
Data_1Y%>%
  mutate(Duration=UPDate-CRDate)%>%
  filter(Owner_Full=="Office of Community Beautification",
         RequestType=="Graffiti Removal",
         AssignTo!="")%>%
  group_by(AssignTo)%>%
  summarise(Ave_Duration=mean(Duration))%>%
  ggplot(aes(reorder(AssignTo,Ave_Duration),Ave_Duration))+
  geom_bar(stat = "identity",fill="#FDC536")+
  xlab("Assign To")+
  ylab("Average Processing Time")+
  ggtitle("Processing Time of Graffiti Removal for Different Assign To")+
  theme_classic()+
  theme(
    axis.title=element_text(face = "bold",colour = "darkgray"),
    axis.text.x = element_text(family="Helvetica"),
    axis.text.y = element_text(family="Helvetica"),
    plot.title = element_text(face="bold",color="darkgrey")
  )


  
  












    