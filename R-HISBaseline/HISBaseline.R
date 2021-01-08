#Load necessary R libraries for Data Analytics
library(plyr)
library(dplyr)
library(reshape2)
library(ggplot2)
library(stringr)
library(tableHTML)
library(data.table)
library(dygraphs)
library(xts)

#******* Definitions
ScriptStart=Sys.time()
#Define Date Time Interval

LogStartTime=as.POSIXct("2018-12-10 20:00:00", "GMT",format="%Y-%m-%d %H:%M:%S")
LogEndTime=  as.POSIXct("2018-12-10 20:59:00", "GMT",format="%Y-%m-%d %H:%M:%S")

TotalHISClientCount<-1800 #Total number of HIS Clients

#Get Filenames off all log files in directory
IISLogFiles = list.files(path="./IISLogs",pattern="*.log",recursive=TRUE,full.names = TRUE)

#Define all Column Names to be read from Log files
HISColumnNames=c("date","time","ServerIP","HTTPMethod","URIStem","URIQuery","TCPPort","Username","ClientIP","UserAgent","Referrer","Status","Substatus","Win32Status","ResponseTime","OriginalClientIP")

#Define Function to read from file
readdata<-function(x){
                      res=fread(x,header=FALSE,sep=" ",skip=4,quote="\"",encoding = "UTF-8",stringsAsFactors=FALSE,fill=TRUE,blank.lines.skip=TRUE)
                      if (ncol(res)>16) res<-res[,-c(12)]
                      return(res)
                      }


#Read all log files, append them
RawHISLogs<-rbindlist(lapply(IISLogFiles,readdata))
names(RawHISLogs)<-HISColumnNames

#Transform Raw Text to special types in Table
RawHISLogs$RequestTime<-as.POSIXct(paste(RawHISLogs$date,RawHISLogs$time,sep=" "),format="%Y-%m-%d %H:%M:%S",tz="GMT")
RawHISLogs$RequestTimeLocal<-format(RawHISLogs$RequestTime, tz="Europe/Minsk",usetz=TRUE)
RawHISLogs$HISAppT<-substr(RawHISLogs$URIStem,2,100)
RawHISLogs$HISApp<-substr(RawHISLogs$URIStem,2,str_locate(RawHISLogs$HISAppT,'/'))
RawHISLogs$HISApp[is.na(RawHISLogs$HISApp)]<-""

#Select from List
HISLogs<-select(RawHISLogs,RequestTime,RequestTimeLocal,ServerIP,HTTPMethod,URIStem,URIQuery,HISApp,ClientIP,OriginalClientIP,UserAgent,Referrer,Status,ResponseTime)
#Filter out IPv4 and IPv6 localhost
HISLogs<-HISLogs[!(HISLogs$ServerIP=='127.0.0.1'),]
HISLogs<-HISLogs[!(HISLogs$ServerIP=="::1"),]
HealthCheckLogs<-HISLogs[str_detect(HISLogs$URIStem,"HealthCheck"),]
HISLogs<-HISLogs[!str_detect(HISLogs$URIStem,"HealthCheck"),]
HISLogs<-HISLogs[!(HISLogs$ClientIP=='127.0.0.1'),]
HISLogs<-HISLogs[!(HISLogs$ClientIP=="::1"),]
HISLogs2<-HISLogs[HISLogs$RequestTime>LogStartTime & HISLogs$RequestTime<LogEndTime,]

#Get Requests hitting each server
Server1Logs<-HISLogs2[HISLogs2$ServerIP=="10.0.0.1",]
Server2Logs<-HISLogs2[HISLogs2$ServerIP=="10.0.0.2",]
Server3Logs<-HISLogs2[HISLogs2$ServerIP=="10.0.0.3",]
Server4Logs<-HISLogs2[HISLogs2$ServerIP=="10.0.0.4",]

#Get Requests from LCD Screens
LCDClientLogs<-HISLogs2[substr(HISLogs2$URIStem,1,14)=='/xxxxx.yyy.LCD',]
LCDClientLogs2<-LCDClientLogs[LCDClientLogs$RequestTime>LogStartTime & LCDClientLogs$RequestTime<LogEndTime,]

#Get HIS direct Connections
HISConnectionLogs<-HISLogs2[!(substr(HISLogs2$URIStem,1,14)=='/xxxxx.yyy.LCD'),]
HISConnections<-as.data.frame(table(HISConnectionLogs$ClientIP))

#Get Remaining Requests to be from HIS Clients
HISClientLogs<-HISConnectionLogs[!HISConnectionLogs$ClientIP=="10.0.1.138",] #Load Balancer
HISClientLogs<-HISClientLogs[!HISClientLogs$ClientIP=="10.0.1.139",] #Load Balancer
HISClientLogs<-HISClientLogs[!HISClientLogs$ClientIP=="10.0.1.186",] #App Server
HISClientLogs<-HISClientLogs[!HISClientLogs$ClientIP=="10.0.1.187",] #App Server
HISClientLogs<-HISClientLogs[!HISClientLogs$ClientIP=="10.0.1.188",] #App Server
HISClientLogs<-HISClientLogs[!HISClientLogs$ClientIP=="10.0.1.200",] #App Server
#Filter Clients with NO Original Client IPs to be investigated
HISNoOriginalClientLogs<-HISClientLogs[HISClientLogs$OriginalClientIP=='-',]
HISNoOriginalClients<-as.data.frame(table(HISNoOriginalClientLogs$ClientIP))
HISClientLogs<-HISClientLogs[!HISClientLogs$OriginalClientIP=="-",] 
#Filter Clients with : in IP Address
HISClientLogs<-HISClientLogs[!str_detect(HISClientLogs$OriginalClientIP,":"),] 

#Get HISClient List
HISClients<-as.data.frame(table(HISClientLogs$OriginalClientIP))
HISClients<-HISClients[!str_detect(HISClients$Var1,",+"),]

#Calculate Apps under xxxxx dir
HISClientLogs$HISApp<-tolower(HISClientLogs$HISApp)
HISClientLogs$xxxxxxxxxT<-substr(HISClientLogs$URIStem,12,100)
HISClientLogs$xxxxxxxxx<-paste("xxxxxxxxx/",tolower(substr(HISClientLogs$xxxxxxxxxT,1,str_locate(HISClientLogs$xxxxxxxxxT,'/')-1)))
HISClientLogs$HISApp[HISClientLogs$HISApp=="xxxxxxxxx"]<-HISClientLogs$xxxxxxxxx[HISClientLogs$HISApp=="xxxxxxxxx"]

#Get HIS App statistics
HISAppResponses<-aggregate(ResponseTime ~HISApp,HISClientLogs,function(x) mean(x))
HISAppResponseSD<-aggregate(ResponseTime ~HISApp,HISClientLogs,function(x) sd(x))
Hisapps<-as.data.frame(table(HISClientLogs$HISApp))
Hisapps$AvgResponseTime<-HISAppResponses$ResponseTime[match(Hisapps$Var1,HISAppResponses$HISApp)]
Hisapps$AvgResponseTimeSD<-HISAppResponses$ResponseTime[match(Hisapps$Var1,HISAppResponses$HISApp)]

HealthChecked<-as.data.frame(table(HealthCheckLogs$URIStem))
HealthChecked$HC1<-substr(HealthChecked$Var1,2,200)
HealthChecked$HChecked<-tolower(substr(HealthChecked$Var1,2,str_locate(HealthChecked$HC1,'/')))
Hisapps$Hchecked<-match(Hisapps$Var1,HealthChecked$HChecked,nomatch=0)
Hisapps$Hchecked[Hisapps$Hchecked>0]<-1
Hisapps$Percent<-prop.table(Hisapps$Freq)
Hisapps$AvgResponseTime<-format(Hisapps$AvgResponseTime,digits=0, scientific = FALSE)
Hisapps$Percent<-format(Hisapps$Percent,digits=1, scientific = FALSE)
HisAppsFocus<-Hisapps[Hisapps$Freq>10000,]
write_tableHTML(tableHTML(HisAppsFocus,theme="rshiny-blue"), file = 'E:/Data/HisAppsFocus.htm')

#Create Data Range
GMTTime<-round(seq(LogStartTime,LogEndTime,by=1),units="sec")
TimeScale<-as.data.frame(GMTTime)
TimeScale$LocalTime<-format(TimeScale$GMTTime,"%Y-%m-%d %H:%M:%S", tz="Europe/Minsk",usetz=TRUE)

GMTTime2<-as.POSIXct(round(seq(LogStartTime,LogEndTime,by=60),units="min"))
LocalTime<-format(GMTTime2,"%Y-%m-%d %H:%M", tz="Europe/Minsk",usetz=TRUE)
GMTTimeMin<-format(GMTTime2,"%Y-%m-%d %H:%M")
TimeScaleMin<-as.data.frame(GMTTimeMin)
TimeScaleMin$LocalTime<-LocalTime

#Calculate Request Distribution from each LCD Screen
LCDRequestFrequencies<-as.data.frame(table(LCDClientLogs2$OriginalClientIP))
LCDRequestFrequencies<-LCDRequestFrequencies[LCDRequestFrequencies$Var1!='-',]
LCDRequestFrequencies2<-LCDRequestFrequencies[order(-LCDRequestFrequencies$Freq),]
LCDRequestAverage<-mean(LCDRequestFrequencies$Freq)
write_tableHTML(tableHTML(LCDRequestFrequencies2,theme="rshiny-blue"), file = 'E:/Data/LCDRequestValues.htm')

#Calculate http result distribution
HttpResultDistribution<-as.data.frame(table(HISClientLogs$Status))
HttpResultDistribution$Percent<-prop.table(HttpResultDistribution$Freq)
Http200Results<-HISClientLogs[HISClientLogs$Status==200,]
Http304Results<-HISClientLogs[HISClientLogs$Status==304,]
Http404Results<-HISClientLogs[HISClientLogs$Status==404,]
Http404Frequencis<-as.data.frame(table(Http404Results$URIStem))
TopHttp404Frequencis<-Http404Frequencis[Http404Frequencis$Freq>500,]
Http500Results<-HISClientLogs[HISClientLogs$Status==500,]

#Calculate http response time distribution
HttpResponseTimeDistribution<-as.data.frame(table(HISClientLogs$ResponseTime))
HttpResponseTimeDistribution$ElapsedSec<-as.integer(HttpResponseTimeDistribution$Var1)

HttpResponseTimeLess1000<-sum(HttpResponseTimeDistribution$Freq[HttpResponseTimeDistribution$ElapsedSec<=1000])/sum(HttpResponseTimeDistribution$Freq)
HttpResponseTimeBetween10002000<-sum(HttpResponseTimeDistribution$Freq[HttpResponseTimeDistribution$ElapsedSec<=2000 & HttpResponseTimeDistribution$ElapsedSec>1000])/sum(HttpResponseTimeDistribution$Freq)
HttpResponseTimeBetween20003000<-sum(HttpResponseTimeDistribution$Freq[HttpResponseTimeDistribution$ElapsedSec<=3000 & HttpResponseTimeDistribution$ElapsedSec>2000])/sum(HttpResponseTimeDistribution$Freq)
HttpResponseTimeBetween30004000<-sum(HttpResponseTimeDistribution$Freq[HttpResponseTimeDistribution$ElapsedSec<=4000 & HttpResponseTimeDistribution$ElapsedSec>3000])/sum(HttpResponseTimeDistribution$Freq)
HttpResponseTimeBetween40005000<-sum(HttpResponseTimeDistribution$Freq[HttpResponseTimeDistribution$ElapsedSec<=5000 & HttpResponseTimeDistribution$ElapsedSec>4000])/sum(HttpResponseTimeDistribution$Freq)
HttpResponseTimeMore5000<-sum(HttpResponseTimeDistribution$Freq[HttpResponseTimeDistribution$ElapsedSec>5000])/sum(HttpResponseTimeDistribution$Freq)

#Calculate Request Frequencies for each second
HISClientRequests<-as.data.frame(table(format(HISClientLogs$RequestTime,"%Y-%m-%d %H:%M:%S")))
HISClientRequests$Date<-as.POSIXct(HISClientRequests$Var1,format="%Y-%m-%d %H:%M:%S",tz="GMT")
#Calculate Concurrent Clients for each second
HISClientConcurrentSec<-aggregate(OriginalClientIP~RequestTime,HISClientLogs,function(x) length(unique(x)))

LCDRequests<-as.data.frame(table(format(LCDClientLogs$RequestTime,"%Y-%m-%d %H:%M:%S")))
LCDRequests$Date<-as.POSIXct(LCDRequests$Var1,format="%Y-%m-%d %H:%M:%S",tz="GMT")

#Calculate Request Frequencies for each minute
HISClientRequestsMin<-as.data.frame(table(format(HISClientLogs$RequestTime,"%Y-%m-%d %H:%M")))
HISClientRequestsMin$Date<-as.POSIXct(HISClientRequestsMin$Var1,format="%Y-%m-%d %H:%M",tz="GMT")
#Calculate Concurrent Clients for each minute
HISClientLogs$RequestTimeMin<-format(HISClientLogs$RequestTime,"%Y-%m-%d %H:%M")
HISClientConcurrentMin<-aggregate(OriginalClientIP~RequestTimeMin,HISClientLogs,function(x) length(unique(x)))
LCDRequestsMin<-as.data.frame(table(format(LCDClientLogs$RequestTime,"%Y-%m-%d %H:%M")))
LCDRequestsMin$Date<-as.POSIXct(LCDRequestsMin$Var1,format="%Y-%m-%d %H:%M",tz="GMT")

#Calculate Requests for each server
ServerRequestDist<-as.data.frame(table(HISClientLogs$ServerIP))
ServerRequestDist$Percent<-prop.table(ServerRequestDist$Freq)

Server1Requests<-as.data.frame(table(format(Server1Logs$RequestTime,"%Y-%m-%d %H:%M:%S")))
Server1Requests$Date<-as.POSIXct(Server1Requests$Var1,format="%Y-%m-%d %H:%M:%S",tz="GMT")
Server2Requests<-as.data.frame(table(format(Server2Logs$RequestTime,"%Y-%m-%d %H:%M:%S")))
Server2Requests$Date<-as.POSIXct(Server2Requests$Var1,format="%Y-%m-%d %H:%M:%S",tz="GMT")
Server3Requests<-as.data.frame(table(format(Server3Logs$RequestTime,"%Y-%m-%d %H:%M:%S")))
Server3Requests$Date<-as.POSIXct(Server3Requests$Var1,format="%Y-%m-%d %H:%M:%S",tz="GMT")
Server4Requests<-as.data.frame(table(format(Server4Logs$RequestTime,"%Y-%m-%d %H:%M:%S")))
Server4Requests$Date<-as.POSIXct(Server4Requests$Var1,format="%Y-%m-%d %H:%M:%S",tz="GMT")

#Spread Requests over each second
TimeScale$HISClientRequests<-HISClientRequests$Freq[match(TimeScale$GMTTime,HISClientRequests$Date)]
TimeScale$LCDRequests<-LCDRequests$Freq[match(TimeScale$GMTTime,LCDRequests$Date)]
TimeScale$Server1<-Server1Requests$Freq[match(TimeScale$GMTTime,Server1Requests$Date)]
TimeScale$Server2<-Server2Requests$Freq[match(TimeScale$GMTTime,Server2Requests$Date)]
TimeScale$Server3<-Server3Requests$Freq[match(TimeScale$GMTTime,Server3Requests$Date)]
TimeScale$Server4<-Server4Requests$Freq[match(TimeScale$GMTTime,Server4Requests$Date)]
TimeScale$AllServers<-TimeScale$Server1+TimeScale$Server2+TimeScale$Server3+TimeScale$Server4
write_tableHTML(tableHTML(TimeScale,theme="rshiny-blue"), file = 'E:/Data/RequestValuesPerSec.htm')

#Spread Requests over each minute
TimeScaleMin$HISClientRequests<-HISClientRequestsMin$Freq[match(TimeScaleMin$GMTTime,HISClientRequestsMin$Var1)]
TimeScaleMin$LCDClientRequests<-LCDRequestsMin$Freq[match(TimeScaleMin$GMTTime,LCDRequestsMin$Var1)]
TimeScaleMin$ConcurrentClients<-HISClientConcurrentMin$OriginalClientIP[match(TimeScaleMin$GMTTimeMin,HISClientConcurrentMin$RequestTimeMin)]
TimeScaleMin$HISRequestPerClient<-TimeScaleMin$HISClientRequests/TimeScaleMin$ConcurrentClients
write_tableHTML(tableHTML(TimeScaleMin,theme="rshiny-blue"), file = 'E:/Data/RequestValuesPerMin.htm')


#Plot Request Status over Time min resolution on Server logs
TimeScaleMin$LocalTime2<-as.POSIXct(TimeScaleMin$LocalTime,format="%Y-%m-%d %H:%M",tz="Europe/Minsk")
TimeScale6<-select(TimeScaleMin,LocalTime2,HISClientRequests,LCDClientRequests)
HISMinValue<-min(TimeScale6$HISClientRequests[!is.na(TimeScale6$HISClientRequests)])
HISMaxValue<-max(TimeScale6$HISClientRequests[!is.na(TimeScale6$HISClientRequests)])
HISClientMaxMinRatio=HISMaxValue/HISMinValue
meltTimeScale6<-melt(TimeScale6,id="LocalTime2")
jpeg(filename = "E:/Data/DailyWebRequest%01d.jpg",width = 3800, height = 2800, res=600) 
ggplot(data=meltTimeScale6,aes(LocalTime2,value,color=variable,group=variable))+
  geom_line(size=0.5)+
  ylim(0,31000)+
  labs(x = "Saat", y = sprintf("Web istek/Dakika"), color = "Tipler\n") +
  scale_colour_manual(labels=c("HIS İstemcisi","LCD İstemcisi"), values = c("blue","Red"))+ 
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
dev.off()

#Plot Concurrent Clients over Time min resolution on Server logs
TimeScale7<-select(TimeScaleMin,LocalTime2,ConcurrentClients)
meltTimeScale7<-melt(TimeScale7,id="LocalTime2")
jpeg(filename = "E:/Data/Concurrency%01d.jpg",width = 3800, height = 2800, res=600) 
ggplot(data=meltTimeScale7,aes(LocalTime2,value,color=variable,group=variable))+
  geom_line(size=0.5)+
  ylim(0,500)+
  labs(x = "Saat", y = sprintf("Istek Yapan istemci/Dakika"), color = "Tipler\n") +
  scale_colour_manual(labels=c("HIS İstemcisi"), values = c("blue"))+ 
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
dev.off()

x1<-xts(TimeScale7,order.by = TimeScale7$LocalTime2)
dygraph(x1, main = "Concurrent Clients") # %>% 


ScriptEnd=Sys.time()
ScriptRunTime=difftime(ScriptEnd,ScriptStart,units="secs")

