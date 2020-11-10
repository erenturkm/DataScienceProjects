#Load necessary R libraries for Data Analytics
library(plyr)
library(dplyr)
library(reshape2)
library(stringr)
library(data.table)
library(bit64)
library(MESS)
#******* Definitions
ScriptStart=Sys.time()

#Get Filenames off all log files in directory
FolderLogFiles = list.files(path="./Data",pattern="*.log",recursive=TRUE,full.names = TRUE)
#Define all Column Names to be read from Log files
ColumnNames=c("FolderPath","FileCount","FolderSize","FileName")

#Define Function to read from file
readdata<-function(x){
  res=fread(x,header=FALSE,sep=",",skip=0,quote="\"",encoding = "UTF-8",stringsAsFactors=FALSE,fill=TRUE,blank.lines.skip=TRUE)
  res$FileName<-x
  return(res)
}

#Read all log files, append them
RawFolderLogs<-rbindlist(lapply(FolderLogFiles,readdata))
names(RawFolderLogs)<-ColumnNames

RawFolderLogs$FolderSizeGB<-RawFolderLogs$FolderSize/(1024*1024*1024)
RawFolderLogs$FolderSizeGB[is.na(RawFolderLogs$FolderSizeGB)]<-0
RawFolderLogs$FolderDepth<-str_count(RawFolderLogs$FolderPath,"\\\\")
RawFolderLogs$FolderLength<-str_length(RawFolderLogs$FolderPath)
RawFolderLogs$FolderLength[is.na(RawFolderLogs$FolderLength)]<-0

RawFolderLogs$Server<-substr(RawFolderLogs$FileName,8,9)
RawFolderLogs$Server[RawFolderLogs$Server=="02"]<-"Server2"
RawFolderLogs$Server[RawFolderLogs$Server=="01"]<-"Server1"

F1<-strsplit(RawFolderLogs$FolderPath,"\\\\")
F2<-as.data.frame(do.call(rbind, lapply(F1,"[", 1:max(lengths(F1)))))
RawFolderLogs$PathL1<-substr(F2$V1,1,1)
RawFolderLogs$PathL2<-F2$V2
RawFolderLogs$PathL3<-F2$V3
RawFolderLogs$Bins<-cumsumbinning(RawFolderLogs$FolderSizeGB,12000)
FolderLogs<-select(RawFolderLogs,Server,PathL1,PathL2,PathL3,FolderDepth,FolderLength,FileCount,FolderSizeGB,FileName)
EmptyFolderCount<-length(FolderLogs$FileCount[FolderLogs$FileCount==0])
FoldersDepthsMoreThan2<-FolderLogs[FolderLogs$FolderDepth>2]
FolderLogs01<-FolderLogs[FolderLogs$Server=="Server1"]
FolderLogs01$Bin<-cumsumbinning(FolderLogs01$FolderSizeGB,10000)
FolderLogs02<-FolderLogs[FolderLogs$Server=="Server2"]
FolderLogs02$Bin<-cumsumbinning(FolderLogs02$FolderSizeGB,10000)
write.csv(FolderLogs01,"FolderLogs01.csv",row.names = FALSE)
write.csv(FolderLogs02,"FolderLogs02.csv",row.names = FALSE)

FolderDist<-as.data.frame(table(FolderLogs$PathL3[FolderLogs$FolderDepth=="2"]))
FolderDist1<-FolderDist[FolderDist$Freq>2,]
FolderDist2<-lapply(FolderDist1$Var1,as.character)
FoldersMoreThan2Copies<-FolderLogs[FolderLogs$PathL3 %in% FolderDist2,]

