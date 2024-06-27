#Yakutat figures 2023
#Luca Adelfio
#Rachel Hughes
#Amaryllis Adey
#Erik Curtis

#Developed in:
#R version 3.6.0 (2019-04-26) -- "Planting of a Tree"
#Copyright (C) 2019 The R Foundation for Statistical Computing
#Platform: x86_64-w64-mingw32/x64 (64-bit)
#14 Jan 2020

#Run in: 
#R version  4.2.2 GUI 1.79 High Sierra build
#Begun 27 Jan 2023

#call packages
library(ggplot2) #for plotting
library(plyr) #for data organization
library(reshape2)  #for data organization
library(zoo) #for rmse, data interpolation
#library("zoo") #for interpolating to fill data gaps
library(tidyverse)
library(ggpubr)#for arranging plots
library(base)
library(lubridate)
library(ModelMetrics)

#remove all existing variables
rm(list=ls())

#Get working directory
this.dir = dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(this.dir)

plot.folder<- paste(this.dir,"/Plots/",sep = "") #address for exported plots

#########################Section 1############################################################

 
#This section is for comparing the climates of CDV and YAK. 
#The findings were that the climates are similar enough to compare.

#call climate data for Cordova and Yakutat
#1 Jan 1991 thru 31 Dec 2019. Can update in one year to be full 30 yr climate period

met1<-read.csv(paste(this.dir,"/TempData/climate data/2001_2019_CDV_YAK/daily_wx_data.csv",sep=""),header=T, na.strings=-9999)
#if file is in the iCloud drive and not downloaded, the line above will not work. Made sure file is downloaded.
#also this needs to be updated through 2020
met1$DATE=as.Date(met1$DATE, format="%m/%d/%Y")
summary(met1)
#fix erroneous Feb 28, 2012 data with known correct value
met1[met1$DATE=="2012-02-28" & met1$STATION== "USW00026410" ,] #
met1$TMAX[met1$DATE=="2012-02-28" & met1$STATION== "USW00026410" ]<-2.2 #note this fix number comes from computing hrly data with mh.all code below LA 1/16/16
met1[met1$DATE=="2012-02-28" & met1$STATION== "USW00026410" ,] #


#find average temperature
met1$TAVG2<-(met1$TMAX+met1$TMIN)/2 #*[This isn't the average though? - Erik]
clim.period<-seq(as.Date("2001-01-01"),as.Date("2019-12-31"), by="day") #change to 1/1/1991 and  12/31/2021 when data are available
clim.df<-data.frame("STATION"=rep(unique(met1$STATION),each=length(clim.period)),"DATE"=rep(clim.period, times= length(unique(met1$STATION))))
met2<-merge(clim.df,met1, by=c("STATION","DATE"), all.x=T)
met2$NAME<-as.character(met2$NAME)
met2$NAME<-factor(substr(met2$NAME,1,nchar(met2$NAME)-7))
summary(met2)

#subset for period of Yakutat water temp data record (nearest year)
met2b<-subset(met2, NAME %in% c("CORDOVA AIRPORT","YAKUTAT AIRPORT") & DATE >= as.Date("2012-01-01"))
met2b$NAME<-factor(met2b$NAME)
range(met2b$DATE)
#Plot Cordova vs. Yakutat air temps
ggplot(data=met2b, aes(x=DATE))+
  geom_hline(yintercept=0,lwd=.5,lty=2)+
  geom_ribbon(aes(ymin=TMIN,ymax=TMAX), fill="azure3",alpha=.6)+
  geom_line(aes(y=TMAX), color="red",linewidth=1)+
  geom_line(aes(y=TMIN), color="blue",linewidth=1)+
  geom_line(aes(y=TAVG2), color="black",linewidth=1)+
  xlab("Date")+
  ylab(expression(paste("Air temperature ( ", degree ~C, " )")))+
  facet_wrap(~NAME,ncol=1)+
  theme_minimal()

#export plot
ggsave("air_temps.png", path=plot.folder, width=6.5, height=4, units="in",dpi=300)

#Weather data
#LA wrote this for wkyr, RH changed to monyr--> AA thinks this need to be wkyr to work?
met2b$monyr<-format(met2b$DATE, "%m-%Y") #add month-year column
met2b$wkyr<-format(met2b$DATE,"%U-%Y") # add week-year column

met3<-ddply(met2b, .(NAME,monyr,wkyr), summarize,
            Awkavg=round(mean(TAVG2, na.rm=T),2),
            Awkmax=round(max(TMAX, na.rm=T),2),
            Awkmin=round(min(TMIN, na.rm=T),2))

#Missing Data for wkyr 50-2019
met2b %>% subset(NAME=="YAKUTAT AIRPORT" & wkyr == "50-2019")
#Remove 
met3 <- met3 %>%
  subset(!(NAME=="YAKUTAT AIRPORT" & wkyr == "50-2019"))


#Regress Cordova vs. Yakutat 
lm1<-merge(subset(met2b, NAME == "CORDOVA AIRPORT", select=c(DATE,TAVG2)), subset(met2b, NAME == "YAKUTAT AIRPORT",select=c(DATE,TAVG2)), by="DATE", all=T)
colnames(lm1)<-c("DATE","CORDOVA","YAKUTAT")
summary(lm(YAKUTAT~CORDOVA,data=lm1))

ggplot(data=lm1, aes(x=CORDOVA,y=YAKUTAT))+
  geom_abline(slope=1, intercept=0, color="gray50", lty=2)+
  geom_point()+
  geom_smooth(method="lm", color="blue")+
  xlab("Mean daily air temperature, Cordova Airport")+
  ylab("Mean daily air temperature, Yakutat Airport")+
  theme_minimal()

ggsave("YakCorreg.png", path=plot.folder, width=6.5, height=4, units="in",dpi=300)

#Air temperature stats Cordova vs. Yakutat
lapply(unique(met2b$NAME), function(x) summary(met2b[met2b$NAME==x,]))

###########################Section 2######################################################
#This section shows that we can use the airport data instead of our own air temp data.
#This might be necessary for the paper although I have not actually run it. It might be a good appendix addition


#Call Air temperature data from Yakutat ponds --> this needs to be run for the Cordova data too!

getwd() 
dir1<-paste0(this.dir,"/PondData/Yakutat/Air/")
yktdat<-list.files(dir1)

#lapply(paste0(dir1,yktdat),function(x){ncol(read.csv(x, skip=1))}) #ID a pesky data.frame with too many columns! fixed now.

sitekey<-c("MP1", "MP3","MP5","MP8","PL1","PL2","PL3","UBP1","UBP2", "UBP3","UBP4")
yktdat
bysite1<-lapply(1:length(sitekey),function(x) grep(sitekey[x],yktdat))
length(bysite1[[1]])
yktlist<-lapply(paste0(dir1,yktdat),read.csv, skip=1, header=T,col.names=c("Date","Time","Temp"))
len.list<-lapply(1:length(yktlist),function(x) nrow(yktlist[[x]]))


df5<-do.call(rbind,yktlist)

lenkey<-lapply(1:length(bysite1), function(x) rep(sitekey[x],length(bysite1[[x]])))
lenkey2<-unlist(lenkey)

df5$site<-unlist(lapply(1:length(lenkey2),function(x){ 
  rep(lenkey2[x],times=as.numeric(len.list[[x]]))
})) #add sitename column


df5$Date<-as.Date(df5$Date,format="%m/%d/%Y") #format date

df5$Region<-"YF"

summary(df5)

df5a<-ddply(df5,.(Date,site), summarize,
            Tmean = round(mean(Temp, na.rm=T),2),
            Tmax= round(max(Temp, na.rm=T),2),
            Tmin= round(min(Temp, na.rm=T),2),
            SD= round(sd(Temp, na.rm=T),2),
            Region = unique(Region))

#Missing Data againm likely a QA/QC issue? - Erik
df5a <- df5a %>% subset(!is.na(Date)) 


YF.period<-seq(min(df5$Date, na.rm=T),max(df5$Date, na.rm=T), by="day") #maximum study period across all sites
YF.df<-data.frame("site"=rep(unique(df5a$site),each=length(YF.period)),"Date"=rep(YF.period, times= length(unique(df5a$site))))
df5b<-merge(YF.df,df5a, by=c("Date","site"), all.x=T)

ggplot(df5b, aes(x=Date, y= Tmean))+
  geom_line()+
  ylab(expression(paste("Daily mean air temperature ( ", degree ~C, " )")))+
  scale_x_date(date_breaks="1 year", date_labels = "%Y")+
  facet_wrap(~site, ncol=3)+
  ggtitle("Yakutat site air temperatures")+
  theme_bw()+
  theme(axis.text.x=element_text(angle=50, vjust=1, hjust=1))

#export plot
ggsave("YK_air_temps.png", path=plot.folder, width=8.5, height=6, units="in",dpi=300)


#find 7 day non-rolling averages to compare to Yakutat Airport
#Yakutat
df5b$wkyr<-format(df5b$Date,"%U-%Y") #add week-year column

df5c<-ddply(df5b, .(site,wkyr), summarise,
            wkavg=round(mean(Tmean, na.rm = T),2),
            wkmax=round(max(Tmax, na.rm = T),2),
            wkmin=round(min(Tmin, na.rm = T),2))

#merge Yakutat dat with weather dat
df5d<-merge(df5c,subset(met3, NAME=="YAKUTAT AIRPORT"), by="wkyr",all.x=T)
df5d$Region<-"YF" #restore region column in prep for merge with Copper River data.

#Air vs. airport
ggplot(na.omit(df5d), aes(x=Awkavg, y=wkavg))+
  geom_hline(yintercept = 0, linetype=2)+
  geom_vline(xintercept = 0,linetype=2)+
  geom_abline(slope=1,intercept = 0, color = "red", lty=3)+
  geom_point(color="darkgray")+
  geom_smooth(method="lm", color="black", se=F)+
  ylab(expression(paste("Site air temperature ( ", degree ~C, " )")))+
  xlab(expression(paste("Airport air temperature ( ", degree ~C, ")")))+
  ggtitle("Yakutat air temperatures")+
  facet_wrap(~site,ncol=3)+
  theme_minimal()

#export plot
ggsave("YK_air_lm_reg.png", path=plot.folder, width=6, height=6, units="in",dpi=300)

#compare site specific air temp data to airport for Yakutat

#Find Regression fit and coefficients

#LM for air vs. air temps at Yakutat sites/airport
sites2<-c("MP1", "MP3","MP5","MP8","PL1","PL2","PL3","UBP1","UBP2", "UBP3","UBP4")

lm.1.res<-lapply(sites2,function(x) try(lm(wkavg~Awkavg,data=df5d, subset= site==x),silent=T)) #run LM 
lm.1.sum<-lapply(1:length(sites2),function(x) summary(lm.1.res[[x]]))
lm.1.sum
lm.1.pred<-lapply(sites2,function(x) try(predict(lm(wkavg~Awkavg,data=df5d, subset= site==x)),silent=T)) 
lm.1.obs<-lapply(sites2, function(x) na.omit(subset(df5d, subset= site==x, select = wkavg )))


#Create summary table for comparing models
sumtab<-as.data.frame(matrix(NA,nrow=length(sites2),ncol=6))
colnames(sumtab)<-c("site","lm1.slope","lm1.intercept","lm1.adj.r.sq","lm1.res.stan.error","lm1.rmse")

sumtab[,1]<-sites2

# get air freeze results for sumtab
sumtab[,2]<-as.vector(unlist(lapply(1:length(sites2),function(x){
  round(coef(lm.1.res[[x]])[2],1)
}))) #slope
sumtab[,3]<-as.vector(unlist(lapply(1:length(sites2),function(x){
  round(coef(lm.1.res[[x]])[1],2)
}))) #intercept

sumtab[,4]<-as.vector(unlist(lapply(1:length(sites2),function(x){
  round(summary(lm.1.res[[x]])$adj.r.squared,2)
}))) #adj.r.sq

sumtab[,5]<-as.vector(unlist(lapply(1:length(sites2),function(x){
  round(summary(lm.1.res[[x]])$sigma,2)
}))) #residual standard error

sumtab[,6]<-round(unlist(lapply(1:length(sites2), function(x) 
  rmse(unlist(lm.1.pred[[x]]),unlist(lm.1.obs[[x]])))),3)


#Get needed info for air at pond vs. airport air temps now, as this step removes those files (repeat use of same data names below)
rm(lm.1.obs,lm.1.pred, lm.1.res, lm.1.sum,sumtab)


#########################Section 3##########################################################

#Call Water temperature data from Yakutat ponds

getwd()
dir1<-paste0(this.dir,"/PondData/Yakutat/Water/")
yktdat<-list.files(dir1)
sitekey<-c("MP1","MP3","MP5","MP8","PL1","PL2","PL3","UBP1","UBP2", "UBP3","UBP4") #creates a vector of all of the site names
yktdat
bysite1<-lapply(1:length(sitekey),function(x) grep(sitekey[x],yktdat))  
#Still working on lapply, but grep(pattern,x) searches for a pattern (sitekey) in a vector of character strings.
#So for this, we are searching for the sitekey in yktdat
length(bysite1[[1]])
yktlist<-lapply(paste0(dir1,yktdat),read.csv, skip=1, header=T,col.names=c("Date","Time","Temp"))
len.list<-lapply(1:length(yktlist),function(x) nrow(yktlist[[x]]))

df1<-do.call(rbind,yktlist)

yktlenkey<-lapply(1:length(bysite1), function(x) rep(sitekey[x],length(bysite1[[x]])))
yktlenkey2<-unlist(yktlenkey)

df1$site<-unlist(lapply(1:length(yktlenkey2),function(x){ 
  rep(yktlenkey2[x],times=as.numeric(len.list[[x]]))
})) #add sitename column


df1$Date<-as.Date(df1$Date,format="%m/%d/%Y") #format date

df1$Region<-"YF"

summary(df1)

df1a<-ddply(df1,.(Date,site), summarize,
            Tmean = round(mean(Temp, na.rm=T),2),
            Tmax= round(max(Temp, na.rm=T),2),
            Tmin= round(min(Temp, na.rm=T),2),
            SD= round(sd(Temp, na.rm=T),2),
            Region = unique(Region))

YF.period<-seq(min(df1$Date, na.rm=T),max(df1$Date, na.rm=T), by="day") #maximum study period across all sites
YF.df<-data.frame("site"=rep(unique(df1a$site),each=length(YF.period)),"Date"=rep(YF.period, times= length(unique(df1a$site))))
df1b<-merge(YF.df,df1a, by=c("Date","site"), all.x=T)

df1b$Tmean[df1b$Tmean<0]<-0  #change mean values less than 0 to 0. Tmin will still reflect days when negative values were recorded, indicating air exposure in winter.


ggplot(df1b, aes(x=Date, y= Tmean))+
  geom_line()+
  ylab(expression(paste("Daily mean water temperature ( ", degree ~C, " )")))+
  scale_x_date(date_breaks="1 year", date_labels = "%Y")+
  facet_wrap(~site, ncol=3)+
  theme_minimal()+
  ggtitle("Yakutat Foreland")+
  theme(axis.text.x=element_text(angle=50, vjust=1, hjust=1))

#export plot
ggsave("YF_water_temps.png", path=plot.folder, width=6, height=6, units="in",dpi=300)

#Call Water temperature data from Cordova ponds

getwd() #should be /Users/RachelHughes/Documents/R/AK Temp project
dir2<-paste0(this.dir,"/PondData/Cordova/Water/")
crdat<-list.files(dir2)
sitekeycr<-c("BeaverS", "Cabin","CanneryControl", "EyakS","RHM","Square","TDN", "TDS", "Wooded") #searching for these names
crdat
bysite2<-lapply(1:length(sitekeycr),function(x) grep(sitekeycr[x],crdat)) #if this organizes the data by site 
length(bysite2) #used to be length(bysite[[1]]), but RH changed because this gives the length of bysite1, not the length of the first bysite1 (bysite[[1]])
crlist<-lapply(paste0(dir2,crdat),read.csv, skip=1, header=T,col.names=c("Date","Time","Temp"),row.names=NULL) 
crlen.list<-lapply(1:length(crlist),function(x) nrow(crlist[[x]]))

df2<-do.call(rbind,crlist)

crlenkey<-lapply(1:length(bysite2), function(x) rep(sitekeycr[x],length(bysite2[[x]])))
crlenkey2<-unlist(crlenkey)
length(crlenkey2)

df2$site<-unlist(lapply(1:length(crlenkey2),function(x){ 
  rep(crlenkey2[x],times=as.numeric(crlen.list[[x]]))
})) #add sitename column

df2$Date<-as.Date(df2$Date,format="%m/%d/%Y") #format date

df2$Region<-"CR"

#Change site names to Carmella's codes for consistency with three character Yakutat codes
df2$site<-mapvalues(df2$site, from = c("BeaverS", "Cabin","CanneryControl", "EyakS","RHM","Square","TDN", "TDS", "Wooded"),
                    to = c("BVS","CAB","CAN", "EYS","RHM","SQR","TIN","TIS","WDD"))
#Problem: when Cabin Lake is called "Cabin Lk" in the code above, the code runs except Cabin Lake is called Cabin
#When I switched "Cabin Lk" to "Cabin," the line of code does not seem to run, but all of the names are changed to the 3 letter codes. 
#So it seems to work, even though I get a warning. 
unique(df2$site)
summary(df2)


df2a<-ddply(df2,.(Date,site), summarize,
            Tmean = round(mean(Temp, na.rm=T),2),
            Tmax= round(max(Temp, na.rm=T),2),
            Tmin= round(min(Temp, na.rm=T),2),
            SD= round(sd(Temp, na.rm=T),2),
            Region = unique(Region))

CR.period<-seq(min(df2$Date, na.rm=T),max(df2$Date, na.rm=T), by="day") #maximum study period across all sites
CR.df<-data.frame("site"=rep(unique(df2a$site),each=length(CR.period)),"Date"=rep(CR.period, times= length(unique(df2a$site))))
df2b<-merge(CR.df,df2a, by=c("Date","site"), all.x=T)

df2b$Tmean[df2b$Tmean<0]<-0  #change mean values less than 0 to 0. Tmin will still reflect days when negative values were recorded, indicating air exposure in winter.
df2b<-subset(df2b, Date >= as.Date("2012-08-01"))

ggplot(df2b, aes(x=Date, y= Tmean))+
  geom_line()+
  ylab(expression(paste("Daily mean water temperature ( ", degree ~C, " )")))+
  scale_x_date(date_breaks="1 year", date_labels = "%Y")+
  facet_wrap(~site, ncol=3)+
  ggtitle("Copper River Delta")+
  theme_minimal()+
  theme(axis.text.x=element_text(angle=50, vjust=1, hjust=1))

#export plot
ggsave("CR_water_temps.png", path=plot.folder, width=6, height=6, units="in",dpi=300)

#finally finished section 3!
###########################Section 4#################################################
#This code was developed but LA to find weekly non-rolling averages for water. 
#RH changed this to monthly

#save.image(file= "PondTemp.RData")  
load("PondTemp.RData")

#Find monthly non-rolling averages for water temp dat
#
#
#
#Yakutat
df1a$monyr<-format(df1a$Date,"%m-%Y") #add month-year column

df1c<-ddply(df1a, .(site,monyr), summarize,
            monavg=round(mean(Tmean, na.rm=T),2),
            monmax=round(max(Tmax, na.rm=T),2),
            monmin=round(min(Tmin, na.rm=T),2))

#Cordova
df2a$monyr<-format(df2a$Date,"%m-%Y") #add mon-year column

df2c<-ddply(df2a, .(site,monyr), summarize,
            monavg=round(mean(Tmean, na.rm=T),2),
            monmax=round(max(Tmax, na.rm=T),2),
            monmin=round(min(Tmin, na.rm=T),2))


#merge Yakutat dat with weather dat
df1d<-merge(df1c,subset(met3, NAME=="YAKUTAT AIRPORT"), by="monyr",all.x=T)
df1d$Region<-"YF" #restore region column in prep for merge with Copper River data.

#merge Cordova dat with weather dat
df2d<-merge(df2c,subset(met3, NAME=="CORDOVA AIRPORT"), by="monyr",all.x=T)
df2d$Region<-"CR" #restore region column in prep for merge with Copper River data.

#Merge YF and CR dat
df3<-rbind(df1d, df2d) 
df3$site<-factor(df3$site)
df3$NAME<-factor(df3$NAME)
df3$Region<-factor(df3$Region)
df3$monavg[df3$monavg<0]<-0

#Yakutat
ggplot(subset(df3, Region== "YF"), aes(x=Awkavg, y=monavg))+
  geom_hline(yintercept = 0, linetype=2)+
  geom_vline(xintercept = 0,linetype=2)+
  geom_point(color="darkgray")+
  geom_smooth(data=df3[df3$Region=="YF" & df3$Awkavg>0,], aes(x=Awkavg,y=monavg), method="lm", color="black", se=F)+
  ylab(expression(paste("Water Temperature ( ", degree ~C, " )")))+
  xlab(expression(paste("Air Temperature ( ", degree ~C, " )")))+
  ggtitle("Yakutat Foreland")+
  facet_wrap(~site, ncol=3)+
  theme_minimal()

#export plot
ggsave("YF_lm_reg.png", path=plot.folder, width=6, height=6, units="in",dpi=300)


#Copper River
ggplot(na.omit(subset(df3, Region== "CR")), aes(x=Awkavg, y=monavg))+
  geom_hline(yintercept = 0, linetype=2)+
  geom_vline(xintercept = 0,linetype=2)+
  geom_point(color="darkgray")+
  geom_smooth(data=na.omit(df3[df3$Region=="CR" & df3$Awkavg>0,]), aes(x=Awkavg,y=monavg), method="lm", color="black", se=F)+
  ylab(expression(paste("Water Temperature ( ", degree ~C, " )")))+
  xlab(expression(paste("Air Temperature ( ", degree ~C, " )")))+
  ggtitle("Copper River Delta")+
  facet_wrap(~site,ncol=3)+
  theme_minimal()

#export plot
ggsave("CR_lm_reg.png", path=plot.folder, width=6, height=6, units="in",dpi=300)


#Find Regression fit and coefficients

#LM for air temps >0 degC all water years
sites2<-c("MP1", "MP3","MP5","MP8","PL1","PL2","PL3","UBP1","UBP2", "UBP3","UBP4","BVS","CAB","CAN", "EYS", "RHM","SQR","TIN","TIS","WDD")

lm.1.res<-lapply(sites2,function(x) try(lm(monavg~Awkavg,data=df3[df3$Awkavg>0,], subset= site==x),silent=T)) #run LM for water values >0
lm.1.sum<-lapply(1:length(sites2),function(x) summary(lm.1.res[[x]]))
lm.1.sum
lm.1.pred<-lapply(sites2,function(x) try(predict(lm(monavg~Awkavg,data=df3[df3$Awkavg>0,],subset= site==x)),silent=T)) 
lm.1.obs<-lapply(sites2, function(x) na.omit(subset(df3, subset= site==x & Awkavg >0, select = monavg )))


#Create summary table for comparing models
sumtab<-as.data.frame(matrix(NA,nrow=length(sites2),ncol=6))
colnames(sumtab)<-c("site","lm1.slope","lm1.intercept","lm1.adj.r.sq","lm1.res.stan.error","lm1.rmse")

sumtab[,1]<-sites2

# get air freeze results for sumtab
sumtab[,2]<-as.vector(unlist(lapply(1:length(sites2),function(x){
  round(coef(lm.1.res[[x]])[2],2)
}))) #slope; RH made this 2 decimal places instead of 1
sumtab[,3]<-as.vector(unlist(lapply(1:length(sites2),function(x){
  round(coef(lm.1.res[[x]])[1],2)
}))) #intercept

sumtab[,4]<-as.vector(unlist(lapply(1:length(sites2),function(x){
  round(summary(lm.1.res[[x]])$adj.r.squared,2)
}))) #adj.r.sq

sumtab[,5]<-as.vector(unlist(lapply(1:length(sites2),function(x){
  round(summary(lm.1.res[[x]])$sigma,2)
}))) #residual standard error

sumtab[,6]<-round(unlist(lapply(1:length(sites2), function(x) 
  rmse(unlist(lm.1.pred[[x]]),unlist(lm.1.obs[[x]])))),3)

#RH: all worked except this last line. rmse requries zoo package, which I cannot find for some reason
#RH: will have to ask Luca if this is necessary - it is not on the table that he sent before our first meeting.
write.csv(sumtab, "sumtab.csv")
#########################Section 5##########################################
#This is a count for how many years of each month we have for each pond
#Water temperature statistics for all sites
#This is primarily for other statistics, not for linear regressions
#

#prep data 
df4<-rbind(df1b[,1:7], df2b) #combine CRD and YK dat
df4$year<-factor(substr(df4$Date,1,4)) #format year col
df4$monyr<-factor(substr(df4$Date,1,7)) #month-year col
df4$month<-factor(substr(df4$Date,6,7)) #mon col

#days of record per site
lapply(sites2, function(x) length(na.omit(df4$Tmean[df4$site==x])))
sites2
#Annual Water temperature statistics for all sites
#NOTE need to revive this code when we have ALL data entered, not enough years to make it worthwhile on YF right now. 
df4b<-ddply(df4, .(site,year), summarize,
            Amean= round(mean(na.omit(Tmean)),2),
            Amax=max(na.omit(Tmax)),
            Amin= min(na.omit(Tmin)),
            SD=round(sd(na.omit(Tmean)),2),
            Acount=length(na.omit(Tmean)))
df4b<-df4b[df4b$Acount>=360,] #complete years

#Monthly stats
#month year
df4c<-ddply(df4, .(site,monyr), summarize,
            Mmean= round(mean(na.omit(Tmean)),2),
            Mmax=max(na.omit(Tmean)),  #maximum daily mean
            Mmin= min(na.omit(Tmean)), #minimum daily mean
            MSD=round(sd(na.omit(Tmean)),2),
            Mcount=length(na.omit(Tmean)),
            month= unique(month))
df4c<-df4c[df4c$Mcount>=28,] #complete months

#month
df4d<-ddply(df4c, .(site,month), summarize,
            mean1= round(mean(na.omit(Mmean)),2),
            SD1=round(sd(na.omit(Mmean)),2),
            max1=max(na.omit(Mmax)),
            min1= min(na.omit(Mmin)),
            count1=length(na.omit(Mmean)))

#Find July for table
df47<-subset(df4d, month=="07")

#Find Jan for table
df41<-subset(df4d, month=="01")


#####################################Section 6##################################
#The Hindcast# 


#hindcast 1917-2019 water temperatures based on linear regressions for all sites at Yakutat
#hindcast 1945-2019 water temperatures for Cordova area sites

#call Yakutat and Cordova daily summary data for periods of record. 
#Note Cordova Period of record goes back further than 1945, but wx station was in town where the climate is different until WW2, 
#so I only use the last 75 years of data for Copper River Delta analyses.

#call Period of record data for Yakutat Airport
#RH downloaded through end of 2020. Must format date cells as dates in excel for this code to work. 
met4<-read.csv("/Users/amaryllisadey/Dropbox/SWEL/Cordova/PondTemps/DataAnalysis/TempData/climate data/YAK_all/2404410.csv",header=T, na.strings=-9999)
met4$DATE=as.Date(met4$DATE)
summary(met4)

#find average temperature
met4$TAVG2<-(met4$TMAX+met4$TMIN)/2
clim.period<-seq(as.Date("1919-01-01"),max(met4$DATE, na.rm=T), by="day") #period of record
clim.df<-data.frame("STATION"=rep(unique(met4$STATION),each=length(clim.period)),"DATE"=rep(clim.period, times= length(unique(met4$STATION))))
met4b<-merge(clim.df,met4, by=c("STATION","DATE"), all.x=T)
met4b$NAME<-"YAKUTAT AIRPORT"
summary(met4b)
#RH manually removed the empty TAVG column, but not sure it made any difference

ggplot(met4b, aes(x=DATE, y=TAVG2))+
  geom_line()+
  geom_smooth()+
  theme_bw()
count(is.na(met4b$TAVG2)==T) #count missing data
#this ggplot removes 2156 rows of data which is equal to the number of NAs in the TAVG2 column,
#it is for data visualization to make sure the data loaded.

met4b$YEAR<-substr(met4b$DATE,1,4)
tapply(met4b$TAVG2, factor(met4b$YEAR), mean,na.rm=T)

#LA developed this to find 7-day averages for period of record
#RH changed to monthly
met4b$monyr<-format(met4b$DATE,"%m-%Y") #add month-year column

met4c<-ddply(met4b, .(NAME,monyr), summarize,
             Aavg=round(mean(TAVG2, na.rm=T),2),
             Acount=length(na.omit(TAVG2))) #ID number of days of data in each month/year combo
#This is Air average (Aavg) which is the monthly mean for all daily averages. 
#LA and RH removed max and min as these are not robust to outliers.

met4c<-met4c[met4c$Acount>=25,] #subset to remove partial months (less than 28 days) RH changed to 25

met4c<-subset(met4c, select=-c(Acount)) #remove Acount now that we are done with it.

#use linear model coefficients to extrapolate mean monthly water temp from mean monthly air temp
YAK.sites<-c("MP1", "MP3","MP5","MP8","PL1","PL2","PL3","UBP1","UBP2", "UBP3","UBP4") 
YAK.lm<-lm.1.res[1:length(YAK.sites)] #subset Yakutat models from total list of both locations
met4c[,YAK.sites]<-NA #add column names
met4c[,YAK.sites]<-sapply(1:length(YAK.sites), function(x) round(coef(YAK.lm[[x]])[1]+(coef(YAK.lm[[x]])[2]*met4c$Aavg),1)) #apply linear models
met4c[,YAK.sites]<-sapply(met4c[,YAK.sites],function(x) ifelse(x<0,0,x)) #change water temps <0 to 0 (model only valid for air temps above freezing)
# RH manually double checked to make sure code worked.

#melt so all temps are in a vector coded by site, region, and month of year. Necessary for plotting
met4d<-melt(met4c[,2:ncol(met4c)], id.vars = "monyr")

met4d<-met4d[complete.cases(met4d$value),] 

met4d$value[abs(met4d$value)==Inf]<-NA

#format month/year combos into dates
met4d$Date<-as.Date(paste(met4d$monyr,"01", sep="-"), "%m-%Y-%d") 

#form complete time series after removing NAs, then merge with full template so gaps plot properly
YF.period<-seq(min(met4d$Date, na.rm=T),max(met4d$Date, na.rm=T), by="month") #maximum study period across all sites; seq can only be run with full date
head(YF.period,25) #visualize first 25 entries in YF.period
YF.df<-data.frame("variable"=rep(unique(met4d$variable),each=length(YF.period)),"Date"=rep(YF.period, times= length(unique(met4d$variable))))
met4f<-merge(YF.df,met4d, by=c("Date","variable"), all.x=T)

#add factors to organize data for plotting
met4f$Type<-"Water" #set as default because most variables are water temps
met4f$Type[met4f$variable %in% c("Aavg")]<-"Air" #override water lable for air data
met4f$Type<-factor(met4f$Type) #make it a factor
met4f$year<-factor(substr(met4f$monyr,4,7)) #find year for plotting annual mean data

#plot for monthly mean temperature
ggplot(met4f, aes(x=Date,y=value))+
  geom_line(aes(group=variable), color="darkgray")+
  #geom_smooth()+
  xlab("Monthly mean temperature")+
  facet_wrap(~Type, ncol=1, scales = "free_y")+
  theme_minimal()

met4fb<-subset(met4f, Type=="Air") #this is for the forecast
write.csv(met4fb, file="met4fb.csv")
met4g<-subset(met4f, Type=="Water") #removes air temp data

#monthly mean temp is very messy and hard to visualize, so 
#plot annual mean temp so that trends are more clear
met4h<-ddply(met4g, .(variable,year),summarize,
             Tavg=round(mean(value),2),
             count=length(na.omit(value))) #I'm guessing this is where I'm losing 1946, but I don't know why

met4h<-met4h[complete.cases(met4h),]
met4h$Tavg[met4h$count!=12]<-NA
met4h$Date<-as.Date(paste(met4h$year,"01-01",sep="-"),"%Y-%m-%d")

ggplot(met4h, aes(x=Date,y=Tavg))+
  geom_point(aes(group=variable), color="darkgray")+
  #geom_line(data=met4i, aes(x=Date,y=Tavg),color="black")+
  geom_smooth(se=F)+
  xlab("Mean water temperature")+
  theme_minimal()

#find mean, max, min annual temp across all sites in YAK Foreland
met4i<-ddply(met4h, .(Date),summarize,
             Tmean=round(mean(Tavg),2),
             Tmin=round(min(Tavg),2),
             Tmax=round(max(Tavg),2))
met4i$year<-as.numeric(substr(met4i$Date,1,4))
summary(met4i)

YF.period2<-data.frame("year"=seq(min(met4i$year, na.rm=T),max(met4i$year, na.rm=T))) #maximum study period across all sites; seq can only be run with full date

head(YF.period2,30) #visualize first 25 entries in YF.period2
met4i2<-merge(YF.period2,met4i, by="year", all.x=T)
met4i2$Date<-as.Date(paste(met4i2$year, "01","01",sep="-"))

#most recent 30 year climate period
met4j<-subset(met4i2,Date %in% seq(as.Date("1981-01-01"),as.Date("2010-12-31"), by="year"))
mean(met4j$Tmean) #mean pond water temperature for last 30 years

#mean, max, and min annual water temperatures for all sites on Yak foreland with a dotted line at the climate period mean
ggplot(met4i2, aes(x=Date))+
  geom_hline(yintercept=mean(met4j$Tmean), lty=2)+ #mean 1981-2010 pond water temperature
  geom_ribbon(aes(ymin=Tmin,ymax=Tmax), fill="gray50")+
  geom_line(aes(y=Tmean), color="black", lwd=1)+
  ylab(expression(paste("Mean annual water temperature ( ", degree ~C, " )")))+
  ggtitle("Yakutat Foreland")+
  theme_minimal()

#export plot
ggsave("YF_hindcast.png", path=plot.folder, width=8, height=6, units="in",dpi=300)


#repeat for CRD. 

#call Period of record data for Cordova Airport
met5<-read.csv("/Users/amaryllisadey/Dropbox/SWEL/Cordova/PondTemps/DataAnalysis/TempData/climate data/CDV_all/2404487RHnewdata.csv",header=T, na.strings=-9999) 
met5$DATE=as.Date(met5$DATE)
summary(met5)
#fix erroneous Feb 28, 2012 data with known correct value
met5[met5$DATE=="2012-02-28" & met5$STATION== "USW00026410" ,] #
met5$TMAX[met5$DATE=="2012-02-28" & met5$STATION== "USW00026410" ]<-2.2 #note this fix number comes from computing hrly data with mh.all code below LA 1/16/16
met5[met5$DATE=="2012-02-28" & met5$STATION== "USW00026410" ,] #

#find average temperature
met5$TAVG2<-(met5$TMAX+met5$TMIN)/2
clim.period<-seq(as.Date("1945-01-01"),max(met4$DATE, na.rm=T), by="day") #period of record
clim.df<-data.frame("STATION"=rep(unique(met5$STATION),each=length(clim.period)),"DATE"=rep(clim.period, times= length(unique(met5$STATION))))
met5b<-merge(clim.df,met5, by=c("STATION","DATE"), all.x=T)
met5b$NAME<-"CORDOVA AIRPORT"
summary(met5b)

ggplot(met5b, aes(x=DATE, y=TAVG2))+
  geom_line()+
  geom_smooth()+
  theme_bw()

met5b$YEAR<-substr(met5b$DATE,1,4)
tapply(met5b$TAVG2, factor(met5b$YEAR), mean,na.rm=T)

#find monthly averages for period of record
met5b$monyr<-format(met5b$DATE,"%m-%Y") #add month-year column

met5c<-ddply(met5b, .(NAME,monyr), summarize,
             Aavg=round(mean(TAVG2, na.rm=T),2),
             Acount=length(na.omit(TAVG2)))
#QAQC'd by RH with DRI database

#modify data (RH): 
#1982-02 would be eliminated because it only has 23 days, but because Feb. only has 28 days, 
  #23 is at the 5 day limit. RH manually changed 1982-02 to 25. 
  #There are probably more elegant ways of doing this, but this is what I can think of right now.
met5c$Acount[114]<- 25

#subset for Acount less than 28 (RH changed to 25)
met5c<-met5c[met5c$Acount>=25,]

#eliminate Acount column
met5c<-subset(met5c, select=-c(Acount))

CRD.sites<-c("BVS","CAB","CAN", "EYS", "RHM","SQR","TIN","TIS","WDD") 
CRD.lm<-lm.1.res[length(YAK.sites)+1:length(CRD.sites)] #subset Cordova models from total list of both locations
met5c[,CRD.sites]<-NA #add column names
met5c[,CRD.sites]<-sapply(1:length(CRD.sites), function(x) round(coef(CRD.lm[[x]])[1]+(coef(CRD.lm[[x]])[2]*met5c$Aavg),1)) #apply linear models
met5c[,CRD.sites]<-sapply(met5c[,CRD.sites],function(x) ifelse(x<0,0,x)) #change water temps <0 to 0 (model only valid for air temps above freezing)

#
#melt so all temps are in a vector coded by site, region, and month of year
met5d<-melt(met5c[,2:ncol(met5c)], id.vars = "monyr")
met5d<-met5d[complete.cases(met5d$value),]
met5d$value[abs(met5d$value)==Inf]<-NA


met5d$Date<-as.Date(paste(met5d$monyr, "1", sep="-"), "%m-%Y-%d") #format date into day of year


CR.period<-seq(min(met5d$Date, na.rm=T),max(met5d$Date, na.rm=T), by="month") #maximum study period across all sites
CR.df<-data.frame("variable"=rep(unique(met5d$variable),each=length(CR.period)),"Date"=rep(CR.period, times= length(unique(met5d$variable))))
met5f<-merge(CR.df,met5d, by=c("Date","variable"), all.x=T)

#Add variables for plotting
met5f$Type<-"Water"
met5f$Type[met5f$variable %in% c("Aavg","Amax","Amin")]<-"Air"
met5f$Type<-factor(met5f$Type)
met5f$year<-factor(substr(met5f$monyr,4,7))

#met5f<-ddply(met5f, .(variable),mutate,
             #roll1=c(rep(NA,11),rollmean(Tmean,12, align="left")))

#monthly mean temp plot
ggplot(met5f, aes(x=Date,y=value))+
  geom_line(aes(group=variable), color="darkgray")+
  #geom_smooth()+
  xlab("Monthly mean temperature")+
  facet_wrap(~Type, ncol=1, scales = "free_y")+
  theme_minimal()

met5g<-subset(met5f, Type=="Water") #remove air temp

met5h<-ddply(met5g, .(variable,year),summarize,
             Tavg=round(mean(value),2),
             count=length(monyr))

met5h<-met5h[complete.cases(met5h),]
met5h$Tavg[met5h$count!=12]<-NA
met5h$Date<-as.Date(paste(met5h$year,"01-01",sep="-"),"%Y-%m-%d")

#plot for annual mean temp
ggplot(met5h, aes(x=Date,y=Tavg))+
  geom_point(aes(group=variable), color="darkgray")+
  #geom_line(data=met4i, aes(x=Date,y=Tavg),color="black")+
  geom_smooth(se=F)+
  xlab("Mean water temperature")+
  theme_minimal()

met5i<-ddply(met5h, .(Date),summarize,
             Tmean=round(mean(Tavg),2),
             Tmin=round(min(Tavg),2),
             Tmax=round(max(Tavg),2)
             )

#most recent 30 year climate period
met5j<-subset(met5i,Date %in% seq(as.Date("1982-01-01"),as.Date("2010-12-31"), by="year")) 
#RH changed from 1981 to 1982 because 1981 is excluded bc of lack of data
mean(met5j$Tmean) #mean pond water temperature
#30 year climate period 1940-1970
#met5k<-subset(met5i,Date %in% seq(as.Date("1941-01-01"),as.Date("1970-12-31"), by="year"))
#mean(met5k$Tmean) #mean pond water temperature

ggplot(met5i, aes(x=Date))+
  geom_hline(yintercept = mean(met5j$Tmean), lty=2)+#mean 1982-2010 pond water temperature
  #geom_hline(yintercept = mean(met5k$Tmean), lty=2)+#mean 1941-1970 pond water temperature
  geom_ribbon(aes(ymin=Tmin,ymax=Tmax), fill="ivory3")+
  geom_line(aes(y=Tmean), color="black", lwd=1)+
  ylab(expression(paste("Mean annual water temperature ( ", degree ~C, " )")))+
  ggtitle("Copper River Delta")+
  theme_minimal()

#export plot
ggsave("CR_hindcast.png", path=plot.folder, width=8, height=6, units="in",dpi=300)


#Combining the two plots?
met5i$Location<-"Copper River Delta"
met4i2$Location<-"Yakutat Foreland"

met6<-rbind(subset(met4i2, select=c("Date","Tmean","Tmax","Tmin","Location")),subset(met5i, select=c("Date","Tmean","Tmax","Tmin","Location")))
met6$Location<-factor(met6$Location)

ggplot(met6, aes(x=Date))+
  geom_hline(yintercept = 6.47, lty=2)+#mean 1981-2010 pond water temperature across both wetlands
  geom_ribbon(aes(ymin=Tmin,ymax=Tmax),fill="gray50")+
  geom_line(aes(y=Tmean), color="black", lwd=1)+
  ylab(expression(paste("Mean annual water temperature ( ", degree ~C, " )")))+
  facet_wrap(~Location, ncol=1)+
  theme_minimal()


#export plot
ggsave("Dual_hindcast.png", path=plot.folder, width=7, height=4, units="in",dpi=300)


#Note: There were months that did have enough data points. I am counting 25 days as a full month.
#The following months were missing data from Cordova, and caused gaps in the graphs:
#1991-01, 1981-07, 1994-09, 1981-10, 1982-02
#Data for 1991-01 and 1994-09 was downloaded from the NOAA site from the next nearest weather station (Cordova North for CRD, would be NCDC for Yak, but did not need it)


###########################Section 7##########################
#This is my third attempt at future projections. I have a clearer idea of where I am headed now, though, so hopefully this will actually be useful this time. 


FYak<-read.csv("/Users/RachelHughes/Documents/R/AK Temp project/climate data/YAK_all/Yakutat_AK_SNAP_comm_charts_export.csv",header=T, na.strings=-9999, stringsAsFactors = F)
FYak <- rename(FYak,
  Jan_Min = janMin,
  Jan_Mean = janMean,
  Jan_Max = janMax,
  Jan_Sd = janSd,
  Feb_Min = febMin,
  Feb_Mean = febMean,
  Feb_Max = febMax,
  Feb_Sd = febSd,
  Mar_Min = marMin,
  Mar_Mean = marMean,
  Mar_Max = marMax,
  Mar_Sd = marSd,
  Apr_Min = aprMin,
  Apr_Mean = aprMean,
  Apr_Max = aprMax,
  Apr_Sd = aprSd,
  May_Min = mayMin,
  May_Mean = mayMean,
  May_Max = mayMax,
  May_Sd = maySd,
  Jun_Min = junMin,
  Jun_Mean = junMean,
  Jun_Max = junMax,
  Jun_Sd = junSd,
  Jul_Min = julMin,
  Jul_Mean = julMean,
  Jul_Max = julMax,
  Jul_Sd = julSd,
  Aug_Min = augMin,
  Aug_Mean = augMean,
  Aug_Max = augMax,
  Aug_Sd = augSd,
  Sep_Min = sepMin,
  Sep_Mean = sepMean,
  Sep_Max = sepMax,
  Sep_Sd = sepSd,
  Oct_Min = octMin,
  Oct_Mean = octMean,
  Oct_Max = octMax,
  Oct_Sd = octSd,
  Nov_Min = novMin,
  Nov_Mean = novMean,
  Nov_Max = novMax,
  Nov_Sd = novSd,
  Dec_Min = decMin,
  Dec_Mean = decMean,
  Dec_Max = decMax,
  Dec_Sd = decSd
)
head(FYak)
FYak.long<-pivot_longer(FYak, cols=11:58, names_to=c("month","datatype"), names_sep = "_", values_to = "val") #makes this a long dataframe
FYak.wide<-pivot_wider(FYak.long, names_from = "datatype", values_from = "val") #puts mean,max,min,sd as separate columns

FYak85<-subset(FYak.wide, scenario=="rcp85" | scenario=="cru32") #subset for the rcp 8.5 (high) projection), cru2km
FYak85<-subset(FYak85, resolution=="2km")

FYak85hist<-subset(FYak85, daterange=="Historical") #subset the historical record
FYak85hist2<-rep(FYak85hist$Mean, times=10) #repeat the historical data
head(FYak85hist2, 100)
FYak85$Hist<-FYak85hist2 #add historical data as a column for ease of delta calculation
histrow<-FYak85$daterange=="Historical"
FYak85 = FYak85[!histrow,] #remove the historical rows because it has its own column now

FYak85b<-subset(FYak85, select=-c(region,country,latitude,longitude,type,scenario,resolution,unit)) #removes extraneous data columns
FYak85b$Adelta<-(FYak85b$Mean-FYak85b$Hist) #add air delta column
FYak85b$Amaxdelta<-(FYak85b$Max-FYak85b$Hist) #add air max delta column
FYak85b$Amindelta<-(FYak85b$Min-FYak85b$Hist) #add air min delta column

FYak85c<-subset(FYak85b, select=-c(Mean,Min,Max,Hist,Sd)) #removes extraneous data columns

#For getting water temps from air deltas
FYak85d<-subset(FYak85c, cols=1:11) #subset

met4d$mon<-format(met4d$Date, format="%m") #this is the monthly averages for historical air temp data
met4d$year<-format(met4d$Date, format="%Y")
met4d2<-subset(met4d, year>="1947") #subset for complete years because will be applying delta only to complete time series
met4d2<-subset(met4d2, variable=="Aavg")

# custom function for mon column addition. I don't know what as.Date did not work, but this is a work around that I found. 
myFun <- function(x, dummyDay = "01", dummyYear = "2013"){
  require(lubridate)
  
  x <- ifelse(substr(x, 1, 3) %in% month.abb,
              paste(match(substr(x, 1, 3), month.abb),
                    dummyDay,
                    dummyYear, sep = "/"), x)
  #return date
  mdy(x)
}
FYak85d$month2 <- lapply(FYak85d$month, myFun)
FYak85d$month2_Test<-unlist(FYak85d$month2)
FYak85d$month2<-as.Date(FYak85d$month2_Test, origin=lubridate::origin, tz="UTC")
FYak85d$mon<-format(FYak85d$month2, format="%m") #added mon column that matches met4d2
  
FYak85e<-merge(FYak85d,met4d2, by=c("mon"), all.x=T)
FYak85e<-subset(FYak85e, select=-c(month,month2_Test,month2))
FYak85e$ProjAirT<-(FYak85e$value+FYak85e$Adelta) #apply air deltas to get projected monthly air temps. Will be useful when we get to water temps



#read in and organize the CRD data, same as above
FCdv<-read.csv("/Users/RachelHughes/Documents/R/AK Temp project/climate data/CDV_all/Cordova_AK_SNAP_comm_charts_export.csv",header=T, na.strings=-9999, stringsAsFactors = F)
FCdv <- rename(FCdv,
               Jan_Min = janMin,
               Jan_Mean = janMean,
               Jan_Max = janMax,
               Jan_Sd = janSd,
               Feb_Min = febMin,
               Feb_Mean = febMean,
               Feb_Max = febMax,
               Feb_Sd = febSd,
               Mar_Min = marMin,
               Mar_Mean = marMean,
               Mar_Max = marMax,
               Mar_Sd = marSd,
               Apr_Min = aprMin,
               Apr_Mean = aprMean,
               Apr_Max = aprMax,
               Apr_Sd = aprSd,
               May_Min = mayMin,
               May_Mean = mayMean,
               May_Max = mayMax,
               May_Sd = maySd,
               Jun_Min = junMin,
               Jun_Mean = junMean,
               Jun_Max = junMax,
               Jun_Sd = junSd,
               Jul_Min = julMin,
               Jul_Mean = julMean,
               Jul_Max = julMax,
               Jul_Sd = julSd,
               Aug_Min = augMin,
               Aug_Mean = augMean,
               Aug_Max = augMax,
               Aug_Sd = augSd,
               Sep_Min = sepMin,
               Sep_Mean = sepMean,
               Sep_Max = sepMax,
               Sep_Sd = sepSd,
               Oct_Min = octMin,
               Oct_Mean = octMean,
               Oct_Max = octMax,
               Oct_Sd = octSd,
               Nov_Min = novMin,
               Nov_Mean = novMean,
               Nov_Max = novMax,
               Nov_Sd = novSd,
               Dec_Min = decMin,
               Dec_Mean = decMean,
               Dec_Max = decMax,
               Dec_Sd = decSd
)
head(FCdv)
FCdv<-subset(FCdv, type=="Temperature")
FCdv.long<-pivot_longer(FCdv, cols=11:58, names_to=c("month","datatype"), names_sep = "_", values_to = "val") #makes this a long dataframe
FCdv.wide<-pivot_wider(FCdv.long, names_from = "datatype", values_from = "val") #puts mean,max,min,sd as separate columns

FCdv85<-subset(FCdv.wide, scenario=="rcp85" | scenario=="cru32") #subset for the rcp 8.5 (high) projection), cru2km
FCdv85<-subset(FCdv85, resolution=="2km")

FCdv85hist<-subset(FCdv85, daterange=="Historical") #subset the historical record
FCdv85hist2<-rep(FCdv85hist$Mean, times=10) #repeat the historical data
head(FCdv85hist2, 100)
FCdv85$Hist<-FCdv85hist2 #add historical data as a column for ease of delta calculation
histrow<-FCdv85$daterange=="Historical"
FCdv85 = FCdv85[!histrow,] #remove the historical rows because it has its own column now

FCdv85b<-subset(FCdv85, select=-c(region,country,latitude,longitude,type,scenario,resolution,unit)) #removes extraneous data columns
FCdv85b$Adelta<-(FCdv85b$Mean-FCdv85b$Hist) #add air delta column
FCdv85b$Amaxdelta<-(FCdv85b$Max-FCdv85b$Hist) #add air max delta column
FCdv85b$Amindelta<-(FCdv85b$Min-FCdv85b$Hist) #add air min delta column

FCdv85c<-subset(FCdv85b, select=-c(Mean,Min,Max,Hist,Sd)) #removes extraneous data columns

#For getting water temps from air deltas
FCdv85d<-subset(FCdv85c, cols=1:11) #subset

met5d$mon<-format(met5d$Date, format="%m") #this is the monthly averages for historical air temp data
met5d$year<-format(met5d$Date, format="%Y")
#met5d2<-subset(met5d, year>="1947") #skipping this subset because time series for Cdv is almost complete after 1945 (missing 81 I think)
met5d2<-subset(met5d, variable=="Aavg")

# use Yak function 
FCdv85d$month2 <- lapply(FCdv85d$month, myFun)
FCdv85d$month2_Test<-unlist(FCdv85d$month2)
FCdv85d$month2<-as.Date(FCdv85d$month2_Test, origin=lubridate::origin, tz="UTC")
FCdv85d$mon<-format(FCdv85d$month2, format="%m") #added mon column that matches met4d2

FCdv85e<-merge(FCdv85d,met5d2, by=c("mon"), all.x=T)
FCdv85e<-subset(FCdv85e, select=-c(month,month2_Test,month2))
FCdv85e$ProjAirT<-(FCdv85e$value+FCdv85e$Adelta) #apply air deltas to get projected monthly air temps. Will be useful when we get to water temps


### Goal: apply projected air temps to models to get projected water temps. Then will use these water temps to get deltas and see if they are stastically significant between ponds 
FYak85e[,YAK.sites]<-NA
FYak85e[,YAK.sites]<-sapply(1:length(YAK.sites), function(x) round(coef(YAK.lm[[x]])[1]+(coef(YAK.lm[[x]])[2]*FYak85e$ProjAirT),1)) #apply linear models
FYak85e[,YAK.sites]<-sapply(FYak85e[,YAK.sites],function(x) ifelse(x<0,0,x))#change water temps <0 to 0 (model only valid for air temps above freezing)
#RH manually checked and code applied correctly
FYak85w<-subset(FYak85e, select=-c(variable,value,Adelta,Amaxdelta,Amindelta,ProjAirT)) #This is the first "water" dataframe, hence the w. Remove extra columns, including air deltas and historical air temps (value column), and projected air temps
FYak85w2<-pivot_longer(FYak85w, cols=7:17, names_to = "site", values_to="monavg")

#NEXT STEP: find yearly annual
#make the years the replicates: average deltas of 1947-2020 to get average delta
#in order to do this I have to fin the average temperature of each year for each pond (all of this relates only to one specific decade such as 2020-29)
#then average all of the deltas for one pond(will have 63 deltas or however many years)
FYak85.20<-subset(FYak85w2, daterange=="2020-2029")
FYak85.20b<-ddply(FYak85.20, .(site,year), summarize,
                  AnAvg=round(mean(monavg),2))
met4h$year<-as.character(met4h$year) #met4h has a historical annual mean for each pond. This will be used to calculate a delta for each year
met4h2<-subset(met4h, year>=1947) #subset for sequential years
met4h2<-rename(met4h2, site=variable, HistAvg=Tavg) #rename to match
Yak85.20delta<-merge(met4h2,FYak85.20b, by=c("site","year"), all.x=T)
Yak85.20delta$delta<-(Yak85.20delta$AnAvg-Yak85.20delta$HistAvg) #find a delta for each year
Yak85.20delta2<-ddply(Yak85.20delta, .(site), summarize, #average the deltas for each pond
                      SeqAvDelta=round(mean(delta),2),
                      Sd=sd(delta)) #this has given me one point for each pond which is a mean of all of the deltas from 1947-2020.
#visualize data
ponddeltas20<-ggplot(Yak85.20delta2, aes(x=site, y=SeqAvDelta)) +
  geom_bar(stat='identity')+
  geom_errorbar(aes(ymin=SeqAvDelta-Sd, ymax=SeqAvDelta+Sd)) +
  ylim(0,7) +
  theme(axis.text.x = element_text(size=5, angle=45))
ponddeltas20

#test for normality
ggdensity(Yak85.20delta$delta) #bimodal? 
ggqqplot(Yak85.20delta$delta) #not terrible I guess
shapiro.test(Yak85.20delta$delta) #cannot assume normality (p<0.01)
#transformations?
logDelta<-log(Yak85.20delta$delta)
shapiro.test(logDelta) #still not normal
sqrtDelta<-sqrt(Yak85.20delta$delta)
shapiro.test(sqrtDelta) #not normal
arcsinDelta<-(Yak85.20delta$delta)
shapiro.test(arcsinDelta) #not normal

#because it is not normally distributed, I am going to try a Kruskal-Wallis test (non-parametric equivalent of ANOVA)
kruskal.test(delta~site, data=Yak85.20delta) #p<0.01, so some deltas are significantly different
pairwise.wilcox.test(Yak85.20delta$delta, Yak85.20delta$site)
#Different: MP3 is different from all. //MP5 different from MP1, UBP4.// PL1 different from UBP4

##repeat with 2050-59 decade
FYak85.50<-subset(FYak85w2, daterange=="2050-2059")
FYak85.50b<-ddply(FYak85.50, .(site,year), summarize,
                  AnAvg=round(mean(monavg),2))
Yak85.50delta<-merge(met4h2,FYak85.50b, by=c("site","year"), all.x=T)
Yak85.50delta$delta<-(Yak85.50delta$AnAvg-Yak85.50delta$HistAvg) #find a delta for each year
Yak85.50delta2<-ddply(Yak85.50delta, .(site), summarize, #average the deltas for each pond
                      SeqAvDelta=round(mean(delta),2),
                      Sd=sd(delta)) #this has given me one point for each pond which is a mean of all of the deltas from 1947-2020.
#visualize data
ponddeltas50<-ggplot(Yak85.50delta2, aes(x=site, y=SeqAvDelta)) +
  geom_bar(stat='identity')+
  geom_errorbar(aes(ymin=SeqAvDelta-Sd, ymax=SeqAvDelta+Sd)) +
  ylim(0,7) +
  theme(axis.text.x = element_text(size=5, angle=45))
ponddeltas50

#test for normality
ggdensity(Yak85.50delta$delta) #slightly less bimodal? 
ggqqplot(Yak85.50delta$delta) #not terrible I guess
shapiro.test(Yak85.50delta$delta) #cannot assume normality (p<0.01, but more normal than 2020s)
#transformations?
logDelta<-log(Yak85.50delta$delta)
shapiro.test(logDelta) #still not normal
sqrtDelta<-sqrt(Yak85.50delta$delta)
shapiro.test(sqrtDelta) #not normal
arcsinDelta<-(Yak85.50delta$delta)
shapiro.test(arcsinDelta) #not normal, but this one worked best

#because it is not normally distributed, I am going to try a Kruskal-Wallis test (non-parametric equivalent of ANOVA)
kruskal.test(delta~site, data=Yak85.50delta) #p<0.01, so some deltas are significantly different
pairwise.wilcox.test(Yak85.50delta$delta, Yak85.50delta$site)
#Different: MP3 is different from all. // MP5 different from MP1, UBP4, PL2.// MP1 diff from MP5,UBP1,UBP2// UBP1 diff from PL2, UBP4 // UBP2 diff from UBP4
#seems more different than 2020s. 

#repeat with 2080s
FYak85.80<-subset(FYak85w2, daterange=="2080-2089")
FYak85.80b<-ddply(FYak85.80, .(site,year), summarize,
                  AnAvg=round(mean(monavg),2))
Yak85.80delta<-merge(met4h2,FYak85.80b, by=c("site","year"), all.x=T)
Yak85.80delta$delta<-(Yak85.80delta$AnAvg-Yak85.80delta$HistAvg) #find a delta for each year
Yak85.80delta2<-ddply(Yak85.80delta, .(site), summarize, #average the deltas for each pond
                      SeqAvDelta=round(mean(delta),2),
                      Sd=sd(delta)) #this has given me one point for each pond which is a mean of all of the deltas from 1947-2020.
#visualize data
ponddeltas80<-ggplot(Yak85.80delta2, aes(x=site, y=SeqAvDelta)) +
  geom_bar(stat='identity')+
  geom_errorbar(aes(ymin=SeqAvDelta-Sd, ymax=SeqAvDelta+Sd)) +
  ylim(0,7) +
  theme(axis.text.x = element_text(size=5, angle=45))
ponddeltas80

#test for normality
ggdensity(Yak85.80delta$delta) #wow looks kind of normal?  
ggqqplot(Yak85.80delta$delta) #not terrible I guess
shapiro.test(Yak85.80delta$delta) #cannot assume normality (p=0.001703, which is the best I've seen but still not normal)
#transformations?
logDelta<-log(Yak85.80delta$delta)
shapiro.test(logDelta) #still not normal
sqrtDelta<-sqrt(Yak85.80delta$delta)
shapiro.test(sqrtDelta) #not normal
arcsinDelta<-(Yak85.80delta$delta)
shapiro.test(arcsinDelta) #not normal, but this one worked best here too (p=0.001703)

#because it is not normally distributed, I am going to try a Kruskal-Wallis test (non-parametric equivalent of ANOVA)
kruskal.test(delta~site, data=Yak85.80delta) #p<0.01, so some deltas are significantly different
pairwise.wilcox.test(Yak85.80delta$delta, Yak85.80delta$site)
#Different: MP3 is different from all. // MP5 different from MP1, MP8 UBP4, PL2.// MP1 diff from MP5,UBP1,UBP2, UBP3// UBP1 diff from MP1,MP3,MP8,PL2,PL3,UBP4 (almost all)// UBP2 diff from MP1,MP3,PL2,UBP4// UBP4 diff from UBP3, MP5
#more different than other two decades


### Goal: apply projected air temps to models to get projected water temps. Then will use these water temps to get deltas and see if they are stastically significant between ponds 
FCdv85e[,CRD.sites]<-NA
FCdv85e[,CRD.sites]<-sapply(1:length(CRD.sites), function(x) round(coef(CRD.lm[[x]])[1]+(coef(CRD.lm[[x]])[2]*FCdv85e$ProjAirT),1)) #apply linear models
FCdv85e[,CRD.sites]<-sapply(FCdv85e[,CRD.sites],function(x) ifelse(x<0,0,x))#change water temps <0 to 0 (model only valid for air temps above freezing)
#RH must come back to check if code applied correctly
FCdv85w<-subset(FCdv85e, select=-c(variable,value,Adelta,Amaxdelta,Amindelta,ProjAirT)) #This is the first "water" dataframe, hence the w. Remove extra columns, including air deltas and historical air temps (value column), and projected air temps
FCdv85w2<-pivot_longer(FCdv85w, cols=7:15, names_to = "site", values_to="monavg")

#NEXT STEP: find yearly annual
#make the years the replicates: average deltas of 1947-2020 to get average delta
#in order to do this I have to fin the average temperature of each year for each pond (all of this relates only to one specific decade such as 2020-29)
#then average all of the deltas for one pond(will have 63 deltas or however many years)
FCdv85.20<-subset(FCdv85w2, daterange=="2020-2029")
FCdv85.20b<-ddply(FCdv85.20, .(site,year), summarize,
                  AnAvg=round(mean(monavg),2))
met5h$year<-as.character(met5h$year) #met4h has a historical annual mean for each pond. This will be used to calculate a delta for each year
#met5h2<-subset(met5h, year>=1947) #skipping this subset because I should have nearly a complete data set for 1945 and up except from some in the 80's
met5h2<-rename(met5h, site=variable, HistAvg=Tavg) #rename to match
Cdv85.20delta<-merge(met5h2,FCdv85.20b, by=c("site","year"), all.x=T)
Cdv85.20delta$delta<-(Cdv85.20delta$AnAvg-Cdv85.20delta$HistAvg) #find a delta for each year
#In this dataset 1981 is missing. Just going to remove it for now but MUST REMEMBER TO ASK ABOUT THIS
row81<-Cdv85.20delta$year=="1981"
Cdv85.20delta2 = Cdv85.20delta[!row81,] 
Cdv85.20delta2<-ddply(Cdv85.20delta2, .(site), summarize, #average the deltas for each pond
                      SeqAvDelta=round(mean(delta),2),
                      Sd=sd(delta)) #this has given me one point for each pond which is a mean of all of the deltas from 1945-2020.
#visualize data
Cponddeltas20<-ggplot(Cdv85.20delta2, aes(x=site, y=SeqAvDelta)) +
  geom_bar(stat='identity')+
  geom_errorbar(aes(ymin=SeqAvDelta-Sd, ymax=SeqAvDelta+Sd)) +
  ylim(0,7) +
  theme(axis.text.x = element_text(size=5, angle=45))
Cponddeltas20

#test for normality
ggdensity(Cdv85.20delta$delta) #bimodal 
ggqqplot(Cdv85.20delta$delta) 
shapiro.test(Cdv85.20delta$delta) #cannot assume normality (p<0.01)
#transformations?
logDelta<-log(Cdv85.20delta$delta)
shapiro.test(logDelta) #still not normal
sqrtDelta<-sqrt(Cdv85.20delta$delta)
shapiro.test(sqrtDelta) #not normal
arcsinDelta<-(Cdv85.20delta$delta)
shapiro.test(arcsinDelta) #not normal

#because it is not normally distributed, I am going to try a Kruskal-Wallis test (non-parametric equivalent of ANOVA)
kruskal.test(delta~site, data=Cdv85.20delta) #p<0.01, so some deltas are significantly different
pairwise.wilcox.test(Cdv85.20delta$delta, Cdv85.20delta$site)
#Different: BVS different from all. CAB diff from all. RHM diff from all. Others not diff. 

##repeat with 2050-59 decade
FCdv85.50<-subset(FCdv85w2, daterange=="2050-2059")
FCdv85.50b<-ddply(FCdv85.50, .(site,year), summarize,
                  AnAvg=round(mean(monavg),2))
Cdv85.50delta<-merge(met5h2,FCdv85.50b, by=c("site","year"), all.x=T)
Cdv85.50delta$delta<-(Cdv85.50delta$AnAvg-Cdv85.50delta$HistAvg) #find a delta for each year
row81<-Cdv85.50delta$year=="1981" #removing 1981 again
Cdv85.50delta2 = Cdv85.50delta[!row81,] 
Cdv85.50delta2<-ddply(Cdv85.50delta2, .(site), summarize, #average the deltas for each pond
                      SeqAvDelta=round(mean(delta),2),
                      Sd=sd(delta)) #this has given me one point for each pond which is a mean of all of the deltas from 1947-2020.
#visualize data
Cponddeltas50<-ggplot(Cdv85.50delta2, aes(x=site, y=SeqAvDelta)) +
  geom_bar(stat='identity')+
  geom_errorbar(aes(ymin=SeqAvDelta-Sd, ymax=SeqAvDelta+Sd)) +
  ylim(0,7) +
  theme(axis.text.x = element_text(size=5, angle=45))
Cponddeltas50

#test for normality
ggdensity(Cdv85.50delta$delta) #bimodal 
ggqqplot(Cdv85.50delta$delta) 
shapiro.test(Cdv85.50delta$delta) #cannot assume normality (p<0.01)
#transformations?
logDelta<-log(Cdv85.50delta$delta)
shapiro.test(logDelta) #still not normal
sqrtDelta<-sqrt(Cdv85.50delta$delta)
shapiro.test(sqrtDelta) #not normal
arcsinDelta<-(Cdv85.50delta$delta)
shapiro.test(arcsinDelta) #not normal, but this one worked best

#because it is not normally distributed, I am going to try a Kruskal-Wallis test (non-parametric equivalent of ANOVA)
kruskal.test(delta~site, data=Cdv85.50delta) #p<0.01, so some deltas are significantly different
pairwise.wilcox.test(Cdv85.50delta$delta, Cdv85.50delta$site)
#Different: BVS from all. CAB from all. RHM from all. CAN from SQR
#seems more different than 2020s. 

#repeat with 2080s
FCdv85.80<-subset(FCdv85w2, daterange=="2080-2089")
FCdv85.80b<-ddply(FCdv85.80, .(site,year), summarize,
                  AnAvg=round(mean(monavg),2))
Cdv85.80delta<-merge(met5h2,FCdv85.80b, by=c("site","year"), all.x=T)
Cdv85.80delta$delta<-(Cdv85.80delta$AnAvg-Cdv85.80delta$HistAvg) #find a delta for each year
row81<-Cdv85.80delta$year=="1981" #removing 1981 again
Cdv85.80delta2 = Cdv85.80delta[!row81,] 
Cdv85.80delta2<-ddply(Cdv85.80delta2, .(site), summarize, #average the deltas for each pond
                      SeqAvDelta=round(mean(delta),2),
                      Sd=sd(delta)) #this has given me one point for each pond which is a mean of all of the deltas from 1947-2020.
#visualize data
Cponddeltas80<-ggplot(Cdv85.80delta2, aes(x=site, y=SeqAvDelta)) + 
  geom_bar(stat='identity')+
  geom_errorbar(aes(ymin=SeqAvDelta-Sd, ymax=SeqAvDelta+Sd)) +
  ylim(0,7) +
 theme(axis.text.x = element_text(size=5, angle=45))
Cponddeltas80

#test for normality
ggdensity(Cdv85.80delta$delta) #sort of normal-er  
ggqqplot(Cdv85.80delta$delta) 
shapiro.test(Cdv85.80delta$delta) #cannot assume normality (p<0.01, which is the best I've seen but still not normal)
#transformations?
logDelta<-log(Cdv85.80delta$delta)
shapiro.test(logDelta) #still not normal
sqrtDelta<-sqrt(Cdv85.80delta$delta)
shapiro.test(sqrtDelta) #not normal
arcsinDelta<-(Cdv85.80delta$delta)
shapiro.test(arcsinDelta) #not normal

#because it is not normally distributed, I am going to try a Kruskal-Wallis test (non-parametric equivalent of ANOVA)
kruskal.test(delta~site, data=Cdv85.80delta) #p<0.01, so some deltas are significantly different
pairwise.wilcox.test(Cdv85.80delta$delta, Cdv85.80delta$site)
#Different: BVS from all. CAB from all. RHM from all. CAN from SQR
#not as much of a difference as YAK data, but still getting more different as projections get further out. 

ggarrange(ponddeltas20, ponddeltas50, ponddeltas80,
          Cponddeltas20,Cponddeltas50,Cponddeltas80,
          labels = c("Y20-29","Y50-59","Y80-89","C20-29","C50-59","C80-89"),
          ncol=3,nrow=2,
          #common.legend = TRUE, legend = "bottom",
          align = "hv")
ggsave("ComboPonddeltas.png", path=plot.folder, width=8, height=4, units="in", dpi=300)
 


#moving on to SNAP-like plot

#for now, I think i am going to average them all, but I might have to come back and change this. 
FYak85mon<-ddply(FYak85w2, .(mon,daterange), summarize,
                  MonthAvg=round(mean(monavg),2),
                 Sd=sd(monavg)) #calculate monthly averages over all site for each daterange (these are TEMPS not deltas)
FYak85monRed<-subset(FYak85mon, daterange=="2020-2029" | daterange=="2050-2059" | daterange=="2080-2089")

#for the historical bar, I am going to try to do the average 30 year climate period (1990-2020), averaged over all ponds and all years for each month
met4g$year<-as.character(met4g$year) #this dataframe has monthly averages for each pond (will be combined)
met4g2<-subset(met4g, year>="1990") #subset for most recent 30 year climate period
met4g2$mon<-substr(met4g2$monyr, 1,2)
ClimPerMon<-ddply(met4g2, .(mon), summarize,
                  MonthAvg=round(mean(value),2),
                  Sd=sd(value))
ClimPerMon$daterange<-"Historical"

FYak85mon4Plot<-rbind(ClimPerMon,FYak85monRed) #add 30 year climate period historical monthly averages to projections
FYak85mon4Plot$daterange<-as.factor(FYak85mon4Plot$daterange)
FYak85mon4Plot$daterange<-factor(FYak85mon4Plot$daterange, levels=c("Historical","2020-2029","2050-2059","2080-2089")) #arrange so that historical appears before the decades
FYak85monPlot<-ggplot(FYak85mon4Plot, aes(fill=daterange, y=MonthAvg, x=mon))+
  geom_bar(position="dodge", stat="identity") +
  geom_errorbar(aes(ymin=MonthAvg-Sd, ymax=MonthAvg+Sd),
                position = position_dodge()) +
  theme_minimal()+
  labs(title="Average Monthly Pond Temperature in Yakutat Forelands", x="Month", y="Temperature (°C)") +
  theme(legend.title=element_blank())
FYak85monPlot
ggsave("FYak85monPlot.png", path=plot.folder, width=8, height=4, units="in", dpi=300)

#this is like the SNAP example. Next do this for CRD and combine. 
#repeat fro CRD
FCdv85mon<-ddply(FCdv85w2, .(mon,daterange), summarize,
                 MonthAvg=round(mean(monavg),2),
                 Sd=sd(monavg)) #calculate monthly averages over all site for each daterange (these are TEMPS not deltas)
FCdv85monRed<-subset(FCdv85mon, daterange=="2020-2029" | daterange=="2050-2059" | daterange=="2080-2089")

#for the historical bar, I am going to try to do the average 30 year climate period (1990-2020), averaged over all ponds and all years for each month
met5g$year<-as.character(met5g$year) #this dataframe has monthly averages for each pond (will be combined)
met5g2<-subset(met5g, year>="1990") #subset for most recent 30 year climate period
met5g2$mon<-substr(met5g2$monyr, 1,2)
C.ClimPerMon<-ddply(met5g2, .(mon), summarize,
                  MonthAvg=round(mean(value),2),
                  Sd=sd(value))
C.ClimPerMon$daterange<-"Historical"

FCdv85mon4Plot<-rbind(C.ClimPerMon,FCdv85monRed) #add 30 year climate period historical monthly averages to projections
FCdv85mon4Plot$daterange<-as.factor(FCdv85mon4Plot$daterange)
FCdv85mon4Plot$daterange<-factor(FCdv85mon4Plot$daterange, levels=c("Historical","2020-2029","2050-2059","2080-2089")) #arrange so that historical appears before the decades
FCdv85monPlot<-ggplot(FCdv85mon4Plot, aes(fill=daterange, y=MonthAvg, x=mon))+
  geom_bar(position="dodge", stat="identity") +
  geom_errorbar(aes(ymin=MonthAvg-Sd, ymax=MonthAvg+Sd),
                position = position_dodge()) +
  theme_minimal()+
  labs(title="Average Monthly Pond Temperature in Copper River Delta", x="Month", y="Temperature (°C)") +
  theme(legend.title=element_blank())
FCdv85monPlot
ggsave("FCdv85monPlot.png", path=plot.folder, width=8, height=4, units="in", dpi=300)

ggarrange(FYak85monPlot,FCdv85monPlot,
          labels=c("A","B","C"),
          ncol=1,nrow=2,
          common.legend= TRUE, legend="bottom",
          align="v")
ggsave("DualMonPondTemps.png", path=plot.folder, width=8, height=4, units="in", dpi=300)




####### Hamlet delta plot for air temps. Will have to come back when I have deltas for water temps
#first have to format RCP45  in order to be combined with 85
FYak45<-subset(FYak.wide, scenario=="rcp45" | scenario=="cru32") #subset for the rcp 8.5 (high) projection), cru2km
FYak45<-subset(FYak45, resolution=="2km")

FYak45hist<-subset(FYak45, daterange=="Historical") #subset the historical record
FYak45hist2<-rep(FYak45hist$Mean, times=10) #repeat the historical data
head(FYak45hist2, 100)
FYak45$Hist<-FYak45hist2 #add historical data as a column for ease of delta calculation
histrow<-FYak45$daterange=="Historical"
FYak45 = FYak45[!histrow,] #remove the historical rows because it has its own column now

FYak45b<-subset(FYak45, select=-c(region,country,latitude,longitude,type,scenario,resolution,unit)) #removes extraneous data columns
FYak45b$Adelta<-(FYak45b$Mean-FYak45b$Hist) #add air delta column
FYak45b$Amaxdelta<-(FYak45b$Max-FYak45b$Hist) #add air max delta column
FYak45b$Amindelta<-(FYak45b$Min-FYak45b$Hist) #add air min delta column

FYak45c<-subset(FYak45b, select=-c(Mean,Min,Max,Hist,Sd)) #removes extraneous data columns
FYak45d<-subset(FYak45c, cols=1:11) #subset
FYak45d$month2 <- lapply(FYak45d$month, myFun)
FYak45d$month2_Test<-unlist(FYak45d$month2)
FYak45d$month2<-as.Date(FYak45d$month2_Test, origin=lubridate::origin, tz="UTC")
FYak45d$mon<-format(FYak45d$month2, format="%m") #added mon column that matches met4d2

#now to begin the plots
FYak45f<-pivot_wider(FYak45d, names_from=daterange, values_from=c(Adelta,Amaxdelta,Amindelta)) 
Yak45df <- FYak45f %>%
  select(mon,`Adelta_2010-2019`,`Adelta_2020-2029`,`Adelta_2030-2039`,`Adelta_2040-2049`,`Adelta_2050-2059`,`Adelta_2060-2069`,`Adelta_2070-2079`,`Adelta_2080-2089`,`Adelta_2090-2099`) %>%
  gather(key = "decade", value = "delta45", -mon)
Yak45df$decade<-factor(substr(Yak45df$decade,8,16))
Yak45df2 <- FYak45f %>%
  select(mon,`Amaxdelta_2010-2019`,`Amaxdelta_2020-2029`,`Amaxdelta_2030-2039`,`Amaxdelta_2040-2049`,`Amaxdelta_2050-2059`,`Amaxdelta_2060-2069`,`Amaxdelta_2070-2079`,`Amaxdelta_2080-2089`,`Amaxdelta_2090-2099`) %>%
  gather(key = "decade", value = "max45", -mon)
Yak45df2$decade<-factor(substr(Yak45df2$decade,11,19))
Yak45df3 <- FYak45f %>%
  select(mon,`Amindelta_2010-2019`,`Amindelta_2020-2029`,`Amindelta_2030-2039`,`Amindelta_2040-2049`,`Amindelta_2050-2059`,`Amindelta_2060-2069`,`Amindelta_2070-2079`,`Amindelta_2080-2089`,`Amindelta_2090-2099`) %>%
  gather(key = "decade", value = "min45", -mon)
Yak45df3$decade<-factor(substr(Yak45df3$decade,11,19))
Yak45df4 <-merge(Yak45df,Yak45df2, by=c("mon","decade"), all.x=T)
Yak45df4b <-merge(Yak45df4,Yak45df3, by=c("mon","decade"), all.x=T)
#to arrange RCP85 to prepare for combining
FYak85f<-pivot_wider(FYak85d, names_from=daterange, values_from=c(Adelta,Amaxdelta,Amindelta)) #this will be needed for later air temp plots (comparing 4.5 to 8.5 for example)
Yak85df <- FYak85f %>%
  select(mon,`Adelta_2010-2019`,`Adelta_2020-2029`,`Adelta_2030-2039`,`Adelta_2040-2049`,`Adelta_2050-2059`,`Adelta_2060-2069`,`Adelta_2070-2079`,`Adelta_2080-2089`,`Adelta_2090-2099`) %>%
  gather(key = "decade", value = "delta85", -mon)
Yak85df$decade<-factor(substr(Yak85df$decade,8,16))
Yak85df2 <- FYak85f %>%
  select(mon,`Amaxdelta_2010-2019`,`Amaxdelta_2020-2029`,`Amaxdelta_2030-2039`,`Amaxdelta_2040-2049`,`Amaxdelta_2050-2059`,`Amaxdelta_2060-2069`,`Amaxdelta_2070-2079`,`Amaxdelta_2080-2089`,`Amaxdelta_2090-2099`) %>%
  gather(key = "decade", value = "max85", -mon)
Yak85df2$decade<-factor(substr(Yak85df2$decade,11,19))
Yak85df3 <- FYak85f %>%
  select(mon,`Amindelta_2010-2019`,`Amindelta_2020-2029`,`Amindelta_2030-2039`,`Amindelta_2040-2049`,`Amindelta_2050-2059`,`Amindelta_2060-2069`,`Amindelta_2070-2079`,`Amindelta_2080-2089`,`Amindelta_2090-2099`) %>%
  gather(key = "decade", value = "min85", -mon)
Yak85df3$decade<-factor(substr(Yak85df3$decade,11,19))
Yak85df4 <-merge(Yak85df,Yak85df2, by=c("mon","decade"), all.x=T)
Yak85df4b <-merge(Yak85df4,Yak85df3, by=c("mon","decade"), all.x=T)

Yakdfcombo<-merge(Yak45df4b,Yak85df4b, by=c("mon","decade"), all.x=T) #merge 8.5 and 4.5 dataframes. 
Yak.df.20<-subset(Yakdfcombo, decade=="2020-2029") #subset for 2020s
Yak.df.20b <- Yak.df.20 %>%
  select(mon,delta45,delta85) %>%
  gather(key = "RCP", value = "delta", -mon)
Yak.df.20b$RCP<-factor(substr(Yak.df.20b$RCP,6,7))
head(Yak.df.20b)
Yak.df.20c <- Yak.df.20 %>%
  select(mon,max45,max85) %>%
  gather(key = "RCP", value = "max", -mon)
Yak.df.20c$RCP<-factor(substr(Yak.df.20c$RCP,4,5))
head(Yak.df.20c)
Yak.df.20d<-Yak.df.20 %>%
  select(mon,min45,min85) %>%
  gather(key = "RCP", value = "min", -mon)
Yak.df.20d$RCP<-factor(substr(Yak.df.20d$RCP,4,5))
head(Yak.df.20d)
Yak.df.20combo<-merge(Yak.df.20b,Yak.df.20c, by=c("mon","RCP"), all.x=T)
Yak.df.20comb1<-merge(Yak.df.20combo, Yak.df.20d, by=c("mon","RCP"), all.x=T)

plot20<-ggplot(Yak.df.20comb1, aes(x = mon, y=delta, fill = RCP)) +
  geom_ribbon(aes(ymin = min, ymax= max, group = RCP, fill = RCP), alpha = 0.4) +
  geom_line(aes(y=delta, group = RCP, color = RCP, linetype = RCP)) +
  ylim(-7,12) +
  labs(title="2020-2029", x = "Month", y = "Delta") +
  theme_minimal()
plot20
#Now I repeat with the 2050-59 data
Yak.df.50<-subset(Yakdfcombo, decade=="2050-2059") #subset for 2050s
Yak.df.50b <- Yak.df.50 %>%
  select(mon,delta45,delta85) %>%
  gather(key = "RCP", value = "delta", -mon)
Yak.df.50b$RCP<-factor(substr(Yak.df.50b$RCP,6,7))
head(Yak.df.50b)
Yak.df.50c <- Yak.df.50 %>%
  select(mon,max45,max85) %>%
  gather(key = "RCP", value = "max", -mon)
Yak.df.50c$RCP<-factor(substr(Yak.df.50c$RCP,4,5))
head(Yak.df.50c)
Yak.df.50d<-Yak.df.50 %>%
  select(mon,min45,min85) %>%
  gather(key = "RCP", value = "min", -mon)
Yak.df.50d$RCP<-factor(substr(Yak.df.50d$RCP,4,5))
head(Yak.df.50d)
Yak.df.50combo<-merge(Yak.df.50b,Yak.df.50c, by=c("mon","RCP"), all.x=T)
Yak.df.50comb1<-merge(Yak.df.50combo, Yak.df.50d, by=c("mon","RCP"), all.x=T)
plot50<-ggplot(Yak.df.50comb1, aes(x = mon, y=delta, fill = RCP)) +
  geom_ribbon(aes(ymin = min, ymax= max, group = RCP, fill = RCP), alpha = 0.4) +
  geom_line(aes(y=delta, group = RCP, color = RCP, linetype = RCP)) +
  ylim(-7,12) +
  labs(title="2050-2059", x = "Month", y = "Delta") +
  theme_minimal()
plot50
#and now 2080
Yak.df.80<-subset(Yakdfcombo, decade=="2080-2089") #subset for 2080s
Yak.df.80b <- Yak.df.80 %>%
  select(mon,delta45,delta85) %>%
  gather(key = "RCP", value = "delta", -mon)
Yak.df.80b$RCP<-factor(substr(Yak.df.80b$RCP,6,7))
head(Yak.df.80b)
Yak.df.80c <- Yak.df.80 %>%
  select(mon,max45,max85) %>%
  gather(key = "RCP", value = "max", -mon)
Yak.df.80c$RCP<-factor(substr(Yak.df.80c$RCP,4,5))
head(Yak.df.80c)
Yak.df.80d<-Yak.df.80 %>%
  select(mon,min45,min85) %>%
  gather(key = "RCP", value = "min", -mon)
Yak.df.80d$RCP<-factor(substr(Yak.df.80d$RCP,4,5))
head(Yak.df.80d)
Yak.df.80combo<-merge(Yak.df.80b,Yak.df.80c, by=c("mon","RCP"), all.x=T)
Yak.df.80comb1<-merge(Yak.df.80combo, Yak.df.80d, by=c("mon","RCP"), all.x=T)
plot80<-ggplot(Yak.df.80comb1, aes(x = mon, y=delta, fill = RCP)) +
  geom_ribbon(aes(ymin = min, ymax= max, group = RCP, fill = RCP), alpha = 0.4) +
  geom_line(aes(y=delta, group = RCP, color = RCP, linetype = RCP)) +
  ylim(-7,12) +
  labs(title="2080-2089", x = "Month", y = "Delta") +
  theme_minimal()
plot80

ggarrange(plot20, plot50, plot80,
          labels = c("A","B","C"),
          ncol=3,nrow=1,
          common.legend = TRUE, legend = "bottom",
          align = "v")
ggsave("YF_deltaRCPs3.png", path=plot.folder, width=8, height=4, units="in", dpi=300)
#this is a delta plot that shows the range of deltas from RCP8.5 and RCP4.5 for each month in three differetn decades
#I am not sure how to go about getting one of these for water temp. Could average all of the ponds for each month, 
#could average all of the ponds that are not stat. diff, but this would change over the decades? Might have to ask Gary


