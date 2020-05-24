data1<- read.csv("/Users/heatherkrause/Dropbox/Active Projects 2020/Orb/Health Worker Data + Research/Heather Orb Health Data Using/ORB Mortalitty and Healthcare MERGED Final.csv")
names(data1)
head(data1)

data2<- read.csv("/Users/heatherkrause/Dropbox/Active Projects 2020/Orb/Health Worker Data + Research/Heather Orb Health Data Using/WHO Data Health Facilities by country.csv")

data3 <- read.csv("/Users/heatherkrause/Dropbox/Active Projects 2020/Orb/Health Worker Data + Research/Heather Orb Health Data Using/per-capita-total-expenditure-on-health-vs-child-mortality.csv")

data3$Country <- data3$Entity
data3$year <- data3$Year

data13 <- merge(data1,data3,by=c("Country"),all=T)
data13b <- merge(data1,data3,by=c("Country","year"),all=T)

dataF <- data13b[which(data13b$year>1980),]


fit1 <- lm(value_Under.five.mortality.rate~year+value_Nurses.and.midwives+value_Physicians,data=dataF)
fit1b <- lm(value_Under.five.mortality.rate~year+value_Nurses.and.midwives+value_Physicians,data=data13)
fit1b2 <- lm(value_Under.five.mortality.rate~year+value_Nurses.and.midwives+value_Physicians,data=data13b)
fit2 <- lm(value_Under.five.mortality.rate~year+value_Hospital.beds,data=dataF)
fit3 <- lm(value_Under.five.mortality.rate~year+value_Nurses.and.midwives+value_Physicians+value_Hospital.beds,data=dataF)
fit4 <- lm(value_Under.five.mortality.rate~year+Health.expenditure.per.capita..PPP..constant.2011.international.....constant.2011.international...,data=dataF)
fit5 <- lm(value_Under.five.mortality.rate~year+value_Nurses.and.midwives+value_Physicians+Health.expenditure.per.capita..PPP..constant.2011.international.....constant.2011.international...,data=dataF)
dataF$Pred1 <- predict(fit1,newdata=dataF)
dataF$Pred2 <- predict(fit2,newdata=dataF)
dataF$Pred3 <- predict(fit3,newdata=dataF)
dataF$Pred4 <- predict(fit4,newdata=dataF)
dataF$Pred5 <- predict(fit5,newdata=dataF)

dataF$Pred1C <- with(dataF,ifelse(Pred1>0,Pred1,0))
dataF$Pred2C <- with(dataF,ifelse(Pred2>0,Pred2,0))
dataF$Pred3C <- with(dataF,ifelse(Pred3>0,Pred3,0))
dataF$Pred4C <- with(dataF,ifelse(Pred4>0,Pred4,0))
dataF$Pred5C <- with(dataF,ifelse(Pred5>0,Pred5,0))

dataF$Difference1 <- dataF$Pred1C - dataF$value_Under.five.mortality.rate
dataF$Difference2 <- dataF$Pred2C - dataF$value_Under.five.mortality.rate
dataF$Difference3 <- dataF$Pred3C - dataF$value_Under.five.mortality.rate
dataF$Difference4 <- dataF$Pred4C - dataF$value_Under.five.mortality.rate
dataF$Difference5 <- dataF$Pred5C - dataF$value_Under.five.mortality.rate

write.csv(dataF, "/Users/heatherkrause/Dropbox/Active Projects 2020/Orb/Health Worker Data + Research/Heather Orb Health Data Using/PREDS + ORB Mortalitty and Healthcare MERGED Final.csv")
write.csv(data123, "/Users/heatherkrause/Dropbox/Active Projects 2020/Chamjari/Capital One/Data123.csv")

data1.csv
dataA <- read.csv("/Users/heatherkrause/Dropbox/Active Projects 2020/Orb/healthcareformerge.csv")
dataB <- read.csv("/Users/heatherkrause/Dropbox/Active Projects 2020/Orb/medicalformerge.csv")
dataC <- read.csv("/Users/heatherkrause/Dropbox/Active Projects 2020/Orb/differenceformerge.csv")

dataAB <- merge(dataA, dataB, by=c("Country"),all=T)
dataABc <- merge(dataAB, dataC, by=c("Country"),all=T)
write.csv(dataABc , "/Users/heatherkrause/Dropbox/Active Projects 2020/Orb/Orb health data to share May 5 2020.csv")


dataCHW<- read.csv("/Users/heatherkrause/Dropbox/Active Projects 2020/Orb/Health Worker Data + Research/data1.csv")
dataDR<- read.csv("/Users/heatherkrause/Dropbox/Active Projects 2020/Orb/Health Worker Data + Research/data3.csv")
dataNurse<- read.csv("/Users/heatherkrause/Dropbox/Active Projects 2020/Orb/Health Worker Data + Research/data2.csv")

dataCHWDR <- merge(dataCHW,dataDR,by=c("Country","Year"),all=T)
dataCHWDRN <- merge(dataCHWDR,dataNurse,by=c("Country","Year"),all=T)
write.csv(dataCHWDRN , "/Users/heatherkrause/Dropbox/Active Projects 2020/Orb/Health Worker Data + Research/WHOCombinedCountData.csv")
library(tidyverse)
group %>% group_by(Subject) %>% top_n(1, pt)

dataCHWnotmissing <- 
  dataCHWDRN %>% 
  filter(!is.na(dataCHWDRN$Community.Health.Workers..number.))

dataMostrecentyear <- dataCHWnotmissing %>% group_by(Country) %>% top_n(1, Year)
dataMostrecent3year <- dataCHWnotmissing %>% group_by(Country) %>% top_n(3, Year)

write.csv(dataMostrecentyear , "/Users/heatherkrause/Dropbox/Active Projects 2020/Orb/Health Worker Data + Research/WHOCombinedCountData_dataMostrecentyear.csv")

write.csv(dataMostrecent3year , "/Users/heatherkrause/Dropbox/Active Projects 2020/Orb/Health Worker Data + Research/WHOCombinedCountData_dataMostrecent3year.csv")

dataIN<- read.csv("/Users/heatherkrause/Dropbox/Active Projects 2020/Orb/Health Worker Data + Research/WB Countries by income CLASS.csv")
dataIN$Country <- dataIN$CountryName

dataF1 <- merge(dataCHWnotmissing,dataIN,by=c("Country"),all=T)
dataF2 <- merge(dataMostrecentyear,dataIN,by=c("Country"),all=T)

write.csv(dataF1 , "/Users/heatherkrause/Dropbox/Active Projects 2020/Orb/Health Worker Data + Research/dataF1.csv")
write.csv(dataF2 , "/Users/heatherkrause/Dropbox/Active Projects 2020/Orb/Health Worker Data + Research/dataF2.csv")

