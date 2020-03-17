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
dataF$Pred2C <- with(dataF,ifelse(Pred2>0,Pred1,0))
dataF$Pred3C <- with(dataF,ifelse(Pred3>0,Pred1,0))
dataF$Pred4C <- with(dataF,ifelse(Pred4>0,Pred1,0))
dataF$Pred5C <- with(dataF,ifelse(Pred5>0,Pred1,0))

write.csv(dataF, "/Users/heatherkrause/Dropbox/Active Projects 2020/Orb/Health Worker Data + Research/Heather Orb Health Data Using/PREDS + ORB Mortalitty and Healthcare MERGED Final.csv")
write.csv(data123, "/Users/heatherkrause/Dropbox/Active Projects 2020/Chamjari/Capital One/Data123.csv")