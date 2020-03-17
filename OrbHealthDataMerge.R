data1<- read.csv("/Users/heatherkrause/Dropbox/Active Projects 2020/Orb/Health Worker Data + Research/Heather Orb Health Data Using/ORB Mortalitty and Healthcare MERGED Final.csv")
names(data1)
head(data1)

data2<- read.csv("/Users/heatherkrause/Dropbox/Active Projects 2020/Orb/Health Worker Data + Research/Heather Orb Health Data Using/WHO Data Health Facilities by country.csv")

data3 <- read.csv("/Users/heatherkrause/Dropbox/Active Projects 2020/Orb/Health Worker Data + Research/Heather Orb Health Data Using/per-capita-total-expenditure-on-health-vs-child-mortality.csv")

data3$Country <- data3$Entity
data3$year <- data3$Year

data13 <- merge(data1,data3,by=c("Country"),all=T)
data13b <- merge(data1,data3,by=c("Country","year"),all=T)

fit1 <- lm(value_Under.five.mortality.rate~year+value_Nurses.and.midwives+value_Physicians,data=data1)
fit1b <- lm(value_Under.five.mortality.rate~year+value_Nurses.and.midwives+value_Physicians,data=data13)
fit1b2 <- lm(value_Under.five.mortality.rate~year+value_Nurses.and.midwives+value_Physicians,data=data13b)
fit2 <- lm(value_Under.five.mortality.rate~year+value_Hospital.beds,data=data13b)
fit3 <- lm(value_Under.five.mortality.rate~year+value_Nurses.and.midwives+value_Physicians+value_Hospital.beds,data=data13b)
fit4 <- lm(value_Under.five.mortality.rate~year+Health.expenditure.per.capita..PPP..constant.2011.international.....constant.2011.international...,data=data13b)
fit5 <- lm(value_Under.five.mortality.rate~year+value_Nurses.and.midwives+value_Physicians+Health.expenditure.per.capita..PPP..constant.2011.international.....constant.2011.international...,data=data13b)


write.csv(data24, "/Users/heatherkrause/Dropbox/Active Projects 2020/Chamjari/Capital One/DataFailure.csv")
write.csv(data123, "/Users/heatherkrause/Dropbox/Active Projects 2020/Chamjari/Capital One/Data123.csv")