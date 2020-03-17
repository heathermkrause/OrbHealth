data1<- read.csv("/Users/heatherkrause/Dropbox/Active Projects 2020/Orb/Health Worker Data + Research/Heather Orb Health Data Using/ORB Mortalitty and Healthcare MERGED Final.csv")



names(data1)
names(data2)
names(data3)

head(data1)

data1$EASE.Username <- data1$CUST_LGIN_NM

data13 <- merge(data1,data3, by=c("EASE.Username"),all=T)
data123 <- merge(data13,data2,by=c("SSO_ID"),all=T)

data24 <- merge(data4,data2,by=c("SESSION_ID"),all=T)


write.csv(data24, "/Users/heatherkrause/Dropbox/Active Projects 2020/Chamjari/Capital One/DataFailure.csv")
write.csv(data123, "/Users/heatherkrause/Dropbox/Active Projects 2020/Chamjari/Capital One/Data123.csv")