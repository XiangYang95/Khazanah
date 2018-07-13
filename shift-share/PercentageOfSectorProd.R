setwd("/Users/Lenovo/Desktop/Khazanah/shift-share")
rm(list = ls(all = T))
library(xlsx)

data = read.xlsx("Productivity 2000 to 2016.xlsx", header = T, sheetName = "Manu and Services", endRow = 14)
data1 = read.csv("Productivity_2010-2016.csv", header = T)
data1 = data1[1:11,]

#divided by the TPP
#1993-2016
temp = data[,-1]
tpp = data[nrow(data),-1]
tpp1 = data.frame()
for (numrow in 1:nrow(temp)){
  tpp1 = rbind(tpp1,tpp)
}

percentage = temp/tpp1
percentage = cbind(data[,1], percentage)

#2010-2016
temp = data1[,-1]
tpp = data1[nrow(data1),-1]
tpp1 = data.frame()
for (numrow in 1:nrow(temp)){
  tpp1 = rbind(tpp1,tpp)
}

percentage1 = temp/tpp1
percentage1 = cbind(data1[,1], percentage1)

library(xts)

dev.off()
#1993-2016
dates <- seq(as.Date("1993-01-01"),length=ncol(percentage)-1,by="years")

xtsdata <- xts(x=t(percentage[,-1]), order.by=dates)
colnames(xtsdata) <- as.character(data[,1])
mining = xtsdata[,ncol(xtsdata)-1]
xtsdata$`Other Market Services` = NULL
otherXTS <- xtsdata
otherXTS$`Total Productivity without Imputed Rent` = NULL
otherXTS$Mining = NULL


windows()
plot(otherXTS, xlab = "year", main="Productivity share by sector excluding mining\n from 1993-2016"
     ,col = rainbow(ncol(otherXTS)),legend.loc = "topleft", legend.cex = 0.1)
windows()
plot(mining, xlab = "year", main="Productivity share by mining\n from 1993-2016"
     ,legend.loc = "topleft")


#2010-2016
dates1 <- seq(as.Date("2010-01-01"),length=ncol(percentage1)-1,by="years")

xtsdata1 <- xts(x=t(percentage1[,-1]), order.by=dates1)
colnames(xtsdata1) <- as.character(data1[,1])
mining1 = xtsdata1[,ncol(xtsdata1)-1]
xtsdata1$`Other Market Services` = NULL
otherXTS1 <- xtsdata1
otherXTS1$`Total Productivity without Imputed Rent` = NULL
otherXTS1$Mining = NULL

windows()
plot(otherXTS1, xlab = "year", main="Productivity share by sector excluding mining\n from 2010-2016"
     ,col = rainbow(ncol(otherXTS1)),legend.loc = "topleft")
windows()
plot(mining1, xlab = "year", main="Productivity share by mining\n from 2010-2016"
     ,legend.loc = "topleft")

