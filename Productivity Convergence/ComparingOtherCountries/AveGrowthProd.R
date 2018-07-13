setwd("/Users/Lenovo/Desktop/Khazanah/Productivity Convergence/ComparingOtherCountries")
rm(list = ls(all=T))
library(xlsx)
library(openxlsx)
MasterData = read.xlsx("GroningenData.xlsx", sheet = "for Adam", colNames = T)
incomeClass = read.xlsx("Data_Extract_From_World_Development_Indicators.xlsx",colNames = T, 
                        sheet = "1")

MasterData0 = subset(MasterData, MasterData[3]>1974 & MasterData[3]<2012)
MasterData1 = MasterData0[,c(1,3,4,9)]

country = as.character(unique(MasterData1[,1]))
sector = unique(MasterData1[,3])

#Differentiate by country first
indA = 1
indE = 1
for (CountryIndex in 1:length(country)){
  temp = subset(MasterData1, MasterData1 == country[CountryIndex])
  class = as.character(subset(incomeClass, incomeClass == country[CountryIndex])[2][[1]])
  if (length(class) == 0){
    print(country[CountryIndex])
  }
  else if (class == "Advanced"){
    assign(paste("A",as.character(indA), sep = ""), temp)
    indA = indA + 1
  }
  else if (class == "EME"){
    assign(paste("E",as.character(indE), sep = ""), temp)
    indE = indE + 1
  }
  else{stop("no classification")}
}

assign(paste('E',indE, sep = ""), subset(MasterData1, MasterData1 == "MOR"))
assign(paste('A',indA, sep = ""), subset(MasterData1, MasterData1 == "TWN"))
assign(paste('E',indE+1, sep = ""), subset(MasterData1, MasterData1 == "VEN"))

#Separate it by sector
#EME
poorCountries = list(E1)
for (PCountryInd in 2:27){
  temp = eval(parse(text = paste("E",as.character(PCountryInd), sep = "")))
  poorCountries[[PCountryInd]] = temp
}

#Which Country
indC = 1
for (PCountryInd in 1:length(poorCountries)){
  for (SectorIndex in 1:length(sector)){
    temp = subset(poorCountries[[PCountryInd]], poorCountries[[PCountryInd]][3] == sector[SectorIndex])
    assign(paste("E",as.character(indC),".",as.character(SectorIndex), sep = ""), temp)
  }
  indC = indC+1
}
 
#Developed
richCountries = list(A1)
for (RCountryInd in 2:15){
  temp = eval(parse(text = paste("A",as.character(RCountryInd), sep = "")))
  richCountries[[RCountryInd]] = temp
}

#Which Country
indC = 1
for (RCountryInd in 1:length(richCountries)){
  for (SectorIndex in 1:length(sector)){
    temp = subset(richCountries[[RCountryInd]], richCountries[[RCountryInd]][3] == sector[SectorIndex])
    assign(paste("A",as.character(indC),".",as.character(SectorIndex), sep = ""), temp)
  }
  indC = indC+1
}

#Finding the average growth rate for each country
#the full period of time: 1975-2011
#Advanced countries
AAdf = list()
index = 1

for (lsInd in 1:length(ls())){
  if(grepl("A\\d",ls()[lsInd], perl = T)){
    AAdf[[index]] = eval(parse(text = ls()[lsInd]))
    index = index + 1
  }
}

AAdf<-AAdf[-seq(1,135,9)]
Adf <- data.frame()
for (secInd in 1:8){
  tempSec = AAdf[seq(secInd,120,8)]
  for(countryInd in 1:length(tempSec)){
    tempCountry = tempSec[[countryInd ]]
    CountryName = tempCountry[1,1]
    growthRate = diff(log(tempCountry[,4]))
    AveGrowthRate = mean(growthRate, na.rm = T)
    Adf[countryInd,1] = CountryName
    Adf[countryInd,secInd+1] = AveGrowthRate
  }
}

colnames(Adf) = c("Country", "Agriculture", "Mining","Manufacturing" ,"Utilities", "Construction", "TRH"
                  , "TSC", "FIRB")

#Developing countries
EEdf = list()
index = 1

for (lsInd in 1:length(ls())){
  if(grepl("E\\d",ls()[lsInd], perl = T)){
    EEdf[[index]] = eval(parse(text = ls()[lsInd]))
    index = index + 1
  }
}

EEdf<-EEdf[-seq(1,length(EEdf),9)]
Edf <- data.frame()
for (secInd in 1:8){
  tempSec = EEdf[seq(secInd,length(EEdf),8)]
  for(countryInd in 1:length(tempSec)){
    tempCountry = tempSec[[countryInd ]]
    CountryName = tempCountry[1,1]
    growthRate = diff(log(tempCountry[,4]))
    AveGrowthRate = mean(growthRate, na.rm = T)
    Edf[countryInd,1] = CountryName
    Edf[countryInd,secInd+1] = AveGrowthRate
  }
}

colnames(Edf) = c("Country", "Agriculture", "Mining","Manufacturing" ,"Utilities", "Construction", "TRH"
                  , "TSC", "FIRB")

#the first half period of time: 1975-1986
#Advanced countries
Adf1 <- data.frame()
for (secInd in 1:8){
  tempSec = AAdf[seq(secInd,120,8)]
  for(countryInd in 1:length(tempSec)){
    tempCountry = tempSec[[countryInd ]]
    CountryName = tempCountry[1,1]
    growthRate = diff(log(tempCountry[1:12,4]))
    AveGrowthRate = mean(growthRate, na.rm = T)
    Adf1[countryInd,1] = CountryName
    Adf1[countryInd,secInd+1] = AveGrowthRate
  }
}

colnames(Adf1) = c("Country", "Agriculture", "Mining","Manufacturing" ,"Utilities", "Construction", "TRH"
                  , "TSC", "FIRB")

#Developing countries
Edf1 <- data.frame()
for (secInd in 1:8){
  tempSec = EEdf[seq(secInd,length(EEdf),8)]
  for(countryInd in 1:length(tempSec)){
    tempCountry = tempSec[[countryInd ]]
    CountryName = tempCountry[1,1]
    growthRate = diff(log(tempCountry[1:12,4]))
    AveGrowthRate = mean(growthRate, na.rm = T)
    Edf1[countryInd,1] = CountryName
    Edf1[countryInd,secInd+1] = AveGrowthRate
  }
}

colnames(Edf1) = c("Country", "Agriculture", "Mining","Manufacturing" ,"Utilities", "Construction", "TRH"
                  , "TSC", "FIRB")

#the second half period of time: 1987-2000
#Advanced countries
Adf2 <- data.frame()
for (secInd in 1:8){
  tempSec = AAdf[seq(secInd,120,8)]
  for(countryInd in 1:length(tempSec)){
    tempCountry = tempSec[[countryInd ]]
    CountryName = tempCountry[1,1]
    growthRate = diff(log(tempCountry[13:26,4]))
    AveGrowthRate = mean(growthRate, na.rm = T)
    Adf2[countryInd,1] = CountryName
    Adf2[countryInd,secInd+1] = AveGrowthRate
  }
}

colnames(Adf2) = c("Country", "Agriculture", "Mining","Manufacturing" ,"Utilities", "Construction", "TRH"
                   , "TSC", "FIRB")

#Developing countries
Edf2 <- data.frame()
for (secInd in 1:8){
  tempSec = EEdf[seq(secInd,length(EEdf),8)]
  for(countryInd in 1:length(tempSec)){
    tempCountry = tempSec[[countryInd ]]
    CountryName = tempCountry[1,1]
    growthRate = diff(log(tempCountry[13:26,4]))
    AveGrowthRate = mean(growthRate, na.rm = T)
    Edf2[countryInd,1] = CountryName
    Edf2[countryInd,secInd+1] = AveGrowthRate
  }
}

colnames(Edf2) = c("Country", "Agriculture", "Mining","Manufacturing" ,"Utilities", "Construction", "TRH"
                   , "TSC", "FIRB")

#the second half period of time: 2001-2011
#Advanced countries
Adf3 <- data.frame()
for (secInd in 1:8){
  tempSec = AAdf[seq(secInd,120,8)]
  for(countryInd in 1:length(tempSec)){
    tempCountry = tempSec[[countryInd ]]
    CountryName = tempCountry[1,1]
    growthRate = diff(log(tempCountry[27:nrow(tempCountry),4]))
    AveGrowthRate = mean(growthRate, na.rm = T)
    Adf3[countryInd,1] = CountryName
    Adf3[countryInd,secInd+1] = AveGrowthRate
  }
}

colnames(Adf3) = c("Country", "Agriculture", "Mining","Manufacturing" ,"Utilities", "Construction", "TRH"
                   , "TSC", "FIRB")

#Developing countries
Edf3 <- data.frame()
for (secInd in 1:8){
  tempSec = EEdf[seq(secInd,length(EEdf),8)]
  for(countryInd in 1:length(tempSec)){
    tempCountry = tempSec[[countryInd ]]
    CountryName = tempCountry[1,1]
    growthRate = diff(log(tempCountry[27:nrow(tempCountry),4]))
    AveGrowthRate = mean(growthRate, na.rm = T)
    Edf3[countryInd,1] = CountryName
    Edf3[countryInd,secInd+1] = AveGrowthRate
  }
}

colnames(Edf3) = c("Country", "Agriculture", "Mining","Manufacturing" ,"Utilities", "Construction", "TRH"
                   , "TSC", "FIRB")

detach(package:openxlsx)
write.xlsx(Adf, "ComparingProductivity.xlsx", sheetName="Developed")
write.xlsx(Edf, "ComparingProductivity.xlsx", sheetName="EME", append = T)

dev.off()
#Boxplot for the whole period
#developed
boxplot(Adf[,-1],par(cex.lab=24), 
        main = 'Average growth rate of productivity\nfor developed countries from 1975-2011', 
        ylim = c(-0.07,0.12))

#EME
boxplot(Edf[,-1],par(cex.lab=24), 
        main = 'Average growth rate of productivity\nfor EME countries from 1975-2011',
        ylim = c(-0.07,0.12))

#Malaysia
stripchart(Edf[9,-1], vertical = T, method = "jitter", 
           add=T, pch=20, col="red")

#Boxplot for the first third of the period
#Developed
png('Advanced1.png', width = 900, height = 480)
boxplot(Adf1[,-1],par(cex.lab=24), 
        main = 'Average growth rate of productivity\nfor developed countries from 1975-1986' 
        ,ylim = c(-0.155, 0.190))
dev.off()

#Developing
png('EME1.png', width = 900, height = 480)
boxplot(Edf1[,-1],par(cex.lab=24), 
        main = 'Average growth rate of productivity\nfor EME countries from 1975-1986'
        ,ylim = c(-0.155, 0.190))
stripchart(Edf1[9,-1], vertical = T, method = "jitter", 
           add=T, pch=20, col="red")
dev.off()

#Boxplot for the second third of the period
#Developed
png('Advanced2.png', width = 900, height = 480)
boxplot(Adf2[,-1],par(cex.lab=24), 
        main = 'Average growth rate of productivity\nfor developed countries from 1987-2000'
        ,ylim = c(-0.15,0.2))
dev.off()

png('EME2.png', width = 900, height = 480)
boxplot(Edf2[,-1],par(cex.lab=24), 
        main = 'Average growth rate of productivity\nfor EME countries from 1987-2000'
        ,ylim = c(-0.15,0.2))
stripchart(Edf2[9,-1], vertical = T, method = "jitter", 
           add=T, pch=20, col="red")
dev.off()

#Boxplot for the final third of the period
#Developed
png('Advanced3.png', width = 900, height = 480)
boxplot(Adf3[,-1],par(cex.lab=24), 
        main = 'Average growth rate of productivity\nfor developed countries from 2001-2011'
        ,ylim = c(-0.15,0.21))
dev.off()

png('EME3.png', width = 900, height = 480)
boxplot(Edf3[,-1],par(cex.lab=24), 
        main = 'Average growth rate of productivity\nfor EME countries from 2001-2011'
        ,ylim = c(-0.15,0.21))
stripchart(Edf3[9,-1], vertical = T, method = "jitter", 
           add=T, pch=20, col="red")
dev.off()

