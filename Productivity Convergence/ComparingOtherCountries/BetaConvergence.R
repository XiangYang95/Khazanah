setwd("/Users/Lenovo/Desktop/Khazanah/Productivity Convergence/ComparingOtherCountries")
rm(list = ls(all=T))
library(xlsx)
library(openxlsx)
MasterData = read.xlsx("GroningenData.xlsx", sheet = "for Adam", colNames = T)
incomeClass = read.xlsx("Data_Extract_From_World_Development_Indicators.xlsx",colNames = T, 
                        sheet = "1")

MasterData0 = subset(MasterData, MasterData[3]>1974 & MasterData[3]<2010)
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

#Beta Convergence
#the full period of time: 1975-2011
#Advanced countries
df = list()
index = 1

for (lsInd in 1:length(ls())){
  if(grepl("[[:upper:]]\\d+\\.[135678]",ls()[lsInd], perl = T)){
    df[[index]] = eval(parse(text = ls()[lsInd]))
    index = index + 1
  }
}

SecList = list()
for (secInd in 1:6){
  tempSec = df[seq(secInd,length(df),6)]
  tempDF = data.frame()
  for(countryInd in 1:length(tempSec)){
    tempCountry = tempSec[[countryInd]]
    CountryName = tempCountry[1,1]
    SectorName = tempCountry[1,3]
    initial = tempCountry[1,4]
    final = tempCountry[length(tempCountry[,4]),4]
    aveGrowthRate = log(final/initial)/length(tempCountry[,4])
    initProd = log(initial)
    tempVar = data.frame(CountryName,SectorName,initProd,aveGrowthRate)
    tempDF = rbind(tempDF, tempVar)
  }
  SecList[[secInd]] = tempDF
}


for (i in 1:6){
  model = lm(SecList[[i]][,4]*1000~SecList[[i]][,3])
  windows()
  plot(SecList[[i]][,3],SecList[[i]][,4]*1000,
       col = ifelse(SecList[[i]][,1] == "MYS", "red", "black"))
  abline(model)
  print(as.character(SecList[[i]][1,2]))
  print(summary(model))
}

#Between 1975-1990
SecList = list()
for (secInd in 1:6){
  tempSec = df[seq(secInd,length(df),6)]
  tempDF = data.frame()
  for(countryInd in 1:length(tempSec)){
    tempCountry = tempSec[[countryInd]]
    CountryName = tempCountry[1,1]
    SectorName = tempCountry[1,3]
    initial = tempCountry[1,4]
    final = tempCountry[16,4]
    aveGrowthRate = log(final/initial)/16
    initProd = log(initial)
    tempVar = data.frame(CountryName,SectorName,initProd,aveGrowthRate)
    tempDF = rbind(tempDF, tempVar)
  }
  SecList[[secInd]] = tempDF
}


for (i in 1:6){
  model = lm(SecList[[i]][,4]~SecList[[i]][,3])
  windows()
  plot(SecList[[i]][,3],SecList[[i]][,4],
       col = ifelse(SecList[[i]][,1] == "MYS", "red", "black"))
  abline(model)
  print(as.character(SecList[[i]][1,2]))
  print(summary(model))
}

#Between 1991-2009
SecList = list()
for (secInd in 1:6){
  tempSec = df[seq(secInd,length(df),6)]
  tempDF = data.frame()
  for(countryInd in 1:length(tempSec)){
    tempCountry = tempSec[[countryInd]]
    CountryName = tempCountry[1,1]
    SectorName = tempCountry[1,3]
    initial = tempCountry[17,4]
    final = tempCountry[length(tempCountry[,4]),4]
    aveGrowthRate = log(final/initial)/19
    initProd = log(initial)
    tempVar = data.frame(CountryName,SectorName,initProd,aveGrowthRate)
    tempDF = rbind(tempDF, tempVar)
  }
  SecList[[secInd]] = tempDF
}


for (i in 1:6){
  model = lm(SecList[[i]][,4]~SecList[[i]][,3])
  windows()
  plot(SecList[[i]][,3],SecList[[i]][,4],
       col = ifelse(SecList[[i]][,1] == "MYS", "red", "black"))
  abline(model)
  print(as.character(SecList[[i]][1,2]))
  print(summary(model))
}




