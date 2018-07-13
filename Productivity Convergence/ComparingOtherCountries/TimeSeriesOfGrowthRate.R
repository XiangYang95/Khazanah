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

#Finding the average growth rate for each country
#the full period of time: 1975-2011
#Advanced countries
AAdf = list()
index = 1

for (lsInd in 1:length(ls())){
  if(grepl("A\\d+\\.[135678]",ls()[lsInd], perl = T)){
    AAdf[[index]] = eval(parse(text = ls()[lsInd]))
    index = index + 1
  }
}

#A list that countains data frames of the productivity growth
AProdGrowthDf = list()
for (i in 1:length(AAdf)){
  tempCountry = AAdf[[i]]
  ProdGrowth = diff(log(tempCountry[,4]))
  newDf = cbind(tempCountry[-1,1:3], ProdGrowth)
  AProdGrowthDf[[i]] = newDf
}

#Plot multiple lines in one graph
noOfSec = length(AProdGrowthDf)/length(richCountries)

for (secInd in 1:noOfSec){
  tempSec = AProdGrowthDf[seq(secInd,length(AProdGrowthDf),noOfSec)]
  windows(title = as.character(paste("Developed",tempSec[[1]][1,3], sep = ":")))
  Colno = 26
  plot(tempSec[[1]][,4], type = "l", col = Colno)
  for (i in 2:length(tempSec)){
    Colno = Colno +1
    lines(tempSec[[i]][,4], col = Colno)
  }
}

#Advanced countries
AAdf = list()
index = 1

for (lsInd in 1:length(ls())){
  if(grepl("E\\d+\\.[135678]",ls()[lsInd], perl = T)){
    AAdf[[index]] = eval(parse(text = ls()[lsInd]))
    index = index + 1
  }
}

#A list that countains data frames of the productivity growth
AProdGrowthDf = list()
for (i in 1:length(AAdf)){
  tempCountry = AAdf[[i]]
  ProdGrowth = diff(log(tempCountry[,4]))
  newDf = cbind(tempCountry[-1,1:3], ProdGrowth)
  AProdGrowthDf[[i]] = newDf
}

#Plot multiple lines in one graph
noOfSec = length(AProdGrowthDf)/length(richCountries)

for (secInd in 1:noOfSec){
  tempSec = AProdGrowthDf[seq(secInd,length(AProdGrowthDf),noOfSec)]
  windows(title = as.character(paste("EME",tempSec[[1]][1,3], sep = ":")))
  Colno = 26
  plot(tempSec[[1]][,4], type = "l", col = Colno)
  for (i in 2:length(tempSec)){
    Colno = Colno +1
    lines(tempSec[[i]][,4], col = Colno)
  }
}
