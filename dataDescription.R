#Cleaning up data
rm(list=ls())

# nolint
#loading libraries
library(sf)
library(ggplot2)
library(pheatmap)
library(readxl)

setwd("/Users/broche/Mon Drive/Docs/ResearchWorks/Moi - Indicateur Zoonoses/Analyse") 

#Loading shapefile
borders <- st_read("../GIS/world-administrative-boundaries.shp")

#Loading expert opinion data
opinionData <- read_xlsx("dataExpert.xlsx")

pathogen <- c("AIV","WNV","CCHF","MERS")
matRes <-matrix(NA,length(pathogen),length(polygonsWithInformation));

nbOcc=c();
for(j in 1:4){
  nbOcc=c(nbOcc,dim(opinionData[opinionData$pathogen==pathogen[j] & !is.na(opinionData$pathogen),1])[1]);
}

nbOccCountry=c();
for(i in 1:length(polygonsWithInformation)){
  nbOccCountry=c(nbOccCountry,dim(opinionData[opinionData$unitSelected==polygonsWithInformation[i] & !is.na(opinionData$unitSelected),1])[1]);
}

for(j in 1:4){
  for(i in 1:length(polygonsWithInformation)){
    matRes[j,i]=dim(opinionData[opinionData$unitSelected==polygonsWithInformation[i] & opinionData$pathogen==pathogen[j] & !is.na(opinionData$unitSelected) & !is.na(opinionData$pathogen),1])[1];
  }
}
# Organize data into a data frame
data <- data.frame(
  Pathogen = rep(pathogen, each = length(polygonsWithInformation)),
  Country = rep(polygonsWithInformation, times = length(pathogen)),
  nbOcc = rep(nbOcc, each = length(polygonsWithInformation)),
  nbOccCountry = nbOccCountry,
  matRes = as.vector(matRes)
)

rownames(matRes)=pathogen
colnames(matRes)=polygonsWithInformation
pdf("heatMapAnswers.pdf")
pheatmap(matRes)
dev.off();
