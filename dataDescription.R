#Cleaning up data
rm(list=ls())

#loading libraries
library(sf)
library(ggplot2)
library(pheatmap)
library(readxl)

setwd("/Users/broche/Mon Drive/Docs/ResearchWorks/Moi - Indicateur Zoonoses/Analyse/WhoPrezodeIndicator") 

#Loading shapefile
borders <- st_read("GIS/world-administrative-boundaries.shp")

#Loading expert opinion data
opinionData <- read_xlsx("data/dataExpert.xlsx")
opinionData <- opinionData[opinionData$participantId!="282dbe47",] #Removing Benjamin participation (adding country absence for MERS and AIV)

#Get number of estimations
length(unique(opinionData$participantId))
length(opinionData$participantId)

length(opinionData$pathogen[opinionData$pathogen=="MERS"])
length(opinionData$pathogen[opinionData$pathogen=="AIV"])
length(opinionData$pathogen[opinionData$pathogen=="CCHF"])
length(opinionData$pathogen[opinionData$pathogen=="WNV"])

#Creating vectors for polygon information
polygonsWithInformation <- unique(opinionData$unitSelected[!is.na(opinionData$unitSelected)]);

pathogen <- c("AIV","WNV","CCHF","MERS")
matRes <-matrix(NA,length(pathogen),length(polygonsWithInformation));

#Number of estiamtes by pathogen
nbOcc=c();
for(j in 1:length(pathogen)){
	nbOcc=c(nbOcc,dim(opinionData[opinionData$pathogen==pathogen[j] & !is.na(opinionData$pathogen),1])[1]);
}

#Number of estiamtes by country
nbOccCountry=c();
for(i in 1:length(polygonsWithInformation)){
	nbOccCountry=c(nbOccCountry,dim(opinionData[opinionData$unitSelected==polygonsWithInformation[i] & !is.na(opinionData$unitSelected),1])[1]);
}

#Preparing the heatmap
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

#preparing the heatmap
rownames(matRes)=pathogen
colnames(matRes)=polygonsWithInformation
pdf("./figures/heatMapAnswers.pdf")
pheatmap(matRes)
dev.off();

#Get the number of estimates by pathogen
opinionData <- read_xlsx("data/dataExpert.xlsx")
length(opinionData$pathogen[opinionData$pathogen=="MERS"])
length(opinionData$pathogen[opinionData$pathogen=="AIV"])
length(opinionData$pathogen[opinionData$pathogen=="CCHF"])
length(opinionData$pathogen[opinionData$pathogen=="WNV"])

