#####################################################################################
# Author: Benjamin Roche
# Date: 04/10/2024
# Description: description of data from elicitation study
#####################################################################################

#Cleaning up data
rm(list=ls())

#loading libraries
library(sf)
library(ggplot2)
library(pheatmap)
library(readxl)

#Setting up the working directory
setwd("/Users/broche/Mon Drive/Docs/ResearchWorks/Moi - Indicateur Zoonoses/Analyse/WhoPrezodeIndicator") 

#Loading shapefile
borders <- st_read("GIS/world-administrative-boundaries.shp")

#Loading expert opinion data
opinionData <- read_xlsx("data/dataExpert.xlsx")
opinionData <- opinionData[opinionData$participantId!="282dbe47",] #Removing Benjamin participation (adding country absence for MERS and AIV)

#Get number of estimations (number of participants and number of estimates)
print(paste0("Number of participants:",length(unique(opinionData$participantId))))
print(paste0("Number of estimates:",length(opinionData$participantId)))

#Getting the geographical unit with expert information
polygonsWithInformation <- unique(opinionData$unitSelected[!is.na(opinionData$unitSelected)]);

continent <- rep(NA,length(polygonsWithInformation))
continent[c(1,4,6,13,14,15,16,20,26,28,29,33,38,42,48)]<-"Africa"
continent[c(21,39,58)]<-"Middle-East"
continent[c(5,8,9,12,17,19,44,49,50,51,52,53,54,55,56)]<-"Europe"
continent[c(2,10,18,23,25,34,35,47)]<-"America"
continent[c(3,7,11,22,24,27,30,31,36,37,40,41,43,45,46,57)]<-"Asia"
continent[c(32)]<-"Oceania"
continentUnique <- unique(continent)

#Creating the matrix for data information
pathogen <- c("AIV","WNV","CCHF","MERS")
matRes <-matrix(NA,length(pathogen),length(polygonsWithInformation));

#Number of estimates by pathogen
nbOcc=c();
for(j in 1:length(pathogen)){
	nbOcc=c(nbOcc,dim(opinionData[opinionData$pathogen==pathogen[j] & !is.na(opinionData$pathogen),1])[1]);
}

#Number of estimates by country
nbOccCountry=c();
for(i in 1:length(polygonsWithInformation)){
	nbOccCountry=c(nbOccCountry,dim(opinionData[opinionData$unitSelected==polygonsWithInformation[i] & !is.na(opinionData$unitSelected),1])[1]);
}

#Preparing the heatmap at a country scale
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

matResCont <-matrix(NA,length(pathogen),length(continentUnique));
#Preparing the heatmap at a country scale
for(j in 1:4){
	for(i in 1:length(continentUnique)){
		matResCont[j,i]=sum(matRes[j,which(continent==continentUnique[i])])
	}
}

#Number of estimates by country
nbOccCont=c();
for(i in 1:length(continentUnique)){
	nbOccCont[i]=sum(nbOccCountry[continent==continentUnique[i]]);
}

# Organize data into a data frame
data <- data.frame(
	Pathogen = rep(pathogen, each = length(continentUnique)),
	Continent = rep(continentUnique, times = length(pathogen)),
	nbOcc = rep(nbOcc, each = length(continentUnique)),
	nbOccCont = nbOccCont,
	matResCont = as.vector(matResCont)
)

#preparing the heatmap
rownames(matResCont)=pathogen
colnames(matResCont)=continentUnique
pdf("./figures/heatMapAnswersCont.pdf")
pheatmap(matResCont)
dev.off();

#Get the number of estimates by pathogen with the inclusion of the data entered by Benjamin on documented absence
opinionData <- read_xlsx("data/dataExpert.xlsx")
print(paste0("Number of estimates for MERS:",length(opinionData$pathogen[opinionData$pathogen=="MERS"])))
print(paste0("Number of estimates for AIV:",length(opinionData$pathogen[opinionData$pathogen=="AIV"])))
print(paste0("Number of estimates for CCHF:",length(opinionData$pathogen[opinionData$pathogen=="CCHF"])))
print(paste0("Number of estimates for WNV:",length(opinionData$pathogen[opinionData$pathogen=="WNV"])))