#Analyze expert data

#Cleaning up data
rm(list=ls())

setwd("/Users/broche/Mon Drive/Docs/ResearchWorks/Moi - Indicateur Zoonoses/Analyse/WhoPrezodeIndicator") 

#loading libraries
library(sf)
library(ggplot2)
library(readxl)
library(RColorBrewer)
library(dplyr)
library(cowplot)

source("sources/risk_category.R", encoding = "UTF-8")
source("sources/weightedAverage.R")

#Loading shapefile
borders <- st_read("./GIS/world-administrative-boundaries.shp")

#Loading expert opinion data
opinionData <- read_xlsx("./data/dataExpert.xlsx")

#Creating vectors for polygon information
polygonsWithInformation <- unique(opinionData$unitSelected[!is.na(opinionData$unitSelected)]);

#Converting into numerical values
opinionData$wildlifePressure <- as.numeric(opinionData$wildlifePressure)
opinionData$wildlifePressureTrust <- as.numeric(opinionData$wildlifePressureTrust)
opinionData$wildlifeContact <- as.numeric(opinionData$wildlifeContact)
opinionData$wildlifePressureTrust <- as.numeric(opinionData$wildlifePressureTrust)
opinionData$livestockPressure <- as.numeric(opinionData$livestockPressure)
opinionData$wildlifePressureTrust <- as.numeric(opinionData$livestockPressureTrust)
opinionData$livestockContact <- as.numeric(opinionData$livestockContact)
opinionData$wildlifeContactTrust <- as.numeric(opinionData$livestockContactTrust)
opinionData$wildlifelivestockContact <- as.numeric(opinionData$wildlifelivestockContact)
opinionData$livestockWildlifeContactTrust <- as.numeric(opinionData$livestockWildlifeContactTrust)

pathogen <- c("AIV","WNV","CCHF","MERS")
#For each pathogen
for(j in 1:4){
  
  #Initializing
  wildlifePressure <- rep(NA,length(polygonsWithInformation));
  wildlifeContact <- rep(NA,length(polygonsWithInformation));
  livestockPressure <- rep(NA,length(polygonsWithInformation));
  livestockContact <- rep(NA,length(polygonsWithInformation));
  livestockWildlife <- rep(NA,length(polygonsWithInformation));

  livestockRisk <- rep(NA,length(polygonsWithInformation));
  wildlifeRisk<- rep(NA,length(polygonsWithInformation));
  compositeRisk<- rep(NA,length(polygonsWithInformation));

  dataPat <- opinionData[opinionData$pathogen==pathogen[j] & !is.na(opinionData$pathogen),]
  #Calculating values for each polygon
  for(i in 1:length(polygonsWithInformation)){
    #Selecting information for this polygon
    subsetData <- dataPat[dataPat$unitSelected==polygonsWithInformation[i] & !is.na(dataPat$unitSelected),]
  
    #Calculating weighted average
    wildlifePressure[i] <- weightedAverage(subsetData$wildlifePressure,subsetData$wildlifePressureTrust)
    wildlifeContact[i] <- weightedAverage(subsetData$wildlifeContact,subsetData$wildlifeContactTrust)
    livestockPressure[i] <- weightedAverage(subsetData$livestockPressure,subsetData$livestockPressureTrust)
    livestockContact[i] <- weightedAverage(subsetData$livestockContact,subsetData$livestockContactTrust)
  }

  #Calculating the color for each polygon with an estimation
  for(i in 1:length(polygonsWithInformation)){

    temp<-risk_category(wildlifePressure[i],wildlifeContact[i], livestockPressure[i],livestockContact[i],livestockWildlife[i]);
    wildlifeRisk[i]<-temp[1];
    livestockRisk[i]<-temp[2]
    compositeRisk[i]<-temp[3]
  }

  #creating the output shape file
  #Setting all the polygons to the NULL value
  wildCirculationShape <- borders;wildCirculationShape$Area <-rep(NA,length(wildCirculationShape$iso3));
  livestockCirculationShape <- borders;livestockCirculationShape$Area <-rep(NA,length(livestockCirculationShape$iso3));
  compositeShape <- borders;compositeShape$Area <-rep(NA,length(livestockCirculationShape$iso3));

  #Setting the polygons summary indices
  for(i in 1:length(polygonsWithInformation)){
    wildCirculationShape$Area[wildCirculationShape$name==polygonsWithInformation[i]] <- wildlifeRisk[i]
    livestockCirculationShape$Area[livestockCirculationShape$name==polygonsWithInformation[i]] <- livestockRisk[i]
    compositeShape$Area[compositeShape$name==polygonsWithInformation[i]] <- compositeRisk[i]
  }

  #Plotting the maps
  source("sources/plotMap.R");

}
