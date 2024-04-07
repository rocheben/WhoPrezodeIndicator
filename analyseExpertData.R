#Analyze expert data

#Cleaning up data
rm(list=ls())

setwd("/Users/broche/Mon Drive/Docs/ResearchWorks/Moi - Indicateur Zoonoses/Analyse") 
# nolint
#loading libraries
library(sf)
library(ggplot2)
library(readxl)
library(RColorBrewer)
library(dplyr)
library(cowplot)

source("risk_category.R", encoding = "UTF-8")

#Loading shapefile
borders <- st_read("../GIS/world-administrative-boundaries.shp")

#Loading expert opinion data
opinionData <- read_xlsx("dataExpert.xlsx")

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
    temp <- subsetData$wildlifePressure[!is.na(subsetData$wildlifePressure)]*subsetData$wildlifePressureTrust[!is.na(subsetData$wildlifePressure)]/sum(subsetData$wildlifePressureTrust[!is.na(subsetData$wildlifePressure)]);
    if(length(temp)>0){wildlifePressure[i] <- round(sum(temp, na.rm=T))};
    temp <- subsetData$wildlifeContact[!is.na(subsetData$wildlifeContact)]*subsetData$wildlifeContactTrust[!is.na(subsetData$wildlifeContact)]/sum(subsetData$wildlifeContactTrust[!is.na(subsetData$wildlifeContact)]);
    if(length(temp)>0){wildlifeContact[i] <- round(sum(temp,na.rm=T));}
    temp <- subsetData$livestockPressure[!is.na(subsetData$livestockPressure)]*subsetData$livestockPressureTrust[!is.na(subsetData$livestockPressure)]/ sum(subsetData$livestockPressureTrust[!is.na(subsetData$livestockPressure)]);
    if(length(temp)>0){livestockPressure[i] <- round(sum(temp,na.rm=T));}
    temp <- subsetData$livestockContact[!is.na(subsetData$livestockContact)]*subsetData$livestockContactTrust[!is.na(subsetData$livestockContact)]/sum(subsetData$livestockContactTrust[!is.na(subsetData$livestockContact)]);
    if(length(temp)>0){livestockContact[i] <- round(sum(temp,na.rm=T));}
  }

  #Calculating the color for each polygon with an estimation
  for(i in 1:length(polygonsWithInformation)){

    temp<-risk_category(wildlifePressure[i],wildlifeContact[i], livestockPressure[i],livestockContact[i],livestockWildlife[i]);
    wildlifeRisk[i]<-temp[1];
    livestockRisk[i]<-temp[2]
    compositeRisk[i]<-temp[3]
  }

  #creating the output shape file
  wildCirculationShape <- borders
  livestockCirculationShape <- borders
  compositeShape <- borders
  #Setting all the polygons to the NULL value
  for(i in 1:length(borders$name)){
    wildCirculationShape$Area[i]=NA;
    livestockCirculationShape$Area[i]=NA;
    compositeShape$Area[i]=NA;
  }

  #Setting the polygons summary indices
  for(i in 1:length(polygonsWithInformation)){
    wildCirculationShape$Area[wildCirculationShape$name==polygonsWithInformation[i]] <- wildlifeRisk[i]
    livestockCirculationShape$Area[livestockCirculationShape$name==polygonsWithInformation[i]] <- livestockRisk[i]
    compositeShape$Area[compositeShape$name==polygonsWithInformation[i]] <- compositeRisk[i]
  }

  #ColorPalette
  palette1 <- colorRampPalette(brewer.pal(9, "Reds"))
  palette2 <- colorRampPalette(brewer.pal(9, "Blues"))
  palette3 <- colorRampPalette(brewer.pal(9, "Purples"))

  #Saving summary indices 
  wildlifePlot <- ggplot() +
    geom_sf(data = wildCirculationShape, aes(fill = as.factor(Area))) +
    scale_fill_manual(values = palette1(n_distinct(as.factor(wildCirculationShape$Area)))) +
    theme_minimal()+labs(fill = "Wildlife aggregated risk")
  ggsave(paste(pathogen[j], "_wildlifePlot.pdf"),wildlifePlot,width = 10, height = 5, dpi = 150, units = "in");
  #st_write(obj = wildCirculationShape, dsn=paste("resultsWildlife_",pathogen[j],".shp"))
  
  liveStockPlot <- ggplot() +
    geom_sf(data = livestockCirculationShape, aes(fill = as.factor(Area))) +
    scale_fill_manual(values = palette2(n_distinct(as.factor(livestockCirculationShape$Area)))) +
    theme_minimal()+labs(fill = "Livestock aggregated risk")
  
  #st_write(obj = livestockCirculationShape, dsn=paste("resultslivestock_",pathogen[j],".shp"))
  ggsave(paste(pathogen[j],"_liveStockPlot.pdf"),liveStockPlot,width = 10, height = 5, dpi = 150, units = "in");

  compositePlot <- ggplot() +
    geom_sf(data = compositeShape, aes(fill = as.factor(compositeShape$Area))) +
    scale_fill_manual(values = palette3(n_distinct(as.factor(compositeShape$Area)))) +
    labs(fill = "Overall aggregated risk")+ theme_minimal()

  
  #st_write(obj = compositeShape, dsn=paste("resultsComposite_",pathogen[j],".shp"))
  ggsave(paste(pathogen[j], "_compositePlot_.pdf"),compositePlot,width = 10, height = 5, dpi = 150, units = "in");

  # Create a composite plot

  temp <- plot_grid(wildlifePlot, liveStockPlot, compositePlot, 
                 nrow = 2, ncol = 2, align = "v", hjust = c(0, 0, 0.5))

  ggsave(paste(pathogen[j], "_AllPlots.pdf"), temp,width = 10, height = 5, dpi = 150, units = "in");

}

print("done")