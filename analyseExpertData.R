#####################################################################################
# Author: Benjamin Roche
# Date: 04/10/2024
# Description: Analyze expert data
#####################################################################################

#Cleaning up data
rm(list = ls())

#Setting up the working directory
setwd("/Users/broche/Mon Drive/Docs/ResearchWorks/Moi - Indicateur Zoonoses/Analyse/WhoPrezodeIndicator")

#loading libraries
library(sf)
library(ggplot2)
library(readxl)
library(RColorBrewer)
library(dplyr)
library(cowplot)
library(vscDebugger)

#Loading source code for risk categories
source("sources/risk_category.R", encoding = "UTF-8")
source("sources/weightedAverage.R")

#Loading shapefile
borders <- st_read("./GIS/world-administrative-boundaries.shp")

#Loading expert opinion data
opinionData <- read_xlsx("./data/dataExpert.xlsx")

#Getting the geographical unit with expert information
polygonsWithInformation <-unique(opinionData$unitSelected[!is.na(opinionData$unitSelected)])

#Converting into numerical values
opinionData$wildlifePressure <-as.numeric(opinionData$wildlifePressure)
opinionData$wildlifePressureTrust <-as.numeric(opinionData$wildlifePressureTrust)
opinionData$wildlifeContact <-as.numeric(opinionData$wildlifeContact)
opinionData$wildlifePressureTrust <-as.numeric(opinionData$wildlifePressureTrust)
opinionData$livestockPressure <-as.numeric(opinionData$livestockPressure)
opinionData$wildlifePressureTrust <-as.numeric(opinionData$livestockPressureTrust)
opinionData$livestockContact <-as.numeric(opinionData$livestockContact)
opinionData$wildlifeContactTrust <-as.numeric(opinionData$livestockContactTrust)
opinionData$wildlifelivestockContact <-as.numeric(opinionData$wildlifelivestockContact)
opinionData$livestockWildlifeContactTrust <-as.numeric(opinionData$livestockWildlifeContactTrust)

#For each pathogen
pathogen <- c("AIV", "WNV", "CCHF", "MERS")
for (j in 1:4) {
	#Initializing the vectors for variables and risk categories
	wildlifePressure <- rep(NA, length(polygonsWithInformation))
	wildlifeContact <- rep(NA, length(polygonsWithInformation))
	livestockPressure <- rep(NA, length(polygonsWithInformation))
	livestockContact <- rep(NA, length(polygonsWithInformation))
	livestockWildlife <- rep(NA, length(polygonsWithInformation))
	
	livestockRisk <- rep(NA, length(polygonsWithInformation))
	wildlifeRisk <- rep(NA, length(polygonsWithInformation))
	compositeRisk <- rep(NA, length(polygonsWithInformation))
	
	dataPat <-opinionData[opinionData$pathogen == pathogen[j] &!is.na(opinionData$pathogen), ]
	#Calculating averaged estimates for each polygon
	for (i in 1:length(polygonsWithInformation)) {
		#subseting the dataset for the current pathogen
		subsetData <-dataPat[dataPat$unitSelected == polygonsWithInformation[i] &!is.na(dataPat$unitSelected), ]
		
		#Calculating weighted average
		wildlifePressure[i] <-weightedAverage(subsetData$wildlifePressure,subsetData$wildlifePressureTrust)
		wildlifeContact[i] <-weightedAverage(subsetData$wildlifeContact,subsetData$wildlifeContactTrust)
		livestockPressure[i] <-weightedAverage(subsetData$livestockPressure,subsetData$livestockPressureTrust)
		livestockContact[i] <-weightedAverage(subsetData$livestockContact,subsetData$livestockContactTrust)
	}
	
	#Calculating the category for each polygon with an estimation
	for (i in 1:length(polygonsWithInformation)) {
		temp <-risk_category(wildlifePressure[i],wildlifeContact[i],livestockPressure[i],livestockContact[i],livestockWildlife[i])
		wildlifeRisk[i] <- temp[1]
		livestockRisk[i] <- temp[2]
		compositeRisk[i] <- temp[3]
	}
	
	#creating the output shape file
	wildCirculationShape <-borders
	wildCirculationShape$Area <-rep(NA, length(wildCirculationShape$iso3))
	
	livestockCirculationShape <-borders
	livestockCirculationShape$Area <-rep(NA, length(livestockCirculationShape$iso3))
	
	compositeShape <-borders
	compositeShape$Area <-rep(NA, length(livestockCirculationShape$iso3))
	
	#Setting the category to each polygon
	for (i in 1:length(polygonsWithInformation)) {
		wildCirculationShape$Area[wildCirculationShape$name == polygonsWithInformation[i]] <-wildlifeRisk[i]
		livestockCirculationShape$Area[livestockCirculationShape$name == polygonsWithInformation[i]] <-livestockRisk[i]
		compositeShape$Area[compositeShape$name == polygonsWithInformation[i]] <-compositeRisk[i]
	}
	
	#Plotting the maps
	#Colour palettes
	palette1 <- colorRampPalette(c("white", brewer.pal(9, "Reds")[2:9]))
	palette2 <- colorRampPalette(c("white", brewer.pal(9, "Blues")[2:9]))
	palette3 <- colorRampPalette(c("white", brewer.pal(9, "Purples")[2:9]))
	
	#Wildlife plot
	wildlifePlot <- ggplot() +
		geom_sf(data = wildCirculationShape, aes(fill = as.numeric(Area))) +
		scale_fill_gradientn(
			colours = palette1(100),
			values = scales::rescale(c(0, 5)),
			breaks = seq(0, 5, by = 1),
			limits = c(0, 5),
			name = "Category of\nhuman exposure\nfrom wildlife"
		)
	#Saving the plot
	ggsave(paste0("./figures/",pathogen[j], "_wildlifePlot.pdf"),wildlifePlot, dpi = 150);
	#st_write(obj = wildCirculationShape, dsn=paste0("./figures/resultsWildlife_",pathogen[j],postfix,".shp"),delete_dsn=T)
	
	#Livestock plot
	liveStockPlot <- ggplot() +
		geom_sf(data = livestockCirculationShape, aes(fill = as.numeric(Area))) +
		scale_fill_gradientn(
			colours = palette2(100),
			values = scales::rescale(c(0, 5)),
			breaks = seq(0, 5, by = 1),
			limits = c(0, 5),
			name = "Category of\nhuman exposure\nfrom livestock"
		)
	#Saving the plot
	ggsave(paste0("./figures/",pathogen[j],"_liveStockPlot.pdf"),liveStockPlot, dpi = 150);
	#st_write(obj = livestockCirculationShape, dsn=paste0("./figures/resultslivestock_",pathogen[j],postfix,".shp"),delete_dsn=T)
	
	#Composite plot
	compositePlot <- ggplot() +
		geom_sf(data = compositeShape, aes(fill = as.numeric(Area))) +
		scale_fill_gradientn(
			colours = palette3(100),
			values = scales::rescale(c(0, 9)),
			breaks = seq(0, 9, by = 1),
			limits = c(0, 9),
			name = "Composite\ncategory of\nhuman exposure\n"
		)
	ggsave(paste0("./figures/",pathogen[j], "_compositePlot.pdf"),compositePlot,dpi = 150, units = "in");
	#st_write(obj = compositeShape, dsn=paste0("./figures/resultsComposite_",pathogen[j],postfix,".shp"),delete_dsn=T)
	
	# Create a composite plot
	temp <- plot_grid(compositePlot,wildlifePlot, liveStockPlot,nrow=3,ncol=1)
	ggsave(paste0("./figures/",pathogen[j], "_AllPlots.pdf"), temp,dpi = 150, units = "in");
	ggsave(paste0("./figures/",pathogen[j], "_AllPlots.png"), temp,dpi = 150, units = "in");
}
