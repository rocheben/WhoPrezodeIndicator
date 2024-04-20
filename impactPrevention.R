#####################################################################################
# Author: Benjamin Roche
# Date: 04/10/2024
# Description: Impact of prevention measures on summarized indicators
#   Two scenarios:
#         pathogen circulation decreased in wildlife and livestock for AIV
#         Decrease human/livestock contact for MERS-Cov
#####################################################################################

#Setting up the working directory
setwd("/Users/broche/Mon Drive/Docs/ResearchWorks/Moi - Indicateur Zoonoses/Analyse/WhoPrezodeIndicator")

#Cleaning up data
rm(list = ls())

#loading libraries
library(sf)
library(ggplot2)
library(readxl)
library(RColorBrewer)
library(dplyr)
library(cowplot)

#loading source code to calculate risk category
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
opinionData$wildlifePressureTrust <- as.numeric(opinionData$wildlifePressureTrust)
opinionData$wildlifeContact <- as.numeric(opinionData$wildlifeContact)
opinionData$wildlifePressureTrust <- as.numeric(opinionData$wildlifePressureTrust)
opinionData$livestockPressure <- as.numeric(opinionData$livestockPressure)
opinionData$wildlifePressureTrust <- as.numeric(opinionData$livestockPressureTrust)
opinionData$livestockContact <- as.numeric(opinionData$livestockContact)
opinionData$wildlifeContactTrust <- as.numeric(opinionData$livestockContactTrust)
opinionData$wildlifelivestockContact <- as.numeric(opinionData$wildlifelivestockContact)
opinionData$livestockWildlifeContactTrust <- as.numeric(opinionData$livestockWildlifeContactTrust)

#Pathogens studied
pathogen <- c("AIV", "WNV", "CCHF", "MERS")

#Odds Ratio calculated during the validation phase
or <- c(1.31, NA, NA, 1.72)

#Initializing colour palette
custom_palette <- colorRampPalette(c("white", brewer.pal(9, "Purples")[2:9]))

#Procedure for each pathogen (only AIV and MERS in this case)
for (j in c(1, 4)) {
	#Initializing vectors for data and risk categories
	wildlifePressure <- rep(NA, length(polygonsWithInformation))
	wildlifeContact <- rep(NA, length(polygonsWithInformation))
	livestockPressure <- rep(NA, length(polygonsWithInformation))
	livestockContact <- rep(NA, length(polygonsWithInformation))
	livestockWildlife <- rep(NA, length(polygonsWithInformation))
	livestockRisk <- rep(NA, length(polygonsWithInformation))
	wildlifeRisk <- rep(NA, length(polygonsWithInformation))
	compositeRisk <- rep(NA, length(polygonsWithInformation))
	
	#Subsetting the dataset for the current pathogen
	dataPat <- opinionData[opinionData$pathogen == pathogen[j] & !is.na(opinionData$pathogen),]
	
	#Calculating average values values for each polygon
	for (i in 1:length(polygonsWithInformation)) {
		#Selecting information for this polygon
		subsetData <-dataPat[dataPat$unitSelected == polygonsWithInformation[i] & !is.na(dataPat$unitSelected),]
		
		#Calculating weighted average for each variable
		wildlifePressure[i] <- weightedAverage(subsetData$wildlifePressure, subsetData$wildlifePressureTrust)
		wildlifeContact[i] <- weightedAverage(subsetData$wildlifeContact, subsetData$wildlifeContactTrust)
		livestockPressure[i] <- weightedAverage(subsetData$livestockPressure, subsetData$livestockPressureTrust)
		livestockContact[i] <- weightedAverage(subsetData$livestockContact, subsetData$livestockContactTrust)
	}
	
	#Calculating the risk category (wildlife, livestock and composite) for each polygon
	for (i in 1:length(polygonsWithInformation)) {
		temp <- risk_category(wildlifePressure[i],wildlifeContact[i],livestockPressure[i],livestockContact[i],livestockWildlife[i])
		compositeRisk[i] <- temp[3]
	}
	
	#Initializating the shapefile object 
	compositeShape <- borders
	compositeShape$Area <-rep(NA, length(compositeShape$iso3))
	
	#Setting the correct values for each polygon inside the shapefile object
	for (i in 1:length(polygonsWithInformation)) {
		compositeShape$Area[compositeShape$name == polygonsWithInformation[i]] <- compositeRisk[i]
	}
	
	#Creating the ggplot object without intervention 
	compositePlot <- ggplot() +
		geom_sf(data = compositeShape, aes(fill = as.numeric(Area))) +
		scale_fill_gradientn(
			colours = custom_palette(100),  # Utilisez la palette avec 100 couleurs
			values = scales::rescale(c(0, 9)),  # Échelle de 0 à 9
			breaks = seq(0, 9, by = 1),
			limits = c(0, 9),
			name = "Composite"
		)
	
	#Calculating the number of zoonotic infections according the relationship between composite risk and the odds-ratio
	nbOutbreaksExpectedWithout <- compositeRisk * or[j]
	
	#Implementing interventions
	for (i in 1:length(polygonsWithInformation)) {
		#Intervention for AIV
		if (j == 1 && !is.na(wildlifePressure[i])) {
			if (wildlifePressure[i] > 0) {
				wildlifePressure[i] = wildlifePressure[i] - 1
			}
			if (livestockPressure[i] > 0) {
				livestockPressure[i] = livestockPressure[i] - 1
			}
		}
		#Intervention for MERS
		if (j == 4 && !is.na(livestockContact[i])) {
			if (livestockContact[i] > 0) {
				livestockContact[i] = livestockContact[i] - 1
			}
		}
	}
	#Calculating the risk category (wildlife, livestock and composite) for each polygon with intervention
	for (i in 1:length(polygonsWithInformation)) {
		temp <-risk_category(wildlifePressure[i],wildlifeContact[i],livestockPressure[i],livestockContact[i],livestockWildlife[i])
		compositeRisk[i] <- temp[3]
	}
	
	#Calculating the number of zoonotic infections according the relationship between composite risk and the odds-ratio
	nbOutbreaksExpectedWith <- compositeRisk * or[j]
	
	#Initializating the shapefile object 
	compositeShape <-borders
	compositeShape$Area <-rep(NA, length(compositeShape$iso3))
	
	#Setting the correct values for each polygon inside the shapefile object
	for (i in 1:length(polygonsWithInformation)) {
		compositeShape$Area[compositeShape$name == polygonsWithInformation[i]] <-compositeRisk[i]
	}
	
	#Creating the ggplot object with intervention 
	compositePlotInterv <- ggplot() +
		geom_sf(data = compositeShape, aes(fill = as.numeric(Area))) +
		scale_fill_gradientn(
			colours = custom_palette(100),  # Utilisez la palette avec 100 couleurs
			values = scales::rescale(c(0, 9)),  # Échelle de 0 à 9
			breaks = seq(0, 9, by = 1),
			limits = c(0, 9),
			name = "Composite"
		)
	
	#Combining the ggplot objects with the legend
	legend<- get_legend(
		compositePlot + theme(legend.box.margin = margin(0, 0, 0, 0))
	)
	temp <- plot_grid(compositePlot+ theme(legend.position="none"),
				   compositePlotInterv+ theme(legend.position="none"),
				   nrow = 2,
				   ncol = 1)
	mapInterv <- plot_grid(legend,rel_widths = c(.4, 3),temp)
	#Saving the plot
	ggsave(
		paste0("./figures/", pathogen[j], "_Interv.pdf"),
		mapInterv,
		dpi = 150,
		units = "in"
	)

	#Computing the proportion of zoonotic infections averted
	propAverted <- (sum(nbOutbreaksExpectedWithout, na.rm = T) - sum(nbOutbreaksExpectedWith, na.rm = T))/sum(nbOutbreaksExpectedWithout, na.rm = T)
	print(paste0("Proportion of zoonotic infections averted for ",pathogen[j],":",propAverted*100))
}
