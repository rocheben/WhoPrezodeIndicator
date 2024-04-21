#####################################################################################
# Author: Benjamin Roche
# Date: 04/10/2024
# Description: Validation of the indicators regarding the number of zoonotic infections
#####################################################################################

#Cleaning up data
rm(list=ls())

#Setting up the working directoru
setwd("/Users/broche/Mon Drive/Docs/ResearchWorks/Moi - Indicateur Zoonoses/Analyse/WhoPrezodeIndicator") 

#loading libraries
library(sf)
library(ggplot2)
library(readxl)
library(RColorBrewer)
library(dplyr)
library(cowplot)

#Loading source code for risk category
source("sources/risk_category.R", encoding = "UTF-8")
source("sources/weightedAverage.R")

#Loading shapefile
borders <- st_read("./GIS/world-administrative-boundaries.shp")

#Loading expert opinion data
opinionData <- read_xlsx("./data/dataExpert.xlsx")

#Loading validation data
validationDataMers <- read.csv("./data/mers.csv")
validationDataMers=validationDataMers[validationDataMers$transmission_route=="zoonotic",];
validationDataAiv <- read_xlsx("./data/aiv.xlsx")

#Getting the geographical unit with expert information
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

#Conversion of country names between different datasets
validationDataAiv$country[validationDataAiv$country=="Laos"]="Lao People\'s Democratic Republic";
validationDataAiv$country[validationDataAiv$country=="United Kingdom"]="U.K. of Great Britain and Northern Ireland";
validationDataAiv$country[validationDataAiv$country=="Netherland"]="Netherlands";
validationDataAiv$country[validationDataAiv$country=="Hong Kong Special Administrative Region of China"]="Hong Kong";
validationDataMers$country[validationDataMers$country=="ARE"]="United Arab Emirates";
validationDataMers$country[validationDataMers$country=="SAU"]="Saudi Arabia";
validationDataMers$country[validationDataMers$country=="KEN"]="Kenya";
validationDataMers$country[validationDataMers$country=="OMN"]="Oman";
validationDataMers$country[validationDataMers$country=="EGY"]="Egypt";
validationDataMers$country[validationDataMers$country=="SDN"]="Sudan";
validationDataMers$country[validationDataMers$country=="ZAF"]="South Africa";
validationDataMers$country[validationDataMers$country=="Italy"]="Italy";
validationDataMers$country[validationDataMers$country=="MLI"]="Mali";
validationDataMers$country[validationDataMers$country=="KOR"]="Republic of Korea";
validationDataMers$country[validationDataMers$country=="UGA"]="Uganda";
validationDataMers$country[validationDataMers$country=="NGA"]="Nigeria";
validationDataMers$country[validationDataMers$country=="TUN"]="Tunisia";
validationDataMers$country[validationDataMers$country=="ETH"]="Ethiopia";
validationDataMers$country[validationDataMers$country=="JOR"]="Jordan";
validationDataMers$country[validationDataMers$country=="QAT"]="Qatar";
validationDataMers$country[validationDataMers$country=="MYS"]="Malaysia";
validationDataMers$country[validationDataMers$country=="SOM"]="Somalia";
validationDataMers$country[validationDataMers$country=="CHN"]="China";
validationDataMers$country[validationDataMers$country=="ESP"]="Spain";
validationDataMers$country[validationDataMers$country=="BFA"]="Bukina Faso";
validationDataMers$country[validationDataMers$country=="MAR"]="Morocco";
validationDataMers$country[validationDataMers$country=="PAK"]="Pakistan";
validationDataMers$country[validationDataMers$country=="BHR"]="Bahrain";
validationDataMers$country[validationDataMers$country=="KWT"]="Kuwait";
validationDataMers$country[validationDataMers$country=="BGD"]="Bangladesh";
validationDataMers$country[validationDataMers$country=="IRN"]="Iran (Islamic Republic of)";
validationDataMers$country[validationDataMers$country=="ISR"]="Israel";
validationDataMers$country[validationDataMers$country=="IRQ"]="Iraq";

#For each pathogen with validation data
pathogen <- c("AIV","WNV","CCHF","MERS")
for(j in c(1,4)){
	#Getting the correct validation dataset according to the pathogen
	if(j==1){
		validationData=validationDataAiv[validationDataAiv$Year>2010,]
	}
	if(j==4){
		validationData=validationDataMers[validationDataMers$year>2010,]
	}
	
	#Initializing variable and risk vectors
	wildlifePressure <- rep(NA,length(polygonsWithInformation));
	wildlifeContact <- rep(NA,length(polygonsWithInformation));
	livestockPressure <- rep(NA,length(polygonsWithInformation));
	livestockContact <- rep(NA,length(polygonsWithInformation));
	livestockWildlife <- rep(NA,length(polygonsWithInformation));
	livestockRisk <- rep(NA,length(polygonsWithInformation));
	wildlifeRisk<- rep(NA,length(polygonsWithInformation));
	compositeRisk<- rep(NA,length(polygonsWithInformation));
	occurenceNb <- rep(NA,length(polygonsWithInformation));
	
	#Subsetting the dataset for the curent pathogen
	dataPat <- opinionData[opinionData$pathogen==pathogen[j] & !is.na(opinionData$pathogen),]
	#Calculating averaged estimates for each polygon
	for(i in 1:length(polygonsWithInformation)){
		#Subsetting the data for this polygon
		subsetData <- dataPat[dataPat$unitSelected==polygonsWithInformation[i] & !is.na(dataPat$unitSelected),]
		
		#Calculating weighted average
		wildlifePressure[i] <- weightedAverage(subsetData$wildlifePressure,subsetData$wildlifePressureTrust)
		wildlifeContact[i] <- weightedAverage(subsetData$wildlifeContact,subsetData$wildlifeContactTrust)
		livestockPressure[i] <- weightedAverage(subsetData$livestockPressure,subsetData$livestockPressureTrust)
		livestockContact[i] <- weightedAverage(subsetData$livestockContact,subsetData$livestockContactTrust)
	}
	
	#Calculating the aggregated risk for each polygon with an estimation
	for(i in 1:length(polygonsWithInformation)){
		temp<-risk_category(wildlifePressure[i],wildlifeContact[i], livestockPressure[i],livestockContact[i],livestockWildlife[i]);
		wildlifeRisk[i]<-temp[1];
		livestockRisk[i]<-temp[2]
		compositeRisk[i]<-temp[3]
		#Getting the number of occurence for this pathogen in this country
		subsetValid <- validationData[validationData$country==polygonsWithInformation[i],]
		occurenceNb[i] <- length(subsetValid$country[!is.na(subsetValid$country)])
	}
	
	#Plotting Histograms
	data1 <- compositeRisk;
	data2 <- compositeRisk[occurenceNb==0];
	data3 <- compositeRisk[occurenceNb>0];
	
	#Mean comparison between groups
	print(paste0("T-test for mean comparison:t=",as.numeric(t.test(data2,data3)[1])," (p-value:",t.test(data2,data3)[3],")"))
	#Correlation test between composite risk and number of zoonotic infections
	print(cor.test(occurenceNb,compositeRisk))
	#Poison GLM and printing the odds-ratio
	model <- glm(occurenceNb ~ compositeRisk, family = poisson)
	print(paste0("Odds-Ratio for ",pathogen[j],":",exp(coef(model))[2]))
	
	# Converting vectors to have the same length
	max_length <- max(length(data1), length(data2), length(data3))
	data1 <- c(data1, rep(NA, max_length - length(data1)))
	data2 <- c(data2, rep(NA, max_length - length(data2)))
	data3 <- c(data3, rep(NA, max_length - length(data3)))
	df <- data.frame(value = c(data1, data2, data3),group = factor(rep(c("All", "No Occurence", "Occurence"), each = max_length)))
	df <- df[complete.cases(df), ]
	
	#Plotting the histograms
	tempPlot<-ggplot(df, aes(x = value, color = group, fill = group)) +
		geom_density(alpha = 0.5) +
		theme_minimal() +
		labs(
			x = "Composite category of human exposure",
			y = "Density",
			title = paste0(pathogen[j]," distribution")
		) +
		scale_color_manual(values = c("All" = "blue", "No Occurence" = "green", "Occurence" = "red")) +
		scale_fill_manual(values = c("All" = "blue", "No Occurence" = "green", "Occurence" = "red"))
	ggsave(paste0("./figures/validation_",pathogen[j],".pdf"),tempPlot)
	ggsave(paste0("./figures/validation_",pathogen[j],".png"),tempPlot)
	
	
}
