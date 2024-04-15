weightedAverage <- function(pData,pDataTrust){
	#temp <- subsetData$wildlifePressure[!is.na(subsetData$wildlifePressure)]*subsetData$wildlifePressureTrust[!is.na(subsetData$wildlifePressure)]/sum(subsetData$wildlifePressureTrust[!is.na(subsetData$wildlifePressure)]);
	#if(length(temp)>0){wildlifePressure[i] <- round(sum(temp, na.rm=T))};
	
	#Formula used: data*dataTrust/sum(dataTrust)
	lReturn<-NA;
	temp <- pData[!is.na(pData)]*pDataTrust[!is.na(pData)]/sum(pDataTrust[!is.na(pData)]);
	if(length(temp)>0){lReturn <- round(sum(temp, na.rm=T))};
	return(lReturn)
	}