computeCCA <- function(inventoryDataFile, sweepDataFile)
{
	require("CCA");
	require("PMA");
	source("functions/getSweepDataFlex.R");
	source("functions/getInventoryData.R");
	
	print("Getting sweep data...");
	sweepData <- getSweepDataFlex(sweepDataFile, chanToKeep = c(), binToKeep = c(), colsToKeep = c());
	print("Getting inventory data...");
	inventoryData <- getInventoryData(inventoryDataFile);
	
	# Hard code subject mappings because of weird naming
	# Will go by the inventory subject IDs since it's what it's supposed to be
	sweepDataSubj = c(1,4,5,6,7,8,9,10,11,12,13,14,15,17,18,20,21,22,23,24,25,26,27,28,29,30,31);
	inventoryDataSubj = c(3,7,8,9,10,11,12,13,14,15,16,17,18,20,21,23,24,25,26,27,28,29,30,31,32,33,34);
	
	# Get sweep and inventory data
	cviSweepData <- sweepData$selectedData[sweepData$selectedData$iTrial %in% sweepDataSubj,];
	cviInventoryData <- inventoryData$qData[rownames(inventoryData$qData) %in% inventoryDataSubj,];
	cviInventoryData <- data.frame(cviInventoryData);
	
	# Need to create new features for sweep data
	allChannels = factor(cviSweepData$iCh);
	channels = levels(allChannels);
	allHarm = factor(cviSweepData$Harm);
	harms = levels(allHarm);
	allBins = factor(cviSweepData$iBin);
	bins = levels(allBins);
	
	# Combine hierarchical names with other feature names
	# For CVI data: 5 channels, 4 freqs, 11 bins (including 0), 10 features from getSweepData
	print("Getting new data...");
	doNotCombine = c("iSess", "iCond", "iTrial", "iCh", "iFr", "AF", "xF1", "xF2", "Harm", "FK_Cond", "iBin");
	newFeatures = c();
	newData = c();	# Data with all the features including hierarchy
	for (channel in channels)
	{
		for (harm in harms)
		{
			for (bin in bins)
			{
				for (feat in colnames(cviSweepData))
				{
					if (!(feat %in% doNotCombine))
					{
						newFeat = paste(channel, harm, bin, feat, sep="_");
						newFeatures = c(newFeatures, newFeat);
						currData = cviSweepData[cviSweepData$iCh == channel,];
						currData = currData[currData$Harm == harm,];
						currData = currData[currData$iBin == bin,];
						currData = currData[[feat]];
						stopifnot(dim(currData)[1] == length(cviInventoryData));
						newData = cbind(newData, currData);
					}
				}
			}
		}
	}
	newData = data.frame(newData);
	colnames(newData) <- newFeatures;
	rownames(newData) <- inventoryDataSubj;
	
	# Do PCA on inventory data
	invenDat.pca <- prcomp(cviInventoryData, center=TRUE, scale=TRUE);
	plot(invenDat.pca, type="l");
	png()
	
	# Do CCA
	print("Do CCA...");
	# out <- CCA(x=cviInventoryData, z=newData, typex="standard", typez="standard", K=27);
	out <- CCA(x=newData, z=cviInventoryData, typex="standard", typez="standard", K=27, penaltyx=0.1, penaltyz=0.1);
	
	# Print features with loadings for specific canonical variates to file
	# For each of the canonical variates, get the loaded variables
	inventoryFeatures = colnames(cviInventoryData);
	sweepFeatures = colnames(newData);
	for (i in 1:dim(out$u)[2])
	{
		# Get variables which load on the respective canonical variates
		inventoryVarsIdx = which(out$v[,i] != 0);
		sweepVarsIdx = which(out$u[,i] != 0);
		currInventoryFeat = inventoryFeatures[inventoryVarsIdx];
		currSweepFeat = sweepFeatures[sweepVarsIdx];
		if (i == 1)
		{
			title = paste("Canonical Variate: ", i, "\t(Correlation: ", out$cors[i], ")", "\n", sep="");
			write(title, file="../../output.txt", ncolumns=1, append=FALSE, sep=" ");
		}
		else
		{
			title = paste("\nCanonical Variate: ", i, "\t(Correlation: ", out$cors[i], ")", "\n", sep="");
			write(title, file="../../output.txt", ncolumns=1, append=TRUE, sep=" ");
		}

		write(currInventoryFeat, file="../../output.txt", ncolumns=length(currInventoryFeat), append=TRUE, sep="\t");
		write(currSweepFeat, file="../../output.txt", ncolumns=length(currInventoryFeat), append=TRUE, sep="\t");
	}
	
	returnVals <- list("sweepData" = newData, "inventoryData" = cviInventoryData, "ccaOut" = out, "pcaOut" = invenDat.pca);
	return(returnVals);
}
