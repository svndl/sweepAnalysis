#================================================
# Script that selects important features
require("MASS");
require("caret");
require("xgboost");
source("../functions/getSweepDataFlex.R");

#================================================
# Read and acquire data
cat("Reading data...\n");
# 13/14 = Sr/Si, 20=phase
CVI_NT_Data <- getSweepDataFlex("/Users/Nathan/Desktop/Research/CVIdata/CVI_NT_20150407_1127/Exp_TEXT_PD1010_5_Cz/RLS_c001.txt", chanToKeep = c(), binToKeep = c(), colsToKeep <- c(3, 4, 9, 11, 13, 14));
CVI_Data <- getSweepDataFlex("/Users/Nathan/Desktop/Research/CVIdata/CVI3top34_20160224_1020/Exp_TEXT_PD1010_5_Cz/RLS_c001.txt", chanToKeep = c(), binToKeep = c(), colsToKeep <- c(3, 4, 9, 11, 13, 14));

CVI_NT_Data = CVI_NT_Data$selectedData;
CVI_Data = CVI_Data$selectedData;

# channel = "Oz-Cz";
# CVI_NT_Data = CVI_NT_Data[CVI_NT_Data$iCh == channel,];
# CVI_Data = CVI_Data[CVI_Data$iCh == channel,];
# CVI_NT_Data = CVI_NT_Data[CVI_NT_Data$iBin == 10,];
# CVI_Data = CVI_Data[CVI_Data$iBin == 10,];

# freqs = c("3F1", "4F1");
# CVI_NT_Data = CVI_NT_Data[!(CVI_NT_Data$Harm %in% freqs),];
# CVI_Data = CVI_Data[!(CVI_Data$Harm %in% freqs),];

# Only want iTrial > 0
CVI_NT_Data = CVI_NT_Data[CVI_NT_Data$iTrial > 0,];
CVI_Data = CVI_Data[CVI_Data$iTrial > 0,];

#================================================
# Combine hierarchical names with other feature names
allChannels = factor(CVI_NT_Data$iCh);
channels = levels(allChannels);
allHarm = factor(CVI_NT_Data$Harm);
harms = levels(allHarm);
allBins = factor(CVI_NT_Data$iBin);
bins = levels(allBins);

cat("Combining features...\n");
doNotCombine = c("iSess", "iCond", "iTrial", "iCh", "iFr", "AF", "xF1", "xF2", "Harm", "FK_Cond", "iBin");
newFeatures = c();
hier_data_NT = c();	# Data with all the features including hierarchy
hier_data_CVI = c();
for (channel in channels)
{
	for (harm in harms)
	{
		for (bin in bins)
		{
			for (feat in colnames(CVI_NT_Data))
			{
				if (!(feat %in% doNotCombine))
				{
					newFeat = paste(channel, harm, bin, feat, sep="_");
					newFeatures = c(newFeatures, newFeat);
					
					currData_NT = CVI_NT_Data[CVI_NT_Data$iCh == channel,];
					currData_NT = currData_NT[currData_NT$Harm == harm,];
					currData_NT = currData_NT[currData_NT$iBin == bin,];
					currData_NT = currData_NT[[feat]];
					hier_data_NT = cbind(hier_data_NT, currData_NT);
					
					currData_CVI = CVI_Data[CVI_Data$iCh == channel,];
					currData_CVI = currData_CVI[currData_CVI$Harm == harm,];
					currData_CVI = currData_CVI[currData_CVI$iBin == bin,];
					currData_CVI = currData_CVI[[feat]];
					hier_data_CVI = cbind(hier_data_CVI, currData_CVI);
				}
			}
		}
	}
}
hier_data_NT <- data.frame(hier_data_NT);
hier_data_CVI <- data.frame(hier_data_CVI);
colnames(hier_data_NT) <- newFeatures;
colnames(hier_data_CVI) <- newFeatures;

#================================================
# Checkers for the data
stopifnot(dim(hier_data_NT)[2] == dim(hier_data_CVI)[2]);

# Make sure all the column names are in the same order
for (i in 1:length(colnames(hier_data_NT)))
{
	stopifnot(colnames(hier_data_NT)[i] == colnames(hier_data_CVI)[i]);
}

#================================================
# Append both sets of data together and create class labels
totalDataSet <- rbind(hier_data_NT, hier_data_CVI);
NT_class <- rep_len("NT", dim(hier_data_NT)[1]);
CVI_class <- rep_len("CVI", dim(hier_data_CVI)[1]);
allClassLabels <- c(NT_class, CVI_class);
allClassLabels.binary <- ifelse(allClassLabels=="CVI", 1, 0);
stopifnot(length(allClassLabels) == dim(totalDataSet)[1]);

#================================================
# Feature engineering
# Either removing or replacing features that are correlated well
cat("Engineering features...\n");
allData.corr <- cor(totalDataSet, use="na.or.complete", method="pearson")
colHdrs <- colnames(totalDataSet);

# Set these:
corrThresh = 0.7;
removeCorrelatedFeats = 1; # If we want to remove correlated features instead of replacing them

numPairs = 0;
totalPairs = 0;
numNewFeats = 0;
numOldFeats = 0;
newFeatures = c();
totalDataSet.featEng = c(); # Replacing correlated features with their sum
totalDataSet.rmDependentFeats = c(); # Correlated features removed

for (i in 1:(dim(allData.corr)[1]-1))
{
	toCombine = c(colHdrs[i]);
	correlated = 0; # Checks if current feature is correlated with anything else at all
	for (j in (i+1):dim(allData.corr)[2])
	{
		if (abs(allData.corr[i,j]) > corrThresh)
		{
			#cat(colHdrs[i], "and", colHdrs[j], ':', allData.corr[i,j], '\n');
			toCombine = c(toCombine, colHdrs[j]);
			numPairs = numPairs + 1;
			correlated = 1;
		}
		totalPairs = totalPairs + 1;
	}
	
	# If we didn't find any features that correlated well with the current feature, continue.
	if (correlated == 0)
	{

		if (removeCorrelatedFeats == 1)
		{
			totalDataSet.rmDependentFeats = cbind(totalDataSet.rmDependentFeats, totalDataSet[[colHdrs[i]]]);
			numCols = ncol(totalDataSet.rmDependentFeats);
			colnames(totalDataSet.rmDependentFeats)[-(1:(numCols-1))] = colHdrs[i];
			numOldFeats = numOldFeats + 1;
			next;
		}
		else
		{
			stopifnot(removeCorrelatedFeats == 0);
			totalDataSet.featEng = cbind(totalDataSet.featEng, totalDataSet[[colHdrs[i]]]);
			numCols = ncol(totalDataSet.featEng);
			colnames(totalDataSet.featEng)[-(1:(numCols-1))] = colHdrs[i];
			numOldFeats = numOldFeats + 1;			
		}
	}
	else
	{
		if (removeCorrelatedFeats == 1) next;
	}
	
	newFeatureVals = totalDataSet[[toCombine[1]]];
	newFeatureName = toCombine[1];
	stopifnot(length(toCombine) >= 2);
	for (k in 2:length(toCombine))
	{
		newFeatureName = paste(newFeatureName, '__', toCombine[k], sep='');
		currFeatVals = totalDataSet[[toCombine[k]]];
		#cat(length(currFeatVals), ',', length(newFeatureVals), '\n');
		stopifnot(length(currFeatVals) == length(newFeatureVals));
		newFeatureVals = newFeatureVals + currFeatVals
	}
	totalDataSet.featEng = cbind(totalDataSet.featEng, newFeatureVals);
	numCols = ncol(totalDataSet.featEng);
	if (numCols == 1)
	{
		colnames(totalDataSet.featEng)[1] = newFeatureName;
	}
	else
	{
		colnames(totalDataSet.featEng)[-(1:(numCols-1))] = newFeatureName;
	}
	# cat(numNewFeats, ':', newFeatureName, '\n');
	numNewFeats = numNewFeats + 1;
}
cat("Number of pairs with correlation >", corrThresh, ":", numPairs, "/", totalPairs, '\n');
cat("Number of new features:", numNewFeats, '\n');
cat("Number of old features:", numOldFeats, '\n');

#================================================
# PCA
if (removeCorrelatedFeats == 1)
	totalDataSet.rmDependentFeats.pca <- prcomp(totalDataSet.rmDependentFeats, scale = TRUE);

dev.new()
plot(totalDataSet.rmDependentFeats.pca, type='l');

#================================================


#================================================
# Split data into training and test set
cat("Partitioning dataset...\n");
trainIndex <- createDataPartition(allClassLabels, p=0.75, list=FALSE, times=1);
trainSet <- totalDataSet[trainIndex,];
testSet <- totalDataSet[!(1:nrow(totalDataSet) %in% trainIndex),];
trainSetLabels <- allClassLabels[trainIndex];
testSetLabels <- allClassLabels[!(1:nrow(totalDataSet) %in% trainIndex)];

#================================================
# train.lda <- lda(totalDataSet.rmDependentFeats, allClassLabels, CV=TRUE)
train.lda <- lda(totalDataSet, allClassLabels, CV=TRUE)
predictTab <- table(train.lda$class, allClassLabels);
# predictTab <- table(test.predict$class, testSetLabels);
for (i in 1:dim(predictTab)[1])
{
	currGroup = colnames(predictTab)[i];
	total <- sum(predictTab[i,]);
	correct <- predictTab[i,i];
	percentCorrect = correct/total;
	cat(paste(currGroup, ":", percentCorrect));
	cat("\n");
}

