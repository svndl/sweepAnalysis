#================================================
# Script that selects features using LOOCV/LDA
setwd("~/Desktop/Research/sweepAnalysis/RLibrary/")
rm(list=ls())
require("MASS");
require("caret");
require("hash");
source("functions/getSweepDataFlex.R");

#================================================
# Read and acquire data
cat("Reading data...\n");
# 13/14 = Sr/Si ; 19/20 = Signal/Phase
CVI_NT_Data <- getSweepDataFlex("/Users/Nathan/Desktop/Research/CVIdata/CVI_NT_20150407_1127/Exp_TEXT_PD1010_5_Cz/RLS_c001.txt", chanToKeep = c(), binToKeep = c(), colsToKeep <- c(3, 4, 9, 11, 13, 14));
CVI_Data <- getSweepDataFlex("/Users/Nathan/Desktop/Research/CVIdata/CVI3top34_20160224_1020/Exp_TEXT_PD1010_5_Cz/RLS_c001.txt", chanToKeep = c(), binToKeep = c(), colsToKeep <- c(3, 4, 9, 11, 13, 14));

CVI_NT_Data = CVI_NT_Data$selectedData;
CVI_Data = CVI_Data$selectedData;

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
# Using LOO for feature selection
totalDataSet.lda <- lda(totalDataSet, allClassLabels);
percentile = 0.5; # Top 25% of LD coefficients
featImpCount = hash()
for (i in 1:dim(totalDataSet)[1])
{
	trainSet <- totalDataSet[!(1:nrow(totalDataSet) %in% i),];
	trainSetLabels <- allClassLabels[!(1:length(allClassLabels) %in% i)];
	trainSet.lda <- lda(trainSet, trainSetLabels);
	featNames = rownames(trainSet.lda$scaling);
	stopifnot(featNames == colnames(totalDataSet));
	contribution = hash(); # From paper
	for (j in 1:dim(trainSet.lda$scaling)[2])
	{
		stopifnot(length(featNames) == length(trainSet.lda$scaling[,j]));
		for (k in 1:length(trainSet.lda$scaling[,j]))
		{
			feat = featNames[k];
			currentWeight = abs(trainSet.lda$scaling[k,j]);
			if (is.null(contribution[[feat]]))
			{
				contribution[[feat]] = currentWeight;
			}
			else
			{
				contribution[[feat]] = contribution[[feat]] + currentWeight;
			}
		}
	}
	# Process contribution for current data set in LOO loop, take top 10%
	range = max(values(contribution)) - min(values(contribution));
	minContr = max(values(contribution)) - (percentile * range);
	for (j in 1:length(featNames))
	{
		feat = featNames[j];
		if (contribution[[feat]] > minContr)
		{
			if (is.null(featImpCount[[feat]]))
			{
				featImpCount[[feat]] = 1;
			}
			else
			{
				featImpCount[[feat]] = featImpCount[[feat]] + 1;
			}
		}
	}
}

featNames = colnames(totalDataSet);
impFeatIdx = c();
for (i in 1:length(featNames))
{
	feat = featNames[i];
	currFeatCount = featImpCount[[feat]];
	if (!is.null(currFeatCount))
	{
		cat(feat, ":", currFeatCount, '\n');
		impFeatIdx = c(impFeatIdx, i);
	}
}

#================================================
# Get new dataset
dataSet.postFeatSelection = totalDataSet[,impFeatIdx];

#================================================
# PCA
dataSet.postFeatSelection.pca = prcomp(dataSet.postFeatSelection)
dev.new();
plot(dataSet.postFeatSelection.pca, type='l');

dataSet.postFeatSelection.pca.component = dataSet.postFeatSelection.pca$x[,1:4];

#================================================
# LDA + CV
dataSet.postFeatSelection.lda <- lda(dataSet.postFeatSelection.pca.component, allClassLabels, CV=TRUE)
# dataSet.postFeatSelection.lda <- lda(dataSet.postFeatSelection.pca.component, allClassLabels, CV=TRUE)
predictTab <- table(dataSet.postFeatSelection.lda$class, allClassLabels);
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


