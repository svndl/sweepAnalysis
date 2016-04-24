#================================================
# Script that selects features using LOOCV/LDA
require("MASS");
require("caret");
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
# Using LOO-CV
for (i in 1:dim(totalDataSet)[1])
{
	
}





