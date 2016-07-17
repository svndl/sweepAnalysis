getSweepDataFlex <- function(datafile, chanToKeep = c(), binToKeep = c(), colsToKeep = c())
{
	# In "colsToKeep", you must include the following indices for the data flow pipeline to
	# work: 3,4,9,11 (just include the first 11 indices, and everything will be ok)
	
	allData  <- read.table(datafile, stringsAsFactors=TRUE, header=TRUE);
	colNames <- colnames(allData);
	
	# 2 iCond, 3 iTrial, 4 iCh, 5 iFr, 7 xF1, 9 Harm, 10 FK_Cond, 11 Bin, 12 SweepVal, 
	# 13 Sr, 14 Si, 19 signal, 20 phase, 21 Noise, 22 StdErr, 24 SNR, 28 Thresh, 30 Slope
	if (length(colsToKeep) == 0)
	{
		# colsToKeep <- c(2, 3, 4, 5, 7, 9, 10, 11, 12, 13, 14, 19, 20);
		# colsToKeep <- c(3, 4, 9, 11, 13, 14, 19, 20);
		colsToKeep <- c(3, 4, 9, 11, 13, 14);
		#colsToKeep <- c(2, 3, 4, 5, 7, 12, 19, 20, 21);
	}
	colsToKeepNames <- c();

	# maxFreq <- allData$iFr[which.max(allData$iFr)];
	freqsAnalyzed <- c();
	freqsAnalyzed <- levels(allData$Harm);
	
	allData <- allData[, colsToKeep];

	for (i in 1:length(colsToKeep))
	{
		colsToKeepNames[i] = colNames[colsToKeep[i]];
	}
	
	if (length(chanToKeep) != 0)
	{
		for (i in 1:length(chanToKeep))
		{
			currChan = chanToKeep[i];
			allData = allData[allData$iCh == currChan];
		}
	}
	if (length(binToKeep) != 0)
	{
		for (i in 1:length(binToKeep))
		{
			currBin = binToKeep[i];
			allData = allData[allData$iBin == currBin,];
		}
	}

	colsToKeepNames = colnames(allData);

	returnVals <- list("selectedData" = allData, "colsKeptNames" = colsToKeepNames, "freqsAnalyzed" = freqsAnalyzed);
	return(returnVals);
}