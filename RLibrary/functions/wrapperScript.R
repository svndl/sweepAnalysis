# Wrapper script
setwd("/Users/Nathan/Desktop/Research/sweepAnalysis/RLibrary/");
source("functions/getSweepDataFlex.R");
source("plots/plotSweepData.R");
source("plots/plotBasic.R");
source("plots/plotSingleHist.R");
source("plots/plotMultipleHists.R");
source("plots/plotMultiVarHist.R");
source("functions/getInventoryData.R");
source("functions/computeCCA.R")

# Acquire data
# data = getSweepDataFlex("/Users/Nathan/Desktop/Research/CVIdata/CVI3top34_20160224_1020/Exp_TEXT_PD1010_5_Cz/RLS_c001.txt");
cviData <- getInventoryData("/Users/Nathan/Desktop/Research/CVIdata/CVI_Inventory.xlsx");

# plotSweepData(data$selectedData, data$colsKeptNames, data$freqsAnalyzed, 2);

# questionHdrs <- cviData$questionHeaders;
# for (i in 1:length(questionHdrs))
# {
	# currQ = questionHdrs[i];
	# plotSingleHist(cviData, currQ);
# }

# corr <- plotBasic(cviData);
# plotMultipleHists(cviData, c(1,2,3,4), 2);
# plotMultiVarHist(cviData, c(1,2));

out <- computeCCA("/Users/Nathan/Desktop/Research/CVIdata/CVI_Inventory.xlsx", "/Users/Nathan/Desktop/Research/CVIdata/CVI3top34_20160224_1020/Exp_TEXT_PD1010_5_Cz/DFT_c001.txt");