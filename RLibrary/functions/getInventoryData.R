getInventoryData <- function(datafile)
{
	require(mice);
	require(xlsx);
	
	allData <- read.xlsx(datafile, sheetName = "CVI", stringsAsFactors = TRUE, header = TRUE);
	colHeaders = colnames(allData);
	questionHeaders = c(colHeaders[7:59]);

	# Convert scores to numbers and translate N/A to empty for now.
	for (i in 1:length(questionHeaders))
	{
		currQuestion = questionHeaders[i];
		allData[[currQuestion]][tolower(allData[[currQuestion]]) == "n/a"] <- "";
		allData[[currQuestion]] <- as.numeric(allData[[currQuestion]]);
	}
	qData <- allData[, 7:59];

	# missingSubj = c(1,4,5,6,19,22);	# Hard code
	# qDataFilter <- qData[!(rownames(qData) %in% missingSubj),];
	# qData <- qData[!(rownames(qData) %in% missingSubj),];

	# FOR MICE HERE =======================
	#
	# for (i in 1:length(questionHeaders))
	# {
		# currQuestion = questionHeaders[i];
		# qDataFilter[[currQuestion]] <- factor(qDataFilter[[currQuestion]]);
	# }

	# print(class(qDataFilter));
	# qDataFilter <- data.frame(qDataFilter);
	# print(levels(qDataFilter[,1]));
	# qDataFilter <- data.matrix(qDataFilter);
	# qDataFilter <- as.numeric(qDataFilter);
	# print(class(qDataFilter));

	# Predict missing data with mice and complete data set
	# qDataPredict <- mice(qDataFilter, m=1, maxit=1, method='polr', seed=500);
	# qData <- complete(qDataPredict, 1);
	#
	# =====================================

	# 7:59 is hard coded question indices in the data file
	averageScorePerSubject = c();
	sdScorePerSubject = c();
	maxScorePerSubject = c();
	minScorePerSubject = c();
	medianScorePerSubject = c();
	for (i in 1:dim(allData)[1])
	{
		currData <- suppressWarnings(as.numeric(allData[i, 7:59]));
		currMean <- mean(currData, na.rm = TRUE);
		currSD <- sd(currData, na.rm = TRUE);
		currMax <- suppressWarnings(max(currData, na.rm = TRUE));
		currMin <- suppressWarnings(min(currData, na.rm = TRUE));
		currMedian <- suppressWarnings(median(currData, na.rm = TRUE));
		averageScorePerSubject = c(averageScorePerSubject, currMean);
		sdScorePerSubject = c(sdScorePerSubject, currSD);
		maxScorePerSubject = c(maxScorePerSubject, currMax);
		minScorePerSubject = c(minScorePerSubject, currMin);
		medianScorePerSubject = c(medianScorePerSubject, currMedian);
	}
	allData$averageScores <- averageScorePerSubject;
	allData$stdScores <- sdScorePerSubject;
	allData$maxScores <- maxScorePerSubject;
	allData$minScores <- minScorePerSubject;
	allData$medianScores <- medianScorePerSubject;

	questionMeans = c();
	for (question in 1:length(questionHeaders))
	{
		currQuestion = questionHeaders[question];
		currData = allData[[currQuestion]];
		questionMeans = c(questionMeans, mean(currData, na.rm=TRUE));
	}
	
	# Fill in blanks with question means
	for (i in 1:dim(qData)[1])
	{
		for (j in 1:dim(qData)[2])
		{
			if (is.na(qData[i,j]))
			{
				qData[i,j] = questionMeans[j];
			}
		}
	}
	
	# questionMeans = t(data.frame(questionMeans));
	# colnames(questionMeans) <- questionHeaders;
	
	# qData <- rbind(qData, questionMeans);

	returnVals <- list("allData" = allData, "questionHeaders" = questionHeaders, "qData" = qData, "qMeans", questionMeans);
	return(returnVals);
}