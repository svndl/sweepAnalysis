plotBasic <- function(data)
{
	# Plots correlation/covariance and histograms of scores per question
	
	require(ggplot2);
	require(reshape2);
	require(polycor);
	library(ggplot2);
	library(reshape2);
	library(polycor);
	
	# Get question only data
	qData <- data$qData;
	
	# Get covariance/correlation matrix of questionnaire data
	covMat <- cov(qData, use="na.or.complete");
	corrMat <- cor(qData, use="na.or.complete", method="pearson")
	
	polyCorMat <- matrix(,dim(qData)[2], dim(qData)[2]);
	# polyCorMat <- hetcor(qData, ML = TRUE, use = "pairwise.complete.obs");
	for (i in 1:dim(qData)[2])
	{
		for (j in i:dim(qData)[2])
		{
			x <- qData[,i];
			y <- qData[,j];
			x <- cut(x, c(0,1,2,3,4,5));
			y <- cut(y, c(0,1,2,3,4,5));
			curr <- polychor(x,y);
			polyCorMat[i,j] = curr;
			polyCorMat[j,i] = curr;
		}
	}

	# polyCorrMat <- polyCorMat$correlations;
	rownames(polyCorMat) <- colnames(qData);
	colnames(polyCorMat) <- colnames(qData);
	# print(polyCorMat)

	# Plot correlation matrix
	absCorrMat <- abs(corrMat);
	melted_corrMat <- melt(absCorrMat);
	print(ggplot(melted_corrMat, aes(x=Var1,y=Var2,fill=value)) +
	geom_tile(color="white") +
	scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = (max(absCorrMat) + min(absCorrMat)) / 2, limit = c(0,1), space = "Lab", 
	name="Pearson\nCorrelation") +
	theme_minimal() + 
	theme(
	axis.title.x = element_blank(),
	axis.title.y = element_blank(),
	axis.text.x = element_text(angle = 45, vjust = 1, 
	size = 12, hjust = 1)) +
	coord_fixed() +
	ggtitle("Pearson Correlation of Questions"));
	
	ggsave("corrPlot.png", path="/Users/Nathan/Desktop/Research/CVIdata/InventoryPlots/");
	
	dev.new();
	
	# Plot polychoric correlation matrix
	abspolyCorrMat <- abs(polyCorMat);
	melted_polycorrMat <- melt(abspolyCorrMat);
	print(ggplot(melted_polycorrMat, aes(x=Var1,y=Var2,fill=value)) +
	geom_tile(color="white") +
	scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = (max(abspolyCorrMat) + min(abspolyCorrMat)) / 2, limit = c(0,1), space = "Lab", 
	name="Polychoric\nCorrelation") +
	theme_minimal() + 
	theme(
	axis.title.x = element_blank(),
	axis.title.y = element_blank(),
	axis.text.x = element_text(angle = 45, vjust = 1, 
	size = 12, hjust = 1)) +
	coord_fixed() +
	ggtitle("Polychoric Correlation of Questions"));
	
	ggsave("polyCorrPlot.png", path="/Users/Nathan/Desktop/Research/CVIdata/InventoryPlots/");
	
	returnVals <- list("polycor" = polyCorMat, "pearson" = corrMat)
	return(returnVals);
}


