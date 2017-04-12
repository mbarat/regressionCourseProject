setwd("~/Documents/DataAnalysisClass/ProjectProposal/kddcup99/database/")

# Reading the training dataset CSV file
kddTrainDataSet=read.csv("kddcup10p.csv",header=F, stringsAsFactors = FALSE)

# Assigning column names to the 
colnames <- read.table("kddcup.names", skip = 1, sep = ":")
names(kddTrainDataSet) <- colnames$V1

dimensionOfDataset <- dim(kddTrainDataSet)
# Assigning the name of the class
names(kddTrainDataSet)[dimensionOfDataset[2]] <- "label"
# Observe the data
names(kddTrainDataSet)

# Observe the distribution of labels. 
sum_label <- aggregate(rep(1, dimensionOfDataset[1]), 
                       by = list(kddTrainDataSet$label), 
                       FUN = sum)
names(sum_label) <- c("label", "count")
barplot(beside = TRUE, log10(sum_label$count), 
        names.arg = sum_label$label, ylim = c(0,6),
        xlab = "Label", ylab = "log(Count)",
        col = "Blue", main = "The distribution of labels")

# Select the features
# install.packages("caret", dependencies = c("Depends", "Suggests"))
# install.packages("ModelMetrics", dependencies = c("Depends", "Suggests"))

library(caret)
# Verify is the feature has an NA value
l <- kddTrainDataSet$label
sum(is.na(l))

# Clean up near zero variance features
nzvcol <- nearZeroVar(kddTrainDataSet)
kddTrainDataSet <- kddTrainDataSet[, -nzvcol]

#label into factor
reducedTrainingDataset <- kddTrainDataSet
reducedTrainingDataset$label <- factor(reducedTrainingDataset$label)

dimensionOfReducedDataset <- dim(reducedTrainingDataset)

####################################################################################
# Preparing TEST DATASET
# Load the data 
kddTestDataset <- read.csv("kddcup10ptest.csv", header=F, stringsAsFactors = FALSE)

# Assigning Column names
names(kddTestDataset) <- colnames$V1
names(kddTestDataset)[dim(kddTestDataset)[2]] <- "label"

# Extract the same features as training data
colnames_train <- names(reducedTrainingDataset)

kddTestDataset <- kddTestDataset[ , which(names(kddTestDataset) %in% colnames_train)]

testing <- kddTestDataset
testing$label <- as.factor(testing$label)

####################################################################################
# MLR model

mod1 = lm(label ~ srv_count + count, data = reducedTrainingDataset)
mod1
