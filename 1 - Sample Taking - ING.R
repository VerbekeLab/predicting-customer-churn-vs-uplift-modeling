## Original code by Floris Devriendt, cleaned and modified by Jordi Vandenhouwe & Maarten de Winter
## Loading packages ##
library(splitstackshape)
source("0 - runSettings.R") # use this file to setup run

## Loading CSV and Transform to RDS (first run only)
# df <- read.csv(paste("RDS/",datasetName,".csv",sep=""), header=TRUE,sep=";",dec=".") #load as csv
# saveRDS(df, paste("RDS/",datasetName,".RDS",sep=""))   #save as RDS

## Loading RDS (subsequent runs)
df <- readRDS(paste("RDS/",datasetName,".RDS",sep=""))

# Setting seed
set.seed(123)

# Check if saveLocation folder exists and if not, create it
if (!file.exists(saveLocation)){
  dir.create(file.path(saveLocation))
}

# Stratified split to make sure both sets have same distribution treated/untreated and churn/non-churn observations.
splitted.stratified <- stratified(df, c(treatmentVariable, targetVariable), size = 0.66, bothSets = TRUE, keep.rownames = TRUE)

# Setting up train and test set.
trainSampleIndices <- as.integer(as.data.frame(splitted.stratified[[1]])$rn)
testSampleIndices <- as.integer(as.data.frame(splitted.stratified[[2]])$rn)
rm(splitted.stratified)

train_sample <- df[trainSampleIndices,]
test_sample <- df[testSampleIndices,]
rm(df)
rm(trainSampleIndices)
rm(testSampleIndices)

# Testing probabilities to see whether training and testing set are similar.
prop.table(table(train_sample[,targetVariable],train_sample[,treatmentVariable]))
prop.table(table(test_sample[,targetVariable],test_sample[,treatmentVariable]))

################################
# Creating subsets             #
################################

## Split & Save Train Sample
splitted.train         <- split(train_sample, f = train_sample[,treatmentVariable])
train_sample_control   <- splitted.train[[1]]
train_sample_treatment <- splitted.train[[2]]
rm(splitted.train)
saveRDS(train_sample, paste(saveLocation,"/train_sample.RDS", sep = ""))
saveRDS(train_sample_control, paste(saveLocation,"/train_sample_control.RDS", sep = ""))
saveRDS(train_sample_treatment, paste(saveLocation,"/train_sample_treatment.RDS", sep = ""))

# Testing probabilities to see whether treatment and control set are similar to the above ratio's.
prop.table(table(train_sample_control[,targetVariable]))
prop.table(table(train_sample_treatment[,targetVariable]))

## Split & Save Test Sample
splitted.test       <- split(test_sample, f = test_sample[,treatmentVariable])
test_sample_control   <- splitted.test[[1]]
test_sample_treatment <- splitted.test[[2]]
rm(splitted.test)
saveRDS(test_sample, paste(saveLocation,"/test_sample.RDS", sep = ""))
saveRDS(test_sample_control, paste(saveLocation,"/test_sample_control.RDS", sep = ""))
saveRDS(test_sample_treatment, paste(saveLocation,"/test_sample_treatment.RDS", sep = ""))

# Testing probabilities to see whether treatment and control set are similar to the above ratio's.
churn_rate_control   <- prop.table(table(test_sample_control[,targetVariable]))[2]
churn_rate_treatment <- prop.table(table(test_sample_treatment[,targetVariable]))[2]

# Calculating retention rate (used for MP-measure)
retention_rate       <- (churn_rate_control - churn_rate_treatment) / churn_rate_control
