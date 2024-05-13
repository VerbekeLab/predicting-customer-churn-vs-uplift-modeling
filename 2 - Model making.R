## Original code by Floris Devriendt, cleaned and modified by Jordi Vandenhouwe & Maarten de Winter
## Loading packages ##
library(caret)
library(randomForest)
library(uplift)

source("Rscripts/CustomQini.R") # Available at: https://data-lab.be/downloads.php
source("Rscripts/Models.R") # Available at: https://data-lab.be/downloads.php
source("0 - runSettings.R") # use this file to setup run

## Setting model variables
CCPtrees <- 500 # Amount of trees generated for CCP RF model
CCUtrees <- 10 # Amount of trees generated for CCU RF model
set.seed(12345)

## Loading Test & Train samples

test_sample <- readRDS(paste(saveLocation,"/test_sample.RDS", sep = ""))
test_sample_control <- readRDS(paste(saveLocation,"/test_sample_control.RDS", sep = ""))
test_sample_treatment <- readRDS(paste(saveLocation,"/test_sample_treatment.RDS", sep = ""))

train_sample <- readRDS(paste(saveLocation,"/train_sample.RDS", sep = ""))
train_sample_control <- readRDS(paste(saveLocation,"/train_sample_control.RDS", sep = ""))
train_sample_treatment <- readRDS(paste(saveLocation,"/train_sample_treatment.RDS", sep = ""))

######################################################################
# Training CCU model: Train on train-full, test on test-full         #
######################################################################

# All variables except for the target variable and treatment variable are predictors.
predictors_CCU <- names(train_sample)[(names(train_sample) != targetVariable) & (names(train_sample) != treatmentVariable)]

fml_Uplift <- as.formula(paste(targetVariable," ~ trt(",treatmentVariable,") + ", paste(predictors_CCU, collapse= "+")))

### Train the model (using dummy approach)
CCU_model_dta <- dta(x = train_sample[,predictors_CCU],
                             ct = train_sample[,treatmentVariable],
                             y = train_sample[,targetVariable])
saveRDS(CCU_model_dta, paste(saveLocation,"/CCU_model_dta_TrainF_TestF.RDS", sep = ""))
# CCU_model_dta <- readRDS(paste(saveLocation,"/CCU_model_dta_TrainF_TestF.RDS", sep = ""))

### Make predictions for dummy approach model
CCU_pred_dta <- predict.dta(CCU_model_dta, newdata = test_sample, y.name = targetVariable, ct.name = treatmentVariable)
saveRDS(CCU_pred_dta, paste(saveLocation,"/CCU_pred_dta_TrainF_TestF.RDS", sep = ""))

### Train the modeling (using random forests)
### originally ntree = 1000 but takes 1.5 minutes per tree so for now change to ntree = 10
CCU_model_rf <- uplift::upliftRF(fml_Uplift, train_sample, split_method="ED", ntree = CCUtrees, verbose="TRUE")
saveRDS(CCU_model_rf, paste(saveLocation,"/CCU_model_rf_TrainF_TestF.RDS", sep = ""))
# CCU_model_rf <- readRDS(paste(saveLocation,"/CCU_model_rf_TrainF_TestF.RDS", sep = ""))

### Make prediction for random forest model
CCU_pred_rf <- predict(CCU_model_rf, test_sample)
saveRDS(CCU_pred_rf, paste(saveLocation,"/CCU_pred_rf_TrainF_TestF.RDS", sep = ""))

rm(CCU_model_dta)
rm(CCU_model_rf)
rm(CCU_pred_dta)
rm(CCU_pred_rf)

######################################################################
# Training CCP Model: Train on train-control, test on test-full      #
######################################################################

# All variables except for the target variable are predictors.
predictors_CCP <- names(train_sample)[(names(train_sample) != targetVariable)]

train_sample_control[,targetVariable] <- as.factor(train_sample_control[,targetVariable])   # Factoring the target-variable
train_sample[,targetVariable] <- as.factor(train_sample[,targetVariable])   # Factoring the target-variable
test_sample[,targetVariable] <- as.factor(test_sample[,targetVariable])   # Factoring the target-variable

fml_Classic <- as.formula(paste(targetVariable," ~ ", paste(predictors_CCP, collapse= "+"))) # making Y = a + b + c

# Caret-measure
fitControl <- trainControl(method = "none")

# Running the model (GLM)
CCP_model_glm <- glm(paste(targetVariable,"~ ."), family = binomial, data = as.data.frame(train_sample_control))
saveRDS(CCP_model_glm, paste(saveLocation,"/CCP_model_glm_TrainC_TestF.RDS", sep = ""))
# CCP_model_glm <- readRDS(paste(saveLocation,"/CCP_model_glm_TrainC_TestF.RDS", sep = ""))

# Making predictions
CCP_pred_glm <- predict(CCP_model_glm, test_sample, type="response")
saveRDS(CCP_pred_glm, paste(saveLocation,"/CCP_pred_glm_TrainC_TestF.RDS", sep = ""))

# Running a second model (RF)
CCP_model_rf <- train(fml_Classic, data = train_sample_control, method = "rf", ntree = CCPtrees, trControl = fitControl) # Randomforest via the caret-way.
saveRDS(CCP_model_rf, paste(saveLocation,"/CCP_model_rf_TrainC_TestF.RDS", sep = ""))
# CCP_model_rf <- readRDS(paste(saveLocation,"/CCP_model_rf_TrainC_TestF.RDS", sep = ""))

# Making predictions
CCP_pred_rf <- predict(CCP_model_rf, newdata = test_sample, type = "prob")  # Predicting on the test-set.
saveRDS(CCP_pred_rf, paste(saveLocation,"/CCP_pred_rf_TrainC_TestF.RDS", sep = ""))
