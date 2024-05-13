## Original code by Floris Devriendt, cleaned and modified by Jordi Vandenhouwe & Maarten de Winter
## Loading packages ##

source("Rscripts/CustomQini.R") # Available at: https://data-lab.be/downloads.php
source("Rscripts/Models.R") # Available at: https://data-lab.be/downloads.php
source("Rscripts/MaximumProfitUplift2b.R")
source("Rscripts/CustomPerformance.R")
source("Rscripts/GetLiftStats.R")
source("0 - runSettings.R") # use this file to setup run

## Loading test sample
test_sample <- readRDS(paste(saveLocation,"/test_sample.RDS", sep = ""))

###############################################
# Loading predictions for testing MPU measure #
###############################################

## Loading predictions

CCP_pred_glm <- readRDS(paste(saveLocation,"/CCP_pred_glm_TrainC_TestF.RDS", sep = ""))
CCP_pred_rf <- readRDS(paste(saveLocation,"/CCP_pred_rf_TrainC_TestF.RDS", sep = ""))
CCU_pred_dta <- readRDS(paste(saveLocation,"/CCU_pred_dta_TrainF_TestF.RDS", sep = ""))
CCU_pred_rf <- readRDS(paste(saveLocation,"/CCU_pred_rf_TrainF_TestF.RDS", sep = ""))

###############################
#  Evaluations                #
###############################

Perf_CCP_GLM <- CustomPerformance(rep(0,length(CCP_pred_glm)),
                                  CCP_pred_glm,
                                  as.numeric(test_sample[,targetVariable]),
                                  test_sample[,treatmentVariable],
                                  direction = 2,
                                  groups = 100)

Q_CCP_GLM <- CustomQini(Perf_CCP_GLM, direction = 2)

Perf_CCP_RF <- CustomPerformance(rep(0,length(CCP_pred_rf[,2])),
                                 CCP_pred_rf[,2],
                                 as.numeric(test_sample[,targetVariable]),
                                 as.numeric(test_sample[,treatmentVariable]),
                                 direction = 2,
                                 groups = 100)

Q_CCP_RF <- CustomQini(Perf_CCP_RF, direction = 2)

Perf_CCU_DTA <- CustomPerformance(CCU_pred_dta[,1],
                                  CCU_pred_dta[,2],
                                  as.numeric(test_sample[,targetVariable]),
                                  test_sample[,treatmentVariable],
                                  direction = 2,
                                  groups = 100)

Q_CCU_DTA <- CustomQini(Perf_CCU_DTA, direction = 2)

Perf_CCU_RF <- CustomPerformance(CCU_pred_rf[,1],
                                 CCU_pred_rf[,2],
                                 as.numeric(test_sample[,targetVariable]),
                                 test_sample[,treatmentVariable],
                                 direction = 2,
                                 groups = 100)

Q_CCU_RF <- CustomQini(Perf_CCU_RF, direction = 2)

###############################
#          Run MPU2           #
###############################

if (runMPU == "YES"){
        MPU2_CCU_DTA <- mpu2(pr.y1_ct1 = CCU_pred_dta[,1],
                             pr.y1_ct0 = CCU_pred_dta[,2],
                             y = test_sample[,targetVariable],
                             ct = test_sample[,treatmentVariable],
                             benefits = benefit,
                             costIncentive = incentiveCost,
                             costSending = sendingCost,
                             administrativeCost = administrativeCosts)
        saveRDS(MPU2_CCU_DTA, paste(saveLocation,"/MPU2_CCU_DTA.RDS", sep = ""))
        
        MPU2_CCU_RF <- mpu2(pr.y1_ct1 = CCU_pred_rf[,1],
                            pr.y1_ct0 = CCU_pred_rf[,2],
                            y = test_sample[,targetVariable],
                            ct = test_sample[,treatmentVariable],
                            benefits = benefit,
                            costIncentive = incentiveCost,
                            costSending = sendingCost,
                            administrativeCost = administrativeCosts)
        saveRDS(MPU2_CCU_RF, paste(saveLocation,"/MPU2_CCU_RF.RDS", sep = ""))
        
        MPU2_CCP_GLM <- mpu2(rep(0,length(CCP_pred_glm)),
                             CCP_pred_glm,
                             test_sample[,targetVariable],
                             test_sample[,treatmentVariable],
                             benefits = benefit,
                             costIncentive = incentiveCost,
                             costSending = sendingCost,
                             administrativeCost = administrativeCosts)
        saveRDS(MPU2_CCP_GLM, paste(saveLocation,"/MPU2_CCP_GLM.RDS", sep = ""))
        
        MPU2_CCP_RF <- mpu2(rep(0,length(CCP_pred_rf[,2])),
                            CCP_pred_rf[,2],
                            test_sample[,targetVariable],
                            test_sample[,treatmentVariable],
                            benefits = benefit,
                            costIncentive = incentiveCost,
                            costSending = sendingCost,
                            administrativeCost = administrativeCosts)   
        saveRDS(MPU2_CCP_RF, paste(saveLocation,"/MPU2_CCP_RF.RDS", sep = ""))
        
} else {
        MPU2_CCU_DTA <- readRDS(paste(saveLocation,"/MPU2_CCU_DTA.RDS", sep = ""))
        MPU2_CCU_RF <- readRDS(paste(saveLocation,"/MPU2_CCU_RF.RDS", sep = ""))
        MPU2_CCP_GLM <- readRDS(paste(saveLocation,"/MPU2_CCP_GLM.RDS", sep = ""))
        MPU2_CCP_RF <- readRDS(paste(saveLocation,"/MPU2_CCP_RF.RDS", sep = ""))
}

################################## 
#       Get max profit values    #
##################################

# CCP GLM
maxProfit_CCP_GLM <- max(MPU2_CCP_GLM[,"totalProfitPerCustomer"])
targetPercentage_CCP_GLM <- MPU2_CCP_GLM[which(grepl(max(MPU2_CCP_GLM[,"totalProfitPerCustomer"]),MPU2_CCP_GLM[,"totalProfitPerCustomer"])),"perc"]

# CCU DTA
maxProfit_CCU_DTA <- max(MPU2_CCU_DTA[,"totalProfitPerCustomer"])
targetPercentage_CCU_DTA <- MPU2_CCU_DTA[which(grepl(max(MPU2_CCU_DTA[,"totalProfitPerCustomer"]),MPU2_CCU_DTA[,"totalProfitPerCustomer"])),"perc"]

# CCP RF
maxProfit_CCP_RF <- max(MPU2_CCP_RF[,"totalProfitPerCustomer"])
targetPercentage_CCP_RF <- MPU2_CCP_RF[which(grepl(max(MPU2_CCP_RF[,"totalProfitPerCustomer"]),MPU2_CCP_RF[,"totalProfitPerCustomer"])),"perc"]

# CCU RF
maxProfit_CCU_RF <- max(MPU2_CCU_RF[,"totalProfitPerCustomer"])
targetPercentage_CCU_RF <- MPU2_CCU_RF[which(grepl(max(MPU2_CCU_RF[,"totalProfitPerCustomer"]),MPU2_CCU_RF[,"totalProfitPerCustomer"])),"perc"]

##################### 
#       Plots       #
#####################

# Check if PLOTS folder already exists and if not, create it
if (!file.exists(paste(saveLocation,"/PLOTS",sep = ""))){
        dir.create(paste(saveLocation,"/PLOTS",sep = ""))
}

#logistic reg
png(file=paste(saveLocation,"/PLOTS/Profit Per Customer - MPU - Log. Regression.png", sep = ""), width=600, height=400) 
plot(MPU2_CCU_DTA[,"perc"],MPU2_CCU_DTA[,"totalProfitPerCustomer"], type="b", pch = 20, lty = 2, ylab="Profit per Customer", 
     xlab="Proportion targeted", main="Profit Per Customer - MPU Measure - Log. Regression", col = "blue", ylim=c(min(MPU2_CCU_DTA[,"totalProfitPerCustomer"],MPU2_CCP_GLM[,"totalProfitPerCustomer"]),max(MPU2_CCU_DTA[,"totalProfitPerCustomer"],MPU2_CCP_GLM[,"totalProfitPerCustomer"])))
lines(MPU2_CCP_GLM[,"perc"], MPU2_CCP_GLM[,"totalProfitPerCustomer"], type="b",pch = 20, lty = 2,col = "red")
legend("bottomleft", c("CCP", "CCU"), col = c("red", "blue"),lty = c(1, 1), bg = "gray90")
dev.off()


#rf
png(file=paste(saveLocation,"/PLOTS/Profit Per Customer - MPU - Random Forests.png", sep = ""), width=600, height=400) 
plot(MPU2_CCU_RF[,"perc"],MPU2_CCU_RF[,"totalProfitPerCustomer"], type="b", pch = 20, lty = 2, ylab="Profit per Customer", 
     xlab="Proportion targeted", main="Profit Per Customer - MPU Measure - Random Forests", col = "blue", ylim=c(min(MPU2_CCU_RF[,"totalProfitPerCustomer"],MPU2_CCP_RF[,"totalProfitPerCustomer"]),max(MPU2_CCU_RF[,"totalProfitPerCustomer"],MPU2_CCP_RF[,"totalProfitPerCustomer"])))
lines(MPU2_CCP_RF[,"perc"], MPU2_CCP_RF[,"totalProfitPerCustomer"], type="b",pch = 20, lty = 2,col = "red")
legend("bottomleft", c("CCP", "CCU"), col = c("red", "blue"), lty = c(1, 1), bg = "gray90")
dev.off()

# QINI Curve - Logistic Regression
# Save plot to file
png(file=paste(saveLocation,"/PLOTS/Qini Curve - Logistic Regression.png", sep = ""), width=600, height=400) 
plot(seq(0,1,by=1/(length(Q_CCP_GLM$random.inc.gains)-1)), Q_CCP_GLM$random.inc.gains, type="l", lwd=3, ylab="Cumulative incremental gains (pc pt)", xlab="Proportion targeted",
     main="Qini Curve - Logistic Regression", col = "black", ylim=c(min(Q_CCP_GLM$inc.gains,Q_CCU_DTA$inc.gains),max(Q_CCP_GLM$inc.gains,Q_CCU_DTA$inc.gains)))
lines(seq(0,1,by=1/(length(Q_CCP_GLM$inc.gains)-1)), Q_CCP_GLM$inc.gains, type="l", lwd=2, col = "red")
lines(seq(0,1,by=1/(length(Q_CCU_DTA$inc.gains)-1)), Q_CCU_DTA$inc.gains, type="l", lwd=2, col = "blue")
legend("bottomright", c("Random", "CCP", "CCU"), col = c("black", "red", "blue"),
       lty = c(1, 1, 1), lwd=3,
       merge = TRUE, bg = "gray90")
dev.off()
# End plot save

#Plot Random Forest (IN PAPER)
# Save plot to file
png(file=paste(saveLocation,"/PLOTS/QINI Curve - Random Forests.png", sep = ""), width=600, height=400) 
plot(seq(0,1,by=1/(length(Q_CCP_RF$random.inc.gains)-1)), Q_CCP_RF$random.inc.gains, type="l", lwd=3, ylab="Cumulative incremental gains (pc pt)", xlab="Proportion targeted", 
     main="Qini Curve - Random Forest", col = "black", ylim=c(min(Q_CCP_RF$inc.gains,Q_CCU_RF$inc.gains),max(Q_CCP_RF$inc.gains,Q_CCU_RF$inc.gains)))
lines(seq(0,1,by=1/(length(Q_CCP_RF$inc.gains)-1)), Q_CCP_RF$inc.gains, type="l", lwd=2, col = "red")
lines(seq(0,1,by=1/(length(Q_CCU_RF$inc.gains)-1)), Q_CCU_RF$inc.gains, type="l", lwd=2, col = "blue")
legend("bottomright", c("Random", "CCP", "CCU"), col = c("black", "red", "blue"),
       lty = c(1, 1, 1),lwd=3,
       merge = TRUE, bg = "gray90")
dev.off()
# End plot save

##########################
#     LiftUP Curve       #
##########################

df.liftStats <- as.data.frame(getLiftStats(Perf_CCU_RF))
df.liftStats2 <- as.data.frame(getLiftStats(Perf_CCU_DTA))

df.liftStats3 <- as.data.frame(getLiftStats(Perf_CCP_RF))
df.liftStats4 <- as.data.frame(getLiftStats(Perf_CCP_GLM))

# Save plot to file
png(file=paste(saveLocation,"/PLOTS/LiftUp Curves.png", sep = ""), width=600, height=400) 
plot(seq(0.01,1,by=1/length(df.liftStats$lift)), df.liftStats$lift, type="l", lwd=3,
     ylab="LiftUp", xlab="Proportion targeted", main="LiftUp Curves", col = "blue", ylim=c(min(df.liftStats$lift,df.liftStats2$lift,df.liftStats3$lift,df.liftStats4$lift),max(df.liftStats$lift,df.liftStats2$lift,df.liftStats3$lift,df.liftStats4$lift)))
lines(seq(0.01,1,by=1/length(df.liftStats$random)), df.liftStats$random, lwd=3, col = "black")
lines(seq(0.01,1,by=1/length(df.liftStats2$lift)), df.liftStats2$lift, lwd=3, lty=2, col = "blue")
lines(seq(0.01,1,by=1/length(df.liftStats3$lift)), df.liftStats3$lift, lwd=3, col = "red")
lines(seq(0.01,1,by=1/length(df.liftStats4$lift)), df.liftStats4$lift, lwd=3, lty=2, col = "red")
legend("topright", c("Random", "ccp: log reg", "ccp: rf", "ccu: log reg", "ccu: rf"), col = c("black", "red", "red", "blue", "blue"),
       lty = c(1, 2, 1, 2, 1),lwd=3,
       merge = TRUE, bg = "gray90")
dev.off()
# End plot save

#Check outlier  number of members in a equal uplift group
# MPU2_CCU_DTA_df <- as.data.frame(MPU2_CCU_DTA)
# plot(MPU2_CCU_DTA_df$group, type="h", as.integer(MPU2_CCU_DTA_df$n.ct1)+as.integer(MPU2_CCU_DTA_df$n.ct0), main="Group membership of observations in test set with equal predicted uplift - CCU Log. Regression", ylab="Number of observations", xlab="Observation bucket")
# MPU2_CCU_RF_df <- as.data.frame(MPU2_CCU_RF)
# plot(MPU2_CCU_RF_df$group, type="h", as.integer(MPU2_CCU_RF_df$n.ct1)+as.integer(MPU2_CCU_RF_df$n.ct0), main="Group membership of observations in test set with equal predicted uplift - CCU Ran. For.", ylab="Number of observations", xlab="Observation bucket")
# MPU2_CCP_GLM_df <- as.data.frame(MPU2_CCP_GLM )
# plot(MPU2_CCP_GLM_df$group, type="h", as.integer(MPU2_CCP_GLM_df$n.ct1)+as.integer(MPU2_CCP_GLM_df$n.ct0), main="Group membership of observations in test set with equal predicted uplift - CCP Log. Regression", ylab="Number of observations", xlab="Observation bucket")
# MPU2_CCP_RF_df <- as.data.frame(MPU2_CCP_RF)
# plot(MPU2_CCP_RF_df$group, type="h", as.integer(MPU2_CCP_RF_df$n.ct1)+as.integer(MPU2_CCP_RF_df$n.ct0), main="Group membership of observations in test set with equal predicted uplift - CCP Ran. For.", ylab="Number of observations", xlab="Observation bucket")
# 
