## Original code by Floris Devriendt, cleaned and modified by Jordi Vandenhouwe & Maarten de Winter
# Maximum profit uplift
mpu2 <- function(pr.y1_ct1, pr.y1_ct0, y, ct, direction = 2, cutoffs.group="all", benefits = 200, costIncentive = 10, costSending = 1, administrativeCost = 50){

  #Direction 2 -> for churning, direction 1 -> for response modeling??
  # Code is a combination of the MP-code (MaximumProfit2.R) and how the performance (see performance.custom.R) blocks are built. 
  
  # Test:
  
  # pr.y1_ct1 = rep(0,length(CCP_pred_rf_trainCr_testFull[,2]))
  # pr.y1_ct0 = CCP_pred_rf_trainCr_testFull[,2]
  # y = test_sample$churn_6mnths
  # ct = test_sample$comm_after6
  # direction = 2
  # cutoffs.group="all"
  # benefits = exp.Benefits
  # costIncentive = exp.costIncentive
  # costSending = exp.costSending
  # administrativeCost = exp.administrativeCost

  # pr.y1_ct1 <- 1 - pr.y1_ct1
  # pr.y1_ct0 <- 1 - pr.y1_ct0
  # y <- 1 - y
  # direction <- 1
  
  labelsChurn <- as.factor(y)
  labelsTreatment <- ct
  
  df.eval <- data.frame(pr.y1_ct1=pr.y1_ct1, pr.y1_ct0=pr.y1_ct0, labelsChurn=labelsChurn, labelsTreatment=labelsTreatment)
  # df.eval.tr <- df.eval[df.eval$labelsTreatment==1,]
  # df.eval.cr <- df.eval[df.eval$labelsTreatment==0,]
  
  ### define dif.pred based on direction
  if (direction == 2) {
    df.eval$dif.pred = df.eval$pr.y1_ct0 - df.eval$pr.y1_ct1
    # dif.pred.tr = df.eval.tr$pr.y1_ct0 - df.eval.tr$pr.y1_ct1
    # dif.pred.cr = df.eval.cr$pr.y1_ct0 - df.eval.cr$pr.y1_ct1
  } else {
    df.eval$dif.pred = df.eval$pr.y1_ct1 - df.eval$pr.y1_ct0
    # dif.pred.tr = df.eval.tr$pr.y1_ct1 - df.eval.tr$pr.y1_ct0
    # dif.pred.cr = df.eval.cr$pr.y1_ct1 - df.eval.cr$pr.y1_ct0
  }
  
  ## convert 'labels' into ordered factors, aborting if the number
  ## of classes is not equal to 2.
  levels <- c()
  levels <- sort(levels(df.eval$labelsChurn))
  labels <- ordered( df.eval$labelsChurn, levels=levels)
  
  # levels.tr <- c()
  # levels.tr <- sort(levels(df.eval.tr$labelsChurn))
  # labels.tr <- ordered( df.eval.tr$labelsChurn, levels=levels.tr)
  # 
  # levels.cr <- c()
  # levels.cr <- sort(levels(df.eval.cr$labelsChurn))
  # labels.cr <- ordered( df.eval.cr$labelsChurn, levels=levels.cr)
  
  n.pos <- sum( labels == levels[2] )
  n.neg <- sum( labels == levels[1] )
  
  # n.pos.tr <- sum( labels.tr == levels.tr[2] )
  # n.neg.tr <- sum( labels.tr == levels.tr[1] )
  # 
  # n.pos.cr <- sum( labels.cr == levels.cr[2] )
  # n.neg.cr <- sum( labels.cr == levels.cr[1] )
  
  ## determine the labels that are used for the pos. resp. neg. class :
  pos.label <- levels(labels)[2]
  neg.label <- levels(labels)[1]
  
  # pos.label.tr <- levels(labels.tr)[2]
  # neg.label.tr <- levels(labels.tr)[1]
  # 
  # pos.label.cr <- levels(labels.cr)[2]
  # neg.label.cr <- levels(labels.cr)[1]
  
  pred.order <- order(df.eval$dif.pred, decreasing=TRUE)
  uplift.sorted <- df.eval$dif.pred[pred.order]
  tp <- cumsum(df.eval[pred.order,"labelsChurn"]==pos.label)
  fp <- cumsum(df.eval[pred.order,"labelsChurn"]==neg.label)
  
  # pred.order.tr <- order(dif.pred.tr, decreasing=TRUE)
  # uplift.tr.sorted <- dif.pred.tr[pred.order.tr]
  # tp.tr <- cumsum(labels.tr[pred.order.tr]==pos.label.tr)
  # fp.tr <- cumsum(labels.tr[pred.order.tr]==neg.label.tr)
  # 
  # pred.order.cr <- order(dif.pred.cr, decreasing=TRUE)
  # uplift.cr.sorted <- dif.pred.cr[pred.order.cr]
  # tp.cr <- cumsum(labels.cr[pred.order.cr]==pos.label.cr)
  # fp.cr <- cumsum(labels.cr[pred.order.cr]==neg.label.cr)

  ## remove fp & tp for duplicated predictions
  ## as duplicated keeps the first occurrence, but we want the last, two
  ## rev are used.
  ## Highest cutoff (Infinity) corresponds to tp=0, fp=0
  dups <- rev(duplicated(rev(uplift.sorted)))
  tp <- c(0, tp[!dups])
  fp <- c(0, fp[!dups])
  cutoffs.all <- c(Inf, uplift.sorted[!dups], -Inf)
    
  ## remove fp & tp for duplicated predictions
  ## as duplicated keeps the first occurrence, but we want the last, two
  ## rev are used.
  # dups.tr <- rev(duplicated(rev(uplift.tr.sorted)))
  # tp.tr <- c(0, tp.tr[!dups.tr])
  # fp.tr <- c(0, fp.tr[!dups.tr])
  # cutoffs.tr <- c(Inf, uplift.tr.sorted[!dups.tr], -Inf)
  # 
  # ## remove fp & tp for duplicated predictions
  # ## as duplicated keeps the first occurrence, but we want the last, two
  # ## rev are used.
  # ## Highest cutoff (Infinity) corresponds to tp=0, fp=0
  # dups.cr <- rev(duplicated(rev(uplift.cr.sorted)))
  # tp.cr <- c(0, tp.cr[!dups.cr])
  # fp.cr <- c(0, fp.cr[!dups.cr])
  # cutoffs.cr <- c(Inf, uplift.cr.sorted[!dups.cr])
  
  if(cutoffs.group == "all"){
    cutoffs <- cutoffs.all
  } else if(cutoffs.group == "tr"){
    cutoffs <- cutoffs.tr
  } else {
    cutoffs <- cutoffs.cr
  }
  
  df.eval[pred.order,"group"] <- length(cutoffs) - findInterval(df.eval[pred.order, "dif.pred"], vec = rev(cutoffs))
  
  ## Consider if we need this step!
  df.eval$labelsChurn <- as.numeric(levels(df.eval$labelsChurn))[df.eval$labelsChurn]
  
  n.y1_ct0 <- tapply(df.eval[df.eval[, 4] == 0, ][, 3], df.eval[df.eval[, 4] == 0, ][, 6], sum)
  n.y1_ct1 <- tapply(df.eval[df.eval[, 4] == 1, ][, 3], df.eval[df.eval[, 4] == 1, ][, 6], sum)
  r.y1_ct0 <- tapply(df.eval[df.eval[, 4] == 0, ][, 3], df.eval[df.eval[, 4] == 0, ][, 6], mean)
  r.y1_ct1 <- tapply(df.eval[df.eval[, 4] == 1, ][, 3], df.eval[df.eval[, 4] == 1, ][, 6], mean)
  n.ct0 <- tapply(df.eval[df.eval[, 4] == 0, ][, 3], df.eval[df.eval[, 4] == 0, ][, 6], length)
  n.ct1 <- tapply(df.eval[df.eval[, 4] == 1, ][, 3], df.eval[df.eval[, 4] == 1, ][, 6], length)
  
  # The formula is focussed on saving non-churners, so first of we need to find the statistics on non-churners.
  # Could be optimized.

  n.y1_ct0.nonChurn <- n.ct0 - n.y1_ct0
  n.y1_ct1.nonChurn <- n.ct1 - n.y1_ct1
  n.ct0.nonChurn <- n.ct0
  n.ct1.nonChurn <- n.ct1
  r.y1_ct0.nonChurn <- n.y1_ct0.nonChurn / n.ct0.nonChurn
  r.y1_ct1.nonChurn <- n.y1_ct1.nonChurn / n.ct1.nonChurn

  n.y1_ct0 <- n.y1_ct0.nonChurn
  n.y1_ct1 <- n.y1_ct1.nonChurn
  n.ct0 <- n.ct0.nonChurn
  n.ct1 <- n.ct1.nonChurn
  r.y1_ct0 <- r.y1_ct0.nonChurn
  r.y1_ct1 <- r.y1_ct1.nonChurn

  # The direction is now the opposite of the original direction.
  # E.g. instead of calculating costs about churners, we now look at non-churners
  if (direction == 1) {
    direction <- 2
  } else {
    direction <- 1
  }

  # For some reason the name needs to be df
  df <- merge(cbind(n.y1_ct0, r.y1_ct0, n.ct0), cbind(n.y1_ct1, r.y1_ct1, n.ct1), by= "row.names", all = TRUE)             
  
  df$Row.names <- as.numeric(df$Row.names)
  df[, c(2, 3, 4, 5, 6, 7)][is.na(df[, c(2, 3, 4, 5, 6, 7)])] <- 0 # missing implies 0 counts
  
  if (direction == 2) {
    df$uplift = df$r.y1_ct0 - df$r.y1_ct1} else {
      df$uplift = df$r.y1_ct1 - df$r.y1_ct0
    }
  df <- df[order(df$Row.names), ]
  
  res <- cbind(group   = df$Row.names,
               n.ct1    = df$n.ct1,
               n.ct0    = df$n.ct0, 
               n.y1_ct1 = df$n.y1_ct1, 
               n.y1_ct0 = df$n.y1_ct0,
               r.y1_ct1 = df$r.y1_ct1, 
               r.y1_ct0 = df$r.y1_ct0,
               uplift   = df$uplift)
  
  class(res) <- "performance"
  
  perf <- res
  
  # CustomQini(perf, direction = 2)
  
  cs.n.ct1 <- cumsum(perf[,"n.ct1"])
  cs.n.ct0 <- cumsum(perf[,"n.ct0"])
  cs.n.y1_ct1 <- cumsum(perf[,"n.y1_ct1"])
  cs.n.y1_ct0 <- cumsum(perf[,"n.y1_ct0"])
  cs.r.y1_ct1 <- cs.n.y1_ct1 / cs.n.ct1
  cs.r.y1_ct0 <- cs.n.y1_ct0 / cs.n.ct0
  
  cs.r.y1_ct1[is.na(cs.r.y1_ct1)] <- 0
  cs.r.y1_ct0[is.na(cs.r.y1_ct0)] <- 0
  
  if(direction == 2){
    cs.uplift <- cs.r.y1_ct0 - cs.r.y1_ct1  
  } else {
    cs.uplift <- cs.r.y1_ct1 - cs.r.y1_ct0
  }
  
  
  perf <- cbind(perf, 
                "cs.n.ct1" = cs.n.ct1,
                "cs.n.ct0" = cs.n.ct0,
                "cs.n.y1_ct1" = cs.n.y1_ct1,
                "cs.n.y1_ct0" = cs.n.y1_ct0,
                "cs.r.y1_ct1" = cs.r.y1_ct1,
                "cs.r.y1_ct0" = cs.r.y1_ct0,
                "cs.uplift" = cs.uplift)
  
  perf[, c("cs.n.ct1", "cs.n.ct0", "cs.n.y1_ct1", "cs.n.y1_ct0", "cs.r.y1_ct1", "cs.r.y1_ct0", "cs.uplift")][is.na(perf[, c("cs.n.ct1", "cs.n.ct0", "cs.n.y1_ct1", "cs.n.y1_ct0", "cs.r.y1_ct1", "cs.r.y1_ct0", "cs.uplift")])] <- 0 # missing implies 0 counts
  
  p <- ((perf[,"cs.n.ct1"]+perf[,"cs.n.ct0"]) * perf[,"cs.uplift"] * benefits) + (perf[,"cs.n.ct1"]+perf[,"cs.n.ct0"]) * -costSending + ((perf[,"cs.n.ct1"]+perf[,"cs.n.ct0"]) * perf[,"cs.r.y1_ct1"] * -costIncentive) - administrativeCost
 
   # perf <- cbind(perf, 
  #               totalProfit = p, 
  #               totalProfitPerCustomer = p/(max(perf[,"cs.n.ct1"])+max(perf[,"cs.n.ct0"])),
  #               perc.all = (perf[,"cs.n.ct0"] + perf[,"cs.n.ct1"]) / (max(perf[,"cs.n.ct1"])+max(perf[,"cs.n.ct0"])),
  #               perc.tr = perf[,"cs.n.ct1"] / max(perf[,"cs.n.ct1"]),
  #               perc.cr = perf[,"cs.n.ct0"] / max(perf[,"cs.n.ct0"]))
  
  # if(cutoffs.group == "all"){
  #   percentage <- (perf[,"cs.n.ct0"] + perf[,"cs.n.ct1"]) / (max(perf[,"cs.n.ct1"])+max(perf[,"cs.n.ct0"]))
  # } else if(cutoffs.group == "tr") {
  #   percentage <- perf[,"cs.n.ct1"] / max(perf[,"cs.n.ct1"])
  # } else {
  #   percentage <- perf[,"cs.n.ct0"] / max(perf[,"cs.n.ct0"])
  # }
  
  percentage <- (perf[,"cs.n.ct0"] + perf[,"cs.n.ct1"]) / (max(perf[,"cs.n.ct1"])+max(perf[,"cs.n.ct0"]))
  
  perf <- cbind(perf, 
                totalProfit = p, 
                totalProfitPerCustomer = p/(max(perf[,"cs.n.ct1"])+max(perf[,"cs.n.ct0"])),
                perc = percentage)

  return(perf)
}

# plot(cperf[,"perc.all"],cperf[,"totalProfit"], type="l", ylab="profit", xlab="Percentage Captured", main="Total Maximum Profit")
# 
# plot(cperf[,"perc.cr"],cperf[,"totalProfitPerCustomer"], type="l", lwd=3, ylab="Profit per Customer", xlab="Percentage captured", main="Maximum Profit Per Customer")
 
# mpu3 <- function(upliftScore, y, ct, direction = 1, cutoffs.group="all", benefits = 200, costIncentive = 10, costSending = 1, administrativeCost = 50){
#   
#   # Test:
#   
#   upliftScore = UCP_pred_rf_trainFull_testFull[,2] - UCP_pred_rf_trainFull_testFull[,1]
#   y = test_sample$churn_6mnths
#   ct = test_sample$comm_after6
#   direction = 2
#   cutoffs.group="all"
#   benefits = exp.Benefits
#   costIncentive = exp.costIncentive
#   costSending = exp.costSending
#   administrativeCost = exp.administrativeCost
#   
#   # pr.y1_ct1 <- 1 - pr.y1_ct1
#   # pr.y1_ct0 <- 1 - pr.y1_ct0
#   # y <- 1 - y
#   # direction <- 1
#   
#   labelsChurn <- as.factor(y)
#   labelsTreatment <- ct
#   
#   df.eval <- data.frame(upliftScore=upliftScore, labelsChurn=labelsChurn, labelsTreatment=labelsTreatment)
#   
#   ## convert 'labels' into ordered factors, aborting if the number
#   ## of classes is not equal to 2.
#   levels <- c()
#   levels <- sort(levels(df.eval$labelsChurn))
#   labels <- ordered( df.eval$labelsChurn, levels=levels)
#   
#   n.pos <- sum( labels == levels[2] )
#   n.neg <- sum( labels == levels[1] )
#   
#   ## determine the labels that are used for the pos. resp. neg. class :
#   pos.label <- levels(labels)[2]
#   neg.label <- levels(labels)[1]
#   
#   pred.order <- order(df.eval$upliftScore, decreasing=TRUE)
#   uplift.sorted <- df.eval$upliftScore[pred.order]
#   tp <- cumsum(df.eval[pred.order,"labelsChurn"]==pos.label)
#   fp <- cumsum(df.eval[pred.order,"labelsChurn"]==neg.label)
#   
#   ## remove fp & tp for duplicated predictions
#   ## as duplicated keeps the first occurrence, but we want the last, two
#   ## rev are used.
#   ## Highest cutoff (Infinity) corresponds to tp=0, fp=0
#   dups <- rev(duplicated(rev(uplift.sorted)))
#   tp <- c(0, tp[!dups])
#   fp <- c(0, fp[!dups])
#   cutoffs <- c(Inf, uplift.sorted[!dups], -Inf)
# 
#   df.eval[pred.order,"group"] <- length(cutoffs) - findInterval(df.eval[pred.order, "upliftScore"], vec = rev(cutoffs))
#   
#   ## Consider if we need this step!
#   df.eval$labelsChurn <- as.numeric(levels(df.eval$labelsChurn))[df.eval$labelsChurn]
#   
#   n.y1_ct0 <- tapply(df.eval[df.eval[, 3] == 0, ][, 2], df.eval[df.eval[, 3] == 0, ][, 1], sum)
#   n.y1_ct1 <- tapply(df.eval[df.eval[, 3] == 1, ][, 2], df.eval[df.eval[, 3] == 1, ][, 1], sum)
#   r.y1_ct0 <- tapply(df.eval[df.eval[, 3] == 0, ][, 2], df.eval[df.eval[, 3] == 0, ][, 1], mean)
#   r.y1_ct1 <- tapply(df.eval[df.eval[, 3] == 1, ][, 2], df.eval[df.eval[, 3] == 1, ][, 1], mean)
#   n.ct0 <- tapply(df.eval[df.eval[, 3] == 0, ][, 2], df.eval[df.eval[, 3] == 0, ][, 1], length)
#   n.ct1 <- tapply(df.eval[df.eval[, 3] == 1, ][, 2], df.eval[df.eval[, 3] == 1, ][, 1], length)
#   
#   # n.y1_ct0.Churn <- n.y1_ct0
#   # n.y1_ct1.Churn <- n.y1_ct1
#   # r.y1_ct0.Churn <- r.y1_ct0
#   # r.y1_ct1.Churn <- r.y1_ct1
#   # n.ct0.Churn <- n.ct0
#   # n.ct1.Churn <- n.ct1
#   
#   # For some reason the name needs to be df
#   df <- merge(cbind(n.y1_ct0, r.y1_ct0, n.ct0), cbind(n.y1_ct1, r.y1_ct1, n.ct1), by= "row.names", all = TRUE)             
#   
#   df$Row.names <- as.numeric(df$Row.names)
#   df[, c(2, 3, 4, 5, 6, 7)][is.na(df[, c(2, 3, 4, 5, 6, 7)])] <- 0 # missing implies 0 counts
#   
#   if (direction == 2) {
#     df$uplift = df$r.y1_ct0 - df$r.y1_ct1} else {
#       df$uplift = df$r.y1_ct1 - df$r.y1_ct0
#     }
#   df <- df[order(df$Row.names), ]
#   
#   res <- cbind(group   = df$Row.names,
#                n.ct1    = df$n.ct1,
#                n.ct0    = df$n.ct0, 
#                n.y1_ct1 = df$n.y1_ct1, 
#                n.y1_ct0 = df$n.y1_ct0,
#                r.y1_ct1 = df$r.y1_ct1, 
#                r.y1_ct0 = df$r.y1_ct0,
#                uplift   = df$uplift)
#   
#   class(res) <- "performance"
#   
#   perf <- res
#   
#   # CustomQini(perf, direction = 2)
#   
#   cs.n.ct1 <- cumsum(perf[,"n.ct1"])
#   cs.n.ct0 <- cumsum(perf[,"n.ct0"])
#   cs.n.y1_ct1 <- cumsum(perf[,"n.y1_ct1"])
#   cs.n.y1_ct0 <- cumsum(perf[,"n.y1_ct0"])
#   cs.r.y1_ct1 <- cs.n.y1_ct1 / cs.n.ct1
#   cs.r.y1_ct0 <- cs.n.y1_ct0 / cs.n.ct0
#   
#   cs.r.y1_ct1[is.na(cs.r.y1_ct1)] <- 0
#   cs.r.y1_ct0[is.na(cs.r.y1_ct0)] <- 0
#   
#   if(direction == 2){
#     cs.uplift <- cs.r.y1_ct0 - cs.r.y1_ct1  
#   } else {
#     cs.uplift <- cs.r.y1_ct1 - cs.r.y1_ct0
#   }
#   
#   
#   perf <- cbind(perf, 
#                 "cs.n.ct1" = cs.n.ct1,
#                 "cs.n.ct0" = cs.n.ct0,
#                 "cs.n.y1_ct1" = cs.n.y1_ct1,
#                 "cs.n.y1_ct0" = cs.n.y1_ct0,
#                 "cs.r.y1_ct1" = cs.r.y1_ct1,
#                 "cs.r.y1_ct0" = cs.r.y1_ct0,
#                 "cs.uplift" = cs.uplift)
#   
#   perf[, c("cs.n.ct1", "cs.n.ct0", "cs.n.y1_ct1", "cs.n.y1_ct0", "cs.r.y1_ct1", "cs.r.y1_ct0", "cs.uplift")][is.na(perf[, c("cs.n.ct1", "cs.n.ct0", "cs.n.y1_ct1", "cs.n.y1_ct0", "cs.r.y1_ct1", "cs.r.y1_ct0", "cs.uplift")])] <- 0 # missing implies 0 counts
#   
#   p <- ((perf[,"cs.n.ct1"]+perf[,"cs.n.ct0"]) * perf[,"cs.uplift"] * benefits) + (perf[,"cs.n.ct1"]+perf[,"cs.n.ct0"]) * -costSending + ((perf[,"cs.n.ct1"]+perf[,"cs.n.ct0"]) * perf[,"cs.r.y1_ct1"] * -costIncentive) 
#   # perf <- cbind(perf, 
#   #               totalProfit = p, 
#   #               totalProfitPerCustomer = p/(max(perf[,"cs.n.ct1"])+max(perf[,"cs.n.ct0"])),
#   #               perc.all = (perf[,"cs.n.ct0"] + perf[,"cs.n.ct1"]) / (max(perf[,"cs.n.ct1"])+max(perf[,"cs.n.ct0"])),
#   #               perc.tr = perf[,"cs.n.ct1"] / max(perf[,"cs.n.ct1"]),
#   #               perc.cr = perf[,"cs.n.ct0"] / max(perf[,"cs.n.ct0"]))
#   
#   # if(cutoffs.group == "all"){
#   #   percentage <- (perf[,"cs.n.ct0"] + perf[,"cs.n.ct1"]) / (max(perf[,"cs.n.ct1"])+max(perf[,"cs.n.ct0"]))
#   # } else if(cutoffs.group == "tr") {
#   #   percentage <- perf[,"cs.n.ct1"] / max(perf[,"cs.n.ct1"])
#   # } else {
#   #   percentage <- perf[,"cs.n.ct0"] / max(perf[,"cs.n.ct0"])
#   # }
#   
#   percentage <- (perf[,"cs.n.ct0"] + perf[,"cs.n.ct1"]) / (max(perf[,"cs.n.ct1"])+max(perf[,"cs.n.ct0"]))
#   
#   perf <- cbind(perf, 
#                 totalProfit = p, 
#                 totalProfitPerCustomer = p/(max(perf[,"cs.n.ct1"])+max(perf[,"cs.n.ct0"])),
#                 perc = percentage)
#   
#   return(perf)
# }