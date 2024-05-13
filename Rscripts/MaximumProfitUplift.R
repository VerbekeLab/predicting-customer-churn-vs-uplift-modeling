mpu <- function(pr.y1_ct1, pr.y1_ct0, y, ct, direction = 2, groups = 100, churnRate = 0.3, benefits = 200, costIncentive = 10, costSending = 1, administrativeCost = 50, plotit = TRUE){
  
  
  ### define dif.pred based on direction
  if (direction == 2) {
    dif.pred = pr.y1_ct0 - pr.y1_ct1} else {
      dif.pred = pr.y1_ct1 - pr.y1_ct0
    }
  
  ### REMARK: ################################################################################
  # - The rank-method deals with ties in a random fashion: ties are placed in random order.  #
  # - To take into account the randomness five iterations are done and the average is taken. #
  ############################################################################################
  
  #set.seed(123) # For experimting
  
  # Multiple iterations are done and averaged
  sumOfRes <- 0
  amountOfIterations <- 5

  for(i in 1:amountOfIterations){
    mm <- cbind(dif.pred = dif.pred, y = y, ct = ct, dif.pred_r = rank(-dif.pred, ties.method = "random")) # This has been changed, original was without 'ties.method = "random"'
    
    # Alternative approach, not tested:
    # mm <- cbind(dif.pred = dif.pred, y = y, ct = ct, dif.pred_r = order(order(-dif.pred, pr.y1_ct0, runif(length(dif.pred)))))
    
    bk <- unique(quantile(mm[, 4], probs = seq(0, 1, 1 / groups)))
    if ((length(bk)-1) != groups){
      warning("uplift: due to many ties in uplift predictions, the ties will dealt with randomly ", groups)
    }
    
    mm <- cbind(mm, decile = cut(mm[, 4], breaks = bk, labels = NULL, 
                                 include.lowest = TRUE)) 
    
    n.y1_ct0 <- tapply(mm[mm[, 3] == 0, ][, 2], mm[mm[, 3] == 0, ][, 5], sum)
    n.y1_ct1 <- tapply(mm[mm[, 3] == 1, ][, 2], mm[mm[, 3] == 1, ][, 5], sum)
    r.y1_ct0 <- tapply(mm[mm[, 3] == 0, ][, 2], mm[mm[, 3] == 0, ][, 5], mean)
    r.y1_ct1 <- tapply(mm[mm[, 3] == 1, ][, 2], mm[mm[, 3] == 1, ][, 5], mean)
    n.ct0 <- tapply(mm[mm[, 3] == 0, ][, 2], mm[mm[, 3] == 0, ][, 5], length)
    n.ct1 <- tapply(mm[mm[, 3] == 1, ][, 2], mm[mm[, 3] == 1, ][, 5], length)
    
    r.y1_ct0 <- ifelse(is.na(r.y1_ct0), 0, r.y1_ct0)
    r.y1_ct1 <- ifelse(is.na(r.y1_ct1), 0, r.y1_ct1)
    
    df <- merge(cbind(n.y1_ct0, r.y1_ct0, n.ct0), cbind(n.y1_ct1, r.y1_ct1, n.ct1), by= "row.names", all = TRUE)             
    
    df$Row.names <- as.numeric(df$Row.names)
    df[, c(2, 4, 5, 7)][is.na(df[, c(2, 4, 5, 7)])] <- 0 # missing implies 0 counts
    
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
    
    res <- round(res, 6)
    sumOfRes <- sumOfRes + res
  }
  
  # Averaging the iterations
  sumOfRes <- sumOfRes / amountOfIterations
  sumOfRes <- round(sumOfRes, 6)
  
  class(sumOfRes) <- "performance"
  
  perf <- sumOfRes
  
  # CustomQini(perf, direction = 2)
  
  cs.n.ct1 <- cumsum(perf[,"n.ct1"])
  cs.n.ct0 <- cumsum(perf[,"n.ct0"])
  cs.n.y1_ct1 <- cumsum(perf[,"n.y1_ct1"])
  cs.n.y1_ct0 <- cumsum(perf[,"n.y1_ct0"])
  cs.r.y1_ct1 <- cs.n.y1_ct1 / cs.n.ct1
  cs.r.y1_ct0 <- cs.n.y1_ct0 / cs.n.ct0
  cs.uplift <- cs.r.y1_ct0 - cs.r.y1_ct1
  
  perf <- cbind(perf, 
                "cs.n.ct1" = cs.n.ct1,
                "cs.n.ct0" = cs.n.ct0,
                "cs.n.y1_ct1" = cs.n.y1_ct1,
                "cs.n.y1_ct0" = cs.n.y1_ct0,
                "cs.r.y1_ct1" = cs.r.y1_ct1,
                "cs.r.y1_ct0" = cs.r.y1_ct0,
                "cs.uplift" = cs.uplift)
  
  p <- ((perf[,"cs.n.ct1"]+perf[,"cs.n.ct0"]) * perf[,"cs.uplift"] * benefits) + (perf[,"cs.n.ct1"]+perf[,"cs.n.ct0"]) * -costSending + ((perf[,"cs.n.ct1"]+perf[,"cs.n.ct0"]) * perf[,"cs.r.y1_ct1"] * -costIncentive) - administrativeCost
  percentage <- (perf[,"cs.n.ct0"] + perf[,"cs.n.ct1"]) / (max(perf[,"cs.n.ct1"])+max(perf[,"cs.n.ct0"]))
  
  perf <- cbind(perf, 
                totalProfit = p, 
                totalProfitPerCustomer = p/(max(perf[,"cs.n.ct1"])+max(perf[,"cs.n.ct0"])),
                perc = percentage)
  return(perf)
}