# Utility function to get the needed lift stats.
getLiftStats <- function(perf){
  # Overall uplift rate. (KEEP IN MIND, this is in CHURN setting. If needed in marketing setting, you'll need to reverse this substraction or adjust technique)
  uplift_rate <- (sum(perf[,"n.y1_ct0"]) / sum(perf[,"n.ct0"])) - (sum(perf[,"n.y1_ct1"]) / sum(perf[,"n.ct1"]))
  
  # Cumulative sums of treated/control and non-responders/responders
  liftStats <- cbind(perf,
                     cs.n.ct1 = cumsum(perf[,"n.ct1"]),
                     cs.n.ct0 = cumsum(perf[,"n.ct0"]),
                     cs.n.y1_ct1 = cumsum(perf[,"n.y1_ct1"]),
                     cs.n.y1_ct0 = cumsum(perf[,"n.y1_ct0"]))
  
  # Fraction of responders/non-responders at each point
  liftStats <- cbind(liftStats,
                     cs.r.y1_ct1 = liftStats[,"cs.n.y1_ct1"] / liftStats[,"cs.n.ct1"],
                     cs.r.y1_ct0 = liftStats[,"cs.n.y1_ct0"] / liftStats[,"cs.n.ct0"])
  
  # Fraction of Cumulative uplift at each point
  liftStats <- cbind(liftStats,
                     cs.uplift = liftStats[,"cs.r.y1_ct0"] - liftStats[,"cs.r.y1_ct1"])
  
  # Compared to overall uplift rate.
  liftStats <- cbind(liftStats,
                     lift = liftStats[,"cs.uplift"] / uplift_rate,
                     random = rep(1, 100))
  return(liftStats)
}
