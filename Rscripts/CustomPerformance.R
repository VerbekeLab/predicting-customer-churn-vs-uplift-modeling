CustomPerformance <- function (pr.y1_ct1, pr.y1_ct0, y, ct, direction = 1, groups = 10) 

# pr.y1_ct1 = rep(0,length(CCP_pred_rf[,2]))
# pr.y1_ct0 = CCP_pred_rf[,2]
# y = as.numeric(test_sample[,targetVariable])
# ct = as.numeric(test_sample[,treatmentVariable])
# direction = 2
# groups = 100
  
{
  if (!direction %in% c(1, 2)) 
    stop("uplift: direction must be either 1 or 2")
  if (!groups %in% c(5, 10, 20, 50, 100)) 
    stop("uplift: groups must be either 5, 10, 20, 50 or 100")
  if (any(is.na(pr.y1_ct1))) 
    stop("uplift: NA not permitted in pr.y1_ct1")
  if (any(is.na(pr.y1_ct0))) 
    stop("uplift: NA not permitted in pr.y1_ct0")
  if (any(is.na(y))) 
    stop("uplift: NA not permitted in y")
  if (any(is.na(ct))) 
    stop("uplift: NA not permitted in ct")
  if (!is.numeric(y)) 
    stop("uplift: y must be a numeric vector")
  if (!is.numeric(ct)) 
    stop("uplift: ct must be a numeric vector")
  if (!all(y %in% c(0, 1))) 
    stop("uplift: y must be either 0 or 1")
  if (!all(ct %in% c(0, 1))) 
    stop("uplift: ct must be either 0 or 1")
  if (length(pr.y1_ct1) != length(pr.y1_ct0) | length(y) != 
      length(ct) | length(pr.y1_ct1) != length(y)) 
    stop("uplift: arguments pr.y1_ct1, pr.y1_ct0, y and ct must all have the same length")
  if (direction == 2) {
    dif.pred = pr.y1_ct0 - pr.y1_ct1
  }
  else {
    dif.pred = pr.y1_ct1 - pr.y1_ct0
  }
  mm <- cbind(dif.pred = dif.pred, y = y, ct = ct, dif.pred_r = rank(-dif.pred, ties.method = 'random'))
  bk <- unique(quantile(mm[, 4], probs = seq(0, 1, 1/groups)))
  
  if ((length(bk)-1) != groups){
    warning("uplift: due to many ties in uplift predictions, the ties will be dealt with randomly ", groups)
  }
  mm <- cbind(mm, decile = cut(mm[, 4], breaks = bk, labels = NULL, 
                               include.lowest = TRUE))
  n.y1_ct0 <- tapply(mm[mm[, 3] == 0, ][, 2], mm[mm[, 3] == 
                                                   0, ][, 5], sum)
  n.y1_ct1 <- tapply(mm[mm[, 3] == 1, ][, 2], mm[mm[, 3] == 
                                                   1, ][, 5], sum)
  r.y1_ct0 <- tapply(mm[mm[, 3] == 0, ][, 2], mm[mm[, 3] == 
                                                   0, ][, 5], mean)
  r.y1_ct1 <- tapply(mm[mm[, 3] == 1, ][, 2], mm[mm[, 3] == 
                                                   1, ][, 5], mean)
  n.ct0 <- tapply(mm[mm[, 3] == 0, ][, 2], mm[mm[, 3] == 0, 
  ][, 5], length)
  n.ct1 <- tapply(mm[mm[, 3] == 1, ][, 2], mm[mm[, 3] == 1, 
  ][, 5], length)
  df <- merge(cbind(n.y1_ct0, r.y1_ct0, n.ct0), cbind(n.y1_ct1, 
                                                      r.y1_ct1, n.ct1), by = "row.names", all = TRUE)
  df$Row.names <- as.numeric(df$Row.names)
  df[, c(2, 4, 5, 7)][is.na(df[, c(2, 4, 5, 7)])] <- 0
  if (direction == 2) {
    df$uplift = df$r.y1_ct0 - df$r.y1_ct1
  }
  else {
    df$uplift = df$r.y1_ct1 - df$r.y1_ct0
  }
  df <- df[order(df$Row.names), ]
  res <- cbind(group = df$Row.names, n.ct1 = df$n.ct1, n.ct0 = df$n.ct0, 
               n.y1_ct1 = df$n.y1_ct1, n.y1_ct0 = df$n.y1_ct0, r.y1_ct1 = df$r.y1_ct1, 
               r.y1_ct0 = df$r.y1_ct0, uplift = df$uplift)
  res <- round(res, 6)
  class(res) <- "performance"
  return(res)
}
