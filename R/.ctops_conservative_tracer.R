days_monthy <- lubridate::days_in_month(seq.Date(from = min(atm$date),
                                                 to = max(atm$date),
                                                 by = "month"))

days_total <- cumsum(days_monthy)

indeces <- 11:20

c_tops <- lapply(indeces, function(i) {

  x <- rep(0, nrow(atm))
  if(i == 1) {
    x_min = 1
  } else {
    x_min = days_total[i - 1] + 1
  }
  x[x_min:days_total[i]] <- rep(100, days_monthy[i])


  tib <- data.frame(x)
  colnames(tib) <- if(i == indeces[1]) {
    "cTop"} else {
      sprintf("cTop%d", which(indeces %in% i))
    }

  tib
}) %>% dplyr::bind_cols()
