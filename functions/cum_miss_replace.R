# function to get info on missing from next variables
cum_miss_replace <- function(data, var) {

  # select small data
  sdata <- select(data, matches(var))

  # replace missing with next wave info
  for (i in 1:(ncol(sdata) - 1)){
    sdata[is.na(sdata[, 1]), 1] <- sdata[is.na(sdata[, 1]), i + 1]
  }

  # print var with less missing
  sdata[, 1]

}
