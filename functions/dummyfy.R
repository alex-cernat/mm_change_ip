

dummyfy <- function(var) {
  if(!is.factor(var)) {
    stop("Variable is not factor type")
  }

  dat <- NULL

  for (i in seq(length(levels(var)))) {
    dat <- cbind(dat, ifelse(var == levels(var)[i], 1, 0))
  }

  colnames(dat) <- levels(var)
  dat2 <- tbl_df(dat)

  dat2
}

