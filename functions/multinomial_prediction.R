
# function to run and predict probabilities from multinomial modes

multinomial_prediction <- function(data, outcome, predictors) {
  form <- str_c(outcome, " ~ ", str_c(predictors, collapse = " + "))


  # run logistic
  mod <- multinom(form, data)

  # get probabilities
  predictions <- predict(mod, data, "probs") %>% tbl_df()


  # save conitional probability based on their group
  data$pred <- NA

  cat <- unique(data[outcome]) %>% unlist() %>% unname()

  for (i in cat) {
    selection <- data[outcome] == i
    data$pred[selection] <- predictions[selection, i] %>% unlist()
  }

  data$pred

}
