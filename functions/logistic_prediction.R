

# function to run and predict probabilities from logistic modes

logistic_prediction <- function(data, outcome, predictors) {
  form <- str_c(outcome, " ~ ", str_c(predictors,
                                      collapse = " + "))

  data <- data %>%
    mutate_at(outcome, as.factor)

  # run logistic
  mod <- glm(form, data, family = binomial(link = 'logit'))

  # get probabilities
  predictions <- predict(mod, data, type = "response") %>% tbl_df()

  predictions
}
