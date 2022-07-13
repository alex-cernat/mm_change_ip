# function to do sumary statistics for variables

sum_tab <- function(data, give_data_name = TRUE) {

  # get name of data
  nm <- deparse(substitute(data))

  # get descriptives table
  capture.output(descriptives <- data %>%
                   map(desc_tab), file = "NULL")

  # get variable names
  var_name <- names(descriptives)

  # get number of categories
  var_cat <- map(descriptives, nrow)

  # made datsaet with descriptives
  desc_info <- descriptives %>%
    map(function(x)
      select(x, Code, Freq., Perc.) %>%
        mutate(Code = as.character(Code))) %>%
    reduce(rbind) %>%
    mutate(
      Variable = rep(var_name, var_cat),
    #  Code = case_when(
    #   Code == 1 &  !var_cat[Variable] > 3  ~ "Yes",
    #    Code == 0 & !var_cat[Variable] > 3 ~ "No",
    #    is.na(Code) ~ "Missing",
    #    TRUE ~ as.character(Code)
    #  )
    ) %>%
    filter(Freq. > 0) %>%
    select(Variable, everything())

  if (give_data_name == TRUE) {
    desc_info <- desc_info %>%
      rename_at(vars(c("Freq.", "Perc.")),
                funs(str_c(., "_", nm)))
  }

  desc_info

}
