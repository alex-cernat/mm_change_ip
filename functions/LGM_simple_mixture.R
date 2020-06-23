






LGM_simple_mixture <- function(df_use,
                       vars_use,
                       group_var,
                       categorical = F,
                       mean_eq = F,
                       var_eq = F,
                       cor_eq = F,
                       weights = F) {


  base_text <- MplusAutomation::prepareMplusData(df_use,
                                                 filename = "./mplus/data.dta")

  base_text <- str_remove_all(base_text, '\\"') %>%
    str_c(collapse = " \n ") %>%
    str_remove_all("./mplus/")

  var_nms <- str_subset(names(df_use), vars_use)
  waves <- str_extract(var_nms, "([0-9])+$")


  syn_use1 <- str_c(str_c(var_nms, collapse = " \n "), ";\n ")
  syn_use <- str_c(" \n USEVARIABLES ARE \n ", syn_use1)

  # add categorical option if
  syn_cat <- ""

  if (categorical == T) {
    syn_cat <- str_c(" \n CATEGORICAL ARE \n ", syn_use1)
  }

  # add grouping variable

  group_nm <- levels(df_use[[group_var]])
  group_level <- 1:length(group_nm)


  syn_group <- str_c("CLASSES = c(2); \n
  KNOWNCLASS = c(", group_var, "= 1 2); \n")

  # add optional weights
  syn_weights <- ""
  if (weights != F) {
  syn_weights <- str_c("WEIGHT IS ", weights, "; \n ")
  }

  # make MLR if continious
    syn_mlr <- " \n ANALYSIS: \n \n
    TYPE = MIXTURE; \n
    ESTIMATOR = MLR; \n
    ALGORITHM = INTEGRATION; \n
    Processors = 8(starts); \n
    STARTS = 400 100;"


  syn_model <- "\n \n Model:\n\n
    %OVERALL% \n\n" %>%
    str_c("i s | ",
    str_c(var_nms, "@",  as.numeric(waves) - 5, collapse = " "),
    "; \n ")

    grp_restriction <- str_c("\n %c#", 1:2, "% \n",
                             "[s] ; \n s ; \n s WITH i ; \n i ;",
                             collapse = " \n ")


  ### add restrictions deending on the model


  if(mean_eq == T) {
    grp_restriction <- str_c("\n %c#", 1:2, "% \n",
          "[s] (a); s ; \n s WITH i ; \n i ; \n",
          collapse = " \n ")
  }

  if(var_eq == T) {
    grp_restriction <- str_c("\n %c#", 1:2, "% \n",
                             "[s] (a); \n s (b); \n s WITH i ; \n i ;",
                             collapse = " \n ")
  }

  if(cor_eq == T) {
    grp_restriction <- str_c("\n %c#", 1:2, "% \n",
                             "[s] (a); \n s (b); \n s WITH i (c); \n i (d);",
                             collapse = " \n ")
  }

  syn_output <- str_c("\n \n OUTPUT: SAMPSTAT; \n
                                STDYX; \n")


  # bring everything together
  out <- str_c(str_c(base_text, collapse = "\n"),
               syn_use, syn_cat, syn_group,
               syn_weights, syn_mlr, syn_model,
               grp_restriction, syn_output,
               collapse = "\n")

  nm <- deparse(substitute(df_use))

  if (mean_eq == T) {
    model <- "mean"
  } else if (var_eq == T) {
    model <- "var"
  } else if (cor_eq == T) {
    model <- "cor"
  } else {
    model <- "config"
  }

  # write .inp file
  write.table(out,
              str_c("./mplus/", nm, "_", vars_use, "_",
                    group_var, "_", model, ".inp"),
                quote = F,
                row.names = F,
                col.names = F)

}










