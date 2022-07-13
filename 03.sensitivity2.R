#############################################################
#
# Project looking at how mode influences estimates of change
#
# Analysis for second R&R
#
#############################################################

# clear working space
rm(list = ls())

pkg <- c("tidyverse",  "haven", "lavaan", "MplusAutomation", "nnet")
sapply(pkg, library, character.only = T)

# load local functions
map(list.files("./functions/", full.names = T), source)


# Import data -------------------------------------------------------------

load("./data/out/identifier_vars_data_charlotte.RData")
load("./data/out/overlap_vars_data_charlotte.RData")


vars_desc <- read_csv("./data/out/vars_desc.csv")

wide_data3 <- read_rds("./data/wide_data3.RDS")
wide_data5 <- read_rds("./data/clean_data.RDS")



# try nonlinear change ----------------------------------------------------


# vars to analyse
cont_vars <- c("finnow", "fiyrdia", "howlng", "netpuse", "scsf1", "scsf3a",
               "scsf3b", "scsf4a", "scsf4b", "scsf5", "scsf6a", "scsf6b",
               "scsf6c", "scsf7")
cat_vars <- c("aidxhh", "caruse", "finfut", "j2has", "jbhas", "jbsemp",
              "jbterm1", "lkmove", "mobuse", "scghqa", "scghqb", "scghqc",
              "scghqd", "scghqe", "scghqf", "scghqg", "scghqh", "scghqi",
              "scghqj", "scghqk", "scghql", "scsf2a", "scsf2b", "smoker",
              "vote1", "vote6", "xpmove")


outcomes <- c("all", "no9", "p567", "p7")



# group var
vars_desc2 <- vars_desc %>%
  select(Code, Wave) %>%
  mutate(group_var = case_when(Wave == 567 ~ "p567",
                               Wave == 78910 ~ "p7",
                               Wave == 567810 ~ "no9",
                               Wave == 5678910 ~ "all"),
         group_var2 = "mm",
         type = ifelse(Code %in% cont_vars, "cont", "cat"),
         weight = case_when(Wave == 567 ~ "pred_p567_w",
                            Wave == 78910 ~ "pred_p7_w",
                            Wave == 567810 ~ "pred_no9_w",
                            Wave == 5678910 ~ "pred_all_w"))


# prepare data for Mplus
# no character or haven_labelled

wide_data6 <- wide_data5 %>%
  mutate_at(outcomes,
            ~factor(., labels = c("f2f", "web"))) %>%
  mutate_if(is.character, as.factor) %>%
  mutate_if(is.double, as.numeric)

vars_desc2b <- vars_desc2 %>%
  mutate(categorical = ifelse(type == "cat", T, F),
         mean_eq = F,
         var_eq = F,
         nonlin_eq = F,
         cor_eq = F) %>%
  rename(vars_use = Code) %>%
  select(-Wave,-group_var2,-type, -group_var, -weight)


# find more elegant way to do this
vars_desc2c <- rbind(vars_desc2b,
                     vars_desc2b %>%
                       mutate(
                         mean_eq = T,
                         var_eq = F,
                         nonlin_eq = F,
                         cor_eq = F
                       )) %>%
  rbind(vars_desc2b %>%
          mutate(
            mean_eq = F,
            var_eq = T,
            nonlin_eq = F,
            cor_eq = F
          )) %>%
  rbind(vars_desc2b %>%
          mutate(
            mean_eq = F,
            var_eq = F,
            nonlin_eq = T,
            cor_eq = F
          )) %>%
  rbind(vars_desc2b %>%
          mutate(
            mean_eq = F,
            var_eq = F,
            nonlin_eq = F,
            cor_eq = T
          ))


# run models using known classes and MLR + integration
pmap(vars_desc2c, LGM_simple_mixture_flex,
     df_use = wide_data6, group_var = "mm")


# Create models by mode ------------------------------------------------------

# clean data for pmap
vars_desc3 <- vars_desc2 %>%
  rename(vars_use = Code, weights = weight) %>%
  mutate(categorical = ifelse(type == "cat", T, F)) %>%
  select(-Wave,-group_var2,-type) %>%
  mutate(mean_eq = F,
         var_eq = F,
         nonlin_eq = F,
         cor_eq = F)

# make combination of models
vars_desc4 <- vars_desc3 %>%
  rbind(
    vars_desc3 %>%
      mutate(
        mean_eq = T,
        var_eq = F,
        nonlin_eq = F,
        cor_eq = F
      ) %>%
      rbind(vars_desc3 %>%
              mutate(
                mean_eq = F,
                var_eq = T,
                nonlin_eq = F,
                cor_eq = F
              )) %>%
      rbind(vars_desc3 %>%
              mutate(
                mean_eq = F,
                var_eq = F,
                nonlin_eq = T,
                cor_eq = F
              )) %>%
      rbind(vars_desc3 %>%
              mutate(
                mean_eq = F,
                var_eq = F,
                nonlin_eq = F,
                cor_eq = T
              ))
  )


# run models using known classes and MLR + integration
pmap(vars_desc4, LGM_simple_mixture_flex, df_use = wide_data6)

# MplusAutomation::runModels("./mplus/flex_time/")









# import ----------------------------------------------------------



folder <- "./mplus/flex_time/"


# runModels(folder)

# read models
out_path <- list.files(folder, pattern = "\\.out", full.names = T)
res <- map(out_path, readModels)


# get indicator if they converged
# use AIC for MLR or CFI for ML
#
converged <- res %>%
  map(function(x) x$summaries %>%
        names %in% "AIC" %>% sum() > 0) %>%
  unlist()

table(converged) # 11 models did not converge firydia all and + vote1
out_path[converged == F]

# get indicator if they had warnings


warning <- res %>%
  map(function(x) x$warnings %>% map(str_detect, "SADDLE|NEGATIVE") %>%
        map(sum) %>%
        reduce(sum) > 0) %>%
  unlist()

table(warning)

# 41 warnings
out_path[converged == T][warning]


all_models <- map(res, function(x) x$summaries$Filename) %>%
  reduce(rbind) %>% as.data.frame() %>% as_tibble() %>%
  cbind(converged, warning) %>%
  tbl_df() %>% rename(Filename = V1)

filter(all_models, converged == F) %>% print(n = 25)
filter(all_models, warning == T) %>% print(n = 25)


res_s <- res[converged]

fit_tab <- map(res_s, read_fit_mlr) %>%
  reduce(rbind) %>% tbl_df()

fit_tab2 <- full_join(all_models, fit_tab, by = "Filename") %>%
  tbl_df() %>%
  mutate(name = str_remove_all(Filename, "wide_data6_|.out|mode_type_")) %>%
  separate(name, into = c("var", "group","model")) %>%
  mutate(model =
           factor(model,
                  levels = c("config", "mean", "var", "cor", "nonlin"))) %>%
  arrange(var, group, model) %>%
  group_by(var, group) %>%
  mutate(dif_bic = lag(BIC) - BIC,
         warn = ifelse(sum(warning) > 0, TRUE, FALSE)) %>%
  ungroup() %>%
  select(var, group, model, everything(), Filename) %>%
  rename_all(~str_remove_all(., "ChiSqM_"))

count(fit_tab2, model, warning)

# exclude comparisons that have models with convergence issues
fit_tab3 <- filter(fit_tab2, warn == F)

count(fit_tab3, model)



# look at some result -----------------------------------------------------

# distribution of delta bic
fit_tab3 %>%
  filter(model != "config") %>%
  # filter(dif_bic > 0) %>% # keep only positive delta bic to help reading
  mutate(model2 = as.factor(model),
         model2 = fct_relevel(model2, "mean", "var"),
         model2 = fct_recode(model2, "Mean" = "mean",
                             "Variance" = "var",
                             "Correlation" = "cor",
                             "Non-linear" = "nonlin"),
         group2 = ifelse(group == "mm", "Mode design", "Primary mode"),
         dif_bic2 = ifelse(dif_bic < 0, -1, dif_bic),
         neg = ifelse(dif_bic2 == -1, TRUE, FALSE) %>% as.factor()) %>%
  ggplot(aes(dif_bic2, fill = group2, alpha = fct_rev(neg))) +
  geom_histogram() +
  facet_wrap( ~ model2) +
  geom_vline(xintercept = 10, color = "red") +
  theme_bw() +
  labs(y = "Count", x = "Difference in BIC" , fill = "Group",
       alpha = "Negative BIC")

ggsave("./output/non-linear_BIC_dif.png", dpi = 500)



fit_tab3 %>%
  filter(model == "cor") %>%
  count(dif_bic > 10) %>%
  mutate(prop = n/sum(n))



