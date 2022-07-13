#############################################################
#
# Project looking at how mode influences estimates of change
#
# redoing the analysis by looking at "primary web respondents" versus
# "primary face to face respondents" in the single mode
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



# change mode variable ---------------------------------------------------

# code primary face to face in the mixed mode as missing


wide_data5 <- wide_data5 %>%
  mutate(all = ifelse(mm == "mm" & all == 1, NA, all),
         no9 = ifelse(mm == "mm" & no9 == 1, NA, no9),
         p567 = ifelse(mm == "mm" & p567 == 1, NA, p567),
         p7 = ifelse(mm == "mm" & p7 == 1, NA, p7))





# no character or haven_labelled

outcomes <- c("all", "no9", "p567", "p7")

wide_data6 <- wide_data5 %>%
  mutate_at(outcomes,
            ~factor(., labels = c("f2f", "web"))) %>%
  mutate_if(is.character, as.factor) %>%
  mutate_if(is.double, as.numeric)

# vars to analyse
cont_vars <- c("finnow", "fiyrdia", "howlng", "netpuse", "scsf1", "scsf3a",
               "scsf3b", "scsf4a", "scsf4b", "scsf5", "scsf6a", "scsf6b",
               "scsf6c", "scsf7")
cat_vars <- c("aidxhh", "caruse", "finfut", "j2has", "jbhas", "jbsemp",
              "jbterm1", "lkmove", "mobuse", "scghqa", "scghqb", "scghqc",
              "scghqd", "scghqe", "scghqf", "scghqg", "scghqh", "scghqi",
              "scghqj", "scghqk", "scghql", "scsf2a", "scsf2b", "smoker",
              "vote1", "vote6", "xpmove")

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





# clean data for pmap
vars_desc3 <- vars_desc2 %>%
  rename(vars_use = Code, weights = weight) %>%
  mutate(categorical = ifelse(type == "cat", T, F)) %>%
  select(-Wave,-group_var2,-type) %>%
  mutate(mean_eq = F,
         var_eq = F,
         cor_eq = F)

# make combination of models
vars_desc4 <- vars_desc3 %>%
  rbind(
    vars_desc3 %>%
      mutate(
        mean_eq = T,
        var_eq = F,
        cor_eq = F
      ) %>%
      rbind(vars_desc3 %>%
              mutate(
                mean_eq = F,
                var_eq = T,
                cor_eq = F
              )) %>%
      rbind(vars_desc3 %>%
              mutate(
                mean_eq = F,
                var_eq = F,
                cor_eq = T
              ))
  )


# run models using known classes and MLR + integration
pmap(vars_desc4, LGM_simple_mixture, df_use = wide_data6,
     path = "./mplus/no_f2f_mm/")

# MplusAutomation::runModels("./mplus/no_f2f_mm/")





# run and import ----------------------------------------------------------



folder <- "./mplus/no_f2f_mm/"

# read models
out_path <- list.files(folder, pattern = "\\.out", full.names = T)
res_s <- map(out_path, readModels)


# get indicator if they converged
# use AIC for MLR or CFI for ML
#
converged_s <- res_s %>%
  map(function(x) x$summaries %>%
        names %in% "AIC" %>% sum() > 0) %>%
  unlist()


table(converged_s) # 6 models did not converge and were excluded
out_path[converged_s == F]

# get indicator if they had warnings


warning_s <- res_s %>%
  map(function(x) x$warnings %>% map(str_detect, "SADDLE|NEGATIVE") %>%
        map(sum) %>%
        reduce(sum) > 0) %>%
  unlist()


# 8 models with warnings
table(converged_s, warning_s)

all_models_s <- map(res_s, function(x) x$summaries$Filename) %>%
  reduce(rbind) %>%
  as.data.frame() %>%
  as_tibble() %>%
  cbind(converged_s, warning_s) %>%
  as_tibble() %>%
  rename(Filename = V1)

filter(all_models_s, converged_s == F) %>% print(n = 25)
filter(all_models_s, warning_s == T) %>% print(n = 25)


res_small_s <- res_s[converged_s]

fit_tab_s <- map(res_small_s, read_fit_mlr) %>%
  reduce(rbind) %>% tbl_df()

fit_tab2_s <- full_join(all_models_s, fit_tab_s, by = "Filename") %>%
  as_tibble() %>%
  mutate(name = str_remove_all(Filename,
                               "wide_data6_|.out")) %>%
  separate(name, into = c("var", "group","model")) %>%
  mutate(model =
           factor(model, levels = c("config", "mean", "var", "cor"))) %>%
  arrange(var, group, model) %>%
  group_by(var, group) %>%
  mutate(dif_bic = lag(BIC) - BIC,
         warn = ifelse(sum(warning_s) > 0, TRUE, FALSE)) %>%
  ungroup() %>%
  select(var, group, model, everything(), Filename) %>%
  rename_all(~str_remove_all(., "ChiSqM_"))



# exclude models with convergence issues
fit_tab3_s <- filter(fit_tab2_s, warn == F)


# look at some result -----------------------------------------------------

# distribution of delta bic
fit_tab3_s %>%
  filter(model != "config") %>%
  filter(dif_bic > 0) %>% # keep only positive delta bic to help reading
  mutate(model2 = as.factor(model),
         model2 = fct_relevel(model2, "mean", "var"),
         model2 = fct_recode(model2, "Mean" = "mean",
                             "Variance" = "var",
                             "Correlation" = "cor"),
         group2 = ifelse(group == "mm", "Mode design", "Primary mode")) %>%
  ggplot(aes(dif_bic, fill = group2)) +
  geom_histogram() +
  facet_wrap( ~ model2) +
  geom_vline(xintercept = 10, color = "red") +
  theme_bw() +
  labs(y = "Count", x = "Difference in BIC" , fill= "Group")

ggsave("./output/sensitivity_BIC_dif_nowebmm.png", dpi = 500)




fit_tab3_s %>%
  filter(model == "cor") %>%
  na.omit() %>%
  count(dif_bic > 10) %>%
  mutate(prop = n/sum(n))
