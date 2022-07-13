#############################################################
#
# Project looking at how mode influences estimates of change
#
#
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



# run only cases in 1 mode ---------------------------------------------------



sdata <- wide_data5 %>%
  select(pidp, mm, all, no9, p567, p7, matches("indmode")) %>%
  filter(!is.na(mm))

sdata2 <- sdata %>%
  mutate_at(vars(matches("indmode")),
            ~ case_when(. == 1 ~ "F2F",
                        . == 3 ~ "Web")) %>%
  mutate_at(vars(matches("indmode")),
            list("miss" = ~ ifelse(is.na(.), 1, 0))) %>%
  mutate(nrm = indmode_5_miss + indmode_6_miss + indmode_7_miss +
           indmode_8_miss + indmode_9_miss + indmode_10_miss) %>%
  filter(nrm < 5) %>%
  select(-matches("miss")) %>%
  mutate_at(vars(matches("indmode")),
            list("f2f" = ~ ifelse(. == "F2F", 1, 0))) %>%
  rowwise() %>%
  mutate(f2f_prop = sum(indmode_5_f2f, indmode_6_f2f, indmode_7_f2f,
                        indmode_8_f2f, indmode_9_f2f, indmode_10_f2f,
                        na.rm = T)/(6 - nrm),
         stabil = ifelse(f2f_prop %in% c(0, 1), 1, 0))




# make new mode variables where we analyse only cases that responded in only one
# mode


sdata2 %>%
  group_by(mm) %>%
  count(f2f_prop, stabil) %>%  na.omit() %>%
  mutate(prop = round(n/sum(n), 2)) %>% print(n = 50)



sdata2 %>%
  filter(stabil == 1) %>%
  group_by(all) %>%
  count(f2f_prop) %>%
  mutate(prop = round(n/sum(n), 2)) %>% print(n = 50)

sdata2 %>%
  group_by(no9) %>%
  count(f2f_prop) %>%
  mutate(prop = round(n/sum(n), 2)) %>% print(n = 50)


sdata2 %>%
  group_by(p567) %>%
  count(f2f_prop) %>% na.omit() %>%
  mutate(prop = round(n/sum(n), 2)) %>% print(n = 50)


sdata2 %>%
  group_by(p7) %>%
  count(f2f_prop) %>%  na.omit() %>%
  mutate(prop = round(n/sum(n), 2)) %>% print(n = 50)

sdata2 %>%
  group_by(all) %>%
  count(f2f_prop) %>%  na.omit() %>%
  mutate(prop = round(n/sum(n), 2)) %>% print(n = 50)


# save new variable for stable answer and prop_f2f
# code missing cases that are not stable

stabil_dat <- select(sdata2, pidp, f2f_prop, stabil)


wide_data5_stabil <- left_join(wide_data5, stabil_dat, by = "pidp") %>%
  mutate_at(vars(all, no9, p567, p7),
            ~ifelse(stabil == 1, NA, .))

count(wide_data5_stabil, all)
count(wide_data5_stabil, no9)
count(wide_data5_stabil, p567)
count(wide_data5_stabil, p7)




# prepare data for Mplus
# no character or haven_labelled

outcomes <- c("all", "no9", "p567", "p7")

wide_data6_stabil <- wide_data5_stabil %>%
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
# pmap(vars_desc4, LGM_simple_mixture, df_use = wide_data6_stabil)

# MplusAutomation::runModels("./mplus/")





# run and import ----------------------------------------------------------



folder <- "./mplus/stable/"

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


table(converged_s) # 9 models did not converge and were excluded
out_path[converged_s == F]

# get indicator if they had warnings


warning_s <- res_s %>%
  map(function(x) x$warnings %>% map(str_detect, "SADDLE|NEGATIVE") %>%
        map(sum) %>%
        reduce(sum) > 0) %>%
  unlist()


# 16 models with warnings
table(converged_s, warning_s)

all_models_s <- map(res_s, function(x) x$summaries$Filename) %>%
  reduce(rbind) %>% as.data.frame() %>% tbl_df() %>%
  cbind(converged_s, warning_s) %>%
  tbl_df() %>% rename(Filename = V1)

filter(all_models_s, converged_s == F) %>% print(n = 25)
filter(all_models_s, warning_s == T) %>% print(n = 25)


res_small_s <- res_s[converged_s]

fit_tab_s <- map(res_small_s, read_fit_mlr) %>%
  reduce(rbind) %>% tbl_df()

fit_tab2_s <- full_join(all_models_s, fit_tab_s, by = "Filename") %>%
  tbl_df() %>%
  mutate(name = str_remove_all(Filename,
                               "wide_data6_stabil_|.out|mode_type_")) %>%
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
fit_tab3 %>%
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

ggsave("./output/sensitivity_BIC_dif.png", dpi = 500)




# what models show differences?
fit_tab3_s %>%
  filter(warn == F) %>%
  filter(dif_bic > 10) %>%
  count(group, model)

fit_tab3_s %>%
  filter(warn == F) %>%
  filter(dif_bic > 10) %>%
  View()



# how big is the difference in coefficients?

# function that get variance and covariance
get_vars_unstand <- function(output) {
  output$parameters$unstandardized %>%
    tbl_df() %>%
    filter(paramHeader %in% c("Variances")) %>%
    mutate(nms = str_c(paramHeader, ".", param)) %>%
    select(nms, est, LatentClass) %>%
    pivot_wider(
      id_cols = nms,
      names_from = LatentClass,
      values_from = est,
      names_prefix = "est_"
    )

}



get_vars_stand <- function(output) {
  output$parameters$stdyx.standardized %>%
    tbl_df() %>%
    filter(paramHeader %in% c("S.WITH")) %>%
    mutate(nms = str_c(paramHeader, ".", param)) %>%
    select(nms, est, LatentClass) %>%
    pivot_wider(
      id_cols = nms,
      names_from = LatentClass,
      values_from = est,
      names_prefix = "est_"
    )

}




# see what models we want

fit_tab4_s <- fit_tab3_s %>%
  mutate(sig_dif = lead(dif_bic > 10))

sig_models_s <- fit_tab4_s %>%
  filter(fit_tab4_s$sig_dif) %>%
  .[["Filename"]]


# give names to list to make it easier to select
names(res_small_s) <- fit_tab_s$Filename


res_small_s[sig_models_s][1] %>%
  map(get_vars_unstand)



sig_vars_s <- map_df(sig_models_s, function(x) res_small_s[[x]] %>%
                     get_vars_unstand()) %>%
  rbind(map_df(sig_models_s, function(x) res_small_s[[x]] %>%
                 get_vars_stand())) %>%
  mutate(nms = c(rep(sig_models_s, each = 2), sig_models_s),
         coef = c(rep(c("i", "s"), length(sig_models_s)),
                  rep("cor", length(sig_models_s)))) %>%
  arrange(nms)


sig_vars2_s <- sig_vars_s %>%
  mutate(name = str_remove_all(nms, "wide_data6_stabil_|.out|mode_type_")) %>%
  separate(name, into = c("var", "group","model")) %>%
  gather(-nms, -var, -group, -model, -coef, value = est, key = mode) %>%
  mutate(grp = ifelse(group == "mm", "Mode design", "Mode"),
         modes = case_when(group == "mm" & mode == "est_1" ~ "Mixed mode",
                           group == "mm" & mode == "est_2" ~ "Single mode",
                           group != "mm" & mode == "est_1" ~ "Face to face",
                           group != "mm" & mode == "est_2" ~ "Web"),
         modes = as.factor(modes),
         modes = fct_relevel(modes, "Face to face", "Web"))


sig_vars2_s %>%
  filter(coef != "s") %>%
  mutate(coef2 = ifelse(coef == "cor",
                        "Correlation", "Variance intercept")) %>%
  group_by(grp, coef2, modes) %>%
  summarise(mean(est))

# biggest differences
sig_vars2_s %>%
  filter(coef != "s") %>%
  mutate(coef2 = ifelse(coef == "cor",
                        "Correlation", "Variance intercept")) %>%
  select(grp, var, coef2, modes, est) %>%
  arrange(grp, var, coef2, modes)

sig_vars2_s %>%
  filter(coef != "s") %>%
  filter(var != "jbhas") %>% # largest difference
  filter(var != "mobuse") %>% # largest difference
  filter(var != "jbterm1") %>% # largest difference
  #  filter(var != "vote1") %>% # largest difference
  mutate(coef2 = ifelse(coef == "cor", "Correlation", "Variance intercept")) %>%
  ggplot(aes(fct_reorder(var, est), est, color = modes)) +
  geom_point(position = position_dodge(), alpha = 0.8, size = 2.5) +
  facet_grid(grp ~ coef2, scales = "free") +
  coord_flip() +
  labs(y = "Estimates", x = "Variables",
       color = "Group") +
  theme(text = element_text(size = 16))




