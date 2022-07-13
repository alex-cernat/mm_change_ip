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


# Look at change patterns -------------------------------------------------



sdata <- wide_data5 %>%
  select(pidp, mm, all, no9, p567, p7, matches("indmode")) %>%
  filter(!is.na(mm))

count(sdata, mm)
count(sdata, mm, all) %>% na.omit() %>% mutate(prop = n/sum(n))
count(sdata, mm, no9) %>% na.omit() %>% mutate(prop = n/sum(n))
count(sdata, mm, p567) %>% na.omit() %>% mutate(prop = n/sum(n))
count(sdata, mm, p7) %>% na.omit() %>% mutate(prop = n/sum(n))





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

sdata2 %>%
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


sdata2 %>%
  group_by(mm) %>%
  count(f2f_prop) %>%  na.omit() %>%
  mutate(prop = round(n/sum(n), 2)) %>% print(n = 50)



trans_data <- count(wide_data5, indmode_5, indmode_6, indmode_7,
      indmode_8, indmode_9, indmode_10) %>%
  mutate_at(vars(matches("indmode")),
            ~ case_when(. == 1 ~ "F2F",
                         . == 3 ~ "Web")) %>%
  arrange(desc(n))


trans_data2 <- trans_data %>%
  mutate_at(vars(matches("indmode")),
            list("miss" = ~ ifelse(is.na(.), 1, 0))) %>%
  mutate(nrm = indmode_5_miss + indmode_6_miss + indmode_7_miss +
           indmode_8_miss + indmode_9_miss + indmode_10_miss) %>%
  filter(nrm < 5) %>%
  select(-matches("miss"))

trans_data2 %>%
  mutate_at(vars(matches("indmode")),
            list("f2f" = ~ ifelse(. == "F2F", 1, 0))) %>%
  rowwise() %>%
  mutate(f2f_prop = sum(indmode_5_f2f, indmode_6_f2f, indmode_7_f2f,
                         indmode_8_f2f, indmode_9_f2f, indmode_10_f2f,
                        na.rm = T)/(6 - nrm),
         stabil = ifelse(f2f_prop %in% c(0, 1), 1, 0)) %>%
  group_by(as.factor(f2f_prop)) %>%
  summarise(sum(n))



trans_data2 %>%
  summarise_all(~n_distinct(.))

# run and import ----------------------------------------------------------



folder <- "./mplus/main_results/"


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

table(converged)
out_path[converged == F]

# get indicator if they had warnings


warning <- res %>%
  map(function(x) x$warnings %>% map(str_detect, "SADDLE|NEGATIVE") %>%
        map(sum) %>%
        reduce(sum) > 0) %>%
  unlist()

table(warning)


out_path[converged == T][warning]


all_models <- map(res, function(x) x$summaries$Filename) %>%
  reduce(rbind) %>% as.data.frame() %>% tbl_df() %>%
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
           factor(model, levels = c("config", "mean", "var", "cor"))) %>%
  arrange(var, group, model) %>%
  group_by(var, group) %>%
  mutate(dif_bic = lag(BIC) - BIC,
         warn = ifelse(sum(warning) > 0, TRUE, FALSE)) %>%
  ungroup() %>%
  select(var, group, model, everything(), Filename) %>%
  rename_all(~str_remove_all(., "ChiSqM_"))



# exclude comparisons that have models with convergence issues
fit_tab3 <- filter(fit_tab2, warn == F)





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
  geom_vline(xintercept = 10, size = 0.8) +
  theme_bw() +
  labs(y = "Count", x = "Difference in BIC" , fill = "Group") +
  scale_fill_grey()

ggsave("./output/BIC_dif.png", dpi = 500)


# what models show differences?
fit_tab3 %>% count(model, dif_bic > 10)
fit_tab3 %>% count(group, model, dif_bic > 10) %>% na.omit()

fit_tab3 %>%
  filter(warn == F) %>%
  filter(dif_bic > 10) %>%
  count(group, model)

fit_tab3 %>%
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

fit_tab4 <- fit_tab3 %>%
  mutate(sig_dif = lead(dif_bic > 10))

sig_models <- fit_tab4 %>%
  filter(fit_tab4$sig_dif) %>%
  .[["Filename"]]


# give names to list to make it easier to select
names(res_s) <- fit_tab$Filename


res_s$wide_data6_aidxhh_mm_var.out %>% get_vars_stand()


res_s[sig_models][1] %>%
  map(get_vars_unstand)



sig_vars <- map_df(sig_models, function(x) res_s[[x]] %>%
                     get_vars_unstand()) %>%
  rbind(map_df(sig_models, function(x) res_s[[x]] %>%
                 get_vars_stand())) %>%
  mutate(nms = c(rep(sig_models, each = 2), sig_models),
         coef = c(rep(c("i", "s"), length(sig_models)),
                  rep("cor", length(sig_models)))) %>%
  arrange(nms)


sig_vars2 <- sig_vars %>%
  mutate(name = str_remove_all(nms, "wide_data6_|.out|mode_type_")) %>%
  separate(name, into = c("var", "group","model")) %>%
  gather(-nms, -var, -group, -model, -coef, value = est, key = mode) %>%
  mutate(grp = ifelse(group == "mm", "Mode design", "Mode"),
         modes = case_when(group == "mm" & mode == "est_1" ~ "Mixed mode",
                           group == "mm" & mode == "est_2" ~ "Single mode",
                           group != "mm" & mode == "est_1" ~ "Face to face",
                           group != "mm" & mode == "est_2" ~ "Web"),
         modes = as.factor(modes),
         modes = fct_relevel(modes, "Face to face", "Web"))


sig_vars2 %>%
  filter(coef != "s") %>%
  mutate(coef2 = ifelse(coef == "cor",
                        "Correlation", "Variance intercept")) %>%
  group_by(grp, coef2, modes) %>%
  summarise(mean(est))

# biggest differences
sig_vars2 %>%
  filter(coef != "s") %>%
  mutate(coef2 = ifelse(coef == "cor",
                        "Correlation", "Variance intercept")) %>%
  select(grp, var, coef2, modes, est) %>%
  arrange(grp, var, coef2, modes)

sig_vars2 %>%
  filter(coef != "s") %>%
  filter(var != "jbhas") %>% # largest difference
  filter(var != "mobuse") %>% # largest difference
  filter(var != "jbterm1") %>% # largest difference
#  filter(var != "vote1") %>% # largest difference
  mutate(coef2 = ifelse(coef == "cor",
                        "Correlation", "Variance intercept"),
         grp = fct_rev(grp),
         coef2 = fct_rev(coef2),
         modes = fct_relevel(modes, "Mixed mode", "Single mode", "Web")) %>%
  ggplot(aes(fct_reorder(var, est), est,
             shape = modes)) +
  geom_point(position = position_dodge(), alpha = 0.8, size = 2.5) +
  geom_hline(yintercept = 0, size = 0.8) +
  facet_grid(grp ~ coef2, scales = "free") +
  coord_flip() +
  labs(y = "Estimates", x = "Variables",
       color = "Group", shape = "Group") +
  theme(text = element_text(size = 16)) +
  theme_bw() +
  scale_color_grey() +
  scale_shape_manual(values = c(15, 16, 17, 18))


ggsave("./output/dif_coeficients.png", dpi = 500, height = 8)



# bias in change estimates ------------------------------------------------


output <- res_s[[1]]

# function that get variance and covariance
get_change_unstand <- function(output) {
  output$parameters$unstandardized %>%
    tbl_df() %>%
    filter(param == "S") %>%
    mutate(nms = str_c(paramHeader, ".", param)) %>%
    select(nms, est, LatentClass) %>%
    pivot_wider(
      id_cols = nms,
      names_from = LatentClass,
      values_from = est,
      names_prefix = "est_"
    )

}



get_change_stand <- function(output) {
  output$parameters$stdyx.standardized %>%
    tbl_df() %>%
    filter(param == "S") %>%
    mutate(nms = str_c(paramHeader, ".", param)) %>%
    select(nms, est, LatentClass) %>%
    pivot_wider(
      id_cols = nms,
      names_from = LatentClass,
      values_from = est,
      names_prefix = "est_"
    )

}







change_sig_var <- map_df(sig_models, function(x) res_s[[x]] %>%
                     get_change_unstand()) %>%
  rbind(map_df(sig_models, function(x) res_s[[x]] %>%
                 get_change_stand())) %>%
  mutate(nms = c(rep(sig_models, each = 2), rep(sig_models, each = 2)),
         type = c(rep("Unstandardized", length(sig_models) * 2),
                  rep("Standardized", length(sig_models) * 2)),
         coef = c(rep(c("Mean", "Variance"),
                      length(sig_models) * 2)),
         grp = ifelse(str_detect(nms, "mm"), "Mode design", "Mode"),
         var = str_remove_all(nms,
                              "wide_data6_|_mm|_all|_no9|_p567|_var.out"),
         model = "Eq variance") %>%
  arrange(nms)


sig_models_cor <- str_replace(sig_models, "_var.out", "_cor.out")

change_sig_cor <- map_df(sig_models_cor, function(x) res_s[[x]] %>%
                           get_change_unstand()) %>%
  rbind(map_df(sig_models_cor, function(x) res_s[[x]] %>%
                 get_change_stand())) %>%
  mutate(nms = c(rep(sig_models_cor, each = 2), rep(sig_models_cor, each = 2)),
         type = c(rep("Unstandardized", length(sig_models_cor) * 2),
                  rep("Standardized", length(sig_models_cor) * 2)),
         coef = c(rep(c("Mean", "Variance"),
                      length(sig_models_cor) * 2)),
         grp = ifelse(str_detect(nms, "mm"), "Mode design", "Mode"),
         var = str_remove_all(nms,
                              "wide_data6_|_mm|_all|_no9|_p567|_cor.out"),
         model = "Eq corr") %>%
  arrange(nms)


change_diff <- left_join(select(change_sig_var, var, coef, type, grp, est_1),
          select(change_sig_cor, var, coef, type, grp, est_2),
          by = c("var", "coef", "type", "grp")) %>%
  rename(est_var = est_1, est_cor = est_2) %>%
  mutate(dif = abs(est_var) - abs(est_cor))

change_diff %>%
  filter(type == "Unstandardized") %>%
  gather(value = value, key = key, -var, -coef, -type,- grp, -dif) %>%
  mutate(key2 = ifelse(key == "est_var", "Different correlation",
                       "Equal corelation") %>%
           as.factor() %>% fct_rev()) %>%
  ggplot(aes(value, fct_reorder(var, dif, .desc = T),
             color = key2, shape = key2)) +
  geom_line(aes(group = var, linetype = "Bias"), size = 1) +
  geom_point(size = 3) +
  facet_grid(coef~grp, scales = "free_x") +
  theme_bw() +
  labs(x = "Value",
       y = "Variable",
       color = "Assumption",
       shape = "Assumption",
       linetype = "") +
  scale_color_grey()

ggsave("./output/change_bias.png", dpi = 500, height = 8)


change_diff %>%
  filter(type == "Unstandardized") %>%
  group_by(coef) %>%
  summarise(mean_cor = mean(abs(est_cor)),
            mean_var = mean(abs(est_var)),
            mean_dif = mean(abs(dif)))

change_diff %>%
  filter(type == "Unstandardized") %>%
  filter(var == "caruse")


#
# sig_vars2 <- sig_vars %>%
#   mutate(name = str_remove_all(nms, "wide_data6_|.out|mode_type_")) %>%
#   separate(name, into = c("var", "group","model")) %>%
#   gather(-nms, -var, -group, -model, -coef, value = est, key = mode) %>%
#   mutate(grp = ifelse(group == "mm", "Mode design", "Mode"),
#          modes = case_when(group == "mm" & mode == "est_1" ~ "Mixed mode",
#                            group == "mm" & mode == "est_2" ~ "Single mode",
#                            group != "mm" & mode == "est_1" ~ "Face to face",
#                            group != "mm" & mode == "est_2" ~ "Web"),
#          modes = as.factor(modes),
#          modes = fct_relevel(modes, "Face to face", "Web"))
#
#
# sig_vars2 %>%
#   filter(coef != "s") %>%
#   mutate(coef2 = ifelse(coef == "cor",
#                         "Correlation", "Variance intercept")) %>%
#   group_by(grp, coef2, modes) %>%
#   summarise(mean(est))
#


# give example of bias from caruse comparing modes ------------------------

#waves 5, 6, 7, 8
# results assuming different correlation:
# means: i = -1.114, s = 0.257
# var: i = 28.066, s = 0.365

# results allowing for equal correlations
# means: i = -1.765, s = -0.294
# var: i = 50.467, s = 0.641

dif_cor <- data.frame(i = rnorm(1000, -1.114, sqrt(28.066)),
                        s = rnorm(1000, 0.257, sqrt(0.365)))

equal_cor <- data.frame(i = rnorm(1000, -1.765, sqrt(50.467)),
                      s = rnorm(1000, -0.294, sqrt(0.641)))


long_equal_cor <- map(0:3, # loop over time
                     function(x) equal_cor[, 1] + x * equal_cor[, 2]) %>%
  reduce(cbind) %>% # bring together the wave predictions
  as.data.frame() %>% # make data frame
  setNames(str_c("Wave ", 5:8)) %>% # give names to variables
  mutate(id = row_number()) %>% # make unique id
  gather(-id, key = wave, value = pred) %>% # make long format
  mutate(group = "Equal correlation")


long_dif_cor <- map(0:3, # loop over time
                      function(x) dif_cor[, 1] + x * dif_cor[, 2]) %>%
  reduce(cbind) %>% # bring together the wave predictions
  as.data.frame() %>% # make data frame
  setNames(str_c("Wave ", 5:8)) %>% # give names to variables
  mutate(id = 1000 + row_number()) %>% # make unique id
  gather(-id, key = wave, value = pred) %>% # make long format
  mutate(group = "Different correlation")

sim_data <- rbind(long_equal_cor, long_dif_cor) %>%
  as_tibble()

sim_data %>%
  mutate(group = as.factor(group)) %>%
  ggplot(aes(wave, pred,
                     group = group, color = group, linetype = group)) +
  geom_smooth(method = "lm") +
  theme_bw() +
  scale_color_grey() +
  labs(x = "Time", y = "Predicted caruse (logit scale)",
       color = "Model assumption", linetype = "Model assumption")


ggsave("./output/example_bias_change.png", dpi = 500)



# Save data ---------------------------------------------------------------

# save all the tables
write_csv(fit_tab3, "./output/fit_tab1_mlr.csv")
write_csv(sig_vars2, "./output/sig_vars.csv")






