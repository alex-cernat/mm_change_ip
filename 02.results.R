#############################################################
#
# Project looking at how mode influences estimates of change
#
#
#
#############################################################

# clear working space
rm(list = ls())



# use local packages on work machine
if (Sys.getenv("USERNAME") == "msassac6") {.libPaths(c(
  paste0(
    "C:/Users/",
    Sys.getenv("USERNAME"),
    "/Dropbox (The University of Manchester)/R/package"
  ),
  .libPaths()[-1]
))}



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


# average % only web for "primary web respondents"
# > (0.57+0.64+0.62+0.61) /4
# [1] 0.61

# average % only face to face for "primary face to face respondents"
# > (0.73 + 0.7 + 0.71 + 0.72)/4
# [1] 0.715




wide_data5$mod

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



trans_data2$indmode_5 == trans_data2$indmode_6 ==  trans_data2$indmode_7

trans_data2 %>%
  summarise_all(funs(n_distinct(.)))

# run and import ----------------------------------------------------------



# # run in different folder for space issues
folder <- "./mplus/"

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

tibble(models = out_path,
       converged = converged) %>%
  View()

table(converged) # 8 models did not converge


# get indicator if they had warnings


warning <- res %>%
  map(function(x) x$warnings %>% map(str_detect, "SADDLE") %>%
        map(sum) %>%
        reduce(sum) > 0) %>%
  unlist()

table(warning)

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



# exclude models with convergence issues
fit_tab3 <- filter(fit_tab2, warn == F)





# look at some result -----------------------------------------------------

# distribution of delta bic
fit_tab3 %>%
  filter(model != "config") %>%
  filter(dif_bic > 0) %>% # keep only positive delta bic to help reading
  ggplot(aes(dif_bic)) +
  geom_density() +
  facet_wrap( ~ model, scales = "free") +
  geom_vline(xintercept = 10, color = "red")

# what models show differences?
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


get_vars(res[[4]])

# give names to list to make it easier to select
names(res_s) <- fit_tab$Filename



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
  filter(var != "jbhas") %>% # largest diffence
  filter(var != "mobuse") %>% # largest diffence
  filter(var != "jbterm1") %>% # largest diffence
#  filter(var != "vote1") %>% # largest diffence
  mutate(coef2 = ifelse(coef == "cor", "Correlation", "Variance intercept")) %>%
  ggplot(aes(fct_reorder(var, est), est, color = modes)) +
  geom_point(position = position_dodge(), alpha = 0.8, size = 2.5) +
  facet_grid(grp ~ coef2, scales = "free") +
  coord_flip() +
  labs(y = "Estimates", x = "Variables",
       color = "Group") +
  theme(text = element_text(size = 16))




# Save data ---------------------------------------------------------------

# save all the tables
write_csv(fit_tab3, "./output/fit_tab1_mlr.csv")
write_csv(sig_vars2, "./output/sig_vars.csv")

