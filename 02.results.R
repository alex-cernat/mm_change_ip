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
map(str_c("./functions/",
          list.files("./functions/")),
    source)


# Import data -------------------------------------------------------------

load("./data/out/identifier_vars_data_charlotte.RData")
load("./data/out/overlap_vars_data_charlotte.RData")


vars_desc <- read_csv("./data/out/vars_desc.csv")

wide_data3 <- read_rds("./data/wide_data3.RDS")
wide_data5 <- read_rds("./data/clean_data.RDS")

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

