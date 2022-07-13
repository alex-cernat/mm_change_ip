#############################################################
#
# Project looking at how mode influences estimates of change
#
# do descriptives
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



# variables ---------------------------------------------------


# vars to analyse
cont_vars <- c("finnow", "fiyrdia", "howlng", "netpuse", "scsf1", "scsf3a",
               "scsf3b", "scsf4a", "scsf4b", "scsf5", "scsf6a", "scsf6b",
               "scsf6c", "scsf7")
cat_vars <- c("aidxhh", "caruse", "finfut", "j2has", "jbhas", "jbsemp",
              "jbterm1", "lkmove", "mobuse", "scghqa", "scghqb", "scghqc",
              "scghqd", "scghqe", "scghqf", "scghqg", "scghqh", "scghqi",
              "scghqj", "scghqk", "scghql", "scsf2a", "scsf2b", "smoker",
              "vote1", "vote6", "xpmove")



# desc cont ---------------------------------------------------------------

cont_data <- wide_data5 %>%
  select(matches(cont_vars))

cont_data %>%
  select(matches(cont_vars)) %>%
  mutate_at(vars(starts_with("fiyrdia")),
            ~ifelse(is.infinite(.), NA, .)) %>%
  summarise_all(list(mean = ~mean(., na.rm = T),
                     sd = ~sd(., na.rm = T),
                     prop.miss = ~mean(is.na(.)) * 100)) %>%
  pivot_longer(cols = everything(),
               names_sep = "_",
               names_to = c("Variable", "wave", "Statistic")) %>%
  mutate(value = round(value, 2),
         wave = str_c("Wave ", wave)) %>%
  pivot_wider(id_cols = c(Variable, Statistic),
              names_from = wave,
              values_from = value) %>%
  arrange(Variable, Statistic) %>%
  write_csv("./output/cont_desc.csv")


# desc cat ----------------------------------------------------------------


desc_tab()

cat_desc <- wide_data5 %>%
  select(matches(cat_vars)) %>%
  sum_tab() %>%
  setNames(c("Variable", "Code", "Freq", "Perc")) %>%
  separate(Variable, into = c("Variable", "Wave"), sep = "_") %>%
  pivot_wider(id_cols = c(Variable, Code),
              names_from = Wave,
              values_from = c(Freq, Perc)) %>%
    select(Variable, Code, str_c(rep(c("Freq_", "Perc_"), 6),
                                 rep(5:10, each = 2))) %>%
  mutate(Code = as.numeric(Code))


cat_label <- read_csv("./data/cat_label.csv") %>%
  group_by(Variable) %>%
  mutate(Code = row_number(),
         code_max = max(Code),
         Code = ifelse(code_max == 2, Code - 1, Code)) %>%
  select(-code_max)


cat_desc2 <- left_join(cat_desc, cat_label, by = c("Variable", "Code")) %>%
  select(Variable, label, everything(), -Code) %>%
  mutate(label = ifelse(is.na(label), "Missing", label)) %>%
  rename(Label = label)


write_csv(cat_desc2, "./output/cat_desc.csv")

