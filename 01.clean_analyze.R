#############################################################
#
# Project looking at how mode influences estimates of change
#
#
#
#############################################################

# clear working space
rm(list = ls())

# Admin -------------------------------------------------------------------

# create folders and unzip
# dir.create("./output")
# dir.create("./functions")
# dir.create("./data")
#


# load
pkg <- c("tidyverse", "haven", "lavaan", "MplusAutomation", "nnet")

sapply(pkg, library, character.only = T)

# load local functions
map(str_c("./functions/",
          list.files("./functions/")),
    source)


# Import data -------------------------------------------------------------

load("./data/out/identifier_vars_data_charlotte.RData")
load("./data/out/overlap_vars_data_charlotte.RData")


vars_desc <- read_csv("./data/out/vars_desc.csv")


# data_list3_idp includes IDS and includes final cleaned data

# check to see if they have same number of cases
map(data_list3_idp, dim)
map(data_list4_rad, dim)


# select only the variables in list 3 that are in list 4
# first get names of interest
vars_interest <- map(data_list4_rad, colnames)
vars_interest <- map(vars_interest, function(x) c("pidp", x))

# get rid of prefix and then select the variables we want
dat_list <- data_list3_idp %>%
  map(function(x) rename_all(x, ~ str_remove(., "_[a-z]"))) %>%
  map2(vars_interest, select)

# get the continuous and categorical variables
cont_vars <- vars_desc %>%
  filter(`Type of Variable` != "binary",
         `Number of Categories` != 3,
         `Number of Categories` != 4) %>%
  .[["Code"]]


cat_vars <- vars_desc$Code[!vars_desc$Code %in% cont_vars]


# bring all the data together with proper prefixes

i <- 5
wide_data <- map(dat_list, function(x) {
  dat <- x %>%
    rename_all(~str_c(., "_", i)) %>%
    rename_all(~str_replace(., str_c("pidp_", i), "pidp"))

  i <<- i + 1

  dat
}) %>%
  reduce(full_join, by = "pidp")


wide_data


# Get auxiliary info ------------------------------------------------------


# Get mode of interview

ind_data_path <-
  list.files("./data/stata13/", pattern = "[e-j]_indresp",
             full.names = T)
hh_data_path <-
  list.files("./data/stata13/", pattern = "[e-j]_hhsamp",
             full.names = T)

ind_data_list <- map(ind_data_path, read_dta)
hh_data_list <- map(hh_data_path, read_dta)

ind_vars_int <- c("hidp", "indmode")


# here we select just variables of interest, we rename them
# we code f2f answers

i <- 5
mode_data <- ind_data_list %>%
  map(function(x) {
    out <- x %>%
      select(pidp, matches(ind_vars_int)) %>%
      rename_all(~str_remove(., "[a-z]_")) %>%
      mutate(indmode2 = ifelse(indmode %in% c(-7, 2),
                               NA, indmode),
             indmode3 = ifelse(indmode == 1, -1, indmode2)) %>%
      rename_at(c("hidp", str_c("indmode", c("", "2", 3))),
                ~str_c(., "_", i))


    i <<- i + 1

    out

  }) %>%
  reduce(full_join, by = "pidp")


# there are four patterns of participation for our variables
# (see vars_desc$Wave %>% unique()):
# mode_type_all = 5678910; mode_type_no9 = 567810;
# mode_type_567 = 567; mode_type_7 = 78910

# make different groups based on patterns
mode_data <- mode_data %>%
  rowwise() %>%
  mutate(sum_mode_all = sum(indmode3_5, indmode3_6,
                            indmode3_7, indmode3_8,
                            indmode3_9, indmode3_10, na.rm = T),
         mode_type_all = case_when(sum_mode_all == -6 ~ "f2f",
                                   sum_mode_all == 18 ~ "web",
                               TRUE ~ "mixed"),
         sum_mode_no9 = sum(indmode3_5, indmode3_6,
                            indmode3_7, indmode3_8,
                            indmode3_10, na.rm = T),
         mode_type_no9 = case_when(sum_mode_no9 == -5 ~ "f2f",
                                   sum_mode_no9 == 15 ~ "web",
                                   TRUE ~ "mixed"),
         sum_mode_567 = sum(indmode3_5, indmode3_6,
                            indmode3_7, na.rm = T),
         mode_type_567 = case_when(sum_mode_567 == -3 ~ "f2f",
                                   sum_mode_567 == 9 ~ "web",
                                   TRUE ~ "mixed"),
         sum_mode_7 = sum(indmode3_7, indmode3_8,
                          indmode3_9,
                          indmode3_10, na.rm = T),
         mode_type_7 = case_when(sum_mode_7 == -4 ~ "f2f",
                                 sum_mode_7 == 12 ~ "web",
                                   TRUE ~ "mixed")) %>%
  select(pidp, matches("hidp"), matches("indmode_"),
         matches("mode_type"))



# another type of group based on most typical mode used

patterns <- list(all = 5:10,
                 no9 = c(5:8, 10),
                 p567 = 5:7,
                 p7 = 7:10)
grp_mode <- map_df(patterns, make_mode_groups)

mode_data <- cbind(mode_data, grp_mode) %>% as_tibble()


# make variable for having switched modes

mode_data <- mode_data %>%
  mutate(switch_all = ifelse(mode_type_all == "mixed", 1, 0),
         switch_no9 = ifelse(mode_type_no9 == "mixed", 1, 0),
         switch_567 = ifelse(mode_type_567 == "mixed", 1, 0),
         switch_7 = ifelse(mode_type_7 == "mixed", 1, 0))

mode_data %>% count(switch_all)



# get mode design ---------------------------------------------------------



hh_vars_int <- c("hidp", "ff_gridmode")

i <- 5
hh_data <- hh_data_list %>%
  map(function(x) {
  out <- x %>%
      select(matches(hh_vars_int)) %>%
      rename_all(~str_remove_all(., "[a-z]_ff_|[a-z]_")) %>%
      rename_all(~str_c(., "_", i)) %>%
    left_join(select(mode_data, pidp, str_c("hidp_", i))) %>%
    na.omit()

  i <<- i + 1

  out
  }) %>%
  reduce(full_join)

hh_data %>%
  count(gridmodew5_5, gridmodew5_6, gridmodew5_7, gridmodew5_8,
        gridmodew5_9, gridmodew5_10) %>%
  arrange(desc(n)) %>%
  print(n = 100)


# bring together gridmode w5 in waves 5,6 and 7

hh_data <- hh_data %>%
  mutate(gridmodew5 = gridmodew5_5,
         gridmodew5 = ifelse(is.na(gridmodew5),
                             gridmodew5_6, gridmodew5),
         gridmodew5 = ifelse(is.na(gridmodew5),
                             gridmodew5_7, gridmodew5))


# we take the mode and code inconsistent switches as missing


mode_design_switch <- hh_data %>%
  select(gridmodew5, gridmodew5_8,
         gridmodew5_9, gridmodew5_10) %>%
  apply(1, unique) %>%
  map(function(x) na.omit(x) %>% length()) %>%
  unlist()


mode_mode_design <- hh_data %>%
  select(gridmodew5, gridmodew5_8,
         gridmodew5_9, gridmodew5_10) %>%
  apply(1, function(x) {
    uniqv <- unique(x) %>% na.omit()
    uniqv[which.max(tabulate(match(x, uniqv)))]
  })




hh_data2 <- hh_data %>%
  mutate(gridmode = mode_mode_design,
         gridmode_miss = mode_design_switch,
         gridmode = ifelse(gridmode == -9 |
                             gridmode_miss ==2,
                           NA, gridmode),
         mm = case_when(gridmode == 1 ~ "sm",
                        gridmode == 3 ~ "mm")) %>%
  select(pidp, gridmode, mm)





# bring together hh and ind data

wide_data2 <- full_join(hh_data2, mode_data, by = "pidp") %>%
  full_join(wide_data, by = "pidp")


# make log to normalize for fiyrdia
wide_data2 <- wide_data2 %>%
  mutate_at(vars(matches("fiyrdia")),
            log)


# get predictors for weighting ---------------------------------------------

ind4 <- read_dta("./data/stata13/d_indresp_ip.dta")

# add wave 4 to the rest of the data and reorder
names(ind_data_list) <- 5:10
ind_data_list[["4"]] <- ind4

ind_data_list2 <- ind_data_list[order(as.numeric(names(ind_data_list)))]


nms <- ind_data_list2 %>%
  map(function(x) x %>%
        rename_all(~str_remove_all(., "^[a-z]_"))) %>%
  map(names)


vars_int <- c("pidp", "dvage",
              "hiqual_dv", "sex",
              "mastat_dv", "sf1", "gor_dv", "urban_dv",
              "hhorig", "health")

# check if variables are in all waves
map(nms, function(x) vars_int %in% x)

i <- 4
ind_data2 <- map(ind_data_list2, function(x) {
  out <- x %>%
    rename_all( ~ str_remove_all(., "^[a-z]_")) %>%
    select(vars_int) %>% # select variables of interest
    rename_all(~str_remove(., "_dv")) %>% # get rid of prefix
    mutate_all(~ifelse(. < 0, NA, .)) %>% # code missing
    rename_all(~str_c(., "_", i)) %>%
    rename_at(str_c("pidp_", i), ~ str_remove(., str_c("_", i)))


  i <<- i + 1

  out

}) %>%
  reduce(full_join, by = "pidp")


vars_int2 <- str_remove(vars_int, "_dv")

# apply function to replace missing with future wave info
ind_data3 <- map(vars_int2[2:10],
                 cum_miss_replace,
                 data = ind_data2) %>%
  reduce(cbind) %>%
  cbind(pidp = ind_data2$pidp) %>%
  tbl_df() %>%
  rename_all(~str_remove(., "_4")) %>%
  select(pidp, everything())

map(ind_data3, function(x) qplot(x))

# check the proporiton of missing
ind_data3 %>%
  mutate_all(~is.na(.)) %>%
  summarise_all(mean)



# clean variabes
ind_data4 <- ind_data3 %>%
  mutate(edu = case_when(hiqual %in% 1:2 ~ "Higher",
                         hiqual == 3 ~ "A level",
                         hiqual == 4 ~ "GCSE",
                         hiqual %in% 5:9 ~ "Other"),
         edu = as.factor(edu),
         edu = factor(edu,levels(edu)[c(3, 1, 2, 4)]),
         agecat = cut(dvage, breaks = c(15, 35, 55, 75, 102),
                      labels = c("age_35", "age_55",
                                 "age_75", "age_102")),
         female = sex - 1,
         partner = case_when(mastat %in% c(2:3, 10) ~ 1,
                             mastat %in% c(1, 4:9) ~ 0),
         sf1 = ifelse(sf1 < 1, NA, sf1),
         sf1 = as.numeric(5 - sf1),
         longill = case_when(health == 1 ~ 1,
                             health == 2 ~ 0),
         london = ifelse(gor == 7, 1, 0),
         north = ifelse(gor %in% c(1:3, 10:12), 1, 0),
         urban = ifelse(urban == 1, 1, 0),
         refresh = ifelse(hhorig == 11, 0 , 1)) %>%
  select(pidp, edu, agecat, female, partner,
         sf1, longill, london, north, urban, refresh)


# make dummies in case we need later
ind_data5 <- cbind(ind_data4,
                   dummyfy(ind_data4$edu),
                   dummyfy(ind_data4$agecat)) %>%
  tbl_df()




# create weights ----------------------------------------------------------

wide_data3 <- left_join(wide_data2, ind_data5, by = "pidp")

pred <- c("edu", "agecat", "female", "partner",
          "sf1", "longill", "london", "north",
          "urban", "refresh")

outcomes <- c("all", "no9", "p567", "p7")


pred_data <- map(outcomes,
                 logistic_prediction,
                 data = wide_data3,
                 predictors = pred) %>%
  reduce(cbind) %>% as.data.frame() %>%
  setNames(str_c("pred_", outcomes)) %>%
  tbl_df()



pred_data <- pred_data %>%
  mutate_all(list("w" = ~ 1/.)) %>%
  mutate_at(vars(matches("_w")),
            ~ ifelse(is.na(.), 0, .))

wide_data4 <- cbind(wide_data3, pred_data) %>% tbl_df()


saveRDS(wide_data3, "./data/wide_data3.RDS")

# Deal with telephone answers ---------------------------------------------


vars_to_change <- c(cont_vars, cat_vars)

# recode all variables of interest to missing in the waves people answered by
# telephone
wide_data5 <- wide_data4 %>%
  mutate_at(vars(matches(str_c(vars_to_change, "_", 6))),
            ~ ifelse(indmode_6 == 2, NA, .)) %>%
  mutate_at(vars(matches(str_c(vars_to_change, "_", 7))),
            ~ ifelse(indmode_7 == 2, NA, .)) %>%
  mutate_at(vars(matches(str_c(vars_to_change, "_", 8))),
            ~ ifelse(indmode_8 == 2, NA, .)) %>%
  mutate_at(vars(matches(str_c(vars_to_change, "_", 9))),
          ~ ifelse(indmode_9 == 2, NA, .)) %>%
  mutate_at(vars(matches(str_c(vars_to_change, "_", 10))),
          ~ ifelse(indmode_10 == 2, NA, .))




# save data

saveRDS(wide_data5, "./data/clean_data.RDS")



# Run models --------------------------------------------------------------

# vars to analyse
cont_vars
cat_vars

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
         cor_eq = F) %>%
  rename(vars_use = Code) %>%
  select(-Wave,-group_var2,-type, -group_var, -weight)

qplot(wide_data6$fiyrdia_7)
qplot(wide_data6$fiyrdia_7, wide_data6$fiyrdia_8)


# find more elegant way to do this
vars_desc2c <- rbind(vars_desc2b,
                     vars_desc2b %>%
                       mutate(
                         mean_eq = T,
                         var_eq = F,
                         cor_eq = F
                       )) %>%
  rbind(vars_desc2b %>%
          mutate(
            mean_eq = F,
            var_eq = T,
            cor_eq = F
          )) %>%
  rbind(vars_desc2b %>%
          mutate(
            mean_eq = F,
            var_eq = F,
            cor_eq = T
          ))


# run models using known classes and MLR + integration
pmap(vars_desc2c, LGM_simple_mixture,
     df_use = wide_data6, group_var = "mm")


# Create models by mode ------------------------------------------------------

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
pmap(vars_desc4, LGM_simple_mixture, df_use = wide_data6)

# MplusAutomation::runModels("./mplus/")






