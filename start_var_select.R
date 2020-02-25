# start variable for LGM selection
# full syntax is in "master_charlotter.R"



# Import and link by wave -------------------------------------------------

# get list of variables of interest. just indresp for now
data_path <- list.files("./data/stata13/interest/",
                        full.names = T)

# import data
raw_data_list <- map(data_path, read_dta)


# code missings
data_list1 <- map(raw_data_list, function(x) {
  x %>%
    mutate_all(funs(ifelse(. < 0, NA, .)))
}
)

# make coodebook ----------------------------------------------------------

# make list of names and prop. missing in each dataset
miss_list <- map(data_list1, function(x) {
  x %>%
    summarise_all(funs(round(sum(is.na(.))/n(), 3) * 100)) %>%
    t() %>% as.data.frame() %>%
    mutate(vars = row.names(.)) %>%
    tbl_df() %>%
    rename(prop_miss = V1) %>%
    select(vars, prop_miss)
})

miss_list



## keep only variables with less than 50% missing

miss_list2 <- map(miss_list, function(x) {
  x %>%
    filter(prop_miss < 50) %>% # get rid of variables with more than 50% miss
    filter(!str_detect(vars, "_dv$")) %>% # delete dereived variables
    mutate(radical = str_remove(vars, "^[e-j]_")) %>% #make var witout prefix
    filter(!radical %in% c("pidp", "hidp", "pno", "indmode")) %>%
    filter(!str_detect(vars, "^ff_")) # delete expriment variables
})



com_vars <- miss_list2 %>%
  reduce(rbind) %>%
  count(radical) %>%
  filter(n > 2)






# Select variables of interest --------------------------------------------

i <- 1

data_list2 <- map(data_list1, function(x) {

  vars <- str_c(letters[4 + i], "_", com_vars$radical)

  vars_select <- names(data_list1[[i]])[names(data_list1[[i]]) %in% vars]

  out <- data_list1[[i]] %>%
    select(pidp, matches("hidp"), vars_select)

  i <<- i + 1

  out

})

save(data_list2, file = "./data/out/common_vars_data.RData")
