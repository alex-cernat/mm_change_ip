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

# folders for installed packages
#.libPaths(c(paste0("C:/Users/", Sys.getenv("USERNAME"), "/Dropbox (The University of Manchester)/R/package"),  .libPaths()))



# create folders and unzip
# dir.create("./output")
# dir.create("./functions")
# dir.create("./data")
#


# install packages and load
#install.packages("tidyverse")
#library(tidyverse)
#install.packages("haven")
#library(haven)
#install.packages("sjstats")
#library(sjstats)


# use packrat to install packages locally

pkg <- c("tidyverse",  "haven", "sjstats")

sapply(pkg, library, character.only = T)

# load local functions
map(str_c("./functions/",
          list.files("./functions/")),
    source)



# Import and link by wave ---------------------------

#set working directory

#store working directory

wd <- getwd()

# get list of variables of interest. just indresp for now
data_path <- list.files("./data/stata13/interest/",
                        full.names = T)

# import data
raw_data_list <- map(data_path, read_dta)


# code missings
data_list1 <- map(raw_data_list, function(x) {
  x %>%
    mutate_all(funs(ifelse(. < 0, NA, .)))
})

# make codebook ----------------------------------------------------------

setwd(wd)

# make list of names and prop. missing in each dataset
miss_list <- map(data_list1, function(x) {
  x %>%
    summarise_all(funs(round(sum(is.na(
      .
    )) / n(), 3) * 100)) %>%
    t() %>% as.data.frame() %>%
    mutate(vars = row.names(.)) %>%
    tbl_df() %>%
    rename(prop_miss = V1) %>%
    select(vars, prop_miss)
})


## keep only variables with less than 50% missing

miss_list2 <- map(miss_list, function(x) {
  x %>%
    filter(prop_miss < 50) %>% # get rid of variables with more than 50% miss
    filter(!str_detect(vars, "_dv$")) %>% # delete derived variables
    mutate(radical = str_remove(vars, "^[e-j]_")) %>% #make var witout prefix
    filter(!radical %in% c("pidp", "hidp", "pno", "indmode")) %>%
    filter(!str_detect(vars, "_ff_")) # delete expriment variables
})

rm(miss_list)

com_vars <- miss_list2 %>%
  reduce(rbind) %>%
  count(radical) %>%
  filter(n > 2)

rm(miss_list2)


# Select variables of interest ------------------

i <- 1

data_list2 <- map(data_list1, function(x) {
  vars <- str_c(letters[4 + i], "_", com_vars$radical)

  vars_select <-
    names(data_list1[[i]])[names(data_list1[[i]]) %in% vars]

  out <- data_list1[[i]] %>%
    select(pidp, matches("hidp"), vars_select)

  i <<- i + 1

  out

})

# recode dummies (1/2) to dummies (0/1), where 1=Yes, 2=No

## recode lkmove, which is coded the opposite way (2=Yes, 1=No)
### produces an error but still works

for (i in 1:6) {
  j <- i + 4
  data_list2[[i]][[str_c(letters[j], "_lkmove")]][data_list2[[i]][[str_c(letters[j], "_lkmove")]] ==
                                                    1] <- 0
  data_list2[[i]][[str_c(letters[j], "_lkmove")]][data_list2[[i]][[str_c(letters[j], "_lkmove")]] ==
                                                    2] <- 1
}

for (i in 1:6) {
  for (j in 1:ncol(data_list2[[i]])) {
    if (max(data_list2[[i]][, j], na.rm = TRUE) == 2 &&
        min(data_list2[[i]][, j], na.rm = TRUE) == 1) {
      data_list2[[i]][, j][data_list2[[i]][, j] == 2] <- 0
    }
  }
}

# recode finfut (finfut has categories 1=better off, 2=worse off, 3=same, which I will recode to 1=better off, 2=same, 3=worse off)

for (i in 1:6) {
  j <- i + 4
  data_list2[[i]][[str_c(letters[j], "_finfut")]][data_list2[[i]][[str_c(letters[j], "_finfut")]] ==
                                                    3] <- 4
  data_list2[[i]][[str_c(letters[j], "_finfut")]][data_list2[[i]][[str_c(letters[j], "_finfut")]] ==
                                                    2] <- 3
  data_list2[[i]][[str_c(letters[j], "_finfut")]][data_list2[[i]][[str_c(letters[j], "_finfut")]] ==
                                                    4] <- 2
}

save(data_list2, file = "./data/out/common_vars_data_charlotte.RData")


# Delete nominal variables and variables that are not of interest-------------

#subtract the prefixes
ej <- letters[5:10]

data_list2_rad <- data_list2

for (i in 1:6) {
  names(data_list2_rad[[i]]) <-
    sub(str_c(ej[i], "_"), '', names(data_list2_rad[[i]]))
}

## delete from com_vars

## undesired variables

patterns <-
  c(
    "adcts",
    "ftedany",
    "^hcondn",
    "health",
    "nnewborn",
    "trainany",
    #Annual Event History
    "_code$",
    "^chkresp",
    "^indstime",
    "^istrt",
    "^indend",
    "^mtmml",
    #Administrational question
    "^hg",
    "nadoptch",
    "nchresp",
    "nnatch",
    #Grid Variables
    "birthy$",
    "dvage",
    "employ",
    "livesp",
    "marstat",
    "newper",
    "sex",
    #Household Grid
    "coopnxtyr",
    "qus$",
    "ivcoop",
    "ivprsnt",
    "susp",
    #Interviewer Obs
    "^benbase",
    "^bendis",
    "^benpen",
    "^bensta",
    "^missource",
    "^othben",
    #Unearned income and State benefits
    "_if$",
    #imputation flag
    "country",
    "ioutcome",
    "ivfio",
    "jbes2000",
    "jbstat",
    "pno$",
    "ppid$",
    "^hg",
    "hhorig",
    "hidp",
    "pidp",
    "relup",
    "scac",
    "_cc$",
    "deviceused",
    #nominal
    "_lw$",
    "_xw$",
    "psu",
    "sampst",
    "strata",
    #weights and sampling
    "sibling"
  )

## delete all that have the pattern

vars_interest = com_vars[!grepl(paste(patterns, collapse = "|"), com_vars$radical), ]

rm(patterns)

## construct data list only with those contained in var_interest

i <- 1

data_list3_rad <- map(data_list2_rad, function(x) {
  vars_select <-
    names(data_list2_rad[[i]])[names(data_list2_rad[[i]]) %in% vars_interest$radical]

  out <-  select(data_list2_rad[[i]], vars_select)

  i <<- i + 1

  out

})

## Add attributes

###labels

for (i in 1:6) {
  if ("aidhh" %in% colnames(data_list3_rad[[i]])) {
    attr(data_list3_rad[[i]][["aidhh"]], "label") <-
      "cares for handicapped/other in household"
  }
  if ("aidxhh" %in% colnames(data_list3_rad[[i]])) {
    attr(data_list3_rad[[i]][["aidxhh"]], "label") <-
      "non-residents cared for"
  }
  if ("caruse" %in% colnames(data_list3_rad[[i]])) {
    attr(data_list3_rad[[i]][["caruse"]], "label") <-
      "has use of a car or a van"
  }
  if ("finfut" %in% colnames(data_list3_rad[[i]])) {
    attr(data_list3_rad[[i]][["finfut"]], "label") <-
      "subjective financial situation - future"
  }
  if ("finnow" %in% colnames(data_list3_rad[[i]])) {
    attr(data_list3_rad[[i]][["finnow"]], "label") <-
      "subjective financial situation - now"
  }
  if ("fiyrdia" %in% colnames(data_list3_rad[[i]])) {
    attr(data_list3_rad[[i]][["fiyrdia"]], "label") <-
      "amount received in interest/dividends"
  }
  if ("howlng" %in% colnames(data_list3_rad[[i]])) {
    attr(data_list3_rad[[i]][["howlng"]], "label") <-
      "time spent on housework"
  }
  if ("j2has" %in% colnames(data_list3_rad[[i]])) {
    attr(data_list3_rad[[i]][["j2has"]], "label") <- "has a second job"
  }
  if ("jbhas" %in% colnames(data_list3_rad[[i]])) {
    attr(data_list3_rad[[i]][["jbhas"]], "label") <-
      "did paid work last week"
  }
  if ("jbsemp" %in% colnames(data_list3_rad[[i]])) {
    attr(data_list3_rad[[i]][["jbsemp"]], "label") <-
      "employed: current job"
  }
  if ("jbterm1" %in% colnames(data_list3_rad[[i]])) {
    attr(data_list3_rad[[i]][["jbterm1"]], "label") <-
      "current job: permanent"
  }
  if ("lkmove" %in% colnames(data_list3_rad[[i]])) {
    attr(data_list3_rad[[i]][["lkmove"]], "label") <-
      "prefers to move house"
  }
  if ("matleave" %in% colnames(data_list3_rad[[i]])) {
    attr(data_list3_rad[[i]][["matleave"]], "label") <-
      "currently on maternity leave"
  }
  if ("mobuse" %in% colnames(data_list3_rad[[i]])) {
    attr(data_list3_rad[[i]][["mobuse"]], "label") <- "has mobile phone"
  }
  if ("netpuse" %in% colnames(data_list3_rad[[i]])) {
    attr(data_list3_rad[[i]][["netpuse"]], "label") <-
      "frequency of using the internet"
  }
  if ("scghqa" %in% colnames(data_list3_rad[[i]])) {
    attr(data_list3_rad[[i]][["scghqa"]], "label") <-
      "GHQ: concentration"
  }
  if ("scghqb" %in% colnames(data_list3_rad[[i]])) {
    attr(data_list3_rad[[i]][["scghqb"]], "label") <-
      "GHQ: loss of sleep"
  }
  if ("scghqc" %in% colnames(data_list3_rad[[i]])) {
    attr(data_list3_rad[[i]][["scghqc"]], "label") <-
      "GHQ: playing a useful role"
  }
  if ("scghqd" %in% colnames(data_list3_rad[[i]])) {
    attr(data_list3_rad[[i]][["scghqd"]], "label") <-
      "GHQ: capable of making decisions"
  }
  if ("scghqe" %in% colnames(data_list3_rad[[i]])) {
    attr(data_list3_rad[[i]][["scghqe"]], "label") <-
      "GHQ: constantly under strain"
  }
  if ("scghqf" %in% colnames(data_list3_rad[[i]])) {
    attr(data_list3_rad[[i]][["scghqf"]], "label") <-
      "GHQ: problem overcoming difficulties"
  }
  if ("scghqg" %in% colnames(data_list3_rad[[i]])) {
    attr(data_list3_rad[[i]][["scghqg"]], "label") <-
      "GHQ: enjoy day-to-day activities"
  }
  if ("scghqh" %in% colnames(data_list3_rad[[i]])) {
    attr(data_list3_rad[[i]][["scghqh"]], "label") <-
      "GHQ: ability to face problems"
  }
  if ("scghqi" %in% colnames(data_list3_rad[[i]])) {
    attr(data_list3_rad[[i]][["scghqi"]], "label") <-
      "GHQ: unhappy or depressed"
  }
  if ("scghqj" %in% colnames(data_list3_rad[[i]])) {
    attr(data_list3_rad[[i]][["scghqj"]], "label") <-
      "GHQ: losing confidence"
  }
  if ("scghqk" %in% colnames(data_list3_rad[[i]])) {
    attr(data_list3_rad[[i]][["scghqk"]], "label") <-
      "GHQ: believe worthless"
  }
  if ("scghql" %in% colnames(data_list3_rad[[i]])) {
    attr(data_list3_rad[[i]][["scghql"]], "label") <-
      "GHQ: general happiness"
  }
  if ("scsf1" %in% colnames(data_list3_rad[[i]])) {
    attr(data_list3_rad[[i]][["scsf1"]], "label") <- "general health"
  }
  if ("scsf2a" %in% colnames(data_list3_rad[[i]])) {
    attr(data_list3_rad[[i]][["scsf2a"]], "label") <-
      "health limits moderate activities"
  }
  if ("scsf2b" %in% colnames(data_list3_rad[[i]])) {
    attr(data_list3_rad[[i]][["scsf2b"]], "label") <-
      "health limits several flights of stairs"
  }
  if ("scsf3a" %in% colnames(data_list3_rad[[i]])) {
    attr(data_list3_rad[[i]][["scsf3a"]], "label") <-
      "last 4 weeks: physical health limits amount of work"
  }
  if ("scsf3b" %in% colnames(data_list3_rad[[i]])) {
    attr(data_list3_rad[[i]][["scsf3b"]], "label") <-
      "last 4 weeks: physical health limits kind of work"
  }
  if ("scsf4a" %in% colnames(data_list3_rad[[i]])) {
    attr(data_list3_rad[[i]][["scsf4a"]], "label") <-
      "last 4 weeks: mental health meant accomplished less"
  }
  if ("scsf4b" %in% colnames(data_list3_rad[[i]])) {
    attr(data_list3_rad[[i]][["scsf4b"]], "label") <-
      "last 4 weeks: mental health meant worked less carefully"
  }
  if ("scsf5" %in% colnames(data_list3_rad[[i]])) {
    attr(data_list3_rad[[i]][["scsf5"]], "label") <-
      "last 4 weeks: pain interfered with work"
  }
  if ("scsf6a" %in% colnames(data_list3_rad[[i]])) {
    attr(data_list3_rad[[i]][["scsf6a"]], "label") <-
      "last 4 weeks: felt calm and peaceful"
  }
  if ("scsf6b" %in% colnames(data_list3_rad[[i]])) {
    attr(data_list3_rad[[i]][["scsf6b"]], "label") <-
      "last 4 weeks: had a lot of energy"
  }
  if ("scsf6c" %in% colnames(data_list3_rad[[i]])) {
    attr(data_list3_rad[[i]][["scsf6c"]], "label") <-
      "last 4 weeks: felt downhearted and depressed"
  }
  if ("scsf7" %in% colnames(data_list3_rad[[i]])) {
    attr(data_list3_rad[[i]][["scsf7"]], "label") <-
      "last 4 weeks: physical or mental health interfered with social life"
  }
  if ("smoker" %in% colnames(data_list3_rad[[i]])) {
    attr(data_list3_rad[[i]][["smoker"]], "label") <- "smoker"
  }
  if ("vote1" %in% colnames(data_list3_rad[[i]])) {
    attr(data_list3_rad[[i]][["vote1"]], "label") <-
      "supports political party"
  }
  if ("vote2" %in% colnames(data_list3_rad[[i]])) {
    attr(data_list3_rad[[i]][["vote2"]], "label") <-
      "closer to one political party than others"
  }
  if ("vote6" %in% colnames(data_list3_rad[[i]])) {
    attr(data_list3_rad[[i]][["vote6"]], "label") <-
      "level of interest in politics"
  }
  if ("xpmove" %in% colnames(data_list3_rad[[i]])) {
    attr(data_list3_rad[[i]][["xpmove"]], "label") <-
      "expects to move next year"
  }
}

### coding

for (i in 1:6) {
  if ("aidhh" %in% colnames(data_list3_rad[[i]])) {
    attr(data_list3_rad[[i]][["aidhh"]], "coding") <- "1=Yes, 0=No"
  }
  if ("aidxhh" %in% colnames(data_list3_rad[[i]])) {
    attr(data_list3_rad[[i]][["aidxhh"]], "coding") <- "1=Yes, 0=No"
  }
  if ("caruse" %in% colnames(data_list3_rad[[i]])) {
    attr(data_list3_rad[[i]][["caruse"]], "coding") <- "1=Yes, 0=No"
  }
  if ("finfut" %in% colnames(data_list3_rad[[i]])) {
    attr(data_list3_rad[[i]][["finfut"]], "coding") <-
      "1=Better off, 2=Same, 3=Worse off"
  }
  if ("finnow" %in% colnames(data_list3_rad[[i]])) {
    attr(data_list3_rad[[i]][["finnow"]], "coding") <-
      "1=Living comfortably, 2=Doing alright, 3=Just about getting by, 4=Finding it quite difficult, 5=Finding it very difficult"
  }
  if ("fiyrdia" %in% colnames(data_list3_rad[[i]])) {
    attr(data_list3_rad[[i]][["fiyrdia"]], "coding") <- "Range: 0-90000"
  }
  if ("howlng" %in% colnames(data_list3_rad[[i]])) {
    attr(data_list3_rad[[i]][["howlng"]], "coding") <- "0-120"
  }
  if ("j2has" %in% colnames(data_list3_rad[[i]])) {
    attr(data_list3_rad[[i]][["j2has"]], "coding") <- "1=Yes, 0=No"
  }
  if ("jbhas" %in% colnames(data_list3_rad[[i]])) {
    attr(data_list3_rad[[i]][["jbhas"]], "coding") <- "1=Yes, 0=No"
  }
  if ("jbsemp" %in% colnames(data_list3_rad[[i]])) {
    attr(data_list3_rad[[i]][["jbsemp"]], "coding") <- "1=Yes, 0=No"
  }
  if ("jbterm1" %in% colnames(data_list3_rad[[i]])) {
    attr(data_list3_rad[[i]][["jbterm1"]], "coding") <- "1=Yes, 0=No"
  }
  if ("lkmove" %in% colnames(data_list3_rad[[i]])) {
    attr(data_list3_rad[[i]][["lkmove"]], "coding") <- "1=Yes, 0=No"
  }
  if ("matleave" %in% colnames(data_list3_rad[[i]])) {
    attr(data_list3_rad[[i]][["matleave"]], "coding") <- "1=Yes, 0=No"
  }
  if ("mobuse" %in% colnames(data_list3_rad[[i]])) {
    attr(data_list3_rad[[i]][["mobuse"]], "coding") <- "1=Yes, 0=No"
  }
  if ("netpuse" %in% colnames(data_list3_rad[[i]])) {
    attr(data_list3_rad[[i]][["netpuse"]], "coding") <-
      "1=Every day, 2=Several times a week, 3=Several times a month, 4=Once a month, 5=Less than once a month, 6=Never use, 7=TF_NoAccess"
  }
  if ("scghqa" %in% colnames(data_list3_rad[[i]])) {
    attr(data_list3_rad[[i]][["scghqa"]], "coding") <-
      "1=Better than usual, 2=Same as usual, 3=Less than usual, 4=Much less than usual"
  }
  if ("scghqb" %in% colnames(data_list3_rad[[i]])) {
    attr(data_list3_rad[[i]][["scghqb"]], "coding") <-
      "1=Not at all, 2=No more than usual, 3=Rather more than usual, 4=Much more than usual"
  }
  if ("scghqc" %in% colnames(data_list3_rad[[i]])) {
    attr(data_list3_rad[[i]][["scghqc"]], "coding") <-
      "1=More than usual, 2=same as usual, 3=Less so than usual, 4=Much less than usual"
  }
  if ("scghqd" %in% colnames(data_list3_rad[[i]])) {
    attr(data_list3_rad[[i]][["scghqd"]], "coding") <-
      "1=More than usual, 2=same as usual, 3=Less so than usual, 4=Much less capable"
  }
  if ("scghqe" %in% colnames(data_list3_rad[[i]])) {
    attr(data_list3_rad[[i]][["scghqe"]], "coding") <-
      "1=Not at all, 2=No more than usual, 3=Rather more than usual, 4=Much more than usual"
  }
  if ("scghqf" %in% colnames(data_list3_rad[[i]])) {
    attr(data_list3_rad[[i]][["scghqf"]], "coding") <-
      "1=Not at all, 2=No more than usual, 3=Rather more than usual, 4=Much more than usual"
  }
  if ("scghqg" %in% colnames(data_list3_rad[[i]])) {
    attr(data_list3_rad[[i]][["scghqg"]], "coding") <-
      "1=More than usual, 2=same as usual, 3=Less so than usual, 4=Much less than usual"
  }
  if ("scghqh" %in% colnames(data_list3_rad[[i]])) {
    attr(data_list3_rad[[i]][["scghqh"]], "coding") <-
      "1=More than usual, 2=same as usual, 3=Less so than usual, 4=Much less able"
  }
  if ("scghqi" %in% colnames(data_list3_rad[[i]])) {
    attr(data_list3_rad[[i]][["scghqi"]], "coding") <-
      "1=Not at all, 2=No more than usual, 3=Rather more than usual, 4=Much more than usual"
  }
  if ("scghqj" %in% colnames(data_list3_rad[[i]])) {
    attr(data_list3_rad[[i]][["scghqj"]], "coding") <-
      "1=Not at all, 2=No more than usual, 3=Rather more than usual, 4=Much more than usual"
  }
  if ("scghqk" %in% colnames(data_list3_rad[[i]])) {
    attr(data_list3_rad[[i]][["scghqk"]], "coding") <-
      "1=Not at all, 2=No more than usual, 3=Rather more than usual, 4=Much more than usual"
  }
  if ("scghql" %in% colnames(data_list3_rad[[i]])) {
    attr(data_list3_rad[[i]][["scghql"]], "coding") <-
      "1=More so than usual, 2=About the same as usual, 3=Less so than usual, 4=Much less than usual"
  }
  if ("scsf1" %in% colnames(data_list3_rad[[i]])) {
    attr(data_list3_rad[[i]][["scsf1"]], "coding") <-
      "1=Excellent, 2=Very good, 3=Good, 4=Fair, 5=Poor"
  }
  if ("scsf2a" %in% colnames(data_list3_rad[[i]])) {
    attr(data_list3_rad[[i]][["scsf2a"]], "coding") <-
      "1=Yes, limited a lot, 2=Yes, limited a little, 3=No, not limited at all"
  }
  if ("scsf2b" %in% colnames(data_list3_rad[[i]])) {
    attr(data_list3_rad[[i]][["scsf2b"]], "coding") <-
      "1=Yes, limited a lot, 2=Yes, limited a little, 3=No, not limited at all"
  }
  if ("scsf3a" %in% colnames(data_list3_rad[[i]])) {
    attr(data_list3_rad[[i]][["scsf3a"]], "coding") <-
      "1=All of the time, 2=Most of the time, 3=Some of the time, 4=A little of the time, 5=None of the time"
  }
  if ("scsf3b" %in% colnames(data_list3_rad[[i]])) {
    attr(data_list3_rad[[i]][["scsf3b"]], "coding") <-
      "1=All of the time, 2=Most of the time, 3=Some of the time, 4=A little of the time, 5=None of the time"
  }
  if ("scsf4a" %in% colnames(data_list3_rad[[i]])) {
    attr(data_list3_rad[[i]][["scsf4a"]], "coding") <-
      "1=All of the time, 2=Most of the time, 3=Some of the time, 4=A little of the time, 5=None of the time"
  }
  if ("scsf4b" %in% colnames(data_list3_rad[[i]])) {
    attr(data_list3_rad[[i]][["scsf4b"]], "coding") <-
      "1=All of the time, 2=Most of the time, 3=Some of the time, 4=A little of the time, 5=None of the time"
  }
  if ("scsf5" %in% colnames(data_list3_rad[[i]])) {
    attr(data_list3_rad[[i]][["scsf5"]], "coding") <-
      "1=Not at all, 2=A little bit, 3=Moderately, 4=Quite a bit, 5=Extremely"
  }
  if ("scsf6a" %in% colnames(data_list3_rad[[i]])) {
    attr(data_list3_rad[[i]][["scsf6a"]], "coding") <-
      "1=All of the time, 2=Most of the time, 3=Some of the time, 4=A little of the time, 5=None of the time"
  }
  if ("scsf6b" %in% colnames(data_list3_rad[[i]])) {
    attr(data_list3_rad[[i]][["scsf6b"]], "coding") <-
      "1=All of the time, 2=Most of the time, 3=Some of the time, 4=A little of the time, 5=None of the time"
  }
  if ("scsf6c" %in% colnames(data_list3_rad[[i]])) {
    attr(data_list3_rad[[i]][["scsf6c"]], "coding") <-
      "1=All of the time, 2=Most of the time, 3=Some of the time, 4=A little of the time, 5=None of the time"
  }
  if ("scsf7" %in% colnames(data_list3_rad[[i]])) {
    attr(data_list3_rad[[i]][["scsf7"]], "coding") <-
      "1=All of the time, 2=Most of the time, 3=Some of the time, 4=A little of the time, 5=None of the time"
  }
  if ("smoker" %in% colnames(data_list3_rad[[i]])) {
    attr(data_list3_rad[[i]][["smoker"]], "coding") <- "1=Yes, 0=No"
  }
  if ("vote1" %in% colnames(data_list3_rad[[i]])) {
    attr(data_list3_rad[[i]][["vote1"]], "coding") <- "1=Yes, 0=No"
  }
  if ("vote2" %in% colnames(data_list3_rad[[i]])) {
    attr(data_list3_rad[[i]][["vote2"]], "coding") <- "1=Yes, 0=No"
  }
  if ("vote6" %in% colnames(data_list3_rad[[i]])) {
    attr(data_list3_rad[[i]][["vote6"]], "coding") <-
      "1=Very, 2=Fairly, 3=Not very, 4=Not at all interested"
  }
  if ("xpmove" %in% colnames(data_list3_rad[[i]])) {
    attr(data_list3_rad[[i]][["xpmove"]], "coding") <- "1=Yes, 0=No"
  }
}

save(data_list3_rad, file = "./data/out/interest_vars_data_charlotte.RData")


# Range of Variables ------------------------------------------------------

max <-
  matrix(rep(NA), ncol = 242, nrow = 6) #242 is the maximum number of columns in one sublist
min <- matrix(rep(NA), ncol = 242, nrow = 6)
variable.range <- matrix(rep(NA), ncol = 242, nrow = 6)
for (i in 1:6) {
  for (j in 1:ncol(data_list2_rad[[i]])) {
    max[i, j] <- max(data_list2_rad[[i]][, j], na.rm = TRUE)
    min[i, j] <- min(data_list2_rad[[i]][, j], na.rm = TRUE)
    variable.range[i, j] <- colnames(data_list2_rad[[i]][, j])
  }
}

rangelist = list()
for (i in 1:6) {
  subrange <-
    data.frame(matrix(
      cbind(variable.range[i, ], min[i, ], max[i, ]),
      ncol = 3,
      nrow = ncol(min)
    ))
  rangelist[[i]] <- subrange
}

range = do.call(rbind, rangelist)

#table of variables with their respective min and max values
range <- range[order(range[, 1], range[, 3]), ]

write.table(range,
            file = "./data/out/range_charlotte.txt",
            sep = ",",
            quote = FALSE)

rm(min, max, rangelist, subrange, variable.range)


# Transition matrices -----------------------------------------------------

# add pidp to data_list3_rad

i <- 1

data_list3_idp <- map(data_list2_rad, function(x) {
  vars_select <-
    names(data_list2_rad[[i]])[names(data_list2_rad[[i]]) %in% vars_interest$radical]

  out <- select(data_list2_rad[[i]], c(vars_select, "pidp"))

  i <<- i + 1

  out

})

## add the suffixes except on pidp

for (i in 1:6) {
  names(data_list3_idp[[i]])[1:(ncol(data_list3_idp[[i]]) - 1)] <-
    paste(names(data_list3_idp[[i]])[1:ncol(data_list3_idp[[i]]) - 1], sep =
            "_", ej[i])
}

save(data_list3_idp, file = "./data/out/identifier_vars_data_charlotte.RData")
#load("./data/out/identifier_vars_data_charlotte.RData")

## merge all variables on pidp

allVariables <-
  Reduce(
    function(x, y)
      merge(x, y, by = "pidp", all = TRUE),
    list(
      data_list3_idp[[1]],
      data_list3_idp[[2]],
      data_list3_idp[[3]],
      data_list3_idp[[4]],
      data_list3_idp[[5]],
      data_list3_idp[[6]]
    )
  )
allVariables <- allVariables[, order(colnames(allVariables))]

## delete pidp and continuous variables

allordVariables <- allVariables
allordVariables = allVariables[, !grepl("^fiyrdia", names(allVariables))]
allordVariables = allordVariables[, !grepl("^howlng", names(allordVariables))] #all ordinal variables
allordVariables$pidp <- NULL

for (i in 1:(ncol(allordVariables) - 1)) {
  j <- i + 1

  varmat <-
    allordVariables[, i:j] #matrix of the current variable and its succeeding variable

  varmat <- varmat[!(rowSums(is.na(varmat)) == ncol(varmat)), ]

  trmat <-
    round(prop.table(table(varmat[, 1], varmat[, 2], useNA = "always"), margin =
                       1), 3) #transition table

  if (gsub("_.*", "", colnames(varmat[1])) == gsub("_.*", "", colnames(varmat[2]))) {
    cat(str_c(colnames(varmat[1]), sep = "|", colnames(varmat[2])),
        file = "./data/out/transitionmatrix_charlotte.txt",
        append = T)

    write.table(
      trmat,
      file = "./data/out/transitionmatrix_charlotte.txt",
      sep = ",",
      quote = FALSE,
      col.names = NA,
      append = T
    )
  }
}

rm(varmat)


# Correlation of continuous variables -------------------------------------

allcontVariables <-
  data.frame(allVariables[, grepl("^fiyrdia", names(allVariables))],
             allVariables[, grepl("^howlng", names(allVariables))])
#all continuous variables (exactly 2 continuous variables)

variables <- NA
cor <- NA

for (i in 1:(ncol(allcontVariables) - 1)) {
  j <- i + 1

  variables[i] <-
    str_c(colnames(allcontVariables[i]),
          sep = "|",
          colnames(allcontVariables[j]))

  if (gsub("_.*", "", colnames(allcontVariables[i])) == gsub("_.*", "", colnames(allcontVariables[j]))) {
    cor[i] <-
      cor(allcontVariables[, i], allcontVariables[, j], use = "complete.obs")

  }
}

cortab <-
  data.frame(t(rbind(variables, cor))) #table of the variable in a certain wave and the correlation with its succeeding wave

write.table(
  cortab,
  file = "./data/out/correlationtable_charlotte.txt",
  sep = ",",
  quote = FALSE,
  col.names = NA
)

rm(cor, variables)

##frequency tables

for (i in 1:ncol(allordVariables)) {
  freqtab <-
    round(prop.table(table(allordVariables[, i], useNA = "always")), 3)

  cat(colnames(allordVariables[i]),
      file = "./data/out/frequencytable_charlotte.txt",
      append = T)

  write.table(
    freqtab,
    file = "./data/out/frequencytable_charlotte.txt",
    sep = ",",
    quote = FALSE,
    col.names = NA,
    append = T
  )
}

# Transition matrices (valid-missing) -------------------------------------

## To find out which questions were answered by a large fraction of the same people in consecutive waves

allResponse <- allVariables
allResponse[allResponse >= 0] <-
  "V" #all variables, where a valid answer is replaced by V

prop.overlap <- NA
var.overlap <- NA

for (i in 1:(ncol(allResponse) - 1)) {
  j <- i + 1

  varmat <-
    allResponse[, i:j] #matrix of variable and succeeding variable/wave

  varmat <- varmat[!(rowSums(is.na(varmat)) == ncol(varmat)), ]

  respmat <-
    round(prop.table(table(varmat[, 1], varmat[, 2], useNA = "always"), margin =
                       1), 3)

  if (gsub("_.*", "", colnames(varmat[1])) == gsub("_.*", "", colnames(varmat[2]))) {
    cat(str_c(colnames(varmat[1]), sep = "|", colnames(varmat[2])),
        file = "./data/out/responsematrix_charlotte.txt",
        append = T)

    write.table(
      respmat,
      file = "./data/out/responsematrix_charlotte.txt",
      sep = ",",
      quote = FALSE,
      col.names = NA,
      append = T
    )

    prop.overlap[i] <- respmat[1, 1]
    var.overlap[i] <-
      str_c(colnames(varmat[1]), sep = "|", colnames(varmat[2]))

  }
}

overlap <-
  data.frame(t(rbind(var.overlap, prop.overlap))) #table with the percentage of individuals from the first wave that also gave a valid answer in the second wave

write.table(
  overlap,
  file = "./data/out/overlaptable_charlotte.txt",
  sep = ",",
  quote = FALSE,
  col.names = NA
)

## plotting the density of the transition probability

hist(prop.overlap,
     main = "Overlap Density",
     breaks = 150,
     xlab = "How many of those that gave a valid answer in the first wave, also gave a valid answer in the second wave (%)")
abline(v = mean(prop.overlap, na.rm = T),
       col = "red",
       lwd = 2)
abline(
  v = mean(prop.overlap, na.rm = T) - sd(prop.overlap, na.rm = T),
  col = "blue",
  lwd = 2,
  lty = 2
)
abline(
  v = mean(prop.overlap, na.rm = T) - 2 * sd(prop.overlap, na.rm = T),
  col = "purple",
  lwd = 2,
  lty = 2
)
abline(
  v = quantile(prop.overlap, na.rm = T),
  col = "orange",
  lwd = 1,
  lty = 2
)
abline(v = 0.72,
       col = "brown",
       lwd = 2,
       lty = 2)
legend(
  x = "top",
  legend = c(
    str_c("mean", "=", round(mean(
      prop.overlap, na.rm = T
    ), 3)) ,
    str_c("mean-standard deviation", "=", round(
      mean(prop.overlap, na.rm = T) - sd(prop.overlap, na.rm = T), 3
    )),
    str_c("mean-standard deviation", "=", round(
      mean(prop.overlap, na.rm = T) - 2 * sd(prop.overlap, na.rm = T), 3
    )),
    "quartiles",
    "72%"
  ),
  col = c("red", "blue", "purple", "orange", "brown"),
  lty = c(1, 2, 2, 2, 2),
  cex = 0.8,
  bty = "n"
)


rm(prop.overlap,
   var.overlap,
   allResponse,
   allordVariables,
   allcontVariables,
   varmat)


# Delete variables with not enough overlap --------------------------------

# delete all with less than 72% overlap and sometimes the wave before or after

data_list4_rad <- data_list3_rad

## dropping aidhh, matleave, vote2 completely

for (i in 1:6) {
  if ("aidhh" %in% colnames(data_list4_rad[[i]])) {
    data_list4_rad[[i]]["aidhh"] <- NULL
  }
  if ("matleave" %in% colnames(data_list4_rad[[i]])) {
    data_list4_rad[[i]]["matleave"] <- NULL
  }
  if ("vote2" %in% colnames(data_list4_rad[[i]])) {
    data_list4_rad[[i]]["vote2"] <- NULL
  }
}

## dropping certain waves of variables

data_list4_rad[[5]]["caruse"] <- NULL
data_list4_rad[[6]]["caruse"] <- NULL
data_list4_rad[[1]]["jbterm1"] <- NULL
data_list4_rad[[2]]["jbterm1"] <- NULL
data_list4_rad[[1]]["fiyrdia"] <- NULL
data_list4_rad[[2]]["fiyrdia"] <- NULL
data_list4_rad[[1]]["jbsemp"] <- NULL
data_list4_rad[[2]]["jbsemp"] <- NULL

save(data_list4_rad, file = "./data/out/overlap_vars_data_charlotte.RData")

#load("./data/out/overlap_vars_data_charlotte.RData")


# Descriptive tables ------------------------------------------------------

data_list4_cont_rad <- data_list4_rad

# delete all ordinal(<5)/binary variables, such that only the "continuous" variables remain
for (i in 1:6) {
  if ("aidxhh" %in% colnames(data_list4_cont_rad[[i]])) {
    data_list4_cont_rad[[i]]["aidxhh"] <- NULL
  }
  if ("caruse" %in% colnames(data_list4_cont_rad[[i]])) {
    data_list4_cont_rad[[i]]["caruse"] <- NULL
  }
  if ("finfut" %in% colnames(data_list4_cont_rad[[i]])) {
    data_list4_cont_rad[[i]]["finfut"] <- NULL
  }
  if ("j2has" %in% colnames(data_list4_cont_rad[[i]])) {
    data_list4_cont_rad[[i]]["j2has"] <- NULL
  }
  if ("jbhas" %in% colnames(data_list4_cont_rad[[i]])) {
    data_list4_cont_rad[[i]]["jbhas"] <- NULL
  }
  if ("jbsemp" %in% colnames(data_list4_cont_rad[[i]])) {
    data_list4_cont_rad[[i]]["jbsemp"] <- NULL
  }
  if ("jbterm1" %in% colnames(data_list4_cont_rad[[i]])) {
    data_list4_cont_rad[[i]]["jbterm1"] <- NULL
  }
  if ("lkmove" %in% colnames(data_list4_cont_rad[[i]])) {
    data_list4_cont_rad[[i]]["lkmove"] <- NULL
  }
  if ("mobuse" %in% colnames(data_list4_cont_rad[[i]])) {
    data_list4_cont_rad[[i]]["mobuse"] <- NULL
  }
  if ("scghqa" %in% colnames(data_list4_cont_rad[[i]])) {
    data_list4_cont_rad[[i]]["scghqa"] <- NULL
  }
  if ("scghqb" %in% colnames(data_list4_cont_rad[[i]])) {
    data_list4_cont_rad[[i]]["scghqb"] <- NULL
  }
  if ("scghqc" %in% colnames(data_list4_cont_rad[[i]])) {
    data_list4_cont_rad[[i]]["scghqc"] <- NULL
  }
  if ("scghqd" %in% colnames(data_list4_cont_rad[[i]])) {
    data_list4_cont_rad[[i]]["scghqd"] <- NULL
  }
  if ("scghqe" %in% colnames(data_list4_cont_rad[[i]])) {
    data_list4_cont_rad[[i]]["scghqe"] <- NULL
  }
  if ("scghqf" %in% colnames(data_list4_cont_rad[[i]])) {
    data_list4_cont_rad[[i]]["scghqf"] <- NULL
  }
  if ("scghqg" %in% colnames(data_list4_cont_rad[[i]])) {
    data_list4_cont_rad[[i]]["scghqg"] <- NULL
  }
  if ("scghqh" %in% colnames(data_list4_cont_rad[[i]])) {
    data_list4_cont_rad[[i]]["scghqh"] <- NULL
  }
  if ("scghqi" %in% colnames(data_list4_cont_rad[[i]])) {
    data_list4_cont_rad[[i]]["scghqi"] <- NULL
  }
  if ("scghqj" %in% colnames(data_list4_cont_rad[[i]])) {
    data_list4_cont_rad[[i]]["scghqj"] <- NULL
  }
  if ("scghqk" %in% colnames(data_list4_cont_rad[[i]])) {
    data_list4_cont_rad[[i]]["scghqk"] <- NULL
  }
  if ("scghql" %in% colnames(data_list4_cont_rad[[i]])) {
    data_list4_cont_rad[[i]]["scghql"] <- NULL
  }
  if ("scsf2a" %in% colnames(data_list4_cont_rad[[i]])) {
    data_list4_cont_rad[[i]]["scsf2a"] <- NULL
  }
  if ("scsf2b" %in% colnames(data_list4_cont_rad[[i]])) {
    data_list4_cont_rad[[i]]["scsf2b"] <- NULL
  }
  if ("smoker" %in% colnames(data_list4_cont_rad[[i]])) {
    data_list4_cont_rad[[i]]["smoker"] <- NULL
  }
  if ("vote1" %in% colnames(data_list4_cont_rad[[i]])) {
    data_list4_cont_rad[[i]]["vote1"] <- NULL
  }
  if ("vote6" %in% colnames(data_list4_cont_rad[[i]])) {
    data_list4_cont_rad[[i]]["vote6"] <- NULL
  }
  if ("xpmove" %in% colnames(data_list4_cont_rad[[i]])) {
    data_list4_cont_rad[[i]]["xpmove"] <- NULL
  }

}

data_list4_ord_rad <- list()

#create data set with all ordinal(<5) and binary variables
for (i in 1:6) {
  data_list4_ord_rad[[i]] <-
    data_list4_rad[[i]][-which(colnames(data_list4_rad[[i]]) %in% colnames(data_list4_cont_rad[[i]]))]
}

# take data set and add personal and hh identifier

data_list4_cont <- data_list4_cont_rad

for (i in 1:6) {
  names(data_list4_cont[[i]]) <-
    paste0(names(data_list4_cont[[i]]), '', str_c("_", ej[i]))
}

data_list4_ord <- data_list4_ord_rad

for (i in 1:6) {
  names(data_list4_ord[[i]]) <-
    paste0(names(data_list4_ord[[i]]), '', str_c("_", ej[i]))
}

## data set with pidp and all continuous variables and those with more than 4 categories

allcontVar4_p <-
  allVariables[, which(
    colnames(allVariables) %in% colnames(data_list4_cont[[1]]) |
      colnames(allVariables) %in% colnames(data_list4_cont[[2]]) |
      colnames(allVariables) %in% colnames(data_list4_cont[[3]]) |
      colnames(allVariables) %in% colnames(data_list4_cont[[4]]) |
      colnames(allVariables) %in% colnames(data_list4_cont[[5]]) |
      colnames(allVariables) %in% colnames(data_list4_cont[[6]]) |
      colnames(allVariables) == "pidp"
  )]
allcontVar4 <- allcontVar4_p
allcontVar4$pidp <- NULL

## data set with pidp and all binary variables and those with less or exactly 4 categories

allordVar4_p <-
  allVariables[, which(
    colnames(allVariables) %in% colnames(data_list4_ord[[1]]) |
      colnames(allVariables) %in% colnames(data_list4_ord[[2]]) |
      colnames(allVariables) %in% colnames(data_list4_ord[[3]]) |
      colnames(allVariables) %in% colnames(data_list4_ord[[4]]) |
      colnames(allVariables) %in% colnames(data_list4_ord[[5]]) |
      colnames(allVariables) %in% colnames(data_list4_ord[[6]]) |
      colnames(allVariables) == "pidp"
  )]
allordVar4 <- allordVar4_p
allordVar4$pidp <- NULL

# Descriptive statistic tables without MODE indicator--------------------------------------

# function to assign correct column names

columnnames <- function(data, df) {
  if (grepl("_j$", colnames(data[j]))) {
    colnames(df) <- c("W10p", "W10se")
  } else if (grepl("_i$", colnames(data[j]))) {
    colnames(df) <- c("W09p", "W09se")
  } else if (grepl("_h$", colnames(data[j]))) {
    colnames(df) <- c("W08p", "W08se")
  } else if (grepl("_g$", colnames(data[j]))) {
    colnames(df) <- c("W07p", "W07se")
  } else if (grepl("_f$", colnames(data[j]))) {
    colnames(df) <- c("W06p", "W06se")
  } else{
    colnames(df) <- c("W05p", "W05se")
  }
  return(df)
}

# function to create table for ordinal variables

tabl <- function(tab) {
  if (0 %in% tab$value) {
    prop0 <- tab[tab$value == 0, 2][1]
  } else{
    prop0 <- NA
  }
  if (0 %in% tab$value) {
    se0 <- tab[tab$value == 0, 3][1]
  } else{
    se0 <- NA
  }
  if (1 %in% tab$value) {
    prop1 <- tab[tab$value == 1, 2][1]
  } else{
    prop1 <- NA
  }
  if (1 %in% tab$value) {
    se1 <- tab[tab$value == 1, 3][1]
  } else{
    se1 <- NA
  }
  if (2 %in% tab$value) {
    prop2 <- tab[tab$value == 2, 2][1]
  } else{
    prop2 <- NA
  }
  if (2 %in% tab$value) {
    se2 <- tab[tab$value == 2, 3][1]
  } else{
    se2 <- NA
  }
  if (3 %in% tab$value) {
    prop3 <- tab[tab$value == 3, 2][1]
  } else{
    prop3 <- NA
  }
  if (3 %in% tab$value) {
    se3 <- tab[tab$value == 3, 3][1]
  } else{
    se3 <- NA
  }
  if (4 %in% tab$value) {
    prop4 <- tab[tab$value == 4, 2][1]
  } else{
    prop4 <- NA
  }
  if (4 %in% tab$value) {
    se4 <- tab[tab$value == 4, 3][1]
  } else{
    se4 <- NA
  }
  if (NA %in% tab$value) {
    propna <- tab[is.na(tab$value), 2]
  } else{
    propna <- NA
  }
  if (NA %in% tab$value) {
    sena <- tab[is.na(tab$value), 3]
  } else{
    sena <- NA
  }
  return(matrix(
    c(
      prop0,
      se0,
      prop1,
      se1,
      prop2,
      se2,
      prop3,
      se3,
      prop4,
      se4,
      propna,
      sena
    ),
    byrow = T,
    nrow = 6,
    ncol = 2
  ))
}

# function to append existing data set for each iteration

descr_append <-
  function(df,
           d,
           descr,
           name,
           name_before,
           rowname,
           end,
           a,
           br) {
    if (j == a) {
      d <- df
      name_before <- name
    } else if (j > a && j < end && name == name_before) {
      d <- as.data.frame(c(d, df))
      rownames(d) <- rowname
      name_before <- name
    } else if (j == br && name != name_before) {
      descr <- d
      d <- df
      name_before <- name
    } else if (j == end) {
      d <- data.frame(d, df)
      t_descr <- t(descr)
      t_d <- t(d)
      descr.merge <- merge(t_descr, t_d, by = "row.names", all = T)
      t_descr <- descr.merge[, -1]
      rownames(t_descr) <- descr.merge[, 1]
      descr <- t(t_descr)
    } else{
      t_descr <- t(descr)
      t_d <- t(d)
      descr.merge <- merge(t_descr, t_d, by = "row.names", all = T)
      t_descr <- descr.merge[, -1]
      rownames(t_descr) <- descr.merge[, 1]
      descr <- t(t_descr)
      d <- df
      name_before <- name
    }
    descr_sub <- list(df, d, descr, name, name_before)
    return(descr_sub)
  }


## table for continuous variables

end <- ncol(allcontVar4)

for (j in 1:end) {
  mean.var.cont <- round(mean(allcontVar4[, j], na.rm = T), 3)
  se.var.cont <-
    round(se(allcontVar4[, j], na.rm = T), 3) #uses sqrt(n) without counting the NAs
  na.var.cont <- sum(is.na(allcontVar4[, j])) / length(allcontVar4[, j])
  sena.var.cont <-
    round(sqrt(na.var.cont * (1 - na.var.cont) / length(allcontVar4[, j])), 4)
  na.var.cont <- round(na.var.cont, 4)

  df <- as.data.frame(matrix(
    c(
      mean.var.cont,
      se.var.cont,
      na.var.cont * 100,
      sena.var.cont * 100
    ),
    nrow = 2,
    ncol = 2,
    byrow = T
  ))
  df[2, ] <- format.data.frame(df[2, ], 2)

  df <- columnnames(allcontVar4, df)

  name <- sub("_e|_f|_g|_h|_i|_j", "", colnames(allcontVar4[j]))

  rowname <- c(str_c(name, "-mean"), str_c(name, "-NA (%)"))

  if (j == 1) {
    d <- NA
    descr <- NA
    name_before <- NA
    descr_sub <-
      descr_append(df, d, descr, name, name_before, rowname, end, 1, 7)
  } else if (j <= 7) {
    descr <- NA
    descr_sub <-
      descr_append(df, d, descr, name, name_before, rowname, end, 1, 7)
  } else{
    descr_sub <-
      descr_append(df, d, descr, name, name_before, rowname, end, 1, 7)
  }
  df <- descr_sub[[1]]
  d <- descr_sub[[2]]
  descr <- descr_sub[[3]]
  name <- descr_sub[[4]]
  name_before <- descr_sub[[5]]

}#for loop

descr.cont <- descr

write.table(
  descr.cont,
  file = "./data/out/descr_cont_charlotte.txt",
  sep = ",",
  quote = FALSE,
  col.names = NA
)

rm(
  mean.var.cont,
  se.var.cont,
  na.var.cont,
  sena.var.cont,
  name,
  name_before,
  df,
  d,
  descr,
  descr_sub
)

## table for ordinal variables
end <- ncol(allordVar4)

for (j in 1:end) {
  tab <- se(table(allordVar4[, j], useNA = "always"))

  #extract proportions and standard errors
  df <- tabl(tab)

  df <- round(df * 100, 2)

  df <- data.frame(df)

  name <- sub("_e|_f|_g|_h|_i|_j", "", colnames(allordVar4[j]))

  df <- columnnames(allordVar4, df)

  rowname <- c(
    str_c(name, "-0 (%)"),
    str_c(name, "-1 (%)"),
    str_c(name, "-2 (%)"),
    str_c(name, "-3 (%)"),
    str_c(name, "-4 (%)"),
    str_c(name, "-NA (%)")
  )

  if (j == 1) {
    d <- NA
    descr <- NA
    name_before <- NA
    descr_sub <-
      descr_append(df, d, descr, name, name_before, rowname, end, 1, 6)
  } else if (j <= 6) {
    descr <- NA
    descr_sub <-
      descr_append(df, d, descr, name, name_before, rowname, end, 1, 6)
  } else{
    descr_sub <-
      descr_append(df, d, descr, name, name_before, rowname, end, 1, 6)
  }
  df <- descr_sub[[1]]
  d <- descr_sub[[2]]
  descr <- descr_sub[[3]]
  name <- descr_sub[[4]]
  name_before <- descr_sub[[5]]

}#for loop

descr.ord <- descr

descr.ord <- descr.ord[rowSums(is.na(descr.ord)) != ncol(descr.ord), ]

write.table(
  descr.ord,
  file = "./data/out/descr_ord_charlotte.txt",
  sep = ",",
  quote = FALSE,
  col.names = NA
)

rm(tab, name, name_before, df, d, descr, descr_sub)

# Prepare data for mode indicators---------------------------------------------

## Construct a data frame where household identifiers are matched to personal identifiers and
## thereby mode indicators by household can be attributed to answers to questions.

idp <- list()
for (i in 1:6) {
  idp[[i]] <-
    data_list1[[i]][, grepl("pidp|hidp", colnames(data_list1[[i]]))]
}
idp[[1]]["e_pedchpidp"] <- NULL

idpframe <- Reduce(function(x, y)
  merge(x, y, all = TRUE), idp)

#data set with pidp, all hidp and the variables of interest
allcontVar4_idp <- merge(allcontVar4_p, idpframe, all = T)
allordVar4_idp <- merge(allordVar4_p, idpframe, all = T)

rm(idp)


# find mixed mode indicator

# get list of variables
data_path <- list.files("./stata/stata13/hhsamp/",
                        full.names = T) #need to transfer all hhsamp data sets into folder called "hhsamp"

# import data
hhsamplist <- map(data_path, read_dta)

hhsamplist <- map(hhsamplist, function(x) {
  x %>%
    mutate_all(funs(ifelse(. < 0, NA, .)))
})

setwd(wd)

## subtract the prefixes

for (i in 1:6) {
  names(hhsamplist[[i]]) <-
    sub(str_c(ej[i], "_"), '', names(hhsamplist[[i]]))
}

## delete everything except gridmode and hidp

for (i in 1:6) {
  hhsamplist[[i]] <-
    hhsamplist[[i]][, grepl(
      "hidp|ff_gridmodew5|ff_gridmodew8|ff_gridmodew9|ff_gridmodew10",
      colnames(hhsamplist[[i]])
    )]
}

## change the names of the variables

for (i in 1:6) {
  colnames(hhsamplist[[i]]) <- c(str_c(ej[i], "_hidp"), "gridmodew5")
}
colnames(hhsamplist[[4]]) <- c("h_hidp", "gridmodew5", "gridmodew8")
colnames(hhsamplist[[5]]) <-
  c("i_hidp", "gridmodew5", "gridmodew8", "gridmodew9")
colnames(hhsamplist[[6]]) <-
  c("j_hidp",
    "gridmodew5",
    "gridmodew8",
    "gridmodew9",
    "gridmodew10")

## recode to 1=mixed mode (web), 0=single mode (f2f)

hhsamplist <-
  rapply(hhsamplist, function(x)
    ifelse(x == 1, 0, x), how = "replace")
hhsamplist <-
  rapply(hhsamplist, function(x)
    ifelse(x == 3, 1, x), how = "replace")

## merge

allcontVar4_mode <- merge(hhsamplist[[1]], allcontVar4_idp, all = T)
allcontVar4_mode <- merge(hhsamplist[[2]], allcontVar4_mode, all = T)
allcontVar4_mode <- merge(hhsamplist[[3]], allcontVar4_mode, all = T)
allcontVar4_mode <- merge(hhsamplist[[4]], allcontVar4_mode, all = T)
allcontVar4_mode <- merge(hhsamplist[[5]], allcontVar4_mode, all = T)
allcontVar4_mode <- merge(hhsamplist[[6]], allcontVar4_mode, all = T)

allordVar4_mode <- merge(hhsamplist[[1]], allordVar4_idp, all = T)
allordVar4_mode <- merge(hhsamplist[[2]], allordVar4_mode, all = T)
allordVar4_mode <- merge(hhsamplist[[3]], allordVar4_mode, all = T)
allordVar4_mode <- merge(hhsamplist[[4]], allordVar4_mode, all = T)
allordVar4_mode <- merge(hhsamplist[[5]], allordVar4_mode, all = T)
allordVar4_mode <- merge(hhsamplist[[6]], allordVar4_mode, all = T)

## drop identifiers

allcontVar4m <-
  allcontVar4_mode[, !grepl("hidp|pidp", colnames(allcontVar4_mode))]
allordVar4m <-
  allordVar4_mode[, !grepl("hidp|pidp", colnames(allordVar4_mode))]

rm(
  allcontVar4_p,
  allcontVar4_idp,
  allordVar4_p,
  allordVar4_idp,
  allcontVar4_mode,
  allordVar4_mode
)

# Descriptive statistic tables by MODE-----------------------------------------------------------------

mode <- function(data, j) {
  if (grepl("_j$", colnames(data[j]))) {
    data$mode <- data$gridmodew10
  } else if (grepl("_i$", colnames(data[j]))) {
    data$mode <- data$gridmodew9
  } else if (grepl("_h$", colnames(data[j]))) {
    data$mode <- data$gridmodew8
  } else{
    data$mode <- data$gridmodew5
  }
  return(data$mode)
}

## table for continuous variables
end <- ncol(allcontVar4m)

for (j in 5:end) {
  #for(j in 5:69){

  allcontVar4m$mode <- mode(allcontVar4m, j)

  mean.var.cont_single <-
    round(mean(allcontVar4m[, j][allcontVar4m$mode == 0], na.rm = T), 3)
  mean.var.cont_mixed <-
    round(mean(allcontVar4m[, j][allcontVar4m$mode == 1], na.rm = T), 3)
  se.var.cont_single <-
    round(se(allcontVar4m[, j][allcontVar4m$mode == 0], na.rm = T), 3)
  se.var.cont_mixed <-
    round(se(allcontVar4m[, j][allcontVar4m$mode == 1], na.rm = T), 3)
  na.var.cont_single <-
    sum(is.na(allcontVar4m[, j][allcontVar4m$mode == 0])) / length(allcontVar4m[, j][allcontVar4m$mode ==
                                                                                       0])
  na.var.cont_mixed <-
    sum(is.na(allcontVar4m[, j][allcontVar4m$mode == 1])) / length(allcontVar4m[, j][allcontVar4m$mode ==
                                                                                       1])
  sena.var.cont_single <-
    round(sqrt(
      na.var.cont_single * (1 - na.var.cont_single) / length(allcontVar4m[, j][allcontVar4m$mode ==
                                                                                 0])
    ), 4)
  sena.var.cont_mixed <-
    round(sqrt(
      na.var.cont_mixed * (1 - na.var.cont_mixed) / length(allcontVar4m[, j][allcontVar4m$mode ==
                                                                               1])
    ), 4)
  na.var.cont_single <- round(na.var.cont_single, 4)
  na.var.cont_mixed <- round(na.var.cont_mixed, 4)

  df <-
    as.data.frame(matrix(
      c(
        mean.var.cont_single,
        se.var.cont_single,
        mean.var.cont_mixed,
        se.var.cont_mixed,
        na.var.cont_single * 100,
        sena.var.cont_single * 100,
        na.var.cont_mixed * 100,
        sena.var.cont_mixed * 100
      ),
      nrow = 4,
      ncol = 2,
      byrow = T
    ))
  df[3:4, ] <- format.data.frame(df[3:4, ], 2)

  df <- columnnames(allcontVar4m, df)

  name <- sub("_e|_f|_g|_h|_i|_j", "", colnames(allcontVar4m[j]))

  rowname <- c(
    str_c(name, "-mean-single"),
    str_c(name, "-mean-mixed"),
    str_c(name, "-NA-single (%)"),
    str_c(name, "-NA-mixed (%)")
  )

  if (j == 5) {
    d <- NA
    descr <- NA
    name_before <- NA
    descr_sub <-
      descr_append(df, d, descr, name, name_before, rowname, end, 5, 11)
  } else if (j <= 11) {
    descr <- NA
    descr_sub <-
      descr_append(df, d, descr, name, name_before, rowname, end, 5, 11)
  } else{
    descr_sub <-
      descr_append(df, d, descr, name, name_before, rowname, end, 5, 11)
  }
  df <- descr_sub[[1]]
  d <- descr_sub[[2]]
  descr <- descr_sub[[3]]
  name <- descr_sub[[4]]
  name_before <- descr_sub[[5]]

}#for loop

descr.contm <- descr

write.table(
  descr.contm,
  file = "./data/out/descr_cont_mode_charlotte.txt",
  sep = ",",
  quote = FALSE,
  col.names = NA
)

rm(
  mean.var.cont_single,
  se.var.cont_single,
  mean.var.cont_mixed,
  se.var.cont_mixed,
  na.var.cont_single,
  sena.var.cont_single,
  na.var.cont_mixed,
  sena.var.cont_mixed,
  name,
  name_before,
  df,
  d,
  descr,
  descr_sub
)
allcontVar4m$mode <- NULL

## table for ordinal variables
end <- ncol(allordVar4m)

for (j in 5:end) {
  allordVar4m$mode <- mode(allordVar4m, j)

  tab_single <-
    se(table(allordVar4m[, j][allordVar4m$mode == 0], useNA = "always"))
  tab_mixed <-
    se(table(allordVar4m[, j][allordVar4m$mode == 1], useNA = "always"))

  #extract proportions and standard errors
  df_single <- tabl(tab_single)
  df_single <- round(df_single * 100, 2)
  df_mixed <- tabl(tab_mixed)
  df_mixed <- round(df_mixed * 100, 2)

  df <-
    rbind(
      df_single[1, ],
      df_mixed[1, ],
      df_single[2, ],
      df_mixed[2, ],
      df_single[3, ],
      df_mixed[3, ],
      df_single[4, ],
      df_mixed[4, ],
      df_single[5, ],
      df_mixed[5, ],
      df_single[6, ],
      df_mixed[6, ]
    )
  df <- data.frame(df)

  name <- sub("_e|_f|_g|_h|_i|_j", "", colnames(allordVar4m[j]))

  df <- columnnames(allordVar4m, df)

  rowname <- c(
    str_c(name, "-single-0 (%)"),
    str_c(name, "-mixed-0 (%)"),
    str_c(name, "-single-1 (%)"),
    str_c(name, "-mixed-1 (%)"),
    str_c(name, "-single-2 (%)"),
    str_c(name, "-mixed-2 (%)"),
    str_c(name, "-single-3 (%)"),
    str_c(name, "-mixed-3 (%)"),
    str_c(name, "-single-4 (%)"),
    str_c(name, "-mixed-4 (%)"),
    str_c(name, "-single-NA (%)"),
    str_c(name, "-mixed-NA (%)")
  )

  if (j == 5) {
    d <- NA
    descr <- NA
    name_before <- NA
    descr_sub <-
      descr_append(df, d, descr, name, name_before, rowname, end, 5, 10)
  } else if (j <= 10) {
    descr <- NA
    descr_sub <-
      descr_append(df, d, descr, name, name_before, rowname, end, 5, 10)
  } else{
    descr_sub <-
      descr_append(df, d, descr, name, name_before, rowname, end, 5, 10)
  }
  df <- descr_sub[[1]]
  d <- descr_sub[[2]]
  descr <- descr_sub[[3]]
  name <- descr_sub[[4]]
  name_before <- descr_sub[[5]]

}#for loop

descr.ordm <- descr

descr.ordm <-
  descr.ordm[rowSums(is.na(descr.ordm)) != ncol(descr.ordm), ]

write.table(
  descr.ordm,
  file = "./data/out/descr_ord_mode_charlotte.txt",
  sep = ",",
  quote = FALSE,
  col.names = NA
)

rm(df_single,
   df_mixed,
   df,
   tab_mixed,
   tab_single,
   name,
   name_before,
   d,
   descr,
   descr_sub)
allordVar4m$mode <- NULL

# Descriptive tables by mode with significance levels-----------------------------------

#create a function to produce the significance level
significance <- function(z) {
  if (abs(z) >= 2.576) {
    stars <- "***"
  } else if (2.576 > abs(z) && abs(z) >= 1.96) {
    stars <- "**"
  } else if (1.96 > abs(z) && abs(z) >= 1.645) {
    stars <- "*"
  } else {
    stars <- ""
  }
  return(stars)
}

## tables for continuous variables by mode with significance

end <- ncol(allcontVar4m)

for (j in 5:end) {
  allcontVar4m$mode <- mode(allcontVar4m, j)

  mean.var.cont_single <-
    mean(allcontVar4m[, j][allcontVar4m$mode == 0], na.rm = T)
  mean.var.cont_mixed <-
    mean(allcontVar4m[, j][allcontVar4m$mode == 1], na.rm = T)
  se.var.cont_single <-
    se(allcontVar4m[, j][allcontVar4m$mode == 0], na.rm = T)
  se.var.cont_mixed <-
    se(allcontVar4m[, j][allcontVar4m$mode == 1], na.rm = T)
  na.var.cont_single <-
    sum(is.na(allcontVar4m[, j][allcontVar4m$mode == 0])) / length(allcontVar4m[, j][allcontVar4m$mode ==
                                                                                       0])
  na.var.cont_mixed <-
    sum(is.na(allcontVar4m[, j][allcontVar4m$mode == 1])) / length(allcontVar4m[, j][allcontVar4m$mode ==
                                                                                       1])
  sena.var.cont_single <-
    sqrt(na.var.cont_single * (1 - na.var.cont_single) / length(allcontVar4m[, j][allcontVar4m$mode ==
                                                                                    0]))
  sena.var.cont_mixed <-
    sqrt(na.var.cont_mixed * (1 - na.var.cont_mixed) / length(allcontVar4m[, j][allcontVar4m$mode ==
                                                                                  1]))

  z_m <-
    (mean.var.cont_single - mean.var.cont_mixed) / sqrt((se.var.cont_single) ^
                                                          2 + (se.var.cont_mixed) ^ 2)
  z_na <-
    (na.var.cont_single - na.var.cont_mixed) / sqrt((sena.var.cont_single) ^
                                                      2 + (sena.var.cont_mixed) ^ 2)

  mean.var.cont_single <- round(mean.var.cont_single, 3)
  mean.var.cont_mixed <- round(mean.var.cont_mixed, 3)
  se.var.cont_single <- round(se.var.cont_single, 3)
  se.var.cont_mixed <- round(se.var.cont_mixed, 3)
  na.var.cont_single <- round(na.var.cont_single, 4)
  na.var.cont_mixed <- round(na.var.cont_mixed, 4)
  sena.var.cont_single <- round(sena.var.cont_single, 4)
  sena.var.cont_mixed <- round(sena.var.cont_mixed, 4)

  sign_m <- significance(z_m)
  sign_na <- significance(z_na)

  df <-
    as.data.frame(matrix(
      c(
        mean.var.cont_single,
        se.var.cont_single,
        mean.var.cont_mixed,
        se.var.cont_mixed,
        round(mean.var.cont_single - mean.var.cont_mixed, 3),
        sign_m,
        na.var.cont_single * 100,
        sena.var.cont_single * 100,
        na.var.cont_mixed * 100,
        sena.var.cont_mixed * 100,
        round((na.var.cont_single - na.var.cont_mixed) *
                100, 2),
        sign_na
      ),
      nrow = 6,
      ncol = 2,
      byrow = T
    ))
  df[4:6, ] <- format.data.frame(df[4:6, ], 2)

  df <- columnnames(allcontVar4m, df)

  name <- sub("_e|_f|_g|_h|_i|_j", "", colnames(allcontVar4m[j]))

  rowname <-
    c(
      str_c(name, "-mean-single"),
      str_c(name, "-mean-mixed"),
      str_c(name, "-sign.Diff.m"),
      str_c(name, "-pNA-single (%)"),
      str_c(name, "-pNA-mixed (%)"),
      str_c(name, "-sign.Diff.NA")
    )
  if (j == 5) {
    d <- NA
    descr <- NA
    name_before <- NA
    descr_sub <-
      descr_append(df, d, descr, name, name_before, rowname, end, 5, 11)
  } else if (j <= 11) {
    descr <- NA
    descr_sub <-
      descr_append(df, d, descr, name, name_before, rowname, end, 5, 11)
  } else{
    descr_sub <-
      descr_append(df, d, descr, name, name_before, rowname, end, 5, 11)
  }
  df <- descr_sub[[1]]
  d <- descr_sub[[2]]
  descr <- descr_sub[[3]]
  name <- descr_sub[[4]]
  name_before <- descr_sub[[5]]


}#for loop

descr.conts <- descr

write.table(
  descr.conts,
  file = "./data/out/descr_cont_sign_charlotte.txt",
  sep = ",",
  quote = FALSE,
  col.names = NA
)

rm(
  mean.var.cont_single,
  se.var.cont_single,
  mean.var.cont_mixed,
  se.var.cont_mixed,
  na.var.cont_single,
  sena.var.cont_single,
  na.var.cont_mixed,
  sena.var.cont_mixed,
  z_m,
  z_na,
  sign_m,
  sign_na,
  name,
  name_before,
  df,
  d,
  descr,
  descr_sub
)
allcontVar4m$mode <- NULL

## tables for ordinal variables by mode with significance

end <- ncol(allordVar4m)

for (j in 5:end) {
  allordVar4m$mode <- mode(allordVar4m, j)

  tab_single <-
    se(table(allordVar4m[, j][allordVar4m$mode == 0], useNA = "always"))
  tab_mixed <-
    se(table(allordVar4m[, j][allordVar4m$mode == 1], useNA = "always"))

  #extract proportions and standard errors
  df_single <- tabl(tab_single)
  df_mixed <- tabl(tab_mixed)

  if (0 %in% tab_mixed$value) {
    z_0 <-
      (df_single[1, 1] - df_mixed[1, 1]) / sqrt((df_single[1, 2]) ^ 2 + (df_mixed[1, 2]) ^
                                                  2)
    sign_0 <- significance(z_0)
    Diff_0 <- round((df_single[1, 1] - df_mixed[1, 1]) * 100, 2)
    sign.Diff_0 <- c(Diff_0, sign_0)
  } else{
    sign.Diff_0 <- c(NA, NA)
  }
  if (1 %in% tab_mixed$value) {
    z_1 <-
      (df_single[2, 1] - df_mixed[2, 1]) / sqrt((df_single[2, 2]) ^ 2 + (df_mixed[2, 2]) ^
                                                  2)
    sign_1 <- significance(z_1)
    Diff_1 <- round((df_single[2, 1] - df_mixed[2, 1]) * 100, 2)
    sign.Diff_1 <- c(Diff_1, sign_1)
  } else{
    sign.Diff_1 <- c(NA, NA)
  }
  if (2 %in% tab_mixed$value) {
    z_2 <-
      (df_single[3, 1] - df_mixed[3, 1]) / sqrt((df_single[3, 2]) ^ 2 + (df_mixed[3, 2]) ^
                                                  2)
    sign_2 <- significance(z_2)
    Diff_2 <- round((df_single[3, 1] - df_mixed[3, 1]) * 100, 2)
    sign.Diff_2 <- c(Diff_2, sign_2)
  } else{
    sign.Diff_2 <- c(NA, NA)
  }
  if (3 %in% tab_mixed$value) {
    z_3 <-
      (df_single[4, 1] - df_mixed[4, 1]) / sqrt((df_single[4, 2]) ^ 2 + (df_mixed[4, 2]) ^
                                                  2)
    sign_3 <- significance(z_3)
    Diff_3 <- round((df_single[4, 1] - df_mixed[4, 1]) * 100, 2)
    sign.Diff_3 <- c(Diff_3, sign_3)
  } else{
    sign.Diff_3 <- c(NA, NA)
  }
  if (4 %in% tab_mixed$value) {
    z_4 <-
      (df_single[5, 1] - df_mixed[5, 1]) / sqrt((df_single[5, 2]) ^ 2 + (df_mixed[5, 2]) ^
                                                  2)
    sign_4 <- significance(z_4)
    Diff_4 <- round((df_single[5, 1] - df_mixed[5, 1]) * 100, 2)
    sign.Diff_4 <- c(Diff_4, sign_4)
  } else{
    sign.Diff_4 <- c(NA, NA)
  }
  if (NA %in% tab_mixed$value) {
    z_na <-
      (df_single[6, 1] - df_mixed[6, 1]) / sqrt((df_single[6, 2]) ^ 2 + (df_mixed[6, 2]) ^
                                                  2)
    sign_na <- significance(z_na)
    Diff_na <- round((df_single[6, 1] - df_mixed[6, 1]) * 100, 2)
    sign.Diff_na <- c(Diff_na, sign_na)
  } else{
    sign.Diff_na <- c(NA, NA)
  }

  df_single <- round(df_single * 100, 2)
  df_mixed <- round(df_mixed * 100, 2)
  df <-
    rbind(
      df_single[1, ],
      df_mixed[1, ],
      sign.Diff_0,
      df_single[2, ],
      df_mixed[2, ],
      sign.Diff_1,
      df_single[3, ],
      df_mixed[3, ],
      sign.Diff_2,
      df_single[4, ],
      df_mixed[4, ],
      sign.Diff_3,
      df_single[5, ],
      df_mixed[5, ],
      sign.Diff_4,
      df_single[6, ],
      df_mixed[6, ],
      sign.Diff_na
    )

  df <- data.frame(df)
  df <- columnnames(allordVar4m, df)

  name <- sub("_e|_f|_g|_h|_i|_j", "", colnames(allordVar4m[j]))
  rowname <- c(
    str_c(name, "-single-0 (%)"),
    str_c(name, "-mixed-0 (%)"),
    str_c(name, "-sign.Diff-0 (%)"),
    str_c(name, "-single-1 (%)"),
    str_c(name, "-mixed-1 (%)"),
    str_c(name, "-sign.Diff-1 (%)"),
    str_c(name, "-single-2 (%)"),
    str_c(name, "-mixed-2 (%)"),
    str_c(name, "-sign.Diff-2 (%)"),
    str_c(name, "-single-3 (%)"),
    str_c(name, "-mixed-3 (%)"),
    str_c(name, "-sign.Diff-3 (%)"),
    str_c(name, "-single-4 (%)"),
    str_c(name, "-mixed-4 (%)"),
    str_c(name, "-sign.Diff-4 (%)"),
    str_c(name, "-single-NA (%)"),
    str_c(name, "-mixed-NA (%)"),
    str_c(name, "-sign.Diff-NA (%)")
  )
  if (j == 5) {
    d <- NA
    descr <- NA
    name_before <- NA
    descr_sub <-
      descr_append(df, d, descr, name, name_before, rowname, end, 5, 10)
  } else if (j <= 10) {
    descr <- NA
    descr_sub <-
      descr_append(df, d, descr, name, name_before, rowname, end, 5, 10)
  } else{
    descr_sub <-
      descr_append(df, d, descr, name, name_before, rowname, end, 5, 10)
  }

  df <- descr_sub[[1]]
  d <- descr_sub[[2]]
  descr <- descr_sub[[3]]
  name <- descr_sub[[4]]
  name_before <- descr_sub[[5]]
}#for loop

descr.ords <- descr

descr.ords <-
  descr.ords[rowSums(is.na(descr.ords)) != ncol(descr.ords), ]

write.table(
  descr.ords,
  file = "./data/out/descr_ord_sign_charlotte.txt",
  sep = ",",
  quote = FALSE,
  col.names = NA
)

rm(
  sign_0,
  sign_1,
  sign_2,
  sign_3,
  sign_4,
  sign_na,
  tab_mixed,
  tab_single,
  z_0,
  z_1,
  z_2,
  z_3,
  z_4,
  z_na,
  name,
  name_before,
  Diff_0,
  Diff_1,
  Diff_2,
  Diff_3,
  Diff_4,
  Diff_na,
  sign.Diff_0,
  sign.Diff_1,
  sign.Diff_2,
  sign.Diff_3,
  sign.Diff_4,
  sign.Diff_na,
  df,
  df_mixed,
  df_single,
  d,
  descr,
  descr_sub
)
allordVar4m$mode <- NULL
