#############################################################
#
# Project looking at how mode influences estimates of change
#
#
#
#############################################################

# clear working space
rm(list = ls ())



# Admin -------------------------------------------------------------------

# folders for installed packages
#.libPaths(c(paste0("C:/Users/", Sys.getenv("USERNAME"), "/Dropbox (The University of Manchester)/R/package"),  .libPaths()))



# create folders and unzip
# dir.create("./output")
# dir.create("./functions")
# dir.create("./data")
#


# install packages and load

# use packrat to install packages localy

pkg <- c("tidyverse",  "haven", "lavaan")

sapply(pkg, library, character.only = T)

# load local functions
# map(str_c("./functions/",
#           list.files("./functions/")),
#     source)


