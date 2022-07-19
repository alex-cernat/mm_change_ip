# The Impact of Mixing Survey Modes on Estimates of Change: A Quasi-Experimental Study

This is the code used for the analysis done in the peer-reviewed paper:

Cernat, A. & Sakshaug, W. J. (2022) The Impact of Mixing Survey Modes on Estimates of Change: A Quasi-Experimental Study. *Journal of Survey Statistics and Methodology*.

## Data

The analysis is based on the Understanding Society Innovation Panel. The data can be freely downloaded from [the UK Data Archive](https://beta.ukdataservice.ac.uk/datacatalogue/studies/study?id=6849).

## Scripts used

The "00_identify_vars_use.R" script is used to explore the data and identify variables that are eligible for analysis. The "01.clean_analyze.R" script is used to clean data and prepare the Mplus syntax for analysis. The "02.results.R" is used to import the results from Mplus, aggregate and descrive the results. The "03.sensitivity...R" scripts are used for doing sensitivity analyses. Finally, the "04.descriptives.R" is used to create descriptive statistics. Helper functions can be found in the "functions" subfolder.

Git and Renv are used for version control and package history.

## Session information

    sessionInfo()

    R version 4.1.0 (2021-05-18)
    Platform: x86_64-w64-mingw32/x64 (64-bit)
    Running under: Windows 10 x64 (build 19042)

    Matrix products: default

    locale:
    [1] LC_COLLATE=English_United Kingdom.1252  LC_CTYPE=English_United Kingdom.1252   
    [3] LC_MONETARY=English_United Kingdom.1252 LC_NUMERIC=C                           
    [5] LC_TIME=English_United Kingdom.1252    

    attached base packages:
    [1] stats     graphics  grDevices datasets  utils     methods   base     

    other attached packages:
     [1] nnet_7.3-16           MplusAutomation_1.0.0 lavaan_0.6-9         
     [4] haven_2.4.1           forcats_0.5.1         stringr_1.4.0        
     [7] dplyr_1.0.7           purrr_0.3.4           readr_1.4.0          
    [10] tidyr_1.1.3           tibble_3.1.2          ggplot2_3.3.5        
    [13] tidyverse_1.3.1      

    loaded via a namespace (and not attached):
     [1] Rcpp_1.0.6        lubridate_1.7.10  lattice_0.20-44   assertthat_0.2.1 
     [5] digest_0.6.27     utf8_1.2.1        R6_2.5.0          cellranger_1.1.0 
     [9] plyr_1.8.6        backports_1.2.1   reprex_2.0.0      stats4_4.1.0     
    [13] coda_0.19-4       httr_1.4.2        pillar_1.6.1      rlang_0.4.11     
    [17] readxl_1.3.1      rstudioapi_0.13   data.table_1.14.0 texreg_1.37.5    
    [21] pbivnorm_0.6.0    checkmate_2.0.0   gsubfn_0.7        proto_1.0.0      
    [25] pander_0.6.4      fastDummies_1.6.3 munsell_0.5.0     broom_0.7.8      
    [29] compiler_4.1.0    modelr_0.1.8      pkgconfig_2.0.3   mnormt_2.0.2     
    [33] tmvnsim_1.0-2     tidyselect_1.1.1  fansi_0.5.0       crayon_1.4.1     
    [37] dbplyr_2.1.1      withr_2.4.2       grid_4.1.0        xtable_1.8-4     
    [41] jsonlite_1.7.2    gtable_0.3.0      lifecycle_1.0.0   DBI_1.1.1        
    [45] magrittr_2.0.1    scales_1.1.1      cli_3.0.0         stringi_1.6.2    
    [49] renv_0.13.2       fs_1.5.0          xml2_1.3.2        ellipsis_0.3.2   
    [53] generics_0.1.0    vctrs_0.3.8       boot_1.3-28       tools_4.1.0      
    [57] glue_1.4.2        hms_1.1.0         parallel_4.1.0    colorspace_2.0-2 
    [61] rvest_1.0.0 
