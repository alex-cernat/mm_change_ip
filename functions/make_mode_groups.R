# function to get the mode of the modes in which people answered
make_mode_groups <- function(waves) {

  # mode function
  mode <- function(x) {
    ux <- unique(x)
    ux <- ux[!is.na(ux)]
    ux[which.max(tabulate(match(x, ux)))]
  }

  vars <- str_c("indmode_", waves)

  mode_data %>%
    select(vars) %>%
    mutate_all(~ifelse(. %in% c(-7, 2), NA, .)) %>%
    apply(1, mode)

}
