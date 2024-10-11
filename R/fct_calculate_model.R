#' calc_mini_table_1
#' @param prod_temp temperature in Celsius
#' @param prod_days period in days
#' @param initial_conc initial concentration of bacteria
#'
#' @details
#' #' This function calculates the values that are seen in the
#' Lagring i produksjonsbedrift av laks (H14) mini table in the
#' Excel sheet.
#'
#' It returns values that are equivalent to row 1 and row 2 of
#' the results table (A54) BUT without the cumulative sum.
#'
#' Reference data points are always calculated with default values.
#' For the first mini table those are temperature = 4 and days = 3.
#'
#' @return a list of calculated data
#'
#' @noRd
calc_mini_table_1 <- function(prod_temp = 4,
                              prod_days = 3,
                              initial_conc = 1) {
  log_ic <- log10(as.numeric(initial_conc))

  ref_growth <- 0.007343
  ref_temp <- 4
  ref_days <- 3

  likely_growth <- ref_growth * (prod_temp + 1.5) ^ 2 / (ref_temp + 1.5) ^ 2
  row_1 <- c(
    reference = 0,
    likely = log_ic ,
    min = log_ic,
    max = log_ic
  )
  row_2 <- c(
    reference = ref_growth * ref_days * 24,
    likely = likely_growth * prod_days * 24,
    min = 0.9 * likely_growth * prod_days * 24,
    max = 1.1 * likely_growth * prod_days * 24
  )

  return(list(start = row_1, production = row_2))

}


#' calc_mini_table_2
#'
#' @param store_temp temperature in Celsius
#' @param store_days period in days
#'
#' @details
#' This function calculates the values that are seen in the
#' Innkjøp i butikk (H21) mini table in the  Excel sheet.
#'
#' It returns values that are equivalent to row 3 of
#' the results table (A54) BUT without the cumulative sum.
#'
#' Reference data points are always calculated with default values.
#' For the second mini table those are temperature = 4 and days = 3.
#'
#' @return a list of calculated data
#'
#' @noRd
calc_mini_table_2 <- function(store_temp = 4,
                              store_days = 3) {


  ref_growth <- 0.007343
  ref_temp <- 4
  ref_days <- 3
  likely_growth <- ref_growth * (store_temp + 1.5) ^ 2 / (ref_temp + 1.5) ^ 2
  row_3 <- c(
    reference = ref_growth * ref_days * 24,
    likely = likely_growth * store_days * 24,
    min = 0.9 * likely_growth * store_days * 24,
    max = 1.1 * likely_growth * store_days * 24
  )

  return(list(start = row_3))

}


#' calc_mini_table_3_4
#'
#' @param prod_temp temperature in Celsius
#' @param home_temp temperature in Celsius
#' @param home_hours period in hours
#' @param salmon_temp temperature in Celsius
#' @param salmon_hours period in hours
#'
#' @details
#' This function calculates the values that are seen in the
#' Transport hjem (H29) and lagring av laks hjemme før laging av sushi
#' (H35) mini table in the  Excel sheet.
#'
#' It returns values that are equivalent to row 4 and row 5 of
#' the results table (A54) BUT without the cumulative sum.
#'
#' We do the two mini tables together because the reference growth for
#' the next step depends on the one in the previous step.
#'
#' Reference data points are always calculated with default values.
#' For the third and fourth mini table those are
#' temperature = 10 and hours = 3.
#'
#' @return a list of calculated data
#'
#' @noRd
calc_mini_table_3_4 <- function(prod_temp = 4,
                                home_temp = 10,
                                home_hours = 3,
                                salmon_temp = 10,
                                salmon_hours = 3) {

  ref_growth <- 0.007343 * (10 + 1.5) ^ 2 / (4 + 1.5) ^ 2
  ref_temp <- 10
  ref_hours <- 3

  likely_growth <- ref_growth * (home_temp + 1.5) ^ 2 / (ref_temp + 1.5) ^ 2

  row_4 <- c(
    reference = ref_growth * ref_hours,
    likely = likely_growth * home_hours,
    min = 0.9 * likely_growth * home_hours,
    max = 1.1 * likely_growth * home_hours
  )


  ref_growth_2 <- 0.007343 * (10 + 1.5) ^ 2 / (10 + 1.5) ^ 2
  ref_temp_2 <- 10
  ref_hours_2 <- 3

  likely_growth_2 <- ref_growth_2 * (salmon_temp + 1.5) ^ 2 / (ref_temp_2 + 1.5) ^ 2

  row_5 <- c(
    reference = ref_growth_2 * ref_hours_2,
    likely = likely_growth_2 * salmon_hours,
    min = 0.9 * likely_growth * salmon_hours,
    max = 1.1 * likely_growth * salmon_hours
  )

  
  return(list(home = row_4, salmon = row_5))

}


#' calc_mini_table_5_6
#'
#' @param sushi_temp temperature in Celsius
#' @param sushi_hours period in hours
#' @param period_temp temperature in Celsius
#' @param period_hours period in hours
#'
#' @details
#' This function calculates the values that are seen in the
#' Lagring av sushi hjemme, før temperering (H42) and
#' Tempereringsperiode (H48) mini table in the
#' Excel sheet.
#'
#' It returns values that are equivalent to row 1 and row 2 of
#' the results table (A54) BUT without the cumulative sum.
#'
#' Reference data points are always calculated with default values.
#' For the fifth and sixth mini table those are
#' temperature = 4 and hours = 12, and temperature = 22 and hours 6
#' respectively.
#'
#' @return a list of calculated data
#'
#' @noRd
calc_mini_table_5_6 <- function(sushi_temp = 4,
                                sushi_hours = 12,
                                period_temp = 22,
                                period_hours = 6,
                                sushi_pctg = 20) {

  ref_growth <- 0.00286
  ref_temp <- 4
  ref_hours <- 12
  likely_growth <- ref_growth * (sushi_temp + 1.5) ^ 2 / (ref_temp + 1.5) ^ 2
  row_6 <- c(
    reference = ref_growth * ref_hours,
    likely = likely_growth * sushi_hours,
    min = 0.9 * likely_growth * sushi_hours,
    max = 1.1 * likely_growth * sushi_hours
  )

  ref_growth_2 <- 0.06
  ref_temp_2 <- 22
  ref_hours_2 <- 6
  likely_growth_2 <- ref_growth_2 * (period_temp + 1.5) ^ 2 / (ref_temp_2 + 1.5) ^ 2
  row_7 <- c(
    reference = ref_growth_2 * ref_hours_2,
    likely = likely_growth_2 * period_hours,
    min = 0.9 * likely_growth_2 * period_hours,
    max = 1.1 * likely_growth_2 * period_hours
  )

  # here we use salmon % to decrease the value of row_6
  
  sushi_log <- log10(sushi_pctg/100)
  
  row_6_sushi <- row_6 + sushi_log
  
  return(list(sushi = row_6_sushi, period = row_7))

}

#' calc_wrapper
#'
#' @description
#' A wrapper for the calculation functions to produce a single table
#'
#' @inheritParams calc_mini_table_1
#' @inheritParams calc_mini_table_2
#' @inheritParams calc_mini_table_3_4
#' @inheritParams calc_mini_table_5_6
#'
#' @return a data frame with calculated values
#'
#' @noRd
calc_wrapper <- function(prod_temp = 4,
                         prod_days = 3,
                         store_temp = 4,
                         store_days = 3,
                         home_temp = 10,
                         home_hours = 3,
                         salmon_temp = 10,
                         salmon_hours = 3,
                         sushi_temp = 4,
                         sushi_hours = 12,
                         period_temp = 22,
                         period_hours = 6,
                         initial_conc = 1, 
                         sushi_pctg = 20) {
  dat_1 <- calc_mini_table_1(
    prod_temp = prod_temp,
    prod_days = prod_days,
    initial_conc = initial_conc
  )
  dat_2 <- calc_mini_table_2(
    store_temp = store_temp,
    store_days = store_days
  )
  dat_3 <- calc_mini_table_3_4(
    prod_temp = prod_temp,
    home_temp = home_temp,
    home_hours = home_hours,
    salmon_temp = salmon_temp,
    salmon_hours = salmon_hours
  )
  dat_4 <- calc_mini_table_5_6(
    sushi_temp = sushi_temp,
    sushi_hours = sushi_hours,
    period_temp = period_temp,
    period_hours = period_hours,
    sushi_pctg = sushi_pctg
  )
  dat <- data.frame(rbind(
    t(data.frame(dat_1)),
    t(data.frame(dat_2)),
    t(data.frame(dat_3)),
    t(data.frame(dat_4))
  ))

  dat_all <- data.frame(lapply(dat, cumsum))

  dat_all$steps <- c(names(dat_1), names(dat_2), names(dat_3), names(dat_4))
  dat_all$limit_1 <- 2
  dat_all$limit_2 <- 3
  dat_all$limit_3 <- 5

  return(dat_all)
}
