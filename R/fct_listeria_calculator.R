

#' calc_growth
#' 
#' Calculate listeria growth rate given a default growth rate and a 
#' default temperature
#'
#' @param temperature 
#' @param scale
#'
#' @return number, calculated growth rate
calc_growth <- function(temperature, start_temp, start_growth, scale = NULL) {
  
  if (scale) {
    start_growth * scale * (temperature + 1.5) ^ 2 / (start_temp + 1.5) ^ 2
  } 
  
  start_growth * (temperature + 1.5) ^ 2 / (start_temp + 1.5) ^ 2
}


#' log_growth
#'
#' Calculate the logarithmic growth of listeria
#'
#' @param temperature
#' @param time
#' @param concetration
#'
#' @return number, calculated log
log_growth <- function(temperature, time, concetration) {
  if (concetration == 0) {
    time * calc_growth(temperature)
  } else {
    concetration + (time * calc_growth(temperature))
  }
}

calc_column <- function(var = "ref",
                        prod_temp = 4,
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
                        initial_conc = 1) {
  
  if (var == "ref") {
    
    start <- log(initial_conc)
    prod <- log_growth(temperature = prod_temp, time = prod_days*24, concetration = start)
    store <- log_growth(temperature = store_temp, time = store_days*24, concetration = prod)
    home <- log_growth(temperature = home_temp, time = home_hours, concetration = store)
    salmon <- log_growth(temperature = salmon_temp, time = salmon_hours, concetration = home)
    sushi <- log_growth(temperature = sushi_temp, time = sushi_hours, concetration = salmon)
    period <- log_growth(temperature = period_temp, time = period_hours, concetration = sushi)
    
    return(c(start = start, prod = prod, store = store, 
             home = home, salmon = salmon,
             sushi = sushi, period = period))
  }
  
  
}



#' fct_listeria_calculator
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd

make_plot <- function(prod_temp = 4,
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
                      lang = "en") {



  p <- dat |>
    dplyr::mutate(name = stringr::str_wrap(name, 15)) |>
    dplyr::group_by(category) |>
    echarts4r::e_charts(name) |>
    echarts4r::e_line(value) |>
    echarts4r::e_x_axis(axisLabel = list(rotate = 30, interval = 0L))

  return(p)

}
