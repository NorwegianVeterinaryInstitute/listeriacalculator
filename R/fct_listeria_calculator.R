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
                      home_temp = 4,
                      home_hours = 3,
                      salmon_temp = 4,
                      salmon_hours = 3,
                      sushi_temp = 4,
                      sushi_hours = 3,
                      period_temp = 4,
                      period_hours = 3,
                      initial_conc = 0.04,
                      lang = "en") {

  x = c("a", "b", "c", "d", "e")
  y = c(1, 3, 4, 7, 9) * as.numeric(initial_conc)
  dat <- data.frame(x,y)
  p <- dat |>  echarts4r::e_charts(x) |>
    echarts4r::e_line(y)

  return(p)

}
