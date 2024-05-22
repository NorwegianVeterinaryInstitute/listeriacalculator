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
