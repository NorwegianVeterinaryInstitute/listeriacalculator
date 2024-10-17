#' translate_and_pivot
#'
#' @description
#' translate and pivot in ggplot friendly format for plotting
#'
#' @param dat a data frame with calculated growth data
#' @param lang the language to be used for translation
#'
#' @importFrom dplyr relocate everything
#' @importFrom tidyr pivot_longer
#' @importFrom rlang .data
#' @return a data frame, ready for ploting.
#'
#' @noRd
translate_and_pivot <- function(dat, lang = "en") {
  dat <- dat |>
    dplyr::relocate(.data$steps, dplyr::everything())

  if (lang == "en") {
    names(dat) <- c(
      "steps",
      "reference",
      "test conditions - most likely growth",
      "test conditions - least growth",
      "test conditions - most growth",
      "legal limit",
      "limit for greater likelihood of listeriosis cases among vulnerable consumers",
      "limit for greater likelihood of listeriosis cases among healthy adult consumers"
    )
    dat$steps <- c(
      "initial concentration in salmon",
      "shipment of salmon from company",
      "when purchasing salmon in store",
      "in salmon when it arrives home",
      "in salmon stored at home",
      "in sushi stored at home",
      "after tempering sushi"
    )
  } else {
    names(dat) <- c(
      "steps",
      "referanse",
      "testforhold-mest sannsynlig vekst",
      "testforhold minst vekst",
      "testforhold mest vekst",
      "grenseverdi i lovverket",
      "grenseverdi for større sannsynlegheit for listeriosetilfeller hjå utsett forbrukarer",
      "grenseverdi for større sannsynlegheit for listeriosetilfeller hjå friske, vaksne forbrukarer"
    )
    dat$steps <- c(
      "startkonsentrasjon i laks",
      "utsending av laks frå bedrift",
      "ved innkjøp av laks i butikk",
      "i laks når fisken kjem heim",
      "i laks lagra heime",
      "i sushi lagra heime",
      "etter temperering av sushi"
    )
  }

  dat_longer <- dat |>
    tidyr::pivot_longer(!.data$steps, names_to = "category", values_to = "value")


}


#' make_plot
#'
#' @description function to make the plot
#'
#' @importFrom dplyr mutate group_by
#' @importFrom echarts4r e_charts e_line e_x_axis
#' @importFrom rlang .data
#'
#' @return a plot in echarts format
#'
#' @noRd

make_plot <- function(dat) {
  p <- dat |>
    dplyr::mutate(steps = stringr::str_wrap(.data$steps, 15)) |>
    dplyr::group_by(.data$category) |>
    echarts4r::e_charts_("steps") |>
    echarts4r::e_line_("value") |>
    echarts4r::e_x_axis_(axisLabel = list(rotate = 30, interval = 0L))

  return(p)

}


#' calc_plot_wrapper
#'
#' @description
#' a function that wraps the calculation and plotting to be
#' used in the shiny module
#'
#' @inheritParams calc_mini_table_1
#' @inheritParams calc_mini_table_2
#' @inheritParams calc_mini_table_3_4
#' @inheritParams calc_mini_table_5_6
#'
#' @param lang the language to be used for translation
#'
#' @return a plot
#'
#' @noRd
calc_plot_wrapper <- function(prod_temp,
                              prod_days,
                              store_temp,
                              store_days,
                              home_temp,
                              home_hours,
                              salmon_temp,
                              salmon_hours,
                              sushi_temp,
                              sushi_hours,
                              period_temp,
                              period_hours,
                              initial_conc,
                              sushi_pctg,
                              lang) {

  dat <- calc_wrapper(
    prod_temp = prod_temp,
    prod_days = prod_days,
    store_temp = store_temp,
    store_days = store_days,
    home_temp = home_temp,
    home_hours = home_hours,
    salmon_temp = salmon_temp,
    salmon_hours = salmon_hours,
    sushi_temp = sushi_temp,
    sushi_hours = sushi_hours,
    period_temp = period_temp,
    period_hours = period_hours,
    initial_conc = initial_conc,
    sushi_pctg = sushi_pctg
  )

  dat_long <- translate_and_pivot(dat, lang)
  p <- make_plot(dat = dat_long)

  return(p)
}