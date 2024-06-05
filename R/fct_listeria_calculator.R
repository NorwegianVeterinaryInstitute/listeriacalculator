#' Title
#'
#' @param dat
#' @param lang
#'
#' @return
#' @export
#'
#' @examples
translate_and_pivot <- function(dat, lang = "en") {
  dat <- dat |>
    dplyr::relocate(steps, dplyr::everything())

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
      "	grenseverdi i lovverket",
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
    tidyr::pivot_longer(!steps, names_to = "category", values_to = "value")


}


#' fct_listeria_calculator
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd

make_plot <- function(dat) {
  p <- dat |>
    dplyr::mutate(steps = stringr::str_wrap(steps, 15)) |>
    dplyr::group_by(category) |>
    echarts4r::e_charts(steps) |>
    echarts4r::e_line(value) |>
    echarts4r::e_x_axis(axisLabel = list(rotate = 30, interval = 0L))

  return(p)

}


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
                              lang) {
  dat <- calc_wrapper(
    prod_temp,
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
    initial_conc
  )

  dat_long <- translate_and_pivot(dat, lang)
  p <- make_plot(dat = dat_long)

  return(p)
}

