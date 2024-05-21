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

  #x = c("a", "b", "c", "d", "e")
  #y = c(1, 3, 4, 7, 9) * as.numeric(initial_conc)
  #dat <- data.frame(x,y)
  
  dat <- tibble::tribble(
    ~ name,
    ~ referanse,
    ~ `testforhold-mest.sannsynlig.vekst`,
    ~ testforhold.minst.vekst,
    ~ testforhold.mest.vekst,
    ~ grenseverdi.i.lovverket,
    ~ grenseverdi.for.større.sannsynlegheit.for.listeriosetilfeller.hjå.utsett.forbrukarer,
    ~ `grenseverdi.for.større.sannsynlegheit.for.listeriosetilfeller.hjå.friske,.vaksne.forbrukarer`,
    "startkonsentrasjon i laks",
    0,
    -0.698970004,
    -0.698970004,
    -0.698970004,
    2L,
    3L,
    5L,
    "utsending av laks frå bedrift",
    0.528696,
    -0.227076054,
    -0.274265449,
    -0.179886659,
    2L,
    3L,
    5L,
    "ved innkjøp av laks i butikk",
    1.057392,
    2.298430458,
    1.998690412,
    2.598170505,
    2L,
    3L,
    5L,
    "i laks når fisken kjem heim",
    1.153700603,
    2.628865458,
    2.296081912,
    2.961649005,
    2L,
    3L,
    5L,
    "i laks lagra heime",
    1.250009207,
    4.221507541,
    3.729459787,
    4.713555296,
    2L,
    3L,
    5L,
    "i sushi lagra heime",
    1.284329207,
    4.255827541,
    3.760347787,
    4.751307296,
    2L,
    3L,
    5L,
    "etter temperering av sushi",
    1.644329207,
    4.55715846,
    4.031545614,
    5.082771307,
    2L,
    3L,
    5L
  ) |> janitor::clean_names() |> 
    dplyr::mutate(name = factor(name, 
                                levels = c("startkonsentrasjon i laks", 
                                "utsending av laks frå bedrift",
                                "ved innkjøp av laks i butikk",
                                "i laks når fisken kjem heim",
                                "i laks lagra heime",
                                "i sushi lagra heime",
                                "etter temperering av sushi"
                                                 ) , ordered = TRUE)) |> 
    tidyr:: pivot_longer(!name, names_to = "category", values_to = "value")


    
    
  p <- dat |>  
    dplyr::group_by(category) |> 
    echarts4r::e_charts(name) |>
    echarts4r::e_line(value)

  return(p)

}
