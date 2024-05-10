#' listeria_calculator UI Function
#' this module is used for calculating listeria growth
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_listeria_calculator_ui <- function(id){
  ns <- NS(id)

  tagList(
    div(
      class = "d-flex flex-row p-2",
      style = "gap: 1rem;",
      div(
        class = "col-3 bg-light border rounded shadow-sm p-3",
    shiny::uiOutput(ns('sidebar_calc'))
  ),
  div(
    class = "col-9 border rounded shadow-sm p-2",
    h5(shiny::uiOutput(ns("card_title_plot"))),
    shiny::plotOutput(ns("plot_farm")))
  ))
}

#' listeria_calculator Server Functions
#'
#' @noRd
mod_listeria_calculator_server <- function(id, selected_language){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    translator <- golem::get_golem_options(which = "translator")
    i18n <- reactive({
      get_reactive_translator(translator, selected_language())})


    output$sidebar_calc <- shiny::renderUI({

      shiny::tagList(
      fluidRow(
          column(12, align="center",
          shiny::h3("My Numbers")
          )),
      fluidRow(
          column(12, align="center",
           shiny::h6("Storage in a salmon production company")
          )),
      fluidRow(
          column(6,
      shiny::numericInput(
        ns("prod_temp"),
        label = i18n()$t("Temperature (0 - 10°C)"),
        value = 4,
        min = 0,
        max = 10
      )),
      column(6,
      shiny::numericInput(
        ns("prod_days"),
        label = i18n()$t("Days"),
        value = 3
      ))),

     )

    })

    card_title_ui <- reactive({
      if (i18n()$get_translation_language() == 'en') {
        ui <-
          shiny::renderText(
            "Prediction per farm"
          )
      } else {
        ui <-
          shiny::renderText(
            "Prediksjon per gård"
          )
      }})


    output$card_title_plot <- shiny::renderUI(
      {
        card_title_ui()
      }
    )

    output$plot_farm <- shiny::renderPlot(
      {
        tryCatch(
          expr = {
        make_plot_from_location(location = input$locality_number,
                                weight = input$fish_weight_farm,
                                abundance = input$fish_abundance_farm,
                                cleaner = input$cleaner_fish_farm,
                                lang = i18n()$get_translation_language())
          },
        error = function(e) {
          showModal(

            if (i18n()$get_translation_language() == 'en') {
              modalDialog(title = "Error",
                          "The locality was found, but we could not generate predictions.",
                          footer = modalButton("Dismiss"))
            }  else {
              modalDialog(title = "Feil",
                          "Lokaliteten ble funnet, men vi kunne ikke generere spådommer.",
                          footer = modalButton("Avskjedige"))

            }

          )
        }
        )
      }
    ) |> shiny::bindEvent(input$predict)

  })
}

## To be copied in the UI
# mod_listeria_calculator_ui("listeria_calculator_1")

## To be copied in the server
# mod_listeria_calculator_server("listeria_calculator_1")
