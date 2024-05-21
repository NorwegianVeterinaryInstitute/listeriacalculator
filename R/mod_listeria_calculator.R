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
    echarts4r::echarts4rOutput(ns("listeria_plot")))
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
        fluidRow(column(12, align = "center",
                        shiny::h3("My Numbers"))),
        fluidRow(column(
          12, align = "center",
          shiny::h6("Storage in a salmon production company")
        )),
        fluidRow(
          column(
            6,
            shiny::numericInput(
              ns("prod_temp"),
              label = i18n()$t("Temperature (0 - 10°C)"),
              value = 4,
              min = 0,
              max = 10
            )
          ),
          column(6,
                 shiny::numericInput(
                   ns("prod_days"),
                   label = i18n()$t("Days"),
                   value = 3
                 ))
        ),
        fluidRow(column(12, align = "center",
                        shiny::h6("Time in store"))),
        fluidRow(
          column(
            6,
            shiny::numericInput(
              ns("store_temp"),
              label = i18n()$t("Temperature (0 - 10°C)"),
              value = 4,
              min = 0,
              max = 10
            )
          ),
          column(6,
                 shiny::numericInput(
                   ns("store_days"),
                   label = i18n()$t("Days"),
                   value = 3
                 ))
        ),
        fluidRow(column(12, align = "center",
                        shiny::h6("Transport home"))),
        fluidRow(
          column(
            6,
            shiny::numericInput(
              ns("home_temp"),
              label = i18n()$t("Temperature (0 - 25°C)"),
              value = 10,
              min = 0,
              max = 25
            )
          ),
          column(6,
                 shiny::numericInput(
                   ns("home_hours"),
                   label = i18n()$t("Hours"),
                   value = 3
                 ))
        ),
        fluidRow(column(
          12, align = "center",
          shiny::h6("Storing salmon in a refrigerator")
        )),
        fluidRow(
          column(
            6,
            shiny::numericInput(
              ns("salmon_temp"),
              label = i18n()$t("Temperature (0 - 10°C)"),
              value = 10,
              min = 0,
              max = 10
            )
          ),
          column(6,
                 shiny::numericInput(
                   ns("salmon_hours"),
                   label = i18n()$t("Hours"),
                   value = 3
                 ))
        ),
        fluidRow(column(
          12, align = "center",
          shiny::h6("Storing sushi in a refrigerator")
        )),
        fluidRow(
          column(
            6,
            shiny::numericInput(
              ns("sushi_temp"),
              label = i18n()$t("Temperature (0 - 10°C)"),
              value = 4,
              min = 0,
              max = 10
            )
          ),
          column(6,
                 shiny::numericInput(
                   ns("sushi_hours"),
                   label = i18n()$t("Hours"),
                   value = 12
                 ))
        ),
        fluidRow(column(
          12, align = "center",
          shiny::h6("Tempering period")
        )),
        fluidRow(
          column(
            6,
            shiny::numericInput(
              ns("period_temp"),
              label = i18n()$t("Temperature (15 - 25°C)"),
              value = 22,
              min = 15,
              max = 25
            )
          ),
          column(6,
                 shiny::numericInput(
                   ns("period_hours"),
                   label = i18n()$t("Hours"),
                   value = 6
                 ))
        ),
        shiny::div(class="text-center",
        shiny::radioButtons(
          ns("initial_conc"),
          label = "Initial concentration",
          choices = c(0.04, 1, 10),
          selected = 1,
          inline = TRUE,
          width = "100%"
        ))



      )

    })

    card_title_ui <- reactive({
      if (i18n()$get_translation_language() == 'en') {
        ui <-
          shiny::renderText(
            "Listeria growth prediction"
          )
      } else {
        ui <-
          shiny::renderText(
            "Listeria vekst prediksjon"
          )
      }})


    output$card_title_plot <- shiny::renderUI(
      {
        card_title_ui()
      }
    )

    output$listeria_plot <- echarts4r::renderEcharts4r({
      make_plot(
        prod_temp = input$prod_temp,
        prod_days = input$prod_days,
        store_temp = input$store_temp,
        store_days = input$store_days,
        home_temp = input$home_temp,
        home_hours = input$home_hours,
        salmon_temp = input$salmon_temp,
        salmon_hours = input$salmon_hours,
        sushi_temp = input$sushi_temp,
        sushi_hours = input$sushi_hours,
        period_temp = input$period_temp,
        period_hours = input$period_hours,
        initial_conc = input$initial_conc,
        lang = i18n()$get_translation_language()
      )
    }

    ) #|> shiny::bindEvent(input$predict)

  })
}

## To be copied in the UI
# mod_listeria_calculator_ui("listeria_calculator_1")

## To be copied in the server
# mod_listeria_calculator_server("listeria_calculator_1")
