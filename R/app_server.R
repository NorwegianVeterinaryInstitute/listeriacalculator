#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic
  translator <- golem::get_golem_options(which = "translator")
  i18n <- reactive({
    get_reactive_translator(translator, input$selected_language)
  })

  output$app_title <- renderUI({
    shiny::h3(i18n()$t("Listeria Calculator for raw fish"))
  })

  output$home_tab <- renderUI({
    shiny::h6(i18n()$t("Home"))
  })

  mod_home_server("home_1", reactive({
    input$selected_language
  }))

  output$calc_tab <- renderUI({
    shiny::h6(i18n()$t("Calculator"))
  })

  mod_listeria_calculator_server("listeria_calculator_1", reactive({
    input$selected_language
  }))


  output$about_tab <- renderUI({
    shiny::h6(i18n()$t("About"))
  })

  mod_about_server("about_1", reactive({
    input$selected_language
  }))
}
