#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  i18n <- golem::get_golem_options(which = "translator")
  i18n$set_translation_language("en")

  metathis::meta() |>
    metathis::meta_social(
      title = "Listeria Calculator",
      description = "Listeria Calculator for Raw Fish",
      url = "https://connect.posit.vetinst.no/listeriacalculator",
      image = "https://www.vetinst.no/_/image/5c4e853a-130b-4e7f-92a3-8ca38bec0b56:2dcf9428a329fc0044b412c55b8c9e471f742d65/block-1200-630/Logo-vetinst-open-graph-no-svg-1200x630.png.jpg",
      image_alt = "An image for social meda cards"
    )

  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    shiny.i18n::usei18n(i18n),

    # Your application UI logic
    bslib::page_navbar(
      theme = bslib::bs_theme(brand = TRUE),
      navbar_options = bslib::navbar_options(
        underline = FALSE,
        inverse = FALSE
      ),
      bslib::nav_item(
        shiny::tags$a(
          style = "padding: 0;",
          shiny::img(src = "www/vetinst-logo-eng.png", height = "40px"),
          href = "https://www.vetinst.no/",
          target = "_blank"
        )
      ),
      bslib::nav_spacer(),
      bslib::nav_item(shiny::uiOutput("app_title")),
      bslib::nav_spacer(),
      bslib::nav_panel(title = shiny::uiOutput("home_tab"), mod_home_ui("home_1")),
      bslib::nav_panel(
        title = shiny::uiOutput("calc_tab"),
        mod_listeria_calculator_ui("listeria_calculator_1")
      ),
      bslib::nav_panel(title = shiny::uiOutput("about_tab"), mod_about_ui("about_1")),
      bslib::nav_item(
        shiny::selectInput(
          inputId = "selected_language",
          label = NULL,
          choices = c("en", "nb"),
          width = "75px"
        )
      ),
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(ext = "png"),
    tags$html(lang = "en"),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "listeriacalculator"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
