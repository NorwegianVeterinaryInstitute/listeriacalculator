#' get_reactive_translator
#'
#' @description get reactive translator
#'
#' @noRd
get_reactive_translator <- function(translator, selected_language) {

  selected <- selected_language
  if (length(selected) > 0 &&
      selected %in% translator$get_languages()) {
    translator$set_translation_language(selected)
  }
  translator

}
