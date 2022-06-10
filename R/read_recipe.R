#' Read a Recipe in .xml Format
#'
#' Read an `.xml` file that is read by the Minstrel liquid handling robot. Used
#' to tell the robot what recipe to conduct. Individual components of the actual
#' pipetting are calculated by the robot once recipe is given.
#'
#' @param x File
#'
#' @return a list with a tibble for $pipetting and a tibble for recipe components.
#' @export
#'
#' @examples
#' library(minstrel)
#'
#' fl <- system.file("extdata", "test_plate.xml", package = "minstrel")
#'
#' read_recipe(fl)

read_recipe <- function(x) {

  x <- xml2::read_xml(x)
  x <- xml2::as_list(x)

  get_values <- function(x) {
    purrr::map(x, attributes) |>
      purrr::map(tibble::as_tibble) |>
      purrr::reduce(dplyr::bind_rows)
  }

  sourceplate <- x$job$sourceplates$sourceplate |>
    purrr::map(get_values)

  props <- x$job$sourceplates$sourceplate$plate |>
    purrr::map(get_values)

  wells <- x$job$sourceplates$sourceplate$plate$wells |>
    purrr::map(get_values) |>
    purrr::reduce(dplyr::bind_rows)

  list(
    pipetting = dplyr::bind_cols(props, wells),
    stocks = sourceplate$stocks
  )
}


