#' Sort a vector by alphabetic or numeric order
#'
#' @param x the vector to be sorted
#' @param desc logical; if set to true, will return a vector sorted in descending order
#'
#' @export
#'
#' @importFrom tibble as_tibble
#' @importFrom dplyr arrange
#' @importFrom dplyr desc
#' @importFrom dplyr pull
#'
#' @examples
#' \dontrun{
#' # create a character vector
#' myvec <- c("dog", "cat", "bird")
#'
#' # arrange vector by character
#' arrange_vector(myvec)
#'
#' # can also arrange in reverse order
#' arrange_vector(myvec, desc = TRUE)
#'
#' }
#'
arrange_vector <- function(x, desc = FALSE) {

  vector <- tibble::as_tibble(x)

  if (desc == FALSE) {
    vector <- dplyr::arrange(vector, value)
  } else {
    vector <- dplyr::arrange(vector, dplyr::desc(value))
  }

  vector <- dplyr::pull(vector, value)

  return(vector)

}

#' Return the percentage each value or combination of values appear in a tibble or dataframe
#'
#' @param .data a tibble or dataframe
#' @param ... parameters to pass to `dplyr::count()`
#'
#' @export
#'
#' @importFrom dplyr count
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @examples
#' \dontrun{
#' # return a tibble of the percentage of gear type in the `mtcars` dataset
#' percent(mtcars, gear)
#' }
percent <- function(.data, ...) {

  df <- dplyr::count(.data, ...)
  df <- dplyr::mutate(df, pct = n/sum(n))
  df <- dplyr::select(df, -n)

  return(df)

}
