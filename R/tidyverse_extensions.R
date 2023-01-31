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
#' # create a character vector
#' myvec <- c("dog", "cat", "bird")
#'
#' # arrange vector by character
#' arrange_vector(myvec)
#'
#' # can also arrange in reverse order
#' arrange_vector(myvec, desc = TRUE)
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
#' @param .keep_n logical; keep or remove the `n` col created by `dplyr::count()`. Defaults to `FALSE`.
#' @param ... parameters to pass to `dplyr::count()`
#'
#' @export
#'
#' @importFrom dplyr count
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @examples
#' # return a tibble of the percentage of gear type in the `mtcars` dataset
#' percent(mtcars, gear)
percent <- function(.data, ..., .keep_n = FALSE) {

  df <- dplyr::count(.data, ...)
  df <- dplyr::mutate(df, pct = n/sum(n))

  if (.keep_n != TRUE) {
    df <- dplyr::select(df, -n)
  }

  return(df)

}

#' Custom ggplot2 theme
#'
#' A custom ggplot2 theme based on my own personal preferences. This assumes the
#' user (me) is on a windows machine & has Roboto Slab already installed.
#'
#' @param base_size base font size, given in pts
#' @param base_family base font family
#'
#' @importFrom ggplot2 theme_minimal
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 element_rect
#' @importFrom ggtext element_markdown
#'
#' @export
#' @examples
#' \dontrun{
#' library(ggplot2)
#' ggplot(mtcars, aes(mpg, wt)) +
#'   geom_point() +
#'   theme_rieke() +
#'   labs(title = "The title is left-aligned and allows for **bold characters**",
#'        subtitle = "You can also use any markdown formatting, including *italics*",
#'        caption = "Note the use of break<br>for line breaks")
#' }
theme_rieke <- function(base_size = 14, base_family = "Roboto Slab") {

  ggplot2::theme_minimal(base_family = base_family,
                         base_size = base_size) +
    ggplot2::theme(plot.title.position = "plot",
                   plot.background = ggplot2::element_rect(fill = "white", color = "white"),
                   plot.title = ggtext::element_markdown(),
                   plot.subtitle = ggtext::element_markdown(),
                   plot.caption = ggtext::element_markdown(color = "gray40"))

}



#' Call ggsave with default parameters
#'
#' @param filename File name to create on disk.
#' @param plot Plot to save, defaults to last plot displayed.
#' @param width,height,units Plot size in `units` ("in", "cm", "mm", or "px").
#' @param dpi Plot resolution. Also accepts a string input: "retina" (320), "print" (300), or "screen" (72).
#' @param device Device format (e.g., one of "eps", "ps", "tex", "pdf", "jpeg", "tiff", "png", "bmp", "svg", or "wmf")
#' @param ... Other arguments passed to `ggplot2::ggsave()`.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#' ggplot(mtcars, aes(mpg, wt)) +
#'   geom_point()
#'
#' ggquicksave("mtcars.png")
#' }
ggquicksave <- function(filename,
                        plot = ggplot2::last_plot(),
                        width = 9,
                        height = 6,
                        units = "in",
                        dpi = 500,
                        device = grDevices::png,
                        ...) {

  ggplot2::ggsave(
    filename = filename,
    plot = plot,
    width = width,
    height = height,
    units = units,
    dpi = dpi,
    device = device,
    ...
  )

}

#' Color text using HTML color styling
#'
#' @description Useful in conjuction with ggtext and glue
#'
#' @param text text to be colored
#' @param hex_color_code color to be applied to the text
#'
#' @export
#'
#' @examples
#' color_text("Hello there!", "#BD43BF")
color_text <- function(text, hex_color_code) {

  paste0(
    "<span style='color:",
    hex_color_code,
    "'>",
    text,
    "</span>"
  )

}


