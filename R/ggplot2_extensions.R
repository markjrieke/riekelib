#' Custom ggplot2 theme
#'
#' A custom ggplot2 theme based on my own personal preferences. This assumes the
#' user (me) is on a windows machine & has the required custom fonts already installed.
#'
#' @param base_size base font size, given in pts
#' @param title_family font family for the plot title
#' @param base_family font family for all text in the plot except the title
#' @param base_color color for all text in the plot
#'
#' @importFrom ggplot2 theme_minimal
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 element_rect
#' @importFrom ggtext element_markdown
#' @importFrom colorspace lighten
#'
#' @export
#' @examples
#' library(ggplot2)
#'
#' # plotting with NULL families --- must install default fonts locally
#' ggplot(mtcars, aes(mpg, wt)) +
#'   geom_point() +
#'   theme_rieke(title_family = NULL,
#'               base_family = NULL) +
#'   labs(title = "The title is left-aligned and allows for **bold characters**",
#'        subtitle = "You can also use any markdown formatting, including *italics*",
#'        caption = "Note the use of break<br>for line breaks")
theme_rieke <- function(base_size = 14,
                        title_family = "Tiempos Text",
                        base_family = "IBM Plex Sans",
                        base_color = "gray20") {

  ggplot2::theme_minimal(base_family = base_family,
                         base_size = base_size) +
    ggplot2::theme(plot.title.position = "plot",
                   plot.background = ggplot2::element_rect(fill = "white", color = "white"),
                   plot.title = ggtext::element_markdown(family = title_family, color = base_color),
                   plot.subtitle = ggtext::element_markdown(color = base_color),
                   plot.caption = ggtext::element_markdown(color = colorspace::lighten(base_color, 0.4)),
                   axis.text = ggtext::element_markdown(color = base_color),
                   axis.text.x = ggtext::element_markdown(color = base_color),
                   axis.text.y = ggtext::element_markdown(color = base_color),
                   axis.title = ggtext::element_markdown(color = base_color),
                   legend.text = ggtext::element_markdown(color = base_color),
                   legend.title = ggtext::element_markdown(color = base_color))

}

#' Scale shortcuts
#'
#' @description
#' Format the labels on the x-axis, y-axis, or both
#'
#' @param ... parameters passed to `scale_*_continuous`
#' @param accuracy passed to `scales::label_percent()`
#' @param log10 should the axes be on the log10 scale? Defaults to `FALSE`
#'
#' @importFrom ggplot2 scale_x_continuous
#' @importFrom ggplot2 scale_y_continuous
#' @importFrom ggplot2 scale_x_log10
#' @importFrom ggplot2 scale_y_log10
#' @importFrom scales label_percent
#' @importFrom scales label_comma
#'
#' @export
#'
#' @examples
#' library(ggplot2)
#'
#' # quickly add comma format to x/y scales
#' tibble::tibble(x = rlnorm(1e3, 10),
#'                y = rlnorm(1e3, 10)) |>
#'   ggplot(aes(x, y)) +
#'   geom_point() +
#'   scale_xy_comma(log10 = TRUE)
#'
#' # quickly add percent format to x/y scales
#' tibble::tibble(x = rbeta(1e3, 2, 2),
#'                y = rbeta(1e3, 2, 2)) |>
#'   ggplot(aes(x, y)) +
#'   geom_point() +
#'   scale_xy_percent()
scale_xy_percent <- function(..., accuracy = 1) {

  list(scale_x_percent(..., accuracy = accuracy),
       scale_y_percent(..., accuracy = accuracy))

}

#' @rdname scale_xy_percent
#' @export
scale_x_percent <- function(..., accuracy = 1) {

  ggplot2::scale_x_continuous(..., labels = scales::label_percent(accuracy = accuracy))

}

#' @rdname scale_xy_percent
#' @export
scale_y_percent <- function(..., accuracy = 1) {

  ggplot2::scale_y_continuous(..., labels = scales::label_percent(accuracy = accuracy))

}

#' @rdname scale_xy_percent
#' @export
scale_xy_comma <- function(..., log10 = FALSE) {

  list(scale_x_comma(..., log10 = log10),
       scale_y_comma(..., log10 = log10))

}

#' @rdname scale_xy_percent
#' @export
scale_x_comma <- function(..., log10 = FALSE) {

  if (log10 == TRUE) {

    out <- ggplot2::scale_x_log10(..., labels = scales::label_comma())

  } else {

    out <- ggplot2::scale_x_continuous(..., labels = scales::label_comma())

  }

  return(out)

}

#' @rdname scale_xy_percent
#' @export
scale_y_comma <- function(..., log10 = FALSE) {

  if (log10 == TRUE) {

    out <- ggplot2::scale_y_log10(..., labels = scales::label_comma())

  } else {

    out <- ggplot2::scale_y_continuous(..., labels = scales::label_comma())

  }

  return(out)

}

#' Call ggsave with default parameters
#'
#' @description Calls ggsave with some defaults that I happen to like. As of
#' [ggplot2 3.3.4](https://ggplot2.tidyverse.org/news/index.html#ggplot2-334),
#' [ggplot2::ggsave()] uses [ragg](https://ragg.r-lib.org/) as the backend
#' device for saving.
#'
#' @param filename File name to create on disk.
#' @param plot Plot to save, defaults to last plot displayed.
#' @param width,height,units Plot size in `units` ("in", "cm", "mm", or "px").
#' @param dpi Plot resolution. Also accepts a string input: "retina" (320), "print" (300), or "screen" (72).
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
                        ...) {

  ggplot2::ggsave(
    filename = filename,
    plot = plot,
    width = width,
    height = height,
    units = units,
    dpi = dpi,
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
