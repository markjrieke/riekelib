#' Sleep with a progress bar
#'
#' A small wrapper around `Sys.sleep()` that adds a progress bar.
#'
#' @param time the amount of time (in seconds) to sleep
#' @param ... additional parameters passed to `cli::cli_progress_bar()`
#' @param message a message to display alongside the progress bar
#' @param .progress whether or not to sleep with a progress bar. Defaults to `TRUE`.
#'
#' @importFrom cli cli_progress_bar
#' @importFrom cli cli_progress_update
#'
#' @export
#' @examples
#' # sleep with a progress bar
#' sys_sleep(3)
sys_sleep <- function(time,
                      ...,
                      message = "Sleeping...",
                      .progress = TRUE) {

  if (.progress) {

    cli::cli_progress_bar(message, total = time, ...)
    for (t in 1:time) {
      Sys.sleep(1)
      cli::cli_progress_update()
    }

  } else {

    Sys.sleep(time)

  }

}
