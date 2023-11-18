.onAttach <- function(...) {

  if (!interactive()) return()

  msg <-
    cli::cli({
      cli::cli_alert_info("The following fonts must be installed for themes to function properly:")
      cli::cli_li(c("Tiempos Text", "IBM Plex Sans", "Roboto Slab"))
    })

  packageStartupMessage(msg)

}

