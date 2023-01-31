.onAttach <- function(libname, pkgname) {

  if (Sys.info()[1] == "Windows") {

    cli::cli_alert_info("Roboto Slab imported (if installed on machine).")
    grDevices::windowsFonts(`Roboto Slab` = grDevices::windowsFont("Roboto Slab"))

  } else {

    cli::cli_alert_warning("Non-windows device - custom fonts not loaded.")
    cli::cli_alert_info("Recommend installing then using the extrafont package to import.")

  }

}

