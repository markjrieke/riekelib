#' @importFrom grDevices windowsFonts
#' @importFrom grDevices windowsFont
.onAttach <- function(libname, pkgname) {

  grDevices::windowsFonts(`Roboto Slab` = grDevices::windowsFont("Roboto Slab"))

}
