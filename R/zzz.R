.onAttach <- function(libname, pkgname) {
  vers <-  "0.0.0.9000"
  packageStartupMessage("|=====================================================|")
  packageStartupMessage("| Rotinas para analises agronomicas (agrorotinas)     |")
  packageStartupMessage("| Author: Tiago Olivoto                               |")
  packageStartupMessage("| Type 'citation('metan')' to know how to cite metan  |")
  packageStartupMessage("| Type 'vignette('metan_start')' for a short tutorial |")
  packageStartupMessage("|=====================================================|")
}

if (getRversion() >= "2.15.1") {
  utils::globalVariables(
    c("Contorno", "display", "CODE"))
  }
