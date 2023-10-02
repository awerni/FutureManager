# nocov start
.onLoad <- function(libname, pkgname){
  options(
    FutureManager.labels.run = "Run",
    FutureManager.labels.cancel = "Cancel",
    FutureManager.labels.ready = "Ready",
    FutureManager.labels.rerun = "Re-run required",
    FutureManager.labels.missing = "Missing input",
    future.globals.onReference = "ignore"
  )
}
# nocov end
