#' Run button
#' 
#' Creates toggle button, that may be used to control a long background process (start, cancel etc)
#' 
#' See \code{\link{fmError}} for some example.
#' 
#' @param inputId character string, the button ID
#' @param fm FutureManager object
#' @param defaultValue logical, the initial button value
#' @param blocked logical, should the button be blocked?
#' 
#' @return button HTML
#' @export
fmRunButton <- function(inputId, fm, defaultValue = FALSE, blocked = FALSE){
  buttonState <- fm$initButtonState(
    inputId = inputId,
    defaultValue = defaultValue
  )
  status <- buttonState$status
  
  if (blocked){
    status <- "blocked"
    disabled <- TRUE
  } else {
    disabled <- status == "success"
  }
  
  style <- getStyleForStatus(status)
  
  shiny::tagList(
    bsButton(
      inputId = inputId,
      label = getLabelForStatus(status),
      value = buttonState$value,
      style = style,
      disabled = disabled,
      type = "toggle",
      class = "fm-run",
      `data-cancelText` = getOption("FutureManager.labels.cancel")
    ),
    htmltools::htmlDependency(
      name = "FutureManager",
      package = "FutureManager",
      version = utils::packageVersion("FutureManager"),
      src = "FutureManager",
      stylesheet = "FutureManager.css",
      script = "FutureManager.js"
    )
  )
}

#' Update run button
#' 
#' Updates the run button (on frontend) and also its state (on backend)
#' 
#' @param inputId character string, the button ID
#' @param status character string, the process status
#' @param fm FutureManager object
#' @param session shiny session object
#' 
#' @return No return value, called for side effects.
#' @export
fmUpdateRunButton <- function(inputId, status, fm, session = shiny::getDefaultReactiveDomain()) {
  isSuccess <- status == "success"
  
  currentState <- fm$getButtonState(inputId)
  if (!isSuccess && currentState$mustRerun){
    status <- "rerun"
    isSuccess <- FALSE
  }
  
  buttonState <- fm$updateButtonState(
    inputId = inputId,
    value = FALSE,
    status = status
  )
  
  updateButton(
    session = session,
    inputId = inputId,
    label = getLabelForStatus(status),
    value = FALSE,
    style = getStyleForStatus(status),
    disabled = isSuccess
  )
}

getStyleForStatus <- function(status){
  switch(
    EXPR = status,
    success = "success",
    rerun = "danger",
    blocked = "warning",
    "default"
  )
}

getLabelForStatus <- function(status){
  switch(
    EXPR = status,
    success = getOption("FutureManager.labels.ready"),
    rerun = getOption("FutureManager.labels.rerun"),
    blocked = getOption("FutureManager.labels.missing"),
    getOption("FutureManager.labels.run")
  )
}

# from shinyBS

bsButton <- function (inputId, label, icon = NULL, ..., style = "default", 
                      size = "default", type = "action", block = FALSE, disabled = FALSE, 
                      value = FALSE){
  btn <- shiny::actionButton(
    inputId = inputId, 
    label = label, 
    icon = icon, 
    ...
  )
  
  if (type == "toggle") {
    btn <- removeClass(btn, "action-button")
    btn <- addClass(btn, "sbs-toggle-button")
    if (value == TRUE) {
      btn <- addClass(btn, "active")
    }
  }
  
  if (style != "default") {
    btn <- removeClass(btn, "btn-default")
    btn <- addClass(btn, paste0("btn-", style))
  }
  
  size <- getButtonSizeClass(size)
  if (size != "default") {
    btn <- addClass(btn, size)
  }
  
  if (block == TRUE) {
    btn <- addClass(btn, "btn-block")
  }
  
  if (disabled) {
    btn <- addAttribs(btn, disabled = "disabled")
  }
  
  btn
}

updateButton <- function (session, inputId, label = NULL, icon = NULL, value = NULL, 
                          style = NULL, size = NULL, block = NULL, disabled = NULL) {
  if (!is.null(icon)){
    icon <- as.character(icon)
  }
    
  if (!is.null(size)) {
    size <- getButtonSizeClass(size)
  }
  
  data <- dropNulls(list(
    id = inputId, 
    label = label, 
    icon = icon, 
    value = value, 
    style = style, 
    size = size, 
    block = block, 
    disabled = disabled
  ))
  
  session$sendCustomMessage("fmButtonUpdate", data)
}

getButtonSizeClass <- function(size){
  switch(
    EXPR = size, 
    `extra-small` = "btn-xs", 
    small = "btn-sm", 
    large = "btn-lg", 
    default = "default"
  )
}
