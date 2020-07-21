runClick <- function(app, id, cancel = FALSE){
  app$executeScript(sprintf("$('#%s').click();", id))
  app$waitForValue(
    name = id, 
    iotype = "input",
    ignore = list(cancel, NULL)
  )
}

getValue <- function(app, name, input = TRUE){
  state <- app$getAllValues(
    input = input,
    output = !input,
    export = FALSE
  )
  state[[1]][[name]]
}

test_that(
  desc = "integration 2",
  code = {
    app <- shinytest::ShinyDriver$new(
      path = system.file("demoapp", package = "FutureManager"),
      loadTimeout = 10000 # 10s to spin up processes in Rstudio
    )
    app$waitForValue( # wait for the UI render
      name = "yVar", 
      iotype = "input"
    )
    runClick(app, "plot_run")
    Sys.sleep(1)
    app$setInputs(xVar = "Petal.Length")
    Sys.sleep(2)
    
    runClick(app, "plot_run", TRUE) # cancel the process
    
    app$waitFor( # wait for the cancel
      expr = "$('#plot').text() === 'run the process first';",
      checkInterval = 500,
      timeout = 2000 # 2s
    )
    
    plotBtn <- app$findElement("#plot_run")
    expect_true("btn-danger" %in% plotBtn$getClass())
    
    # expected error in the process (fmError)
    app$setInputs(xVar = "Species")
    runClick(app, "plot_run")
    
    app$waitFor(
      expr = "$('#plot').hasClass('shiny-output-error-fm-failed');",
      checkInterval = 500,
      timeout = 2000 # 2s
    )
    
    plot <- getValue(app, "plot", FALSE)
    expect_equal(
      object = plot$message,
      expected = "Species column not allowed as xVar"
    )
    expect_true("fm-failed" %in% plot$type)
    
    plotBtn <- app$findElement("#plot_run")
    expect_true("btn-danger" %in% plotBtn$getClass())
    
    # unexpected error in the process (stop)
    app$setInputs(xVar = "Sepal.Width")
    app$setInputs(yVar = "Species")
    
    runClick(app, "plot_run")
    
    app$waitFor(
      expr = "! $('#plot').hasClass('shiny-output-error-validation');",
      checkInterval = 500,
      timeout = 2000 # 2s
    )
    
    plot <- getValue(app, "plot", FALSE)
    expect_equal(
      object = plot$message,
      expected = "Species column not allowed as yVar"
    )
    expect_null(plot$type)
    
    plotBtn <- app$findElement("#plot_run")
    expect_true("btn-danger" %in% plotBtn$getClass())
  }
)