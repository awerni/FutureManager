runClick <- function(app, id, cancel = FALSE){
  app$run_js(script = sprintf("$('#%s').click();", id))
  app$wait_for_value(
    input = id,
    ignore = list(cancel, NULL),
    timeout = 20000
  )
}

getValue <- function(app, name, input = TRUE){
  if (input){
    app$get_value(input = name)
  } else {
    app$get_value(output = name)
  }
}

test_that(
  desc = "Run tasks work properly",
  code = {
    testthat::skip_on_cran()
    
    # run the app
    app <- shinytest2::AppDriver$new(
      app_dir = system.file("demoapp", package = "FutureManager"),
      load_timeout = 10000
    )
    app$wait_for_value(input = "yVar")
    
    # check the initial state
    plot <- getValue(app, "plot", FALSE)
    expect_equal(
      object = plot$message,
      expected = "run the process first"
    )
    expect_true("fm-wait" %in% plot$type)
    
    # start calculations
    t_start <- Sys.time()
    runClick(app, "plot_run")
    
    app$set_inputs(tabset = "table")
    app$wait_for_value(input = "nRows") # wait for the UI render
    runClick(app, "table_run") # turn on table calculations
    
    # verify running state
    app$set_inputs(tabset = "plot") # go back to plot
    t_init <- Sys.time() - t_start 
    
    # check responsiveness of the app during the calculations and also if the app keeps the state
    expect_true(getValue(app, "plot_run"))
    
    # wait for the results
    app$wait_for_js(
      script = "$('#plot').find('img').length;",
      interval = 500,
      timeout = 25000 # 25s
    )
    
    t_total <- Sys.time() - t_start
    
    # check if the processes were run in another thread
    # each process takes at least 10s, so t_init should be smaller than 9s
    expect_true(t_init < 9)
    # check if the processes were run in parallel, i.e. t_total should be smaller than 20s
    expect_true(t_total > 10 && t_total < 19)
    
    # verify button state
    isBtnSuccess <- app$get_js("$('#plot_run').hasClass('btn-success')")
    expect_true(isBtnSuccess)
    
    # verify button invalidation
    app$set_inputs(xVar = "Petal.Width")
    
    isBtnDanger <- app$get_js("$('#plot_run').hasClass('btn-danger')")
    expect_true(isBtnDanger)
    
    app$set_inputs(xVar = "Petal.Length")
    runClick(app, "plot_run")
    
    plot <- getValue(app, "plot", FALSE)
    expect_equal(
      object = plot$message,
      expected = "wait for the process"
    )
    expect_true("fm-wait" %in% plot$type)
  }
)


test_that(
  desc = "Cancel and error handling",
  code = {
    testthat::skip_on_cran()
    
    app <- shinytest2::AppDriver$new(
      app_dir = system.file("demoapp", package = "FutureManager"),
      load_timeout = 10000
    )
    app$wait_for_value(input = "yVar")
    
    runClick(app, "plot_run")
    Sys.sleep(1)
    app$set_inputs(xVar = "Petal.Length")
    Sys.sleep(2)
    
    runClick(app, "plot_run", TRUE) # cancel the process
    
    app$wait_for_js(
      script = "$('#plot').text() === 'run the process first';",
      interval = 500,
      timeout = 2000 # 2s
    )
    
    isBtnDanger <- app$get_js("$('#plot_run').hasClass('btn-danger')")
    expect_true(isBtnDanger)
    
    # expected error in the process (fmError)
    app$set_inputs(xVar = "Species")
    runClick(app, "plot_run")
    
    app$wait_for_js(
      script = "$('#plot').hasClass('shiny-output-error-fm-failed');",
      interval = 500,
      timeout = 2000 # 2s
    )
    
    plot <- getValue(app, "plot", FALSE)
    expect_equal(
      object = plot$message,
      expected = "Species column not allowed as xVar"
    )
    expect_true("fm-failed" %in% plot$type)
    
    isBtnDanger_again <- app$get_js("$('#plot_run').hasClass('btn-danger')")
    expect_true(isBtnDanger_again)
    
    # unexpected error in the process (stop)
    app$set_inputs(xVar = "Sepal.Width")
    app$set_inputs(yVar = "Species")
    
    runClick(app, "plot_run")
    
    app$wait_for_js(
      script = "! $('#plot').hasClass('shiny-output-error-validation');",
      interval = 500,
      timeout = 2000 # 2s
    )
    
    plot <- getValue(app, "plot", FALSE)
    expect_equal(
      object = plot$message,
      expected = "Species column not allowed as yVar"
    )
    expect_null(plot$type)
    
    isBtnDanger_oneMoreTime <- app$get_js("$('#plot_run').hasClass('btn-danger')")
    expect_true(isBtnDanger_oneMoreTime)
  }
)
