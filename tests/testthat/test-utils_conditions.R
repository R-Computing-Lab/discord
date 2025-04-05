test_that("discord_cond throws correct error condition", {
  expect_error(derr("test error"), class = "discord-error")
})

test_that("discord_cond issues correct warning condition", {
  expect_warning(dwarn("test warning"), class = "discord-warning")
})

test_that("discord_cond emits correct message condition", {
  expect_message(dmess("test message"), class = "discord-message")
})

test_that("discord_cond constructs condition with expected classes", {
  catch_class <- function(expr) {
    tryCatch(expr,
      error = function(e) class(e),
      warning = function(w) class(w),
      message = function(m) class(m)
    )
  }

  expect_equal(catch_class(derr("some error"))[1], "discord-error")
  expect_equal(catch_class(dwarn("some warning"))[1], "discord-warning")
  expect_equal(catch_class(dmess("some message"))[1], "discord-message")
})

test_that("discord_cond validates type argument", {
  expect_error(discord_cond("notatype", "bad"), "Invalid type")
})

test_that("discord_cond allows custom class names", {
  custom_class <- "custom-error-class"
  err <- tryCatch(
    discord_cond("error", "boom", class = custom_class),
    error = function(e) e
  )
  expect_true(custom_class %in% class(err))
  expect_true("error" %in% class(err))
})

test_that("additional arguments are included in the condition structure", {
  cond <- tryCatch(
    discord_cond("error", "error message", extra = "something"),
    error = function(e) e
  )
  expect_equal(cond$extra, "something")
})
