

test_that("returns null when no token exists", {

  suppressWarnings(
    rm(list = c("wave_token", "wave_token_time"), envir = .my_package_env)
  )

  expect_null(wave_check_token())

})


test_that("returns token is returned when stored and active", {

  assign("wave_token", 'ABCD12234', envir = .my_package_env)
  assign("wave_token_time", Sys.time() - 1 * 60, envir = .my_package_env)

  expect_equal(wave_check_token(), 'ABCD12234')

  suppressWarnings(
    rm(list = c("wave_token", "wave_token_time"), envir = .my_package_env)
  )

})

test_that("returns null when no token is outdate", {

  assign("wave_token", 'ABCD12234', envir = .my_package_env)
  assign("wave_token_time", Sys.time() - 30 *60, envir = .my_package_env)

  expect_null(suppressMessages(wave_check_token()))

  suppressWarnings(
    rm(list = c("wave_token", "wave_token_time"), envir = .my_package_env)
  )

})



