test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})

# Test get_ouuid

test_that("Looking up OU UID works", {
  skip_on_ci()
  expect_equal(get_ouuid(operatingunit = "Nigeria"), "PqlFzhuPcF1")
})

# Test get_ouorglevel

test_that("Looking up OU Org Levels", {
  skip_on_ci()
  expect_equal(get_ouorglabel(operatingunit = "Nigeria", org_level = 3), "country")
})

test_that("Looking up OU Org Label", {
  skip_on_ci()
  expect_equal(get_ouorglevel(operatingunit = "Cote d'Ivoire", org_type = "country"), 3)
})
