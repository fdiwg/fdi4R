# test_create_cwp_grid.R
# Author: Emmanuel Blondel <emmanuel.blondel@fao.org>
#
# Description: Unit tests for CWP Grid creation
#=======================
require(fdi4R, quietly = TRUE)
require(testthat)
context("create_cwp_grid")

test_that("CWP size 1 - 10' x 10'",{
  if(FALSE) sf1 <- create_cwp_grid(size = 1)
})

test_that("CWP size 2 - 20' x 20'",{
  if(FALSE) sf2 <- create_cwp_grid(size = 2)
})

test_that("CWP size 3 - 30' x 30'",{
  if(FALSE) sf3 <- create_cwp_grid(size = 3)
})

test_that("CWP size 4 - 30' x 1°",{
  if(FALSE) sf4 <- create_cwp_grid(size = 4)
})

test_that("CWP size 5 - 1° x 1°",{
  if(FALSE) sf5 <- create_cwp_grid(size = 5)
})

test_that("CWP size 6 - 5° x 5°",{
  if(FALSE) sf6 <- create_cwp_grid(size = 6)
})

test_that("CWP size 7 - 10° x 10°",{
  if(FALSE) sf7 <- create_cwp_grid(size = 7)
})

test_that("CWP size 8 - 20° x 20°",{
  if(FALSE) sf8 <- create_cwp_grid(size = 8)
})

test_that("CWP size 9 - 30° x 30°",{
  if(FALSE) sf9 <- create_cwp_grid(size = 9)
})