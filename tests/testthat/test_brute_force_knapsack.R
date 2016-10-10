
library(Knapsackpackage)
library(testthat)
set.seed(42)
n <- 2000
knapsack_objects <-
  data.frame(
    w=sample(1:4000, size = n, replace = TRUE),
    v=runif(n = n, 0, 10000)
  )
context("Knapsackpackage")
test_that('Brute force knapsack',{
  expect_equal(brute_force_knapsack(knapsack_objects[1:8,],3500)$value, 16770) 
  expect_equal(brute_force_knapsack(knapsack_objects[1:8,],3500)$elements, c(5,8))
  expect_equal(brute_force_knapsack(knapsack_objects[1:12,],3500)$value, 16770)
  expect_equal(brute_force_knapsack(knapsack_objects[1:12,],3500)$elements, c(5,8))
  expect_equal(brute_force_knapsack(knapsack_objects[1:8,],2000)$value, 15428)
  expect_equal(brute_force_knapsack(knapsack_objects[1:8,],2000)$elements, c(3,8))
  expect_equal(brute_force_knapsack(knapsack_objects[1:12,],2000)$value, 15428)
  expect_equal(brute_force_knapsack(knapsack_objects[1:12,],2000)$elements, c(3,8))
})

test_that('Brute force search',{
  expect_error(brute_force_knapsack(c(0:00),10))
  expect_error(brute_force_knapsack(knapsack_objects,z))
  expect_error(brute_force_knapsack(c(1,2),5))
})
