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
test_that('Greedy heuristics knapsack',{
  expect_equal(greedy_knapsack(knapsack_objects[1:800,],3500)$value, 192647)
  expect_equal(greedy_knapsack(knapsack_objects[1:1200,],2000)$value, 212337)
})

test_that('Greedy heuristic knapsack',{
  expect_error(greedy_knapsack(c(0:00),10))
  expect_error(greedy_knapsack(knapsack_objects,z))
  expect_error(greedy_knapsack(c(1,2),5))
})
