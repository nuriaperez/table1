library(table1)
context("Full Table")

test_that("buildTable1List runs full table development without any errors", {
  table1TableList <- buildTable1List(theData = table1Dat, theVariables = c("age", "sex", "Race1", "Ethnicity", "Rank"), groupBy="site", percentFirst = TRUE,
                              meanDigits = 1, sdDigits = 1, freqDigits = 2, statDigits = 2, pDigits = 5)
})

test_that("buildTableList stops when given an empty table", {

  expect_error(buildTable1List(theData = NULL, theVariables = c("age", "sex", "Race1", "Ethnicity", "Rank"), groupBy="site", percentFirst = TRUE,
                              meanDigits = 1, sdDigits = 1, freqDigits = 2, statDigits = 2, pDigits = 5), "Called getTable1Row passing something other than a data.frame as the argument to \"theData\"")
})

test_that("buildTableList stops when given an variable that does not exist in the table", {

  expect_error(buildTable1List(theData = table1Dat, theVariables = c("Bob"), groupBy="site", percentFirst = TRUE,
                              meanDigits = 1, sdDigits = 1, freqDigits = 2, statDigits = 2, pDigits = 5),
               "Called getTable1Row with theVariable set to Bob but it is not a column name in theData")
})

test_that("buildTableList stops when given a grouping variable that is not in theData", {

  expect_error(buildTable1List(theData = table1Dat, theVariables = c("sex"), groupBy="Bob", percentFirst = TRUE,
                              meanDigits = 1, sdDigits = 1, freqDigits = 2, statDigits = 2, pDigits = 5),
               "Called getTable1Row with groupBy set to Bob but it is not a column name in theData")
})


test_that("buildTableList stops when given a grouping variable that is not a factor", {

  expect_error(buildTable1List(theData = table1Dat, theVariables = c("sex"), groupBy="age", percentFirst = TRUE,
                              meanDigits = 1, sdDigits = 1, freqDigits = 2, statDigits = 2, pDigits = 5),
               "Called getTable1Row with groupBy = age but age is not a factor")
})
