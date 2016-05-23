## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ----echo=FALSE----------------------------------------------------------

## Final exam CSP 708 - Spring, 2016
##
library(dplyr, warn.conflicts = FALSE, verbose=FALSE, quietly = TRUE)
library(tidyr, warn.conflicts = FALSE, verbose=FALSE, quietly = TRUE)
library(table1, warn.conflicts = FALSE, verbose=FALSE, quietly = TRUE)
#source("~/Documents/GitHub/table1/R/build_table1.R")
source("~/Documents/GitHub/MyGeneralRFunctions/myPrintFunctions.R")

#load("~/Documents/GitHub/table1/data/table1Dat.rda")

## ----echo=TRUE-----------------------------------------------------------

table1 <- buildTable1(theData = table1Dat, theVariables = c("age", "sex", "Race", "Ethnicity", "Rank"), groupBy="site", percentFirst = TRUE, combineTables = TRUE, meanDigits = 1, sdDigits = 1, freqDigits = 2, statDigits = 2, pDigits = 5)

## ----echo=FALSE----------------------------------------------------------
knitr::kable(table1)

