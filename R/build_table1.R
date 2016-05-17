#' Build rows for table-1.
#'
#' One row is produced for numeric variables, multiple rows for factors.
#'
#' @param theVariable The name of the variable that is being reported in the row (a character vector).
#' @param theData The data.frame containing the data for the variable.
#' @param groupBy A variable (factor) used to stratify the variable being reported (see details below).
#' @param percentFirst A boolean, if TRUE numeric variables are printed as x\%(n) otherwise as n(x\%).
#' @param conductGroupTests A boolean, if TRUE group comparisons are conducted (see details below).
#' @param meanDigits An integer indicating number of digits printed following the decimal place (this is not precision as in round, you get the number of digits you ask for, even if they are zero).
#' @param sdDigits An integer indicating number of digits printed following the decimal place for standard deviations (this is not precision as in round, you get the number of digits you ask for, even if they are zero).
#' @param freqDigits An integer indicating number of digits printed following the decimal place for percentages (this is not precision as in round, you get the number of digits you ask for, even if they are zero).
#' @param statDigits An integer indicating number of digits printed following the decimal place for test statistics (this is not precision as in round, you get the number of digits you ask for, even if they are zero).
#' @param pDigits An integer indicating number of digits printed following the decimal place for test P-Values (if numer of zeros in P is greater than pDigits the value will be "< 000...1").
#' @return A data.frame.
#' @examples
#' buildTable1Rows(theData = myDataFrame, theVariable = "age", groupBy="site", percentFirst = TRUE, conductGroupTests = TRUE, meanDigits = 1, sdDigits = 1, freqDigits = 2, statDigits = 2, pDigits = 5)

buildTable1Rows <- function(theVariable, theData, groupBy = NULL, percentFirst = TRUE,
    conductGroupTests = TRUE, meanDigits = getOption("digits"), sdDigits = getOption("digits"),
    freqDigits = getOption("digits"), statDigits = getOption("digits"),
    pDigits = getOption("digits"))
    {
    if (!("dplyr" %in% rownames(installed.packages())))
    {
        stop("Sorry, 'getTable1Row' requires dplyr and tidyr")
    }

    if (!("tidyr" %in% rownames(installed.packages())))
    {
        stop("Sorry, 'getTable1Row' requires dplyr and tidyr")
    }

    if (is.data.frame(theData) == FALSE)
    {
        stop(paste("Called getTable1Row passing something other than a data.frame as the argument to \"theData\""))
    }
    if (nrow(theData) == 0)
    {
        stop(paste("Called getTable1Row with an empty data frame "))
    }

    if (!(theVariable %in% colnames(theData)))
    {
        stop(paste("Called getTable1Row with theVariable set to ", theVariable,
            " but it is not a column name in theData", sep=''))
    }

    if (is.null(groupBy) == FALSE)
    {
        if (!(groupBy %in% colnames(theData)))
        {
            stop(paste("Called getTable1Row with groupBy set to ", groupBy,
                " but it is not a column name in theData", sep=''))
        }
        if (is.factor(theData[, groupBy]) == FALSE)
        {
            stop(paste("Called getTable1Row with groupBy = ", groupBy,
                " but ", groupBy, " is not a factor", sep=''))
        }
    }



    meanFormatStr <- paste("%0.", meanDigits, "f", sep = "")
    sdFormatStr <- paste("%0.", sdDigits, "f", sep = "")
    freqFormatStr <- paste("%0.", freqDigits, "f", sep = "")
    statFormatStr <- paste("%0.", statDigits, "f", sep = "")
    pFormatStr <- paste("%0.", pDigits, "f", sep = "")

    numericFormatStr <- paste(meanFormatStr, " (", sdFormatStr, ")", sep = "")

    # Process the 'main' demographic (i.e. the total value ignoring
    # groupBy)
    if (is.numeric(theData[, theVariable]))
    {
        table1Rows <- data.frame(Demographic = theVariable, Level = '', Value = sprintf(numericFormatStr,
            mean(theData[, theVariable], na.rm = TRUE), sd(theData[, theVariable],
                na.rm = TRUE)), stringsAsFactors = FALSE)
    } else
    {
        rows <- cbind(prop.table(table(theData[, theVariable])) * 100,
            table(theData[, theVariable]))
        table1Rows <- NULL
        if (percentFirst == TRUE)
        {
            firstIndex <- 1
            secondIndex <- 2
            firstString <- "%"
            secondString <- ""
            formatStr <- paste(freqFormatStr, "%s (%0.0f%s)", sep = "")  #%0.1f%s (%0.0f%s)
        } else
        {
            firstIndex <- 2
            secondIndex <- 1
            firstString <- ""
            secondString <- "%"
            formatStr <- paste("%0.0f%s (", freqFormatStr, "%s)", sep = "")  #'%0.0f%s (%0.1f%s)'
        }
        for (i in 1:nrow(rows))
        {
            table1Rows <- rbind(table1Rows, data.frame(demo = row.names(rows)[i],
                value = sprintf(formatStr, rows[i, firstIndex], firstString,
                  rows[i, secondIndex], secondString)))
        }
        colnames(table1Rows) <- c(theVariable, "Value")
    }

    # Do group stratification if asked for
    if (is.null(groupBy) == FALSE)
    {
        # Process the 'main' demographic (i.e. the total value ignoring
        # groupBy)
        if (is.numeric(theData[, theVariable]) == TRUE)
        {
            # Used to have na.omit in here, I don't think it is necessary or wanted
            # but I'm leaving the syntaxt for now groupSummary <-
            # na.omit(select_(theData, theVariable, groupBy)) %>%
            # group_by_(groupBy) %>% select_(theVariable) %>%
            # dplyr::summarize_each(funs(mean, sd))
            groupSummary <- select_(theData, theVariable, groupBy) %>%
                group_by_(groupBy) %>% select_(theVariable) %>% na.omit() %>%
                dplyr::summarize_each(funs(mean, sd))
            groupRow <- NULL
            for (i in 1:nrow(groupSummary))
            {
                groupRow <- rbind(groupRow, data.frame(groupBy = groupSummary[i,
                  1], meanSd = sprintf(numericFormatStr, groupSummary[i,
                  2], groupSummary[i, 3]), stringsAsFactors = FALSE))
            }
            groupSpread <- tidyr::spread_(data = groupRow, key = groupBy,
                value = "meanSd")
            table1Rows <- cbind(table1Rows, groupSpread)

            if (conductGroupTests == TRUE)
            {
                f <- paste(theVariable, "~", groupBy)
                aov_summary <- summary(do.call("aov", list(as.formula(f),
                  data = theData)))
                table1Rows <- cbind(table1Rows, Stat = sprintf(statFormatStr,
                  aov_summary[[1]][["F value"]][1]), PValue = sprintf(pFormatStr,
                  aov_summary[[1]][["Pr(>F)"]][1]), stringsAsFactors = FALSE)
                # Just for snazzy feature, put a '<' sign in front of pvalue if it is
                # showing all zeros One of the few times a direct comparison of real
                # zero is helpful/valid
                if (as.numeric(table1Rows$PValue) == 0)
                {
                  table1Rows$PValue <- paste("< ", gsub("(.*)\\0", "\\1", table1Rows$PValue, '1'),'01', sep='')
                }
            }
        } else
        {
            groupSummary <- as.data.frame(table(theData[, theVariable],
                theData[, groupBy]))
            groupCount <- group_by(groupSummary, Var2) %>% dplyr::summarize(count = sum(Freq))
            groupSummary$percent <- (groupSummary$Freq/groupCount$count) *
                100
            if (percentFirst == TRUE)
            {
                groupSummary$countPercentStr <- sprintf(formatStr, groupSummary$percent,
                  "%", groupSummary$Freq, "")
            } else
            {
                groupSummary$countPercentStr <- sprintf(formatStr, groupSummary$Freq,
                  "", groupSummary$percent, "%")
            }
            groupTable <- select(groupSummary, Var1, Var2, countPercentStr)
            names(groupTable) <- c(theVariable, groupBy, "countPercentStr")
            groupTable <- spread_(groupTable, groupBy, "countPercentStr")
            table1Rows <- left_join(x = table1Rows, y = groupTable, by = theVariable)
            table1Rows <- cbind(Demographic = c(theVariable, rep('', nrow(table1Rows)-1)), table1Rows)
            table1Rows <- rename_(table1Rows, Level = theVariable)
            table1Rows$Level <- as.character(table1Rows$Level)

            # Do chi-square test to see if they differ by grouping variable
            if (conductGroupTests == TRUE)
            {
                names(groupSummary) <- c(theVariable, groupBy, "Freq",
                  "percent", "countPercentStr")
                chiData <- ungroup(groupSummary[, c(2, 3)]) %>% arrange_(groupBy)
                groupByColumn <- match(groupBy, names(theData))
                numGroups <- length(levels(theData[, groupByColumn]))
                varColumn <- match(theVariable, names(theData))
                numVarLevels <- length(levels(theData[, varColumn]))
                chiData$row <- rep(1:numVarLevels, numGroups)
                chiData <- spread_(data = chiData, key = groupBy, value = "Freq")
                chiData$row <- NULL
                chiProbs <- prop.table(table(theData[, theVariable]))
                chiData <- as.matrix(chiData)
                chiSquareResults <- chisq.test(chiData)

                table1Rows$Stat <- c(sprintf(statFormatStr, as.numeric(chiSquareResults$statistic)),
                  rep("", nrow(table1Rows) - 1))
                table1Rows$PValue <- c(sprintf(pFormatStr, as.numeric(chiSquareResults$p.value)),
                  rep("", nrow(table1Rows) - 1))
                # Just for snazzy feature, put a '<' sign in front of pvalue if it is
                # showing all zeros One of the few times a direct comparison of real
                # zero is helpful/valid
                if (as.numeric(table1Rows$PValue[1]) == 0)
                {
                  table1Rows$PValue[1] <- paste("< ", gsub("(.*)\\0", "\\1", table1Rows$PValue[1], '1'),'01', sep='')
                }
            }
        }
    } else
    { # Need to setup the 'Levels' and 'Demographic' here if there is no groupBy being done

      # When it's numeric, we just add an empty Level
      if (is.numeric(theData[, theVariable]) == TRUE)
      {
        table1Rows$Level <- ''
        table1Rows <- select(table1Rows, Demographic, Level, everything())
      } else
      {
        table1Rows <- cbind(Demographic = c(theVariable, rep('', nrow(table1Rows)-1)), table1Rows)
        table1Rows <- rename_(table1Rows, Level = theVariable)
      }

    }
    return(table1Rows)
}

#' Build Table 1
#'
#' Uses buildTable1Rows to build a list of data.frames constituting table 1.
#'
#'
#'
#' @param theData The data.frame containing the data for the variable.
#' @param theVariables A vector of variables that are being reported in the table (a character vector).
#' @param groupBy A variable (factor) used to stratify the variable being reported (see details below).
#' @param percentFirst A boolean, if TRUE numeric variables are printed as x\%(n) otherwise as n(x\%).
#' @param conductGroupTests A boolean, if TRUE group comparisons are conducted (see details below).
#' @param combineTables A boolean, if TRUE tables are combined into 1 data frame, otherwise a list is returned with a data.frame for each variable.
#' @param meanDigits An integer indicating number of digits printed following the decimal place (this is not precision as in round, you get the number of digits you ask for, even if they are zero).
#' @param sdDigits An integer indicating number of digits printed following the decimal place for standard deviations (this is not precision as in round, you get the number of digits you ask for, even if they are zero).
#' @param freqDigits An integer indicating number of digits printed following the decimal place for percentages (this is not precision as in round, you get the number of digits you ask for, even if they are zero).
#' @param statDigits An integer indicating number of digits printed following the decimal place for test statistics (this is not precision as in round, you get the number of digits you ask for, even if they are zero).
#' @param pDigits An integer indicating number of digits printed following the decimal place for test P-Values (if numer of zeros in P is greater than pDigits the value will be "< 000...1").
#' @return If combineTables is TRUE, then a single data.frame is returned. Otherwise, a list of data.frames one for each variable. All columns in the respective data frames are character vectors. For
#' numeric variables, the column will contain the mean and standard deviation in the form(s) described above. For factor variables
#' multiple rows are generated where columns will contain frequencies reported in percentages and counts "n" in the form described above.
#'
#'
#' If groupBy is specified then statistical tests are conducted to compare groups. For numeric variables ANOVA is used to compare
#' group means, factors are evaluated by \eqn{chi}-squared test of independence.
#'
#'\itemize{
#'   \item Demographic: The name of the variable described in the row
#'   \item Level: Blank for numeric variables, one row per level for factor (categorical) variables
#'   \item Value: the mean and standard deviation of theVariable ordered as described above
#'   \item Group: if groupBy is used then there will be a column for each level of the factor with frequencies and counts
#'   \item Stat: if
#' }
#'
#'
#' @examples
#' table1Tables <- buildTable1(theData = table1Dat, theVariables = c("age", "sex", "Race", "Ethnicity", "Rank"), groupBy="site", percentFirst = TRUE, meanDigits = 1, sdDigits = 1, freqDigits = 2, statDigits = 2, pDigits = 5)
#' lapply(table1Tables, print)
#'
#' @seealso \code{\link{buildTable1Rows}}
#'
buildTable1 <- function(theData, theVariables, groupBy, percentFirst = TRUE,
    conductGroupTests = TRUE, combineTables = TRUE, meanDigits = getOption("digits"), sdDigits = getOption("digits"),
    freqDigits = getOption("digits"), statDigits = getOption("digits"),
    pDigits = getOption("digits"))
    {

    table1List <- lapply(theVariables, FUN = buildTable1Rows, theData = theData,
        groupBy = groupBy, conductGroupTests = conductGroupTests, meanDigits = meanDigits,
        sdDigits = sdDigits, freqDigits = freqDigits, statDigits = statDigits,
        pDigits = pDigits)

    # Combine tables into one df if requested, otherwise just return the list of tables
    if (combineTables == TRUE){
      table1 <- NULL
      for (i in seq_along(table1List)){
         table1 <- rbind(table1, table1List[[i]])
      }
      return(table1)
    } else
    {
      return(table1List)
    }
}
