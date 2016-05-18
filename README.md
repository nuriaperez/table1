# table1
Creates a Table of Descriptives Suited for MS-Word

I created this package to ease the process of preparing a typical "table 1" as used in medical and social science research. ***Table 1*** generally contains descriptive information for your sample. For integer variables like *age* or numeric variables like *baseline depression* scores, you generally want means and standard deviations. For categorical variables you generally want to know the number and relative percentage of cases within each category.  

Another common feature of Table-1 is stratefication. Perhaps the most common example is two or more groups in a randomized control trial. Other examples might be different recruitment sites, or different disease status. When using stratification, Table 1 usually has a column presenting the summary statistics for each strata.  

It is also very common to conduct statistical tests to determine if demographic variables differ by stratification groups. For integer and numeric variables it is common to use ANOVA, whereas for categorical variables chi-square tests are commonly used.

While R does all of these tasks very easily, what is missing is a convenient way to assemble them into a table that can be exported to Microsoft Word (MS-Word) that doesn't require subtantial editing.

The Table-1 package aims to make the creation of Table 1 much less painful. Here is an example of the desired final product:


