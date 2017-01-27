### R=script      : factor-filter.R
### Description   : This R script is used to create solid factor columns and clean corresponding levels.
### In-the-context: It will be used for analysis and filtering in Shiny app.

### 1. Factor to be filtered: GENDER
### A set of criteria for regular expressions/ key patterns
gen = c("^gen","^sex")
gen_male = c("^m","^h")
gen_female = c("^f", "^mu")

### Supporting functions
### ismatch
### @input: cond: a vector of key patterns (e.g. gen)
###         x, y, z: 3 columns to look for key
### @return: 0 if no match
###          index of the factor (1, 2, 3() if there is match
ismatch <- function(cond, x, y, z) {
  x <- grep(paste(cond, collapse = "|"), c(x, y, z), ignore.case = T)
  return (ifelse(length(x), x[1], 0))
}

### level_gen_fun
### @input: x is the col index (given by factor_gender)
###         y is the row index
### @return: -1 if no gender factor indicated
###          -2 if gender factor indicated but level_gen key patterns not matched (require future manual check)
###           M/F
### @note: this function is specific to gender only. Needs to think about how to generalize it. 
level_gen_fun <- function(x, y) {
  return (ifelse(x == 0, -1, 
                 ifelse(grepl(paste(gen_female, collapse = "|"),tangram[y,x+4], ignore.case = T),
                        "F", 
                        ifelse(grepl(paste(gen_male, collapse = "|"),tangram[y,x+4], ignore.case = T),
                               "M",-2))))
}

tangram$factor_gender <- mapply(ismatch, gen, tangram$Factor1, tangram$Factor2, tangram$Factor3, SIMPLIFY = TRUE)
tangram$level_gender <- mapply(level_gen_fun, tangram$factor_gender, 1:nrow(tangram), SIMPLIFY = TRUE)

