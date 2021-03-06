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
  x <- grep(paste(cond, collapse = "|"), c(x, y, z), ignore.case = T, value = FALSE)
  return (ifelse(length(x), as.numeric(x), as.numeric(0)))
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
                 ifelse(grepl(paste(gen_female, collapse = "|"),tangram[y,2*x+3], ignore.case = T),
                        "F", 
                        ifelse(grepl(paste(gen_male, collapse = "|"),tangram[y,2*x+3], ignore.case = T),
                               "M",-2))))
} ## cannot be used for data table
# WRONG CODE: tangram$factor_gender <- mapply(ismatch, gen, tangram$Factor1, tangram$Factor2, tangram$Factor3, SIMPLIFY = TRUE) ## wrong: clase301
tangram <- as.data.table(tangram)
tangram[, factor_gender := ismatch(gen, Factor1, Factor2, Factor3), by = 1:nrow(tangram)]
tangram <- as.data.frame(tangram)
tangram$level_gender <- mapply(level_gen_fun, tangram$factor_gender, 1:nrow(tangram), SIMPLIFY = TRUE)

### Stem major


stem <- c("stem")
stem_Y <- c("^y")
stem_N <- c("^n")
#tangram$factor_STEM <- mapply(ismatch, stem, tangram$Factor1, tangram$Factor2, tangram$Factor3, SIMPLIFY = TRUE)
tangram[, factor_STEM := ismatch(stem, Factor1, Factor2, Factor3), by = 1:nrow(tangram)]
level_stem_fun <- function(x, y) {
  return (ifelse(x == 0, -1, 
                 ifelse(grepl(paste(stem_Y, collapse = "|"),tangram[y,2*x+3], ignore.case = T),
                        "Y", 
                        ifelse(grepl(paste(stem_N, collapse = "|"),tangram[y,2*x+3], ignore.case = T),
                               "N",-2))))
}

tangram$level_stem <- mapply(level_stem_fun, tangram$factor_STEM, 1:nrow(tangram), SIMPLIFY = TRUE)

### 3. Age
age <- c("^age")
tangram$factor_age <- mapply(ismatch, age, tangram$Factor1, tangram$Factor2, tangram$Factor3, SIMPLIFY = TRUE)
level_age_fun <- function(x, y) {
  return (ifelse(x == 0, -1, 
                 ifelse(!is.na(as.numeric(tangram[y,2*x+3])), 
                               as.numeric(tangram[y,2*x+3]),
                               -2)))
}

tangram$level_age <- mapply(level_age_fun, tangram$factor_age, 1:nrow(tangram), SIMPLIFY = TRUE)


