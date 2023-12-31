
# ---------------------------------------------

dfinfo <- function(df, diagnostics = FALSE, verbose = TRUE) {

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # 1.1 : get variable names
  dfout <- data.frame('Variables' = colnames(df), stringsAsFactors = FALSE) 

  # 1.2 : check if variable names are well-formed
  names.orig <- colnames(df)
  names.good <- make.names(names.orig)
  
  # 1.3 : change if bad names
  if (length(setdiff(names.good, names.orig)) > 0) {
    # give the df good names
    if (verbose) print(' ... WARNING : changing to well formed variable names')
    
    dfout <- dfout %>% 
      mutate(GoodNames = names.good) %>% 
      mutate(Changed = if_else(Variables==Variables.Orig,"","CHANGED"))
    
    colnames(df) <- names.good
  }
  
  # 1.3 : get excel column header
  dfout$Excel <- sapply(1:ncol(df), excelcol) 
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # 2.1 : get variable class
  # sometime there is more than 1 description, eg. ordered factor
  dfout$Class <- df %>% sapply(function(x) paste(class(x), collapse = '.')) %>% unlist()
  
  # 2.2 : get variable type
  dfout$Type <- df %>% sapply(function(x) paste(typeof(x), collapse = '.')) %>% unlist()

  # 2.2.1 : now, convert any factors to character
  factor_columns <- sapply(df, is.factor)   
  df[factor_columns] <- lapply(df[factor_columns], as.character) 

  # 2.3 : get NAs
  dfout$NA_Tot <- df %>% sapply(function(x) as.character(sum(is.na(x))))
  
  # 2.3.1 : get NA percent
  dfout$NA_PC <- sprintf("%.0f%%", as.numeric(dfout$NA_Tot) / nrow(df) * 100)

  # 2.4 : get uniques
  dfout$Unique_Tot <-  df %>% sapply(function(x) as.character(n_distinct(x)))
  
  # 2.4.1 : get unique percent
  dfout$Unique_PC <- sprintf("%.0f%%", as.numeric(dfout$Unique_Tot) / nrow(df) * 100)
  
  # 2.5 : get min values
  dfout$Min <- df %>% sapply(function(x) as.character(min(x, na.rm = TRUE)))
  
  # 2.6 : get max values
  dfout$Max <- df %>% sapply(function(x) as.character(max(x, na.rm = TRUE)))
  
  # 2.7 : get means for numerics
  dfout$Mean <- df %>% sapply(custom_mean)
  
  # 2.8 : get skew for numerics
  dfout$Skew <- df %>% sapply(custom_skew)
  
  return(dfout)
}

# get the alpha excel column designation 
excelcol <- function(number) {
  
  # 1 : more variables than permitted columns in excel
  if (number > 16384) return('16384 max')
  if (number < 1) return('must be > 0')
  
  # 2 : find number of letters
  letter_count = 0
  if (number <= 26^1) {
    letter_count <- 1
  } else if (number <= 26^2) {
    letter_count <- 2
  } else if (number <= 26^3) {
    letter_count <- 3 
  } else {
    return('some problem')
  }
  
  # 3 : single letter columns
  #     number will be in range 1 - 26, A is 65 in utf8
  if (letter_count == 1) return(intToUtf8(64+number))
  
  # 4 : 2 letter columns
  #     number will be in range 27 to 676
  if (letter_count == 2) {
    # first letter, how many 26's are there
    num1 <- as.integer(number / 26)
    # remainder, if 0 its the last in the set
    rem1 <- number %% (num1 * 26)
    
    if (rem1 == 0) {
      num1 <- num1 - 1
      rem1 <- 26
    }
    
    letter1 <- intToUtf8(64+num1)
    letter2 <- intToUtf8(64+rem1)
    return(paste0(letter1,letter2))
  }
  
  # 5 : 3 letter columns
  #     number will be in range 677 to 16384
  if (letter_count == 3) {
    # first letter, how many 26^2's are there
    num1 <- as.integer(number / 26^2)
    # remainder, if it's 0 it's the last in the set
    rem1 <- number %% (num1 * 26^2)
    
    if (rem1 == 0) {
      num1 <- num1 - 1
      rem1 <- 676
    }
    
    # second letter, how many 26's are there
    # find whats left for the 26 x 26 outer matrix
    number2 <- number - ((num1 * 26^2) - 26)
    
    num2 <- as.integer(number2 / 26)
    # remainder, if 0 its the last in the set
    rem2 <- number2 %% (num2 * 26)
    print(number2)

    if (num2 == 0) {
      num2 <- num2 - 1
      rem2 <- 26
    }

    letter1 <- intToUtf8(64+num1)
    letter2 <- intToUtf8(64+num2)
    letter3 <- intToUtf8(64+rem2)
    return(paste0(letter1,letter2,letter3))
  }
  
  # error
  return('ERROR')
}

# get means only for numeric columns
custom_mean <- function(variable) {
  
  varclass <- paste(class(variable), collapse = '.')
  vartype <- paste(typeof(variable), collapse = '.')
  
  if (varclass %in% c('numeric') & vartype %in% c('double')) {
    return_val <- mean(variable, na.rm = TRUE)
    return(sprintf("%.1f", return_val))
  } else {
    return('-')
  }
}

# get skewness only for numeric columns
custom_skew <- function(variable) {
  
  varclass <- paste(class(variable), collapse = '.')
  vartype <- paste(typeof(variable), collapse = '.')
  
  if (varclass %in% c('numeric') & vartype %in% c('double')) {
    
    n <- length(variable)
    mean_x1 <- mean(variable)
    sd_x1 <- sqrt(sum((variable - mean_x1)^2) / (n))
    z1 <- (variable - mean_x1) / sd_x1
    skewness <- sum(z1^3) / n
    
    return(sprintf("%.1f", skewness))
  } else {
    return('-')
  }
}

