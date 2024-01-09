
# ---------------------------------------------

# information about a dataframe
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

# -------------------------------------------------------------------------

# Crosstabs : generate columns
generate_xtab_column <- function(df_tab, col_item = '', options=c(), na_text = na_text) {
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # generates each column of the crosstab
  # assumes df_tab is an input dataframe of 2 columns called sidevar, headvar
  # if col_item == 'Make_Total' then generate a total column
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  text_total <- 'TOTAL'
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # total column for sidevar or headvar to form the framework of
  # the table output. everything s joined to this, so 
  # order and so on goes here
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (tolower(col_item) == 'make_total') {
    line_tot <- df_tab %>% mutate(sidevar = text_total) %>% count(sidevar)
    line_all <- df_tab %>% filter(!is.na(sidevar)) %>% count(sidevar)
    line_nas <- df_tab %>% filter(is.na(sidevar)) %>% count(sidevar) %>% mutate(sidevar = na_text)
    
    # assumes at this point the sidevar can have any variable type
    # need to sort first then change to character
    # so that if sidevar is integer the sorting is correct
    if ('sortside_n' %in% options) line_all <- line_all %>% arrange(desc(n))
    line_all <- line_all %>% mutate(sidevar = as.character(sidevar))
    
    col_stub <- bind_rows(line_tot, line_all, line_nas) %>% rename(TOTAL = n)
    return(col_stub)
  }
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # this will generate a column of occurance counts for the
  # sidevar for a specific item in the headvar denoted by col_item
  # assumes at this point df_tab will be all character
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  df_tab <- df_tab %>% filter(headvar == col_item)
  
  # for each element of the sidevar
  col_items <- bind_rows(
    # generate a total
    df_tab %>% 
      summarise(n = n()) %>% 
      mutate(sidevar = text_total) %>% 
      select(sidevar, n),
    
    # totals for each side item
    df_tab %>% 
      group_by(sidevar) %>% 
      summarise(n = n()) %>% 
      ungroup() 
  )
  
  return(col_items)
}

# Crosstabs : generate a crosstab
xtab <- function(.data, side, header, options = c(), na_text = 'N/A') {
  
  df <- .data
  
  # STEP 0 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # get the two variables involved in the cross tabulation
  # so the code is generic make a dataframe with just those
  # two variables, and call them sidevar and headvar
  # rename these back to the original values before 
  # returning the result
  
  df_tab <- df %>% 
    select(sidevar = (!!enquo(side)),
           headvar = (!!enquo(header)))
  
  # get string variables
  side_txt = deparse(substitute(side))
  head_txt = deparse(substitute(header))
  
  # get the type of variables
  df_types <- sapply(df_tab, class)
  
  # STEP 1 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # create the stub/side/column 0 from the sidevar
  # it has a total as the first element
  # and if there are NAs those go at the end
  col0 <- generate_xtab_column(df_tab, 
                               col_item = 'Make_Total', 
                               options = options,
                               na_text = na_text) 
  
  # STEP 2 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # now that column 0 knows about any NAs, and has done
  # the ordering appropriately for the variable type
  # sidevar and headvar can be character and NAs can 
  # be character with text as specified in na_text
  df_tab <- df_tab %>% 
    mutate(sidevar = as.character(sidevar)) %>% 
    mutate(headvar = as.character(headvar)) %>% 
    mutate(sidevar = coalesce(sidevar, na_text)) %>% 
    mutate(headvar = coalesce(headvar, na_text))  
  
  # STEP 3 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # create an output column for each occurance in the headvar
  head_items <- df_tab %>% count(headvar)
  if ('sorthead_n' %in% options) head_items <- head_items %>% arrange(desc(n))
  head_items <- head_items$headvar
  if (na_text %in% head_items) head_items <- c(head_items[!head_items %in% c(na_text)], na_text)
  
  for (item in head_items) {
    # make a column for each item
    colx <- generate_xtab_column(df_tab,
                                 col_item=item,
                                 options = options,
                                 na_text = na_text)
    
    out_name <- paste0(head_txt, '_', item)
    colx <- colx %>% rename(!!out_name := n)
    col0 <- col0 %>% full_join(colx, by = 'sidevar')
  }
  
  # STEP 4 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # rename sidevar to the original variable name
  # set any nas to zero
  col0 <- col0 %>% 
    rename(!!side_txt := sidevar) %>% 
    mutate(across(where(is.numeric), coalesce, 0)) %>% 
    identity()
  
  return(col0)
}

# -------------------------------------------------------------------------

# count the contents of variables in a dataframe
hcount <- function(dataframe, elem_max = 20, elem_sort = TRUE) {
  
  for (i in seq_along(dataframe %>% colnames())) {
    
    # get the list of columns on the first pass
    if (i == 1) myvars <- dataframe %>% colnames()
    
    varname = myvars[i]
    
    # count occurances  
    df_count <- dataframe %>%
      count(!!sym(varname), sort = elem_sort) %>%
      setNames(c('dataset', 'total')) %>%
      mutate(source = varname, .before = 1) %>%
      mutate(info = 'element', .before = 2) %>%
      identity
    
    # get some information
    elem_count <- nrow(df_count)
    elem_type <- typeof(df_count$dataset)
    unique_text <- paste0(', uniques : ', elem_count)
    
    # truncate if there are too many elements
    if (nrow(df_count) > elem_max) {
      df_count <- df_count %>%
        mutate(dataset = as.character(dataset)) %>%
        slice(1:elem_max)
      
      unique_text <- paste0(unique_text, ', max printed : ', elem_max)
    }
    
    # make counts collapsable
    df_count <- df_count %>%
      mutate(dataset = as.character(dataset))
    
    # calculate %ages
    df_count <- df_count %>%
      mutate(pcent = paste0(round(total / nrow(dataframe) * 100), ' %'))
    
    df_count <- bind_rows(
      data.frame(source = varname,
                 info = paste0('summary - datatype :',elem_type, unique_text),
                 dataset = '',
                 total = elem_count,
                 pcent = ''),
      df_count)
    
    if (i == 1) {
      df_counts <- df_count
    } else {
      df_counts <- bind_rows(df_counts, df_count)
    }
  }
  
  return(df_counts)
}

# ---------------------------------------------
# supporting functions

# get the alpha excel column designation of a number
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

