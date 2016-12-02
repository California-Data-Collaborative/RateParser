
#' Calculate water bills.
#'
#' \code{calculate_bill} applies a rate structure to water use data to
#' calculate water bills. Applies \code{calculate_class_bill} to each
#' \code{cust_class} separately.
#'
#' @param df Data frame containing all data values referenced in the rate
#' file but not defined in the rate file. E.g. \code{usage_ccf},
#' \code{cust_class}, \code{meter_size}, \code{etc.}
#' @param parsed_rate
#' \href{https://github.com/California-Data-Collaborative/Open-Water-Rate-Specification}{OWRS file},
#' parsed into an \code{R} list by the \code{yaml} package.
#'
#' @return Original data frame with additional columns appended. Additional columns are those
#' defined in the rate file, as well as columns \code{Xi} representing the usage in the
#' ith tier, and \code{XRi} representing revenue in the ith tier (where applicable).
#'
#' NOTE: rows are not guarenteed to be in the same order as originally passed,
#' and will likely be in a different order
#'
#' @importFrom dplyr %>% tbl_df group_by do bind_cols
#' @export
calculate_bill <- function(df, parsed_rate){
  tmp <- df %>% group_by(cust_class) %>% do(calculate_class_bill(., parsed_rate))
  return(tmp)
}

#' Calculate water bills for a single customer class.
#'
#' \code{calculate_class_bill} applies a rate structure to water use data to
#' calculate water bills.
#'
#' @param df Data frame containing all data values referenced in the rate
#' file but not defined in the rate file. E.g. \code{usage_ccf},
#' \code{cust_class}, \code{meter_size}, \code{etc.}
#' @param parsed_rate
#' \href{https://github.com/California-Data-Collaborative/Open-Water-Rate-Specification}{OWRS file},
#' parsed into an \code{R} list by the \code{yaml} package.
#'
#' @return Original data frame with additional columns appended. Additional columns are those
#' defined in the rate file, as well as columns \code{Xi} representing the usage in the
#' ith tier, and \code{XRi} representing revenue in the ith tier (where applicable).
#'
#' @importFrom dplyr %>% tbl_df group_by do bind_cols
calculate_class_bill <- function(df, parsed_rate){
  rate_structure <- parsed_rate$rate_structure
  df <- tbl_df(df)
  i <- sapply(df, is.factor)
  df[i] <- lapply(df[i], as.character)

  class_rate <- rate_structure[[df$cust_class[1]]]
  stopif(is.null(class_rate), paste("No rate information for customer class ", df$cust_class[1],
                                    " is present in rate file.") )

  for(i in 1:length(class_rate)){
    rate_part <- class_rate[[i]]
    name <- names(rate_part)

    stopif(!is_valid_rate_part(rate_part),
           paste("The OWRS file might not be formatted properly. ",
                 "\nError occured in customer class ", df$cust_class[1],
                 ", near to: ", paste(name,collapse=" ") )
    )

    if( is_map(rate_part[[name]]) ){# if rate_part is a map
      df[[name]] <- eval_map(df, rate_part)
    }
    else if(length(rate_part[[name]]) > 1){# if rate_part specifies tiers
      df[[name]] <- paste(rate_part[[name]], collapse="\n")
    }
    else if(is_rate_type(rate_part)){
      rate_type <- rate_part[[name]]
      variable_bills <- df %>% group_by(tier_starts, tier_prices) %>%
                          do(calculate_variable_bill(., rate_type))
      #rename the column
      names(variable_bills)[names(variable_bills)=="variable_bill"] <- name
      df <- bind_cols( df, variable_bills )
    }
    else{
      df[[name]] <- eval_field_or_formula(df, rate_part)
    }
  }

  return(df)
}

#' Read an OWRS file.
#'
#' \code{read_owrs_file} reads a raw \code{yaml} file and parses it
#' into a list structure.
#'
#' @param filepath Path to the OWRS file.
#' @return Nested list representing the parsed yaml structure.
#'
#' @export
read_owrs_file <- function(filepath){
  raw_yaml <- readChar(filepath, file.info(filepath)$size)
  parsed_yaml <- yaml::yaml.load(raw_yaml)
  return( parsed_yaml )
}

#' Evaluate a field or formula.
#'
#' \code{eval_field_or_formula} evaluates a field or formula in the context of
#' the provided dataframe.
#'
#' @param df Data frame containing all data values referenced in field or formula.
#' @param rate_part List representing a portion of a rate structure, defined in the
#' \href{https://github.com/California-Data-Collaborative/Open-Water-Rate-Specification}{OWRS file}
#'
#' @return Either a single value or a vector the length of \code{df}. The result of
#' evaluating a field or formula in the context of the dataframe.
#'
#' @keywords internal
eval_field_or_formula <- function(df, rate_part){
  name <- names(rate_part)
  return(eval(parse(text=rate_part[[name]]), df))
}

#' Collapse a multi-level list.
#'
#' \code{collapse_tiers} collapses all elements of the 2nd level of a list into a string.
#' elements are pasted together separated by "\\n".
#'
#' @param map List representing a conditional/map in OWRS format.
#'
#' @return Either the original list or the collapsed list.
#'
#' @keywords internal
collapse_tiers <- function(map){
  # if dealing with mapped tiers
  # ASSERTION, only tiers that depend on data values should be depth > 1
  if(length(map[[1]]) > 1){
    collapsed <- lapply(map, FUN=paste, collapse="\n")
  }else{
    collapsed <- map
  }

  return(collapsed)
}

#' Evaluate a map.
#'
#' \code{eval_map} evaluates a conditional/map in the context of
#' the provided dataframe.
#'
#' @param df Data frame containing all data values referenced in the map.
#' @param rate_part List representing a portion of a rate structure, defined in the
#' \href{https://github.com/California-Data-Collaborative/Open-Water-Rate-Specification}{OWRS file}
#'
#' @return Vector the length of \code{df}. The result of evaluating a map in the context
#' of the dataframe.
#'
#' @keywords internal
eval_map <- function(df, rate_part){
  name <- names(rate_part)
  # append the column names together
  # depends_col <- paste(rate_part[[name]]$depends_on, collapse="|")

  check <- !all(rate_part[[name]]$depends_on %in% names(df))
  stopif( check, paste0("One of the fields (", rate_part[[name]]$depends_on, ") is not present.",
                        " It can be defined either in the data or in the rate file.") )

  # Appened together each element in the dependency columns
  keys <- do.call(paste, c(df[rate_part[[name]]$depends_on], sep = "|"))

  # Get the value mapping, and flatten together in the case of tiers
  pricemap <- rate_part[[name]]$values
  pricemap <- collapse_tiers(pricemap)

  return( unname(unlist(pricemap)[keys]) )
}

#' Check whether a rate part is a map.
#'
#' \code{is_map} checks whether the rate part contains a \code{depends_on}
#' and a \code{values} clause. If it does, then it is a proper map.
#'
#' @param rate_part List representing a portion of a rate structure, defined in the
#' \href{https://github.com/California-Data-Collaborative/Open-Water-Rate-Specification}{OWRS file}
#'
#' @return Boolean representing whether the rate part is a map.
#'
#' @export
is_map <- function(rate_part){
  parts <- names(rate_part)
  has_depends <- "depends_on" %in% parts
  has_values <- "values" %in% parts

  if(has_depends && has_values){
    return(TRUE)
  } else if (has_depends || has_values){
    stop("Each 'depends_on' clause must have a corresponding 'values' clause, and vice versa.")
  } else{
    return(FALSE)
  }
}

is_rate_type <- function(rate_part){
  name <- names(rate_part)
  if(rate_part[[name]] %in% c("Budget", "Tiered")){
    return(TRUE)
  }else{
    return(FALSE)
  }
}

depth <- function(this,thisdepth=0){
  if(!is.list(this)){
    return(thisdepth)
  }else{
    return(max(unlist(lapply(this,depth,thisdepth=thisdepth+1))))
  }
}

is_valid_rate_part <- function(rate_part){
  tryCatch({
    rate_part[[names(rate_part)]]
    return(TRUE)
  }, error = function(err){
    return(FALSE)
  })
}

stopif <- function(bool, message){
  if(bool){
    stop(message, call.=FALSE)
  }
}



