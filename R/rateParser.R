
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

  name_list <- names(class_rate)
  names(name_list) <- name_list
  name_list <- as.list(name_list)

  for(i in 1:length(name_list)){
    name <- name_list[[i]]

    #if the field has not already been evaluated
    if(!(name %in% names(df))){
      df <- add_rate_part_to_frame(df, name, name_list, class_rate, df$cust_class[1])
    }

  }

  return(df)
}

#' Evaluate a rate part and add to the dataframe.
#'
#' @importFrom dplyr %>% tbl_df group_by do bind_cols ungroup select
add_rate_part_to_frame <- function(df, name, name_list, class_rate, cust_class){
  rate_part <- class_rate[[name]]

  #     stopif(!is_valid_rate_part(rate_part),
  #            paste("The OWRS file might not be formatted properly. ",
  #                  "\nError occured in customer class ", df$cust_class[1],
  #                  ", near to: ", paste(name,collapse=" ") )
  #     )

  df <- tryCatch({

    if( is_map(rate_part) ){# if rate_part is a map
      df[[name]] <- eval_map(df, rate_part)
    }
    else if(length(rate_part) > 1){# if rate_part specifies tiers
      df[[name]] <- paste(rate_part, collapse="\n")
    }
    else if(is_rate_type(rate_part)){

      rate_type <- rate_part

      if(name=="commodity_charge"){
        stopif(!(("tier_starts" %in% names(df))), "object 'tier_starts' not found")
        stopif(!(("tier_prices" %in% names(df))), "object 'tier_prices' not found")
        stopif(!(("budget" %in% names(df)))&&rate_type=='Budget', "object 'budget' not found")

        variable_bills <- df %>% group_by(tier_starts, tier_prices) %>%
          do(calculate_variable_bill(., rate_type)) %>%
          ungroup() %>% select(-tier_starts, -tier_prices)

      }else if(name=="sewer_charge"){
        stopif(!(("sewer_tier_starts" %in% names(df))), "object 'sewer_tier_starts' not found")
        stopif(!(("sewer_tier_prices" %in% names(df))), "object 'sewer_tier_prices' not found")

        variable_bills <- df %>% group_by(sewer_tier_starts, sewer_tier_prices) %>%
          do(calculate_variable_bill(., rate_type, start_name="sewer_tier_starts",
                                     price_name="sewer_tier_prices", is_sewer=TRUE)) %>%
          ungroup() %>% select(-sewer_tier_starts, -sewer_tier_prices)
      }

      #rename the column
      names(variable_bills)[names(variable_bills)=="variable_bill"] <- name
      df <- bind_cols( df, variable_bills )
    }
    else{
      df[[name]] <- eval_field_or_formula(df, name, rate_part)
    }

    return(df)
  }, error = function(e) {

    #if error is caused by field not found
    if( grepl("object .* not found", e$message)){

      field_not_found <- strsplit(e$message, "^object '|' not found$")[[1]][2]

      #field is present in OWRS file just not in proper ordering
      if(field_not_found %in% names(name_list)){

        #recursive call to evaluate missing sub-field
        df <- add_rate_part_to_frame(df, field_not_found, name_list, class_rate, cust_class)
      }else{
        #the field is not present at all
        stop(paste0("The field ",
                    field_not_found,
                    " is not present in the OWRS file for customer class ",
                    cust_class)
             )
      }

    }else{
      #error was caused by something other than a missing field
      stop(e$message)
    }

    return(df)
  })

  #if the field has not already been evaluated,
  # go back and evaluate the original now that the subfield(s) have been handled.
  if(!(name %in% names(df))){
    df <- add_rate_part_to_frame(df, name, name_list, class_rate, cust_class)
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
#' @param name Name of the rate part
#' @param rate_part List representing a portion of a rate structure, defined in the
#' \href{https://github.com/California-Data-Collaborative/Open-Water-Rate-Specification}{OWRS file}
#'
#' @return Either a single value or a vector the length of \code{df}. The result of
#' evaluating a field or formula in the context of the dataframe.
#'
#' @keywords internal
eval_field_or_formula <- function(df, name, rate_part){
  #if field/formula represent a budget, we round each of the budget components
  if(grepl("budget", name)){
    rate_part <- gsub("\\+", " + ", rate_part)
    rate_part <- gsub("\\*", " * ", rate_part)
    rate_part <- gsub("\\^", " ^ ", rate_part)

    ls <- unlist(strsplit(rate_part, " +"))
    for(i in 1:length(ls)){
      s <- ls[i]
      if(!(s %in% c("+","*","^")))
        ls[i] <- paste("round(",s,")")
    }
    rate_part <- paste(ls, collapse=" ")
  }


  return(eval(parse(text=rate_part), df))
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
  # name <- names(rate_part)
  # append the column names together
  # depends_col <- paste(rate_part[[name]]$depends_on, collapse="|")

  check <- !all(rate_part$depends_on %in% names(df))
  stopif( check, paste0("One of the fields (", rate_part$depends_on, ") is not present.",
                        " It can be defined either in the data or in the rate file.") )

  # Append together each element in the dependency columns
  keys <- do.call(paste, c(df[rate_part$depends_on], sep = "|"))

  # Get the value mapping, and flatten together in the case of tiers
  pricemap <- rate_part$values
  pricemap <- collapse_tiers(pricemap)
  results <- unname(unlist(pricemap)[keys])

  nonmaps <- unique(keys[is.na(results)])
  valid_nas <- c("", "NA")
  check <- any(!(nonmaps %in%  valid_nas))
  stopif(check, paste0("The following map keys are missing from the OWRS file: (",
                       paste(nonmaps[!(nonmaps %in% valid_nas)], collapse = ", "), ")") )

  return( results )
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
  if(rate_part %in% c("Budget", "Tiered")){
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



