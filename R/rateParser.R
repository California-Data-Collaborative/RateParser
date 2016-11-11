library(dplyr)
library(yaml)

mnwd <- readChar("mnwd.owrs", file.info("mnwd.owrs")$size)
mnwdList <- yaml.load(mnwd)

# yaml_chars <- as.yaml(mnwdList)
# writeChar(yaml_chars, "out_test.owrs")
# load(file="data/santamonica.rda")

# df_mnwd <- read.csv("../RateComparison/data/mnwd_test.csv", stringsAsFactors = FALSE)
# names(df_mnwd) <- c( "cust_id", "usage_month", "usage_year", "usage_date", "usage_ccf",
#                      "ET", "hhsize", "irr_area", "cust_class", "rate_code")


calculate_bill <- function(df, rate_structure){
  class_rate <- rate_structure[[df$cust_class[1]]]
  for(i in 1:length(class_rate)){
    rate_part <- class_rate[[i]]
    name <- names(rate_part)

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

eval_field_or_formula <- function(df, rate_part){
  name <- names(rate_part)
  return(eval(parse(text=rate_part[[name]]), df))
}

collapse_tiers <- function(map){
  # if dealing with mapped tiers
  # ASSERTION, only tiers that depend on data values should be depth > 1
  if(depth(map) > 1){
    collapsed <- lapply(map, FUN=paste, collapse="\n")
  }else{
    collapsed <- map
  }

  return(collapsed)
}

eval_map <- function(df, rate_part){
  name <- names(rate_part)
  # append the column names together
  # depends_col <- paste(rate_part[[name]]$depends_on, collapse="|")

  # Appened together each element in the dependency columns
  keys <- do.call(paste, c(df[rate_part[[name]]$depends_on], sep = "|"))

  # Get the value mapping, and flatten together in the case of tiers
  pricemap <- rate_part[[name]]$values
  pricemap <- collapse_tiers(pricemap)

  return( unname(unlist(pricemap)[keys]) )
}

# check whether the
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


# df_row <- tbl_df(santamonica)[2,]
df_row <- tbl_df(df_mnwd) %>% filter(cust_class=="RESIDENTIAL_SINGLE")
df_row$days_in_period <- 30.4
df_row$meter_size <- '5/8"'


# df_row$irr_area <- 1300
# df_row$hhsize <- 4
# df_row$ET <- 2.3


# df_calced <- calculate_bill(df_row, mnwdList$rate_structure)


# tbl_df(santamonica)[1:10,] %>% rowwise() %>% do(calculate_bill(.,mnwdList$rate_structure))
