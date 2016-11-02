library(dplyr)
library(yaml)

mnwd <- readChar("mnwd.owrs", file.info("mnwd.owrs")$size)
mnwdList <- yaml.load(mnwd)

load(file="data/santamonica.rda")
df_mnwd <- read.csv("../RateComparison/data/mnwd_test.csv", stringsAsFactors = FALSE)
names(df_mnwd) <- c( "cust_id", "usage_month", "usage_year", "usage_date", "usage_ccf",
                     "ET", "hhsize", "irr_area", "cust_class", "rate_code")


calculate_bill <- function(df, rate_structure){
  browser()
  class_rate <- rate_structure[[df$cust_class[1]]]

  for(i in 1:length(class_rate)){
    rate_part <- class_rate[[i]]
    name <- names(rate_part)

    if( is_map(rate_part[[1]]) ){
      df[[name]] <- eval_map(df, rate_part)
    }else{
      df[[name]] <- eval_field_or_formula(df, rate_part)
    }
    browser()
  }

  return(df)
}

eval_field_or_formula <- function(df, rate_part){
  name <- names(rate_part)
  return(eval(parse(text=rate_part[[name]]), df))
}

eval_map <- function(df, rate_part){
  name <- names(rate_part)
  depends_col <- rate_part[[name]]$depends_on
  pricemap <- rate_part[[name]]$values

  return(pricemap[[df[[depends_col]]]])
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


# df_row <- tbl_df(santamonica)[2,]
df_row <- tbl_df(df_mnwd) %>% filter(cust_class=="RESIDENTIAL_SINGLE")
df_row$days_in_period <- 30.4
df_row$meter_size <- '5/8"'


df_row$irr_area <- 1300
df_row$hhsize <- 4
df_row$ET <- 2.3


calculate_bill(df_row, mnwdList$rate_structure)


tbl_df(santamonica)[1:10,] %>% rowwise() %>% do(calculate_bill(.,mnwdList$rate_structure))
