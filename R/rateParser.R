library(dplyr)
library(yaml)

mnwd <- readChar("mnwd.owrs", file.info("mnwd.owrs")$size)
mnwdList <- yaml.load(mnwd)

load(file="data/santamonica.rda")

calculate_bill <- function(df, rate_structure){
  browser()
  class_rate <- rate_structure[[df$cust_class]]
  rate_part <- class_rate[[1]]
  name <- names(rate_part)
  df[[name]] <-
  return(df)
}

eval_field_or_formula <- function(df, rate_part){
  name <- names(rate_part)
  return(eval(parse(text=rate_part[[name]]), df))
}

#TODO check that is "map_..."
eval_map <- function(df, rate_part){
  name <- names(rate_part)
  #remove "map_" from column name
  field_to_map <- substr(name,5, nchar(name))
  return(rate_part[[name]][[df[[field_to_map]]]])
}


df_row <- tbl_df(santamonica)[2,]
df_row$irr_area <- 1300
df_row$hhsize <- 4
df_row$days_in_period <- 30.4
df_row$meter_size <- '5/8"'


calculate_bill(df_row, mnwdList$rate_structure)


tbl_df(santamonica)[1:10,] %>% rowwise() %>% do(calculate_bill(.,mnwdList$rate_structure))
