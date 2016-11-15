library(dplyr)
library(yaml)

mnwd <- readChar("mnwd.owrs", file.info("mnwd.owrs")$size)
mnwdList <- yaml.load(mnwd)

# yaml_chars <- as.yaml(mnwdList)
# writeChar(yaml_chars, "out_test.owrs")
# load(file="data/santamonica.rda")

df_mnwd <- read.csv("../RateComparison/data/mnwd_test.csv", stringsAsFactors = FALSE)
names(df_mnwd) <- c( "cust_id", "usage_month", "usage_year", "usage_date", "usage_ccf",
                     "ET", "hhsize", "irr_area", "cust_class", "rate_code")

# df_row <- tbl_df(santamonica)[2,]
# df_row <- tbl_df(df_mnwd) %>% filter(cust_class=="RESIDENTIAL_SINGLE")
df_row <- tbl_df(df_mnwd) %>%
  filter(cust_class %in% c("IRRIGATION", "RESIDENTIAL_SINGLE", "RESIDENTIAL_MULTI") ) %>%
  group_by(cust_class)
df_row$days_in_period <- 30.4
df_row$meter_size <- '5/8"'
df_row$water_type <- 'POTABLE'


# df_row$irr_area <- 1300
# df_row$hhsize <- 4
# df_row$ET <- 2.3


# df_calced <- calculate_bill(df_row, mnwdList$rate_structure)


df_calced <- df_row %>% group_by(cust_class) %>% do(calculate_bill(., mnwdList$rate_structure))
