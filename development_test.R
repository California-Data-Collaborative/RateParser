library(dplyr)
library(yaml)

mnwdList <- read_owrs_file("../open-water-rate-specification/examples/mnwd.owrs")

# yaml_chars <- as.yaml(mnwdList)
# writeChar(yaml_chars, "out_test.owrs")

# load(file="data/santamonica.rda")

# df_mnwd <- read.csv("../RateComparison/data/mnwd_test.csv", stringsAsFactors = FALSE)
df_mnwd <- read.csv("../../../../Desktop/DELETE_ME/mnwd_full-201201-201607_v2.csv", stringsAsFactors = FALSE)

names(df_mnwd) <- c( "cust_id", "usage_month", "usage_year", "usage_date", "usage_ccf",
                     "ET", "hhsize", "irr_area", "cust_class", "rate_code")

names(df_mnwd) <- c("cust_id","cust_id_from_utility","location_id_from_utility" ,"cust_class" ,
  "rate_code","hhsize"   ,"cust_loc_has_variance"  ,"water_type",
"cust_loc_is_master_meter"   ,"irr_area"       ,"cust_loc_apn",
"cust_loc_street_num"        ,"cust_loc_street_name","cust_loc_city",
"cust_loc_state" ,"cust_loc_zip" , "cust_loc_info_start_date",
"cust_loc_info_end_date","cust_loc_is_current"  ,"utility_id" ,"utility_name" , "census_block_num",
"et_zone_from_utility", "cust_loc_county" , "cust_loc_latitude" ,"cust_loc_longitude",
"cust_loc_geocode_accuracy" , "cust_loc_geocode_date" ,"meter_size"  , "cust_loc_pressure_zone",
"cust_loc_full_address" ,"geom" ,"usage_year"   ,"usage_month"  , "usage_start_date",
"usage_end_date","usage_ccf"  , "usage_indoor_budget_ccf","usage_outdoor_budget_ccf", "et_amount")

# df_row <- tbl_df(santamonica)[2,]
# df_row <- tbl_df(santamonica) %>% filter(cust_class != "OTHER")


df_row <- tbl_df(df_mnwd) %>%
  filter(cust_class %in% c("RESIDENTIAL_MULTI") ) %>%
  group_by(cust_class)
# df_row$days_in_period <- 30.4
# df_row$meter_size <- '5/8"'
# df_row$water_type <- 'POTABLE'


# df_row$irr_area <- 1300
# df_row$hhsize <- 4
# df_row$ET <- 2.3


# df_calced <- calculate_bill(df_row, mnwdList$rate_structure)


df_calced <- calculate_bill(df_row, mnwdList))
