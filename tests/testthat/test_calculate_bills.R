library(RateParser)
library(yaml)
context("Calculate Bills")

#create test data
rows <- list()
rows[[1]] <- list("usage_ccf"=388,
                 "meter_size"='3"',
                 "cust_class"="COMMERCIAL",
                 "water_type"="POTABLE",
                 "et_amount"=0,
                 "irrigable_area"=0,
                 "hhsize"=0)
rows[[2]] <- list("usage_ccf"=27.3,
                 "meter_size"='5/8"',
                 "cust_class"="RESIDENTIAL_SINGLE",
                 "water_type"="POTABLE",
                 "et_amount"=4.8,
                 "irrigable_area"=1300,
                 "hhsize"=3)
rows[[3]] <- list("usage_ccf"=41,
                 "meter_size"='1"',
                 "cust_class"="IRRIGATION",
                 "water_type"="RECYCLED",
                 "et_amount"=4.8,
                 "irrigable_area"=4500,
                 "hhsize"=0)
rows[[4]] <- list("usage_ccf"=41,
                  "meter_size"='1"',
                  "cust_class"="RESIDENTIAL_MULTI",
                  "water_type"="RECYCLED",
                  "et_amount"=4.8,
                  "irrigable_area"=4500,
                  "hhsize"=0)
df_test <- do.call(rbind.data.frame, rows[1:3])

yaml_rates <- '
metadata:
  effective_date: 2016-03-01
  utility_name: "City of Santa Monica"
  bill_frequency: bimonthly
rate_structure:
  RESIDENTIAL_SINGLE:
    - service_charge:
        depends_on: meter_size
        values:
          5/8": 11
          1": 22
          3": 33
    - tier_starts:
      - 0
      - 15
      - 21
      - 26
    - tier_prices:
      - 2.87
      - 4.29
      - 6.44
      - 10.07
    - commodity_charge: Tiered
    - bill: commodity_charge + service_charge
  IRRIGATION:
    - service_charge:
        depends_on: meter_size
        values:
          5/8": 11
          1": 22
          3": 33
    - et_factor: 0.7
    - outdoor: et_factor * irrigable_area * et_amount * 0.62 * (1/748)
    - budget: outdoor
    - tier_starts:
      - 0
      - outdoor
    - tier_prices:
        depends_on: water_type
        values:
          POTABLE:
            - 4.07
            - 10.03
          RECYCLED:
            - 3.66
            - 6.33
    - commodity_charge: Budget
    - bill: commodity_charge + service_charge
  COMMERCIAL:
    - service_charge:
        depends_on: meter_size
        values:
          5/8": 11
          1": 22
          3": 33
    - tier_starts:
        depends_on: meter_size
        values:
          5/8":
            - 0
            - 211
          1":
            - 0
            - 211
          3":
            - 0
            - 611
    - tier_prices:
        depends_on: water_type
        values:
          POTABLE:
            - 4.07
            - 10.03
          RECYCLED:
            - 3.66
            - 6.33
    - commodity_charge: Tiered
    - bill: commodity_charge + service_charge
'
test_rates <- yaml.load(yaml_rates)


calc <- function(df){
  calculate_class_bill(df, test_rates)
}

manual_bill_1 <- 33 + 4.07*388
manual_bill_2 <- 11 + 2.87*14 + 4.29*6 + 6.44*5 + 10.07*2.3

manual_budget_3 <- 0.7*4.8*4500*0.62*(1/748)
manual_bill_3 <- 22 + 3.66*floor(manual_budget_3) + 6.33*(41 - floor(manual_budget_3) )

manual_bills <- c(manual_bill_1, manual_bill_2, manual_bill_3)


test_that("Individual bills calculated accurately", {
 expect_equal(calc(as.data.frame(rows[[1]]))$bill, manual_bill_1)
 expect_equal(calc(as.data.frame(rows[[2]]))$bill, manual_bill_2)
 expect_equal(calc(as.data.frame(rows[[3]]))$bill, manual_bill_3)
})

test_that("Error thrown when a class is not defined in rate file", {
  expect_error(calc(as.data.frame(rows[[4]])), "No rate information for customer class")
})

test_that("Bills accurate when summed accross customer classes", {
  expect_equal( sum(calculate_bill(df_test, test_rates)$bill), sum(manual_bills) )
})



