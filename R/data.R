#' Water Use Data for the City of Santa Monica
#'
#' A dataset containing metered water use data for customers of the
#' City of Santa Monica public works department. Data has been modified slightly
#' from its original format to drop extra columns and rename columns to simpler
#' descriptive names.
#'
#' @format A data frame with 218067 rows and 7 variables:
#' \describe{
#'   \item{cust_id}{customer identifier}
#'   \item{usage_ccf}{monthly water use, in hundred cubic feet (CCF)}
#'   \item{usage_month}{the month the customer was billed}
#'   \item{usage_year}{the year the customer was billed}
#'   \item{cust_class}{the general customer class (RESIDENTIAL_SINGLE, IRRIGATION, etc)}
#'   \item{usage_date}{a date created from as.Date(usage_year, usage_month, 1)}
#' }
#' @source \url{https://data.smgov.net/Public-Services/Water-Usage/4nnq-5vzx}
"santamonica"
