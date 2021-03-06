
#******************************************************************
# Calculate the variable portion of a bill
#******************************************************************
calculate_variable_bill <- function(data, rate_type, start_name="tier_starts",
                                    price_name="tier_prices", is_sewer=FALSE){
  if(is_sewer){budget_col <- "sewer_budget"}
  else{budget_col <- "budget"}

  tier_start_str <- data[[start_name]][1]
  tier_price_str <- data[[price_name]][1]

  #call correct bill calculator function
  if(rate_type == "Tiered"){
    stopif(grepl("%", tier_start_str),
           paste("Tiers are formatted for budget-based rates, but commodity_charge is set to 'Tiered'.",
                 "Please set 'commodity_charge: Budget' or change your tier_starts to billing units."))

    tier_starts <- parse_numerics(tier_start_str)
    tier_prices <- parse_numerics(tier_price_str)
    #check that prices are same length as tiers
    stopifnot(length(tier_starts)==length(tier_prices))
    bill_info <- calculate_tiered_charge(data, tier_starts, tier_prices, is_sewer=is_sewer)
  }
  else if(rate_type == "Budget"){
    tier_starts <- get_budget_tiers(data, parse_strings(tier_start_str), budget_col=budget_col)
    tier_prices <- parse_numerics(tier_price_str )
    #check that prices are same length as tiers
    stopifnot(ncol(tier_starts)==length(tier_prices))
    bill_info <- calculate_tiered_charge(data, tier_starts, tier_prices, budget_based=TRUE, is_sewer=is_sewer)
  }

  return(bill_info)
}


#******************************************************************
# Calculate a tiered usage charge
#******************************************************************
calculate_tiered_charge <- function(data, tier_starts, tier_prices, budget_based=FALSE, is_sewer=FALSE){
  if(is_sewer){suffix <- "_sewer"}
  else{suffix <- ""}

  usage_in_tiers <- get_usage_in_tiers(data, tier_starts, budget_based=budget_based)
  revenue_in_tiers <- t(tier_prices*t(usage_in_tiers))

  #change name of usage columns to X#{suffix}
  colnames(usage_in_tiers) <- c( paste("X", 1:ncol(usage_in_tiers), suffix, sep="") )
  #change name of revenue columns to XR#{suffix}
  colnames(revenue_in_tiers) <- c( paste("XR", 1:ncol(revenue_in_tiers), suffix, sep="") )
  usage_in_tiers <- tbl_df(data.frame(usage_in_tiers, revenue_in_tiers, variable_bill=usage_in_tiers%*%tier_prices))
  return(usage_in_tiers)
}

#******************************************************************
# Get the amount of water usage in each tier.
#
# Assumes a 1-d vector of tier starts for a normal IBR, and a
# matrix for budget-based rates (tier start depends on budget)
#******************************************************************
get_usage_in_tiers <- function(data, tier_starts, budget_based=FALSE){
  # tier_stars is a matrix if budget, else is a vector
  if(budget_based){
    num_tiers <- ncol(tier_starts)
  }
  else{
    num_tiers <- length(tier_starts)
  }

  # Assumes tier structure starts at 0
  usage_in_tiers <- matrix(0, nrow(data), num_tiers)

  for(i in 1:(num_tiers-1) ){
    # tier_stars is a matrix if budget, else is a vector
    if(budget_based){
      t <- tier_starts[,i+1] + 1
    }
    else{
      t <- tier_starts[i+1]
    }

    if(i==1){
      # Usage in first tier
      usage_in_tiers[,i] <- pmax(pmin(data$usage_ccf, t-1), 0)
    }
    else{
      # Usage in middle tiers
      lower_tier_use <- rowSums(usage_in_tiers)
      usage_in_tiers[,i] <- pmax(pmin(data$usage_ccf-lower_tier_use, t-lower_tier_use-1), 0)
    }
  }
  # Usage in final tier
  lower_tier_use <- rowSums(usage_in_tiers)
  usage_in_tiers[,num_tiers] <- pmax(data$usage_ccf-lower_tier_use, 0)

  return(usage_in_tiers)
}

#******************************************************************
# Split text box input into a vector of strings
#******************************************************************
parse_strings <- function(str){
  return( unlist(strsplit(str, "[\n| ]+")) )
}

#******************************************************************
# Convert vector of numeric strings into actual numerics
#******************************************************************
parse_numerics <- function(str){
  return( suppressWarnings(as.numeric(parse_strings(str))) )
}


#******************************************************************
# Calculate the the tier starting values (in CCF) for a budget
# based rate.
#
# Uses indoor and outoor tiers in CCF, along with a vector of
# strings marking relative starting values like "Indoor" and "125%"
# and converts them to a matrix of CCF tier starting values for
# each customer.
#******************************************************************
get_budget_tiers <- function(data, tier_start_strs, budget_col){
  budget <- data[[budget_col]]
  budget_tiers <- matrix(0, nrow(data), length(tier_start_strs))

  for(i in 1:length(tier_start_strs)){
    t <- tier_start_strs[i]

    # if t is numeric
    if( !is.na(suppressWarnings(as.numeric(t))) ){
      budget_tiers[,i] <- suppressWarnings(as.numeric(t))
    }
    else if(tolower(t) == "indoor"){
      budget_tiers[,i] <- round(data$indoor)
    }
    else if(tolower(t) == "outdoor"){
      budget_tiers[,i] <- round(data$outdoor)
    }
    else if( grepl("%", t) ){
      percent <- as.numeric( gsub("[^0-9\\.]", "", t, "") )
      stopifnot(is.finite(percent))

      budget_tiers[,i] <- round((percent/100)*budget)
    }
  }

  return(budget_tiers)
}
