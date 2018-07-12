#functions
invtypekey <- read_csv('../data/invtype_legend.csv')

# function: get invtype from inventorytype
getInvtype <- function(inventorytype) {
  plyr:: mapvalues(
    x = inventorytype,
    from = invtypekey$inventorytype,
    to = invtypekey$invtype,
    warn_missing = FALSE)
}

# Make date column from a set of year-month columns
make_date <- function(yyyy, m, quarter=F) {
  # we need this for negative substring indexing
  require(stringr)
  # year is 4 digits; month is either 1 or 2 digits.
  # Correct month by prefixing 0, getting all but last digit
  month <- str_sub(paste0(0, m), -2) 
  
  as.Date(paste0(yyyy, "-", month, "-01"))
}