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

