# Load library calls
library(tidyverse)
library(knitr)

# Functions ---------------------------------------------------------------

# Smoothing function for the potency dataset (THC/CBD%)

# Wrapper function to amke this work. oy!
smooth_h_wrapper <- function(data_orig, h) {
  
  out_dfs <- list()
  invtypes <- unique(data_orig$invtype)
  
  # separate data by invtype and loop thourgh
  for(i in 1:length(invtypes)) {
    
    # Filter to inventory type
    invtype <- invtypes[i]
    data_invtype <- data_orig[data_orig$invtype == invtype, ]
    
    # Proceed only if there are actually rows in that dataframe!
    
    if(nrow(data_invtype) != 0) {
      # Calculate year-month totals
      grouped_invtype <- data_invtype %>% 
        group_by(invtype, year, month, n, inventorytype) %>% 
        summarise(TotalTHC = mean(TotalTHC, na.rm = T),
                  TotalCBD = mean(TotalCBD, na.rm=T)) %>%
        # make sure rows are ordered by time within invtype before calling smoot_h()
        ungroup %>% arrange(year, month) %>% group_by(invtype) %>% mutate(t=1:n()) %>% arrange(t)
      
      # Apply smoothing function, output to new DF
      grouped_invtype$TotalTHCs <- smooth_h(grouped_invtype, 'TotalTHC', h)
      grouped_invtype$TotalCBDs <- smooth_h(grouped_invtype, 'TotalCBD', h)
      out_dfs[[i]] <- grouped_invtype
    }
  }
  bind_rows(out_dfs)
}

# Apply function to smooth; takes dataframe and outputs a column
smooth_h <- function(data, x_col='TotalTHC', h = 3, t_col = 't') {
  
  # loop through rows in dataframe, 
  smoothed <- c()
  t_max <- max(data[[t_col]], na.rm = T)
  
  for (i in 1:nrow(data)) {
    t <- data[i, ][[t_col]]
    t_before <- min(h, t-1, na.rm = T)
    t_after <- max(h, t_max-t, na.rm = T)
    t_smooth <- min(t_before, t_after, na.rm = T) # number of periods to smooth pre/post
    
    if (t_smooth == 0) {
      out <- data[i, x_col]
    } else {
      out <- mean(data[ (i-t_smooth) : (i+t_smooth), ][[x_col]], na.rm=T)
    }
    smoothed <- c(smoothed, out)
  }
  names(smoothed) <- NULL
  unlist(smoothed)
}



library(tidyverse)
invtypekey <- read_csv('../data/invtype_legend.csv')

# function: get invtype from inventorytype
getInvtype <- function(inventorytype) {
  plyr:: mapvalues(
    x = inventorytype,
    from = invtypekey$inventorytype,
    to = invtypekey$invtype,
    warn_missing = FALSE)
}

# get endtype from inventorytype
getEndtype <- function(inventorytype) {
  plyr:: mapvalues(
    x = inventorytype,
    from = invtypekey$inventorytype,
    to = invtypekey$endtype,
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

# Coding variables --------------------------------------------------------

# usable weight
has_usableweight <- c("Usable Marijuana",
                      "Marijuana Extract for Inhalation",
                      "Marijuana Mix Infused",
                      "Marijuana Mix Package",
                      "Hash",
                      "Kief"# might want to check out
                      #"Marijuana Infused Topicals" # would need further inspection
)

# Look only at Ingredient Types
ingrdnts <- c("Hydrocarbon Wax", "Bubble Hash", "C02 Hash Oil", "Marijuana Mix", "Food Grade Solvent Extract", 
              "Infused Dairy Butter or Fat in Solid Form", "Infused Cooking Oil",
              "Hash", "Kief")

retailOther <- c("Solid Marijuana Infused Edible",
                 "Liquid Marijuana Infused Edible",
                 "Marijuana Extract for Inhalation",
                 "Marijuana Mix Infused",
                 "Capsule",
                 "Marijuana Infused Topicals",
                 "Tincture",
                 "Suppository")

retailFlower <- c("Usable Marijuana", "Marijuana Mix Package")

plants <- c("Seed", "Plant Tissue", "Mature Plant", "Clone", "Wet Flower")

lots <- c("Flower Lot", "Other Plant Material Lot")

options(scipen=12); options(knitr.kable.NA = '-') # leave NA as dashed
align_pattern <- "lcccccccccc" # align 1st col left, others center.

# Load Potency data ------------------------------------------------------------

# Load potency test data scores (raw)
potency <- read_csv("../data/potencytests.csv") %>%
  rename(year='test_year',
         month='test_month') %>% select(-X1)

# Clean the potency data for outliers
potency <- potency %>% 
  # If any value is > 100, divide it by 100 (assume decimals are off)
  mutate(THC = THC %>% ifelse(.>100, ./100, .) %>% ifelse(.>100, . / 100, .),
         THCA = THCA %>% ifelse(.>100, ./100, .) %>% ifelse(.>100, . / 100, .),
         CBD = CBD %>% ifelse(.>100, ./100, .) %>% ifelse(.>100, . / 100, .),
         CBDA = CBDA %>% ifelse(.>100, ./100, .) %>% ifelse(.>100, . / 100, .),
         TotalTHC = THC + 0.877 * ifelse(!is.na(THCA), THCA, 0),
         TotalCBD = CBD + 0.877 * ifelse(!is.na(CBDA), CBDA, 0))

# Aggregate monthly
potencyMonthly <- potency %>%
  mutate(invtype = factor(invtype)) %>%
  group_by(invtype, year, month, inventorytype) %>%
  summarise(TotalTHC = mean(TotalTHC, na.rm=T),
            TotalCBD = mean(TotalCBD, na.rm=T),
            n=n())


# Smooth the potency test by inventorytype
potencyMonthly <- smooth_h_wrapper(potencyMonthly, 5) %>% 
  filter(!is.na(TotalTHCs)) %>%
  mutate(date=make_date(year, month)) %>%
  ungroup 


# annual is an aggregated of the smoothed (not perfect but we shouldn't really use it any way)
potencyAnnual <- potencyMonthly %>%
  group_by(invtype, year, inventorytype) %>%
  summarise(TotalTHC = mean(TotalTHC, na.rm=T),
            TotalCBD = mean(TotalCBD, na.rm=T)) %>%
  ungroup 

# Export file for use in estimating THC content of products!
potencyAnnual %>% write_csv("../data/thccbdannual.csv")



# Load Retail Data --------------------------------------------------------


# Get spending per year by inventory tpe
# GoogleQuery: RetailUSDxType
# SELECT invtype,
#   YEAR(sale_time) as YEAR,
#   MONTH(sale_time) as MONTH,
#   -- number of rows (item-transactions)
#   count(*) as itemtransactions,
#   sum(price_x) as price_x,
#   -- TODO: After remaking retail view, replace with proper "TotalTHC"
#   -- TODO: Replace with weighted average, w wts varied by inventorytype
#   avg(thc + thca*0.877) as totalTHC,
#   -- TODO: Replace this with a cleaned version of totalCBD? (
#   avg(case when cbda is null then cbd
#        when cbda is not null then cbd + cbda * 0.877
#        end) as totalCBD,
#         --avg(cbd + (if is not null cbda, cbda *0.877, 0)) as totalCBD
#    sum(case when usableweight is not null then usableweight
#              else 0 end) as usableweight
# FROM [full_WA_data__proper.Retail_ALL]
# GROUP BY invtype, YEAR, MONTH
# ORDER BY invtype, YEAR,MONTH
retailUSDxType <- read_csv("../gqoutput/retailusd.csv") %>%
  mutate(date=make_date(YEAR, MONTH)) %>%
  # Convert to $1000s
  mutate(price_x = price_x / 1e3) %>%
  filter(
    # remove NA
    !is.na(invtype),
    # remove non-retail products in here for no reason
    !invtype%in%c("Clone", "Mature Plant"),
    # Remove the partial month of October 2017
    date <= "2017-10-01")


# Load Transfers Data -----------------------------------------------------


# Get Transfer Volumes
# Get Ingredient weights.
# -- Query usable weight of eligible transfers
# SELECT inventorytype,
#    sum(CAST(USABLEWEIGHT as float)) * SUM(CAST(weight as float)) as usableweight,
#    sum(CAST(USABLEWEIGHT as float)) as usableweightraw
# FROM [full_WA_data.biotrackthc_inventorytransfers]
# WHERE USABLEWEIGHT is not null and CAST(inventorytype as integer) <=40
# GROUP BY inventorytype
# ORDER BY inventorytype
# transferUsableWeight <- read_csv("../gqoutput/results-20180914-110802.csv") # uw = uw * w
transferUsableWeight <- read_csv("../gqoutput/results-20180912-182538.csv") # Version w simple sum usableweight


# -- Query Sale Price of All Transfers
# SELECT inventorytype,
#    count(*) as nTransfers,
#    sum(CAST(saleprice as integer)) as saleprice
# FROM [full_WA_data.biotrackthc_inventorytransfers]
# WHERE saleprice is not null and CAST(inventorytype as integer) <=40
# GROUP BY inventorytype
# ORDER BY inventorytype
transferVolumes <- read_csv("../gqoutput/results-20180912-181851.csv")

transfers <- left_join(transferVolumes, transferUsableWeight) %>%
  mutate(invtype = getInvtype(inventorytype)) %>%
  select(invtype, nTransfers, saleprice, usableweight) %>%
  mutate(pricePer = saleprice / nTransfers) 

# Inventory Transfers
# Import inventorytransfers with 10 time points cut by ID
# -- getinventorytransfersYrMo -- ordered version
# SELECT sum(weight) as units,
#        sum(usableweight)/1000 as kg,
#        sum(saleprice)/1000 as kUSD,
#        FLOOR(rowN / 255250) as t,
#        inventorytype
#        from(
#        -- Inner Query adds row numbers by ID, cleans columns for summing
#           SELECT CAST(inventorytype as integer) as inventorytype,
#                 ROW_NUMBER() OVER(ORDER BY ID ASC) rowN,
#                 Description, location, inventoryid,
#                 case when weight is null then 0 else cast(weight as float) end as weight,
#                 case when usableweight is null then 0 else cast(usableweight as float) end as usableweight,
#                 case when saleprice is null then 0 else cast(saleprice as float) end as saleprice
#           FROM [full_WA_data__proper.biotrackthc_inventorytransfers_ordered])
# where inventorytype <= 40
# GROUP BY t, inventorytype
# order by t
# this is ordered correctly now

# Aggregate monthly
transfersYrMo <- read.csv("../gqoutput/invTransfersYearMonth.csv") %>% 
  mutate(inventorytype = getInvtype(inventorytype),
         pricePerG = kUSD/kg,
         unitsK=units/1e3,
         perPerUnit = kUSD/unitsK,
         date=make_date(year, month)) %>%
  rename(kgusable='kg')

# Aggregate annually
transfersYr <- transfersYrMo %>%
  group_by(year, inventorytype) %>%
  summarise(units = sum(units),
            kgusable = sum(kgusable),
            kUSD = sum(kUSD)) %>%
  mutate(pricePerG = kUSD/kgusable,
         unitsK=units/1e3,
         perPerUnit = kUSD/unitsK)


# Load Conversions (Parent-Child Pair) ---------------------------------------------------

# Get inventory conversions (dated)
# -- QUERY
# Run in legacy SQL
# convDetailedT
# SELECT inventorytype, inventorytype_parent, location,
#     count(*) as nConversions,
#     sum(CAST(parent_difference as float)) as parentwt,
#     sum(CAST(childweight as float)) as childwt,
#     sum(CAST(child_usableweight as float)) as childusablewt
# 
# FROM [rand-systems:full_WA_data__proper.biotrackthc_inventoryconversions_dated] 
# GROUP BY inventorytype, inventorytype_parent, location, year, month
convDetailedT <- read_csv("../gqoutput/convDetailedT.csv") %>%
  mutate(parent = getInvtype(inventorytype_parent),
         child = getInvtype(inventorytype))  %>%
  select(-inventorytype_parent, -inventorytype) %>%
  arrange(-nConversions) # TODO: Add licensee name, date?

# Make a non-timed version
convDetailed <- convDetailedT %>%
  group_by(location, parent, child) %>%
  summarise(nConversions = sum(nConversions),
            parentwt = sum(parentwt),
            childwt = sum(childwt),
            childusablewt = sum(childusablewt))
write.csv(convDetailed, "../output/conversionsDetailed.csv")


# Get data on conversion paths (e.g. for prevalence)
convPaths <- read_csv("../gqoutput/results-20180630-124915.csv") %>%
  mutate(parent = getInvtype(inventorytype_parent),
         child = getInvtype(inventorytype))  %>%
  select(parent, child, nConversions) %>%
  arrange(-nConversions)

# -- Inventory Conversion parent-child
# SELECT inventorytype, inventorytype_parent, count(*) as nConversions
# FROM [full_WA_data.biotrackthc_inventoryconversions]
# GROUP BY inventorytype, inventorytype_parent


# Load conversion-parent table
# -- QUERY
# SELECT inventorytype_parent,
#   count(*) as nConversions,
#   sum(CAST(parent_difference as integer)) as parentwt
# FROM [full_WA_data.biotrackthc_inventoryconversions]
# GROUP BY inventorytype_parent
convParent <- read_csv("../gqoutput/results-20180630-130516.csv") %>%
  mutate(inventorytype_parent = getInvtype(inventorytype_parent)) %>%
  mutate(wtPerConv = parentwt/nConversions) %>%
  arrange(-parentwt)


# Load Conversions (Child View) ----------------------------------------


# Get Ingredient weights.
# -- Outer query calculates grouped sums
# SELECT year, month, location, inventorytype, 
#     sum(childwt) as childwt, 
#     sum(childusablewt) as childusablewt,
#     count(*) as nConversions
#     -- Middle query topcodes usableweight based on invtype-specific uw:w ratio
#     FROM( SELECT year, month, location, childwt, inventorytype,
#             case when ratio_dirty > (28.5) AND inventorytype=="28" then childwt * (28.5) -- usable MJ, enforce 1 ounce
#                  when ratio_dirty > (28.5)*16*10 AND inventorytype=="30" then childwt * (28.5)*16*10  -- MJ mix: 10 pounds
#                  when ratio_dirty > (28.5)*16*2 AND inventorytype IN("5", "16") then childwt * (28.5)*16*2  --  Hash/Kief: 2 pounds
#                  when ratio_dirty > (28.5)*16*4 AND inventorytype=="18" then childwt * (28.5)*16*4  --  Co2Hashoil: 4 pounds
#                  ELSE childusablewt_dirty END AS childusablewt
#           -- Inner query fixes types and top-codes weight.
#           from( SELECT 
#                   -- top code childwt at 5000 in general (spike for mjmix; biggest value in data for OPM; maybe lower for others)
#                   -- Note: this is not sufficient to clean the MJ Mix spikes
#                   case when CAST(childweight as float) > 7000 then 7000 ELSE CAST(childweight as float) END AS childwt,
#                   CAST(child_usableweight as float) as childusablewt_dirty,
#                   CAST(child_usableweight as float) / CAST(childweight as float) AS ratio_dirty,
#                   *
#                 FROM [rand-systems:full_WA_data__proper.biotrackthc_inventoryconversions_dated]
#                 /* below to lines are useful for debugging 
#                 WHERE year == 2017 AND month IN (1,2) and cast(location as integer) == 1273
#                 order by ratio_dirty desc 
#                 */
#           )
#       )
# group by year, month, location, inventorytype

convChildT <- read.csv("../gqoutput/convChildTclean.csv") %>%
  mutate(inventorytype = getInvtype(inventorytype),
         uwper = childusablewt / childwt) # for validatinf


# get non-dated version
convChild <- convChildT %>%
  group_by(inventorytype) %>%
  summarise(nConversions = sum(nConversions),
            childwt = sum(childwt),
            childusablewt = sum(childusablewt),
            wtPer = childwt/nConversions,
            usablewtPer = childusablewt/nConversions
  ) %>%  arrange(-childwt)

# Aggregate
convChildTYrMo <- convChildT %>%
  group_by(year, month, inventorytype) %>%
  summarise(nConversions = sum(nConversions),
            childwt = sum(childwt),
            childusablewt = sum(childusablewt)) %>%
  mutate(date=make_date(year, month)) %>%
  filter( nConversions>1,
          (year!= 2017 & month !=11))


