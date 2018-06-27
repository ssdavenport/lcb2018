#

# Code to share with CMU team

# The functions below (one is a helper function to the other) search the productname field
# for text preceding mg/g. This a good but imperfect way to extract THC content from edibles.
# An improvement could be made by also searching for presence of "CBD" 
# and for nubmers around ":", which when present indicate THC:CBD ratio (or visa versa; check!).

# Single-Variable Functions -----------------------------------------------

# TO DO: modify so it will also find '5( )?g' immediately before space or end of line.
# Check for a pattern, find A) num_matches, B) high match, c) low match
search_productname <- function(productname, # search this column
                               invtype_col=NULL, # allows enforcement of mg_text_limit.
                               search_pattern = "mg", 
                               num_match_name = 'text_matches',
                               max_match_name = 'text_max',
                               min_match_name = 'text_min',
                               prefix_name = NA) {
  
  # search_pattern numbers before "MG"; not case sensitive, space optional
  pattern_mg <- "(?i)[0-9]+ ?(?=mg)"
  pattern_g <- "(?i)[0-9]?\\.?[0-9]+?(?=( )?g($| |ram(s)?))" # end with EOL or words g / gram / grams
  pattern_pk <- "(?i)[0-9]+ ?(?=pack|pk)"
  
  if(search_pattern == "mg") pattern <- pattern_mg
  if(search_pattern == "pk") pattern <- pattern_pk
  
  num_matches <- rep(NA, length(productname))
  max_match <- rep(NA, length(productname))
  min_match <- rep(NA, length(productname))
  
  # In this complex case, search for the milligrams and grams pattern.
  if(search_pattern == 'mg' | search_pattern =='g') {
    
    # To handle fractions, any "1/2 g" should be replaced with "0.5 g"
    productname <- str_replace_all(productname, "(?i)1/2 ?g", "0.5 g")
    
    regexp_matches_g <- vector('list', length(productname))
    regexp_matches_mg <- vector('list', length(productname))
    
    for (i in seq_along(productname)) {
      
      # pre-process productname to fix 0.Xmg errors: if a fraction before mg, replace mg with G
      # (nothing is ever sold in less than 1 mg)
      productname[i] <- str_replace_all(productname[i], "(?<=( 0?\\.[0-9]{1}[:space:]?))mg", 'g')
      
      regexp_matches_g[[i]] <- str_extract_all(productname[i], pattern_g) %>% unlist %>% as.numeric
      regexp_matches_mg[[i]] <- str_extract_all(productname[i], pattern_mg) %>% unlist %>% as.numeric
      
      # find the total number of matches across both terms
      num_matches[i] <- length(regexp_matches_g[[i]]) + length(regexp_matches_mg[[i]])
      
      # Find min and max, across both matches, accounting for multiplier.
      max_match[i] <- if (num_matches[i] >= 1) max(1000 * regexp_matches_g[[i]], regexp_matches_mg[[i]]) else NA
      min_match[i] <- if (num_matches[i] >= 2) min(1000 * regexp_matches_g[[i]], regexp_matches_mg[[i]]) else NA
    }
    
    
  } else {
    
    # If only one entry, then it fills to "max match"
    regexp_matches <- vector('list', length(productname))
    
    for (i in seq_along(productname)) {
      regexp_matches[[i]] <- str_extract_all(productname[i], pattern) %>% unlist %>% as.numeric
      num_matches[i] <- length(regexp_matches[[i]])
      max_match[i] <- if (num_matches[i] >= 1) max(regexp_matches[[i]]) else NA
      min_match[i] <- if (num_matches[i] >= 2) min(regexp_matches[[i]]) else NA
    }
  }
  
  # If invtype is provided, then also enforce the MG text limit.
  if(is.null(invtype_col)) {
    output <-  tibble(num_matches, 
                      max_match, 
                      min_match)
  } else if (!is.null(invtype_col)) {
    output <-  tibble(num_matches, 
                      enforce_mg_text_limit(max_match, invtype_col), 
                      enforce_mg_text_limit(min_match, invtype_col))
  }
  
  if(!is.na(prefix_name)) search_pattern <- prefix_name
  
  # Output, named correctly
  output %>% 
    `colnames<-`(paste0(search_pattern, "_", c(num_match_name, 
                                               max_match_name, 
                                               min_match_name)))
  
}



# Refrain from estimating mg_text that seems impossible, by product type.
enforce_mg_text_limit <- function(mg_text_col, invtype_col=invtype) {
  
  # Declare limits for each product type, based on regs / personal observation of data.
  # Only medical patients can access > 500mg THC packages
  # Rec EDIBLES limited to 100mg. (not sure about other derived products)
  
  mg_text_limits <- list(
    "Usable Marijuana" = 28500, # this is one ounce, the legal limit.
    "Marijuana Mix Package" = 28000, # 1 gram
    "Marijuana Extract for Inhalation" = 10000, # arbitrary; 6g was largest found.
    "Marijuana Mix Infused" = 28000, # lots of 2g blunts
    "Marijuana Infused Topicals" = 500, # data suggests max is around 250?
    "Liquid Marijuana Infused Edible" = 500, # 500mg limit for medical; 100 for rec; label is often THC + CBD
    "Solid Marijuana Infused Edible" = 500,  # 500mg limit for medical; 100 for rec; label is often THC + CBD
    "Tincture" = 100000, # non-binding; all seem to be 100mg.
    "Suppository" = 500, # arbitrary and non-binding
    "Capsule" = 500 # arbitrary and non-binding
  )
  
  for(i in seq_along(mg_text_limits)) {
    product_type <- names(mg_text_limits)[i]
    limit <- mg_text_limits[i]
    mg_text_col <- ifelse(invtype_col %in% product_type,
                          # if a match
                          ifelse(mg_text_col > limit, NA, mg_text_col),
                          # if not, leave it
                          mg_text_col)
  }
  
  mg_text_col
}

#from July 1, 2016 until June 30, 2017
x <- as.integer(as.POSIXct("2016-07-01 00:00:00 PST"))
y <- as.integer(as.POSIXct("2017-06-30 23:59:59 PST"))

as_datetime()

as.integer(as.POSIXct("2016-07-01 00:00:00 PST"))
