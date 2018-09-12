# Load Lab


library(magrittr)
library(lubridate)

invtype_key = read_csv("../data/invtype_key.csv")
################################################################################
##################### Lab Results Samples ######################################
samples <- read_csv("../data/biotrackthc_labresults_samples.csv",
                    col_types=cols(sample_amount_other="n", inventoryid="n",
                                   lab_license="n", other_sample_id="n"))

# Re-name columns
colnames(samples)[1] <- 'sample_id' # this one fixed the messed up import
samples %<>% rename(
  testee_orgid=orgid, 
  testee_loc=location, 
  test_strain=strain)

# Add columns
samples <- mutate(samples, test_time = as.POSIXct(sessiontime, origin='1970-01-01'),
                  test_year = year(test_time)) # sample_time
samples <- left_join(samples, invtype_key)

# Narrow dataset
samples <- select(samples,
                  inventoryparentid, # Link to Dispensing/Inventory
                  sample_id, # Link to Potency Results
                  lab_license, # identifier for lab
                  testee_loc, # locationID of submitting licensee
                  inventorytype, 
                  invtype,
                  test_strain, # strain name from submitting licensee
                  test_time, # Time of Test
                  test_year,
                  sample_amount_used) # this is the size of the sample. could be useful if we want to think about sampling error

# Remove values without an inventoryparentID (impossible to link; useless)
samples <- subset(samples, !is.na(inventoryparentid))

# Remove duplicate tests
# Now, for each inventoryparentID, take only the one with the maximum test ID...
# get row number of all rows who have max sample_id for their inventoryparentid ...
# This is following Valinda's methodology. It means that we will use the latest test.
sample_ids_to_keep <- with(samples, tapply(X = sample_id, INDEX = inventoryparentid, FUN = max))
samples <- samples[samples$sample_id %in% sample_ids_to_keep, ] # this drops 10k extra rows.
rm(sample_ids_to_keep)

################################################################################
################## Lab Results Potency Analysis ################################
# Load.
labresults_potency_analysis <- readr::read_csv("../data/biotrackthc_labresults_potency_analysis.csv")
labresults_potency_analysis %<>% select(-id, -failure, -lab_provided, -units)
# Re-shape; remove duplicates.
tests <- c('THC', "THCA", "CBD", "CBDA", "Total")
test_list <- list()
for( i in 1:length(tests)) {
  test_list[[i]] <- labresults_potency_analysis[labresults_potency_analysis$name==tests[i],]
  test_list[[i]] <- test_list[[i]][!duplicated(test_list[[i]]$sample_id), ]
  test_list[[i]][tests[i]] <- test_list[[i]]$value
  test_list[[i]] <- test_list[[i]] %>% select(-name, -value)
  if(i!=1) test_list[[i]] <- test_list[[i]] %>% select(-orgid, -location)
  names(test_list)[i] <- tests[i]
}
labresults_potency_analysis <- full_join(test_list[[1]], test_list[[2]])
labresults_potency_analysis <- full_join(labresults_potency_analysis, test_list[[3]])
labresults_potency_analysis <- full_join(labresults_potency_analysis, test_list[[4]])
labresults_potency_analysis <- full_join(labresults_potency_analysis, test_list[[5]])
rm(test_list); rm(tests); rm(i)
# Merge (note that since we already removed duplicate tests and NA for inventoryparentID, we can safely left-merge).
samples_potency <- left_join(samples,
                             labresults_potency_analysis %>% select(-location, -orgid))
mean(is.na(samples_potency$inventoryparentid))



################################################################################
############################# Save merge dataset ##################################
# Save the data for merging to other tables
# write_csv(samples_potency, "labresults.csv")

################################################################################
######################### Export mini analysis dataset #############################
# Do other operations for final analysis:
samples_potency <- samples_potency %>% 
  mutate(test_month=month(test_time), test_mday=mday(test_time), test_yday=yday(test_time))
# Add lab info
samples_potency <- left_join(samples_potency, location_key_lab %>% select(location, name, licensenum) %>% rename(lab=name),
                             by=c("lab_license"="licensenum"))
# Add testee info
samples_potency <- left_join(samples_potency, location_key %>% select(location, name, locationtype), 
                             by=c("testee_loc"="location"))

write.csv(samples_potency, "../data/potencytests.csv")
rm(samples_potency); rm(labresults_potency_analysis)

