# load in required packages
library("dplyr")

# read in data
campaign_data = read.csv(file = "~/PSYC417-SP25/Data/campaign_contributions.csv", header = TRUE)

# create dataset of conservative leaning donors
leans_c <- campaign_data %>% 
  dplyr::filter(lean == "Leans Conservative") 

# print first 10 rows
leans_c %>%
  dplyr::slice_head(n = 10)

# create dataset of liberal leaning donors
leans_d <- campaign_data %>%
  dplyr::filter(lean == "Leans Liberal")

# print 10 random rows
leans_d %>%
  dplyr::slice_sample(n = 10)

# filter top 10 conservative leaning spenders and create two new variables
big_c <- leans_c %>%
  dplyr::slice_max(total, prop = .1) %>%
  dplyr::mutate(perc_dems = round(to_dems/total*100,3), perc_reps = round(to_repubs/total*100,3))

# filter top 10 liberal leaning spenders and create two new variables
big_d <- campaign_data %>%
  dplyr::filter(lean == "Leans Democrat") %>%
  dplyr::slice_max(total, prop = .1) %>%
  dplyr::mutate(perc_dems = round(to_dems/total*100,3), perc_reps = round(to_repubs/total*100,3))

# summarize by industry and lean by mean
new_dat = campaign_data %>%
  group_by(industry, lean) %>%
  summarize(total=mean(total))

### end of script