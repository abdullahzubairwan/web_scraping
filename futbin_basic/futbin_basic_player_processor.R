# Title: FUTBIN22 Player Basic Table Processor
# Version: 1.0
# Author: Zubs
# Date: 28 September 2022
# Edit: -

# 1.0 Libraries
library(dplyr)
library(tidyr)
library(readr)
library(stringr)
# 2.0 Read Data
futbin_22_data <- read_csv("raw_data/futbin_22_raw_2.csv")
futbin_22_data = futbin_22_data[,-c(1,20)]
# 3.0 Process Data

# 3.1 Rename Columns
# Option 1: Use Column Names as provided
# a3 = read_html(path) %>% html_nodes("th") %>% html_text()
# a3[15:16] = c("HEI","POP")

# Option 2: Use own Column Names
new_col = c("name","rating","position","version","current_price",
            "skill_rating","weakfoot_rating","work_rate","pace","shooting","passing","dribbling","defending",
            "physicality","height","order_by_popularity","order_by_basestat","order_by_ingame_stats","club","nation","league")

colnames(futbin_22_data) = new_col

# 3.2 Fix Data
# Slight Processing to separate columns with more than 1 data point
## Column height have heights in cm and inches, body type and weight

height_df = data.frame(str_split_fixed(futbin_22_data$height," ",4))
height_df = height_df %>% mutate(height_cm = gsub("cm","",X1),height_in = trimws(X3),X4 = trimws(X4)) %>% 
  tidyr::separate(.,X4,into = c("body_type","weight_kg"),sep = "\\(") %>% 
  mutate(body_type = trimws(body_type),weight_kg = gsub("kg\\)","",weight_kg)) %>% 
  select(height_cm,height_in,body_type,weight_kg)

futbin_22_new = data.frame(do.call(cbind, list(futbin_22_data[1:14],height_df,futbin_22_data[16:21])))
rm(height_df)
# A warning message came up; which means that there are missing data.
# Need to fix this so that data didn't end up in wrong column
# Warning message:
# Expected 2 pieces. Missing pieces filled with `NA` in 67 rows [812, 2890, 3370, 3710, 4684, 4688, 5305, 5306, 5341, 6889, 6903, 7790, 7791, 7802, 7803, 7804, 7866, 8794, 8795, 9878, ...]. 

# checker
which_na = which(is.na(futbin_22_new$weight_kg))
futbin_missing = futbin_22_new[which_na,]
# our assumption was correct. Shifting the data into the right column.
futbin_missing[,17:18] = futbin_missing[,15:16] 
futbin_missing[,15:16] = ""

# Recombine
futbin_notmissing = futbin_22_new[-which_na,]
futbin_22_new = bind_rows(futbin_notmissing,futbin_missing)
rm(futbin_notmissing,futbin_missing)
## Column workrate have Attacking Workrate and Defending Workrate
futbin_22_new = futbin_22_new %>% separate(.,work_rate,into = c("attacking_wr","defending_wr"),sep = "\\\\")

# Set suitable data classes to each column
col_to_integer = c("rating","skill_rating","weakfoot_rating", 
                   "pace","shooting","passing","dribbling","defending","physicality",
                   "height_cm","weight_kg","order_by_popularity","order_by_basestat","order_by_ingame_stats")
futbin_22_new[col_to_integer] <- lapply(futbin_22_new[col_to_integer], as.integer)
# Reorder Columns for better representation
futbin_22_new = futbin_22_data %>% relocate(c(club,nation,league), .after = rating)
# Save Data
readr::write_csv(futbin_22_new,"data/futbin22_basicplayer_data_2.csv")
