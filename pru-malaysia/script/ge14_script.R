# Title: GE14 Scraper and Cleaner
# Version: 1.0
# Author: Zubs
# Date: 9 November 2022
# Edit: -

# Libraries
library(rvest)
library(dplyr)
library(purrr)
library(tidyr)
library(stringr)

# Get Wiki Data
pru_2018_url = "https://en.wikipedia.org/wiki/Results_of_the_2018_Malaysian_general_election_by_parliamentary_constituency"

a = read_html(pru_2018_url) %>% html_table()
a = a[3:18]

coln = c("parliament","constituency","winner","votes_winner","majority","opponent","votes_opponent","incumbent","incumbent_majority")
b = lapply(a,setNames,coln)
b = purrr::map(b,~dplyr::select(.x,1:9))

# 1.0 Start with filtering out empty columns
b = purrr::map(b,~dplyr::slice(.x,-1))
b = purrr::map(b,~mutate_all(.x,as.character))
pru_2018_result = do.call(bind_rows,b)
rm(a)

# 2.0 Separating parliament to add State metadata
pru_2018_result = pru_2018_result %>% mutate(parliament_number = as.integer(gsub("P","",parliament)),
                                             parliament = case_when(
                                               nchar(parliament_number) == 1 ~ paste0("P00",parliament_number),
                                               nchar(parliament_number) == 2 ~ paste0("P0",parliament_number),
                                               nchar(parliament_number) == 3 ~ paste0("P",parliament_number)
                                             ),
                                             state = case_when(
                                               parliament_number %in% c(1:3) ~ "Perlis",
                                               parliament_number %in% c(4:18) ~ "Kedah",
                                               parliament_number %in% c(19:32) ~ "Kelantan",
                                               parliament_number %in% c(33:40) ~ "Terengganu",
                                               parliament_number %in% c(41:53) ~ "Penang",
                                               parliament_number %in% c(54:77) ~ "Perak",
                                               parliament_number %in% c(78:91) ~ "Pahang",
                                               parliament_number %in% c(92:113) ~ "Selangor",
                                               parliament_number %in% c(114:124) ~ "Kuala Lumpur",
                                               parliament_number == 125 ~ "Putrajaya",
                                               parliament_number %in% c(126:133) ~ "Negeri Sembilan",
                                               parliament_number %in% c(134:139) ~ "Melaka",
                                               parliament_number %in% c(140:165) ~ "Johor",
                                               parliament_number == 166 ~ "Labuan",
                                               parliament_number %in% c(167:191) ~ "Sabah",
                                               parliament_number %in% c(192:222) ~ "Sarawak"
                                             ))
# 3.0 Converting vote numbers into integer by removing comma
coln_remove_comma = c("votes_winner","votes_opponent","majority","incumbent_majority")
pru_2018_result = pru_2018_result %>% mutate(across(.cols = tidyr::all_of(coln_remove_comma),gsub,pattern =",",replacement = ""),
                                             across(.cols = tidyr::all_of(coln_remove_comma),as.integer))

# 5.0 Separating Coalition and Party from Name
pru_2018_result = pru_2018_result %>% 
  separate(winner, into = c("winner","winner_coalition"),sep = "\\(") %>% 
  separate(winner_coalition, into = c("winner_coalition","winner_party"), sep = "-") %>% 
  separate(opponent,into = c("opponent","opponent_coalition"),sep = "\\(") %>% 
  separate(opponent_coalition, into = c("opponent_coalition","opponent_party"), sep = "-") %>% 
  separate(incumbent, into = c("incumbent","incumbent_coalition"),sep = "\\(") %>% 
  separate(incumbent_coalition, into = c("incumbent_coalition","incumbent_party"), sep = "-") 

# 6.0 Recorrecting Coalition and Party for those with no Coalition
for(i in 1:nrow(pru_2018_result)){
  if(is.na(pru_2018_result$winner_party[i])){
    pru_2018_result$winner_party[i] = pru_2018_result$winner_coalition[i]
    pru_2018_result$winner_coalition[i] = NA_character_
  }
  
  if(is.na(pru_2018_result$opponent_party[i])){
    pru_2018_result$opponent_party[i] = pru_2018_result$opponent_coalition[i]
    pru_2018_result$opponent_coalition[i] = NA_character_
  }
  
  if(is.na(pru_2018_result$incumbent_party[i])){
    pru_2018_result$incumbent_party[i] = pru_2018_result$incumbent_coalition[i]
    pru_2018_result$incumbent_coalition[i] = NA_character_
  }
}

# 7.0 Cleaning symbols in Winner/Opponent Party

pru_2018_result$winner_party = str_squish(gsub(")","",pru_2018_result$winner_party))
pru_2018_result$opponent_party = str_squish(gsub(")","",pru_2018_result$opponent_party))
pru_2018_result$incumbent_party = str_squish(gsub(")","",pru_2018_result$incumbent_party))

# 8.0
# Standardizing coalition for winner and opponents.
# Adding metadata whether the Parliament is part of government or opposition. 
pru_2018_result = pru_2018_result %>% mutate(winner_coalition = ifelse(winner_party == "WARISAN","PH",winner_coalition),
                                             opponent_coalition = ifelse(opponent_party == "WARISAN","PH",opponent_coalition),
                                             opponent_coalition = ifelse(opponent_party == "STAR","USA",opponent_coalition)) %>% 
  mutate(parliament_status = ifelse(winner_coalition == "PH","Government","Opposition"))

# 9.0 Adding metadata whether the incumbent is contesting or not
pru_2018_result = pru_2018_result %>% group_by(parliament) %>% 
  mutate(incumbent_contesting = ifelse(incumbent %in% c(winner,opponent),"Yes","No"))

# 10.0 Extracting new constituency name and also recording previous name

pru_2018_change_name = pru_2018_result[grep("\\(",pru_2018_result$constituency),]
pru_2018_name_ok = pru_2018_result[!grepl("\\(",pru_2018_result$constituency),]

new_name = vector()
old_name = vector()

for(i in 1:nrow(pru_2018_change_name)){
  # Although there are repeat, we just create an easy code to do a simple process
  cons_name = pru_2018_change_name$constituency[i]
  # New Name
  new_name_locator = str_locate_all(cons_name,"\\(")[[1]][1]
  new_name[i] = str_squish(str_sub(cons_name,end = (new_name_locator-1)))
  # Old Name
  old_name_locator = str_locate_all(cons_name,"as ")[[1]][2]
  other_locator = str_locate_all(cons_name,"\\)")[[1]][1]
  old_name[i] = str_squish(str_sub(cons_name,start =(old_name_locator+1), end = (other_locator-1)))
}
pru_2018_change_name$constituency = new_name
pru_2018_change_name$constituency_old_name = old_name

pru_2018_result = bind_rows(pru_2018_name_ok,pru_2018_change_name) %>% arrange(parliament_number)
rm(coln,coln_remove_comma,cons_name,i,new_name,new_name_locator,old_name,old_name_locator,
   other_locator,pru_2018_url,pru_2018_change_name,pru_2018_name_ok,b)

# Get Supporting Data (Due to Wiki Data not having similar metadata as previous GE)
## Checked codes are for reference
# ext_data_pru_18 = RCurl::getURL("https://raw.githubusercontent.com/Thevesh/analysis-election-msia/main/data/results_parlimen_ge14.csv")
# ext_data_pru_18 = read.csv(text = ext_data_pru_18)
# write.csv(ext_data_pru_18,"data/external_data/ge_14_ext_data.csv")

# Full credits to Thevesh for this data

ge_14_ext_data = read.csv("data/external_data/ge_14_ext_data.csv")
needed_cols = c("seat","undi_keluar_peti","undi_rosak","pengundi_jumlah")
new_coln = c("parliament","voter_turnout","spoilt_votes","eligible_voters")

ge_14_ext_data = ge_14_ext_data[needed_cols]
colnames(ge_14_ext_data) = new_coln
ge_14_ext_data = ge_14_ext_data %>% mutate(parliament = gsub("\\.","",parliament)) %>% 
  separate(parliament, into = c("parliament","constituency"),sep = " ") %>% select(-constituency)

# Rejoin and Rearrange
pru_2018_result = left_join(pru_2018_result,ge_14_ext_data)
pru_2018_result = pru_2018_result %>% mutate(votes_winner_pct = round(votes_winner/eligible_voters*100,1),
                                             votes_opponent_pct = round(votes_opponent/eligible_voters*100,1),
                                             voter_turnout_pct = round(voter_turnout/eligible_voters*100,1),
                                             spoilt_votes_pct = round(voter_turnout/eligible_voters*100,1)) %>% 
  # rearranging columns to match previous data
  select(parliament,constituency,constituency_old_name,winner,winner_coalition,winner_party,votes_winner,votes_winner_pct,
         opponent,opponent_coalition,opponent_party,votes_opponent,votes_opponent_pct,
         majority,incumbent,incumbent_coalition,incumbent_party,eligible_voters,              
         voter_turnout,voter_turnout_pct,spoilt_votes,spoilt_votes_pct,
         parliament_number,state,parliament_status,incumbent_contesting)
# Save
readr::write_csv(pru_2018_result,"data/election_result/malaysia_ge14_p_result.csv")
