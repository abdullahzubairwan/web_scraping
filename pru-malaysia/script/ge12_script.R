# Title: GE14 Scraper and Cleaner
# Version: 1.0
# Author: Zubs
# Date: 3 November 2022
# Edit: -

library(rvest)
library(dplyr)
library(purrr)
library(tidyr)
library(stringr)

pru_2008_url = "https://en.wikipedia.org/wiki/Results_of_the_2008_Malaysian_general_election_by_parliamentary_constituency"

a = read_html(pru_2008_url) %>% html_table()
a = a[2:17]

coln = c("parliament","constituency","winner","votes_winner","votes_winner_pct","opponent","votes_opponent","votes_opponent_pct",                     
         "majority","incumbent","eligible_voters","malay_voters_pct","chinese_voters_pct","indian_voters_pct","other_voters_pct",
         "muslim_bumiputera_voters_pct","nonmuslim_bumiputera_voters_pct","voter_turnout","voter_turnout_pct","spoilt_votes","spoilt_votes_pct")

b = lapply(a,setNames,coln)
b = purrr::map(b,~mutate_all(.x,as.character))
pru_2008_result = do.call(bind_rows,b)

# 1.0 Start with filtering out empty columns
pru_2008_result = pru_2008_result %>% filter(parliament != "")

# 2.0 Separating parliament to add State metadata
pru_2008_result = pru_2008_result %>% mutate(parliament_number = as.integer(gsub("P","",parliament)),
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
coln_remove_comma = c("votes_winner","votes_opponent","majority","eligible_voters","voter_turnout","spoilt_votes")
pru_2008_result = pru_2008_result %>% mutate(across(.cols = tidyr::all_of(coln_remove_comma),gsub,pattern =",",replacement = ""),
                                             across(.cols = tidyr::all_of(coln_remove_comma),as.integer))

# 4.0 Removing % symbol from percentage columns
coln_remove_pct = c("votes_winner_pct","votes_opponent_pct","malay_voters_pct","chinese_voters_pct","indian_voters_pct","other_voters_pct",
                    "muslim_bumiputera_voters_pct","nonmuslim_bumiputera_voters_pct","voter_turnout_pct","spoilt_votes_pct")

pru_2008_result = pru_2008_result %>% mutate(across(.cols = tidyr::all_of(coln_remove_pct),gsub,pattern ="%",replacement = ""),
                                             across(.cols = tidyr::all_of(coln_remove_pct),as.numeric))

# 5.0 Separating Coalition and Party from Name

pru_2008_result = pru_2008_result %>% 
  separate(winner, into = c("winner","winner_coalition"),sep = "\\(") %>% 
  separate(winner_coalition, into = c("winner_coalition","winner_party"), sep = "–") %>% 
  separate(opponent,into = c("opponent","opponent_coalition"),sep = "\\(") %>% 
  separate(opponent_coalition, into = c("opponent_coalition","opponent_party"), sep = "–") %>% 
  separate(incumbent, into = c("incumbent","incumbent_coalition"),sep = "\\(") %>% 
  separate(incumbent_coalition, into = c("incumbent_coalition","incumbent_party"), sep = "–") 

# 6.0 Recorrecting Coalition and Party for those with no Coalition
for(i in 1:nrow(pru_2008_result)){
  if(is.na(pru_2008_result$winner_party[i])){
    pru_2008_result$winner_party[i] = pru_2008_result$winner_coalition[i]
    pru_2008_result$winner_coalition[i] = NA_character_
  }
  
  if(is.na(pru_2008_result$opponent_party[i])){
    pru_2008_result$opponent_party[i] = pru_2008_result$opponent_coalition[i]
    pru_2008_result$opponent_coalition[i] = NA_character_
  }
  
  if(is.na(pru_2008_result$incumbent_party[i])){
    pru_2008_result$incumbent_party[i] = pru_2008_result$incumbent_coalition[i]
    pru_2008_result$incumbent_coalition[i] = NA_character_
  }
}

# 7.0 Cleaning symbols in Winner/Opponent Party

pru_2008_result$winner_party = str_squish(gsub(")","",pru_2008_result$winner_party))
pru_2008_result$opponent_party = str_squish(gsub(")","",pru_2008_result$opponent_party))
pru_2008_result$incumbent_party = str_squish(gsub(")","",pru_2008_result$incumbent_party))

# 8.0 Adding metadata whether the Parliament is part of government or opposition
pru_2008_result = pru_2008_result %>% mutate(parliament_status = ifelse(winner_coalition == "BN","Government","Opposition"))

# 9.0 Adding metadata whether the incumbent is contesting or not
pru_2008_result = pru_2008_result %>% group_by(parliament) %>% 
  mutate(incumbent_contesting = ifelse(incumbent %in% c(winner,opponent),"Yes","No"))

# Save
readr::write_csv(pru_2008_result,"data/election_result/malaysia_ge12_p_result.csv")
