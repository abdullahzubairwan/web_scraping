# Title: FUTBIN22 Player Basic Table Scraper
# Version: 1.0
# Author: Zubs
# Date: 28 September 2022
# Edit: -

# Libraries
library(dplyr)
library(rvest)
library(Rcrawler)
library(xml2)

# Testing html_table output
path = "https://www.futbin.com/22/players?page=1"
# a1 = read_html(path) %>% html_table()

# Get Number of Page to do For Loop
node_endpage = ".page-link"
a2 = read_html(path) %>% html_nodes(node_endpage) %>% html_text()
a2 = a2[6]

futbin_22_data = data.frame()
for(i in 1:a2){
  # URL Generator
  url_generate = paste0("https://www.futbin.com/22/players?page=",i)
  # Scrape Tabular Data
  fut_html = read_html(url_generate) 
  fut_table = fut_html %>% html_table()
  # Second Round of scrape, provider changed format so here we are trying to mitigate that
  fut_table = fut_table[[1]] %>% filter(!is.na(X7))
  # Scrape Club,Nation and League that is not within the table
  fut_club = fut_html %>% html_nodes("span.players_club_nation a") %>% html_attr("data-original-title")
  chunklength=3
  club_s = split(fut_club,ceiling(seq_along(fut_club)/chunklength))
  club_df = do.call(rbind.data.frame, club_s)
  colnames(club_df) = c("Club","Nation","League")
  
  fut_table = data.frame(append(fut_table,club_df))
  # A failsafe ; ensuring that all columns are converted into character to prevent difference of class that will fail the scrape process
  fut_table[,1:ncol(fut_table)] <- lapply(fut_table[,1:ncol(fut_table)], as.character)
  # A gesture of respect ; giving some time from every scrape to avoid jamming their website.
  Sys.sleep(10)
  # Combining every page of Data
  futbin_22_data = bind_rows(futbin_22_data,fut_table)
  #
  cat(paste0("Now at ",i," \n"))
}
# A good practice to save the raw data first before proceeding
readr::write_csv(futbin_22_data,"raw_data/futbin_22_raw_2.csv")
