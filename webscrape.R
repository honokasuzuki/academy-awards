library(rvest)
library(tidyverse)

# WEB SCRAPE from Academy Awards database
# search all acceptance speeches from Actor & Actress in Leading & Supporting Role
# result: 299 records
# first record needs to be scraped separately
# automatic loop begins with record 2

link_part1 = "http://aaspeechesdb.oscars.org/results.aspx?AC=NEXT_RECORD&XC=/results.aspx&BU=http%3A%2F%2Faaspeechesdb.oscars.org%2F&TN=aatrans&SN=AUTO2530&SE=823&RN="
link_part2 = "&MR=0&TR=0&TX=1000&ES=0&CS=0&XP=&RF=WebReportList&EF=&DF=WebReportOscars&RL=0&EL=0&DL=0&NP=255&ID=&MF=oscarsmsg.ini&MQ=&TI=0&DT=&ST=0&IR=0&NR="
link_part3 = "&NB=0&SV=0&SS=0&BG=&FG=&QS=&OEX=ISO-8859-1&OEH=utf-8"

records = 299

# set up data frame to store scraped data
speeches = data.frame(Speech = character(records),
                      Year = character(records),
                      Gender = character(records),
                      Film = character(records),
                      Name = character(records),
                      stringsAsFactors = F)

# loop through records to scrape from each one
for (i in 2:records){
  link = paste0(link_part1, i-2, link_part2, i-1, link_part3)
  speeches[i,'Speech'] = link %>% 
    read_html() %>% 
    html_nodes(".MInormal") %>% 
    html_text()
  speeches[i, -1] = link %>% 
    read_html() %>% 
    html_nodes("strong") %>% 
    html_text()
}

# manually fill in record 1
first_link = "http://aaspeechesdb.oscars.org/results.aspx?AC=PREV_RECORD&XC=/results.aspx&BU=http%3A%2F%2Faaspeechesdb.oscars.org%2F&TN=aatrans&SN=AUTO2530&SE=823&RN=1&MR=0&TR=0&TX=1000&ES=0&CS=0&XP=&RF=WebReportList&EF=&DF=WebReportOscars&RL=0&EL=0&DL=0&NP=255&ID=&MF=oscarsmsg.ini&MQ=&TI=0&DT=&ST=0&IR=0&NR=2&NB=0&SV=0&SS=0&BG=&FG=&QS=&OEX=ISO-8859-1&OEH=utf-8"
speeches[1,'Speech'] = first_link %>% 
  read_html() %>% 
  html_nodes(".MInormal") %>% 
  html_text()
speeches[1, -1] = first_link %>% 
  read_html() %>% 
  html_nodes("strong") %>% 
  html_text()

saveRDS(speeches, 'speeches.rds')
