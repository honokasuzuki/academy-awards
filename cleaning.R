library(stringr)

# PREPROCESS / CLEANING THE SCRAPED DATA

speeches = readRDS("speeches.rds")

# remove leading white spaces
speeches[] <- lapply(speeches, trimws)

# some categories say 'Actor/Actress' while others say 'Actor/Actress in a Leading Role'
# make all consistent
speeches$Gender[speeches$Gender == "Actor"] = "Actor in a Leading Role"
speeches$Gender[speeches$Gender == "Actress"] = "Actress in a Leading Role"
# check
unique(speeches$Gender)

# make an award category column
speeches$Category = ifelse(str_detect(speeches$Gender, "Leading"), 
                           "Leading Role", 
                           "Supporting Role")

# modify gender column to only contain actor or actress
speeches$Gender = sub("([A-Za-z]+).*", "\\1", speeches$Gender)

# separate year number from index of awards
# extract characters inside parentheses
index = unlist(str_extract_all(speeches$Year, "\\([^()]+\\)"))
# remove parenthesis
index = substring(index, 2, nchar(index)-1)
speeches$AcademyAwards = index

# year number is the first 4 characters of Year column
speeches$Year = substr(speeches$Year, 1, 4)

# speech texts have the winner's name + a colon before actual speech
# remove name by removing all text up until the first colon
speeches$Speech = gsub("^[^:]*:", "", speeches$Speech)

# format data to appropriate type
str(speeches)
speeches$Gender = as.factor(speeches$Gender)
speeches$Category = as.factor(speeches$Category)
speeches$Year = as.numeric(speeches$Year)
str(speeches)

saveRDS(speeches, 'speeches_clean.rds')
