library(dplyr)

censusKey = "99f02d15745b04acaf40f60535ae45a70ecde2ac"

censusLink = paste("https://api.census.gov/data/2010/surname?get=NAME,PCTWHITE,PCTAPI,PCT2PRACE,PCTAIAN,PCTBLACK,PCTHISPANIC&RANK=1:1000&key=", 
                   censusKey, 
                   sep = "")

surnames = as.data.frame(jsonlite::fromJSON(censusLink, simplifyDataFrame = TRUE, flatten = TRUE), 
                         stringsAsFactors = FALSE)

names(surnames) = surnames[1, ]

surnames = surnames[-c(1), ]

surnames = surnames %>% 
  select(-RANK) %>% 
  tidyr::gather(key, value, -NAME) %>% 
  mutate(value = as.numeric(value)) %>% 
  group_by(NAME) %>% 
  arrange(NAME, desc(value)) %>% 
  filter(value > 20)

write.csv(surnames, "surnames.csv", row.names = FALSE)
