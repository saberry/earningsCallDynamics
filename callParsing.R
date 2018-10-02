#######################
### Initial Testing ###
#######################

library(dplyr)

library(stringr)

library(gender)

namesRace = readr::read_csv("tzioumisNames.csv")

namesRace$firstname = stringr::str_to_title(namesRace$firstname)

namesRace = namesRace %>% 
  select(-obs, -X9) %>% 
  tidyr::gather(key, value, -firstname) %>% 
  group_by(firstname) %>% 
  arrange(firstname, desc(value)) %>% 
  filter(value > 20)

surnameRace = readr::read_csv("surnames.csv")

# textTest = data.frame(raw = readLines("1000000_T.xml"), stringsAsFactors = FALSE)

textTest = data.frame(raw = readLines("1000046_T.xml"), stringsAsFactors = FALSE)

textTest = textTest %>% 
  mutate(raw = ifelse(grepl("^\\=|^\\-", .$raw), NA, raw), 
         raw = ifelse(raw == "", NA, raw)) %>% 
  na.omit()

allPeopleRows = which(grepl("^(Corporate Participants)", textTest$raw)):(which(grepl("^(Presentation)", textTest$raw)) - 1)

allPeople = textTest[allPeopleRows, ]

frontMatter = 1:which(grepl("^(TEXT)", textTest$raw))

endMatter = which(grepl("]]></Body>", textTest$raw)):nrow(textTest)

textTest = textTest[-c(allPeopleRows, frontMatter, endMatter),] %>% 
  data.frame(raw = ., stringsAsFactors = FALSE) %>% 
  mutate(callPart = ifelse(raw == "Presentation", "Presentation", 
                           ifelse(raw == "Questions and Answers", "q&a", NA)),
         raw2 = ifelse(grepl("\\[[0-9]+\\]", .$raw), raw, ""),
         tracker = ifelse(raw == raw2, paste(1:nrow(.), raw2, sep = "_"), NA)) %>% 
  tidyr::fill(., tracker, .direction = "down") %>% 
  tidyr::fill(., callPart, .direction = "down") %>% 
  mutate(callPart = ifelse(raw == callPart, NA, callPart), 
         raw = ifelse(raw == raw2, NA, raw)) %>% 
  na.omit(.$raw) %>% 
  group_by(tracker) %>% 
  summarize(text = paste(raw, collapse = ""), 
            callPart = first(callPart)) %>% 
  ungroup() %>% 
  mutate(order = as.numeric(stringr::str_extract(tracker, "^[0-9]+")), 
         tracker = gsub("^[0-9]+_|\\s\\[[0-9]+\\]", "", .$tracker), 
         firstName = str_trim(str_extract(tracker, "(\\b\\w+\\b)+")), 
         name = str_trim(str_extract(tracker, "([^,]+)")), 
         organization = str_trim(str_extract(tracker, ",.*\\s(?=-)")), 
         organization = gsub(",\\s+", "", organization),
         title = str_trim(str_extract(tracker, "(?<=-).*"))) %>% 
  arrange(order) %>% 
  select(name, firstName, organization, title, text)
  
genderPredictions = gender(textTest$firstName) %>% 
  distinct() %>% 
  select(name, gender)

textTest = left_join(textTest, genderPredictions, by = c("firstName" = "name"))
  
