require(geojsonio)
require(leaflet)
require(dplyr)
require(tidyr)
require(rjson)
require(tidyverse)
require(reshape2)
require(readr)
require(stringr)
require(textclean)

## CS Ranking data
cs_rankings_data = read_csv(file.path("CSrankings", "csrankings.csv"))
acm_fellows = read_csv(file.path("CSrankings", "acm-fellows.csv"))
turing = read_csv(file.path("CSrankings", "turing.csv"))
nsf_fellows = read_csv(file.path("nsf-awards.csv"), col_types = cols("Current Institution" = col_character()))

departments_data = read.csv(file.path("all_departments.csv"))
professor_data = read_csv(file.path("all_professors.csv"))

# Turn NA's to 0's 
professor_data$Citations[is.na(professor_data$Citations)] = 0

# Get num_citations for each university and join data
citation_data = professor_data %>% group_by(UniversityID) %>% summarise(total_citations = sum(Citations))
departments_data = left_join(departments_data, citation_data, by = c("ID" = "UniversityID"))

# Fix university names to be more consistent with other datasets
departments_data = departments_data %>% mutate(University.Name = 
                                                 replace_non_ascii(str_squish(str_replace_all(departments_data$University.Name, c("-" = " ",
                                                                                                                                "\u2013" = " ",
                                                                                                                                "\u2014" = " ",
                                                                                                                                " at " = " ")))))
# Fix colnames, fix names of schools to be more compliant
colnames(nsf_fellows) = c("year", "name", "award_type", "undergrad_institution", "field", "proposed_institution", "current_institution")
nsf_fellows_clean = filter(nsf_fellows, str_detect(field, "^CS|comp/")) %>%
  mutate(undergrad_institution = str_squish(str_to_title(str_replace_all(undergrad_institution, c("^U | U$" = " University ",
                                                                                                  " [tT]ech | [tT]ech$"  = " Technology ",
                                                                                                  " [iI]nst | [iI]nst$" = " Institute ",
                                                                                                  "-" = " ")))),
         proposed_institution  = str_squish(str_to_title(str_replace_all(proposed_institution, c("^U | U$" = " University ",
                                                                                                 " [tT]ech | [tT]ech$"  = " Technology ",
                                                                                                 " [iI]nst | [iI]nst$" = " Institute ",
                                                                                                 "-" = " ")))),
         current_institution   = str_squish(str_to_title(str_replace_all(current_institution, c("^U | U$" = " University ",
                                                                                                " [tT]ech | [tT]ech$"  = " Technology ",
                                                                                                " [iI]nst | [iI]nst$" = " Institute ",
                                                                                                "-" = " ")))))


# Rename aliased names
colnames(acm_fellows)[colnames(acm_fellows)=="year"] <- "acm_year"
colnames(turing)[colnames(turing)=="year"] <- "turing_year"

# Add in acm_fellows data and turing data
cs_rankings_data = left_join(cs_rankings_data, acm_fellows, by="name")
cs_rankings_data = left_join(cs_rankings_data, turing, by="name")

venues = read_csv(file=file.path("CSrankings", "venues.csv"))

# Articles starts as json, unfortunately
articles = fromJSON(file=file.path("CSrankings", "articles.json"))

# Fix to be csv
articles_melted = melt(t(as.data.frame(articles)))
articles_melted$Var1 = str_replace(articles_melted$Var1, "\\.[:digit:]+", "")
articles_clean = dcast(articles_melted, (seq_len(nrow(articles_melted)) - 1) %/% 11 ~ Var1)[,-1]

# Fix some names to map correctly to college location data
articles_clean$institution = str_squish(str_replace_all(articles_clean$institution, c("Univ\\." = "University",
                                                                                      "-" = " ",
                                                                                      "\u2013" = " ",
                                                                                      "\u2014" = " ",
                                                                                      " at " = " ")))

# Add articles and cs_rankings_data together
cs_rankings_data = left_join(cs_rankings_data, articles_clean, by="name")

## Trim data to only US stuff
cs_rankings_data_copy = cs_rankings_data

# Remove dumb whitespace
cs_rankings_data$institution = str_trim(cs_rankings_data$institution)
cs_rankings_data$affiliation = str_trim(cs_rankings_data$affiliation)

# Fix `area` field to be area rather than just conf again
cs_rankings_data = cs_rankings_data %>% select(-starts_with("area"))
cs_rankings_data = left_join(cs_rankings_data, venues, by = c("conf" = "alternate"))

if(!dir.exists(file.path("clean"))) {
  dir.create(file.path("clean"))
}

## Write data 
write.csv(cs_rankings_data, file = file.path("clean", "pubs.csv"), row.names = FALSE)
write.csv(venues, file = file.path("clean", "venues.csv"), row.names = FALSE)
write.csv(articles_clean, file = file.path("clean", "articles.csv"), row.names = FALSE)
write.csv(nsf_fellows_clean, file = file.path("clean", "nsf-awards.csv"), row.names = FALSE)
write.csv(departments_data, file = file.path("clean", "departments_data.csv"))
          