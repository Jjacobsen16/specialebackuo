
# Intro hent pakker og data -----------------------------------------------
# Hvor mange penge brugte partierne i løbet af valgkampen?
# I dette script bruger jeg det aggregerede (men præcise) data, som jeg har fra den sidste måned i valgkampen

# Det er en ny udgave af den gamle "danish_election_month_script.R".

library(pacman)
p_load(tidyverse)

#Loading data
month_raw <- read_csv("C:/R/Speciale/Facebook/FB DK Daily/måned/8maj til 6 juni DK_last_30_days/FacebookAdLibraryReport_2019-06-06_DK_last_30_days_advertisers.csv")


# Renser og sorterer data -------------------------------------------------

glimpse(month_raw)

# Alle sider, som har brugt 100kr eller mindre, har fået værdien "≤100". 
# Jeg vil gerne bruge den som en numerisk variabel, så jeg laver denne værdi om til 50kr. for alle.

måned_data <- month_raw %>%
  mutate(ad_spend = as.numeric(`Amount Spent (DKK)`))
# Alle "≤100" bliver lavet om til NA's.

# Laver alle NA's om til 50. (Der er ingen NA's i den originale variabel, så intet problem)
måned_data$ad_spend <- måned_data$ad_spend %>% 
  replace_na(50)

måned_data %>%
  select(ad_spend) %>%
  tail() # Sådan. Virker fint.

# Generer liste over page_id's som er med i fb_ads (opstillede kandidater og parti-sider)
politiske_sider <- fb_ads %>%
  distinct(page_id) %>%
  pull # 592 sider

# Med ovenstående liste kender jeg 523 ud af 807 sider, som har kørt politiske annoncer under valgkampen. Hvem er de øvrige 284 sider?
måned_data %>%
  filter(!`Page ID` %in% politiske_sider) %>%
  select(`Page Name`, `Number of Ads in Library`, ad_spend) %>%
  arrange(desc(ad_spend))
# Det ser fint ud. Ingen overraskelser. Det er fagforeninger, interesseorganisationer mv.
  
# Det store spørgsmål: Hvor mange penge har de politiske partier så brugt på facebook annoncer samlet set (på 4 uger)?

# Først skal jeg lige importere parti_navne fra fb_ads. 
glimpse(måned_data)
måned_data <- (left_join(måned_data, distinct(select(fb_ads, page_id, parti_navn, kandidat_d)), by = c("Page ID" = "page_id"))) %>%
  filter(`Page ID` %in% politiske_sider)

glimpse(måned_data)
# Tjekker for missing values
sapply(måned_data, function(x) sum(is.na(x))) # Ingen NA's. 

# Så... Hvor mange penge brugte de på de 4 uger?
sum(måned_data$ad_spend) # 9.277.461 kr. sindssygt!!

# Hvad med partierne?
måned_data %>%
  group_by(parti_navn) %>%
  summarise(total_spend_4w = sum(ad_spend)) %>%
  arrange(desc(total_spend_4w))

# Og kandidaternes egne sider?
måned_data %>%
  filter(kandidat_d == 1) %>%
  select(`Page Name`, ad_spend) %>%
  head()

# VISUALISER RESULTATERNE. Brug kode fra danish_election_month_script.R.
# nu videre. 
