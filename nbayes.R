# 1. Forberedelse til Quanteda ---------------------------------
## 1.1 Indlæser pakker og data =================================

library(pacman)
p_load(tidyverse, quanteda, xtable, tictoc, caret)
# måske lubridate, topicmodels, readtext

fb_ads <- read_rds("FB API data/data/fb_ads.rds")

## 1.2 Datamanipulation og ID =================================

fb_ads %>%
  count(PARTI)

# Udfordring: parti-sider har ikke noget parti-bogstav.

fb_ads <- fb_ads %>%
  mutate(PARTI = case_when(
    parti_navn == "Socialdemokratiet" ~ "A",
    parti_navn == "Radikale Venstre" ~ "B",
    parti_navn == "Konservative" ~ "C",
    parti_navn == "Nye Borgerlige" ~ "D",
    parti_navn == "Parti Klaus Riskær Pedersen" ~ "E",
    parti_navn == "Socialistisk Folkeparti" ~ "F",
    parti_navn == "Liberal Alliance" ~ "I",
    parti_navn == "Kristendemokraterne" ~ "K",
    parti_navn == "Folkebevægelsen mod EU" ~ "N",
    parti_navn == "Dansk Folkeparti" ~ "O",
    parti_navn == "Stram Kurs" ~ "P",
    parti_navn == "Venstre" ~ "V",
    parti_navn == "Enhedslisten" ~ "OE", # NB! skifter til OE og AA fordi Quanteda får ÆØÅ galt i halsen
    parti_navn == "Alternativet" ~ "AA"
  ))
# Alle partier har nu også parti-bogstaver i variablen 'PARTI'.


###  Sorterer data i alfabetisk rækkeføge efter partibogstav og page_name ###

fb_ads <- fb_ads %>%
  arrange(PARTI, page_name)

# Giver hver ad et unikt id.

# Gammel kode: Her lod jeg page_name indgå i hver ID. Nu beholder jeg kun partinavn.

# fb_ads <- fb_ads %>%
#   group_by(page_name) %>%
#   mutate(ad_id = paste(PARTI,
#                        str_trunc(str_remove_all(page_name, "[:space:]"), 20, "center", ellipsis = "."), 
#                        # fjerner mellemrum og sætter max længde på page_name
#                        row_number(), sep = "-")) %>%
#   ungroup() # NB! funktionen virker ikke hvis plyr er loaded!


fb_ads <- fb_ads %>%
  mutate(ad_id = paste0(PARTI, 
                       #str_trunc(str_remove_all(page_name, "[:space:]"), 20, "center", ellipsis = "."), 
                       # fjerner mellemrum og sætter max længde på page_name
                       1:nrow(fb_ads)))

# Virker det?
n_distinct(fb_ads$ad_id)

# Virker fint.
fb_ads %>%
  select(PARTI, page_name, ad_id) %>%
  head()

# Detalje: Fjerner white space i begyndelsen eller slutningen af annonceteksten for at sikre, 
# at distinct() ikke lader sig narre af whitespace og kun beholder tekstmæssigt unikke annoncer.
# TIlføj evt. til jjscript all
fb_ads <- fb_ads %>%
  mutate(ad_creative_body = str_trim(ad_creative_body),
         ad_id_copy = ad_id) # Nødvendig fordi ad_id forsvinder fra corpus metadata når den bruges som doc_id.

## 1.3 Konstruktion af corpus =================================
# Udvælger 14 variable, som er relevante at have med i corpus, så jeg ikke har alt med:
corpus_data <- select(fb_ads, c(ad_id, ad_id_copy, ad_creative_body, page_name, PARTI, parti_navn, KØN, ALDER, spend_mid, spend_interval, 
                                impressions_mid, impressions_interval, kandidat_d, facebook, instagram))

corpus_body <- corpus(corpus_data, 
                      docid_field = "ad_id_copy", # sætter dokumentid = ad_id
                      text_field = "ad_creative_body")

# 2. Naive Bayes estimation ---------------------------------

## 2.1 Opsætning af trænings- og testdata =================================

# Brug Grimmer & Steward ref til udvælgelse til training set.
# Training-data og test-data skal være repræsentativt for data.
# Jeg bruger derfor stratificeret udvælgelse for at sikre, at partierne udgør en lige stor andel i træningssettet
  # Fordi: supervised machine learning metoder bruger features in the training set to classify the remaining documents in the test set (G&S: 276)
  # Hvis der er mange flere annoncer fra store partier (random sample) så vil deres features være overrepræsenteret i training data,
  # og HVIS partierne har forskelligt sprogbrug (forskellige ord), så vil modellen underestimere de små partier.
  # Overvej evt. at lave proportionel stratificering for partierne MEN med et minimum af annoncer fra de små partier? Måske fin løsning. 
  # Så bliver det både repræsentativt for data i det hele taget og repræsentativt for de enkelte partier. 

# Denne samplingmetode kan kaldes down-sampling, fordi den reducerer størrelsen af de store partier for at matche størrelsen på de små.
# Down-sampling balances the dataset by reducing the size of the abundant class(es) to match the frequencies in the least prevalent class.
# Se 2.2.3 Class imbalances her: https://bradleyboehmke.github.io/HOML/process.html 

# Tekstmæssigt unikke annoncer? Jeg skal selvfølgelig have så meget sproglig variation i training data som muligt
  # Derfor sørger jeg for, at der ikke er tekstmæssige dubletter i training data.

# Undersøger populationen af kandidat-annoncer med tekstmæssigt unikt indhold
fb_ads %>%
  filter(!ad_creative_body =="") %>% # sorterer 2 tekstmæssigt tomme annoncer fra
  filter(kandidat_d == T) %>% # Bruger kun personlige sider, ikke parti-sider
  distinct(ad_creative_body) %>% # Beholder kun tekstmæssigt unikke kandidat-annoncer 
  nrow() # Det giver n=5.520. Det er denne population jeg tager en stikprøve af i udvælgelsen af training data.

# Tjekker hvor mange tekstmæssigt unikke annoncer hvert partis kandidater har kørt:
fb_ads %>%
  filter(!ad_creative_body =="") %>% 
  filter(kandidat_d == T) %>% 
  distinct(ad_creative_body, .keep_all = T) %>%
  group_by(PARTI) %>%
  count() %>%
  arrange(n)
# Det strækker sig fra 39 (Partiet Klaus Riskær Pedersen) til 1.072 (Venstre).

### Stratificeret stikprøve af 39 kandidatannoncer fra hvert parti til training data

set.seed(6867) # For replicerbarhed

training_id <- fb_ads %>%
  filter(!ad_creative_body =="") %>% 
  filter(kandidat_d == T) %>% 
  distinct(ad_creative_body, .keep_all = T) %>% 
  group_by(PARTI) %>% # Stratificeret sample inden for hvert parti
  sample_n(39, replace = F) %>%
  pull(ad_id)

# Det giver et udtræk på 507 annoncer i training_data som skal kodes i hånden.

# Trækker de 507 annoncer ud som en Excel-fil, hvor jeg koder dem manuelt
training <- fb_ads %>%
  filter(ad_id %in% training_id) %>%
  select(ad_id, ad_creative_body)
write_excel_csv(training, "training.csv") # Eksporterer til csv

# Excel kan sagtens læse det på en pæn måde. Excel fil oprettet.

# Next step: code the data. JOHAN HER
 
### 2.1.1 Corpus subset training test #############################

# Training subset
corpus_training <- corpus_subset(corpus_body, ad_id %in% training_id, select = c(ad_id, PARTI)) # keeping only ad_id for later merge.

# Andelen af ads fra de forskellige partier er lige:
prop.table(table(docvars(corpus_training, "PARTI")))*100 # hvert parti udgør 7,7 %

# Test subset
corpus_test <- corpus_subset(corpus_body, !ad_id %in% training_id, select = ad_id)

# Det stemmer. 

## 2.2 Laver DFM baseret på de to datasæt =================================
## 2.3 Træner algoritmen og tester dens performance =================================
## 2.4 Sammenligner med random prediction =================================
