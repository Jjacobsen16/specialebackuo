
# INDLÆSER NØDVENDIGE PAKKER ----------------------------------------------

library(pacman)
p_load(tidyverse,
       lubridate, 
       readtext, 
       httr, 
       tictoc, 
       purrr,
       readxl,
       readr,
       stringr,
       stringdist,
       fuzzyjoin)


# Facebook API data ------------------------------------------------

# Henter alle politiske annoncer, som har kørt på Facebook og Instagram siden Facebook vha. hhtr-pakken
# Seneste version af data er hentet d. 10/11 2019, hvor det var muligt at inkludere en variabel for,
# om annoncen kørte på Facebook, Instagram eller begge.

#  # Link til FB API
#  my_link<- "https://graph.facebook.com"
# 
#  # Vælger variable fra FB API
#  search_fields=c("ad_creation_time",
#                  "ad_delivery_start_time",
#                  "ad_delivery_stop_time",
#                  "ad_creative_body",
#                  "ad_creative_link_caption",
#                  "ad_creative_link_title",
#                  "ad_creative_link_description",
#                  "ad_snapshot_url",
#                  "page_id",
#                  "page_name",
#                  "spend",
#                  "currency",
#                  "demographic_distribution",
#                  "funding_entity",
#                  "impressions",
#                  "region_distribution",
#                  "publisher_platforms") %>% # Den nye variabel i november versionen af data
#    stringr::str_c(., collapse=", ")
# 
#  # Indhent data fra første side med de 100 første ads
#  page_one_response <- GET(my_link,
#                           path = "/ads_archive",
#                           query = list(access_token = "EAAGU2IAXOUMBABzMXx0TtqE4vIRTDoSWeU6A3dX93WEpg5CvJVTeOnlBXZBIZCwJwTGVYbwzIxoA4hI4J6omhoKew104baWZBbDZCjH35RGgxFiyuBkZBCzVx4AkiYRB6SVD4xTqU3o45DnyYz47T2fqyK6i18LbYBHbgeTwYro1qDeZBObO2n8d6mZC27xfpuagVONpFUhr6dm2OXx2Tnn",
#                                        limit=100,
#                                        ad_active_status="ALL", # Henter alle annoncer uanset om de er aktive eller inaktive.
#                                        search_terms="''", # Henter alle annoncer uanset hvad der står i dem.
#                                        fields=search_fields, # Alle variable.
#                                        ad_reached_countries="DK")) # Kun annoncer, som har kørt i Danmark.
#  page_one_content<- content(page_one_response)
# 
#  x <- tibble(data=page_one_content$data)
#  df_imp <- x %>%
#    unnest_wider(data)
# 
# # Link som henviser til næste side
#  next_link <- page_one_content$paging$`next`
# 
# # Gentager for hver side der er, indtil der ikke er flere sider
#  tic() # tager tid
#  while(length(next_link)>0) {
# 
#    next_response <- GET(next_link)
#    next_content<- content(next_response)
# 
#    y <- tibble(data=next_content$data)
#    df_next <- y %>%
#      unnest_wider(data)
# 
#    df_imp <- bind_rows(df_imp, df_next)
# 
#    next_link <- next_content$paging$`next`
# 
#  }
# toc() # 672.95 sec 
# 672.95/60 # Det tog 11 minutter.
# 
#  # Det virker!
#  glimpse(df_imp)
# 
#  # Gemmer en kopi af rådata
#  # csv virker ikke med list columns, så det bliver .rds format
#  write_rds(df_imp, "FB API data/data/raw-data-nov10.rds")

nov_data <- read_rds("FB API data/data/raw-data-nov10.rds")

#
# # Succes
glimpse(nov_data) # 37.906 ad buys (annonce-indrykninger) 20 variable


# SORTERER DATA ---------------------------------
## DATO: Kun Annoncer, som har kørt i valgkampen =================================


# nov_data indeholder alle politiske annoncer, som har kørt fra marts til 10. november 2019. 
# Jeg sorterer data, så kun annoncer, der har kørt i perioden marts til valgdagen d. 5. juni inkluderes.

# Datoen kommer i string format. Laver det om til dataformat, så det er til at arbejde med.
nov_data <- nov_data %>%
  mutate(ad_start = ymd_hms(ad_delivery_start_time),
         ad_stop = ymd_hms(ad_delivery_stop_time),
         ad_creation = ymd_hms(ad_creation_time))

# Fjerner de gamle dato-variable
nov_data <- select(nov_data, -ad_creation_time, -ad_delivery_start_time, -ad_delivery_stop_time)
dim(nov_data) # 37.906 og 17

# Laver ny df med alle ads, der har kørt frem til valgdagen d. 5. juni
election_all <- nov_data %>%
  filter(ad_start < ymd("2019-06-06"),
         # ad start time er senest på valgdagen d. 5. juni
         # --> Betyder, at den skal starte med at køre INDEN valgkampen slutter.
         ad_start > ymd("2019-02-28"))
         # Men jeg medtager ikke annoncer, som kørte før marts 2019, hvor Facebook åbnede deres Ads Library.
         # Det ser ud til >= "less than or equal to" ikke virker her. Anvender derfor kun >.

# Hvor mange ad buys var der så i perioden marts til valgdagen 5. juni?
dim(election_all) # 22.843 annonce-indrykninger

# Hvor mange unikke sider betalte for dem?
n_distinct(election_all$page_name) # 1.540 sider. # Bemærk at det er 1.536 med id.

# Fjerner nov_data igen og kører videre med election_all
rm(nov_data)


## KANDIDATER OG PARTIER: Kun annoncer fra politikere og politiske partier =================================

# I denne sektion indhenter jeg navne anden information om alle opstillede kandidater til Folketings- og Europaparlamentsvalget.
# Jeg merger kandidatlisterne med Facebook API data for at identificere alle opstillingsparate kandidater, som har kørt annoncer på FB og Instagram i løbet af deres valgkamp.
# Udfordringen her er, at navnet på stemmesedlen ikke altid stemmer overens med navnet på politikernes Facebooksider. 

# Importerer kandidatlister. Kilde : Danmarks Statistik: http://www.dst.dk/valg/index.htm

# Alle opstillede kandidater til Folketingsvalget 
ft19kandidat <- read_excel("C:/Users/johan/OneDrive/Dokumenter/Københans Universitet/Specialegryden/DST FT19/KandidaterDetaljeret2019.xlsx")

# Alle opstillede kandidater til Europaparlamentsvalget
ep19kandidat <- read_excel("C:/Users/johan/OneDrive/Dokumenter/Københans Universitet/Specialegryden/DST FT19/KandidaterDetaljeretEP19.xlsx")


# Strategien for at merge er som følger:
  # 1. Tjek om der er dubletter på stemmesedlerne eller på listen over Facebook-sider, som forhindrer merging og lav en samlet liste over kandidater, der skal marges.
  # 2. Identificer alle kandidater, hvis navne på stemmesedlen optræder præcist i navnet på deres Facebookside.
  # 3. Identificer alle kandidater, hvis navne på stemmesedlen og Facebook-sider næsten stemmer overens (tager højde for evt. stavefejl eller forskellige måder at stave et navn på)
  # 4. Identificer alle kandidater, hvis fornavn og efternavn på stemmesedlen optræder på Facebook-navnet.

# For hvert skridt vil jeg sortere matchede kandidater fra, så jeg kun leder videre blandt de umatchede kandidater.

### 1a. Dubletter i FB-data ############################# 

# Laver først page_id om fra string til numeric
election_all <- election_all %>%
  mutate(page_id = as.numeric(page_id))

# Liste over unikke FB-navne
FB_navne_temp <- election_all %>%
  select(page_name, page_id) %>%
  distinct() # 1.538 kombinationer af 1.534 unikke FB-navne (name) og 1.536 unikke FB-sider (id)
# = Nogle politikere (names) har altså haft flere sider (id) eller omvendt!

# Tjekker om der er FB-sider (page_id), som har haft forskellige navne (page_name)
FB_navne_temp %>%
  group_by(page_id) %>%
  filter(n()>1)

# Det er et problem, fordi jeg kun kan matche 1 FB-navn med 1 stemmeseddelnavn. 

# Løsning: Jeg identificerer alle sider (page_name), som har haft forskellige FB-navne og omdøber dem til 1 navn.
election_all <- election_all %>%
  mutate(page_name = replace(page_name, page_id == 779582492178326, "René Nord Hansen"), # hvorfor siger den, 'page_id not found?' 
         page_name = replace(page_name, page_id == 1593837374204311, "Anders Stjernholm"),
         page_name = replace(page_name, page_id == 1875899649300807, "Tom Andkjær"),
         page_name = replace(page_name, page_id == 231583183664778, "Flemming Leer Jakobsen"),
         page_name = replace(page_name, page_id == 56810821434, "HK Privat"),
         page_name = replace(page_name, page_id == 445657632847922, "Giv Klimaet Lov"))

# Tjekker om det virker
election_all %>%
  select(page_name, page_id) %>%
  distinct() %>%
  group_by(page_id) %>%
  filter(n()>1) # Det virker. = Alle FB sider i data har nu unikke navne for deres FB-sider.

# Tjekker om nogle politikere (page_name) har haft forskellige facebook-sider (page_id)
FB_navne_temp %>%
  group_by(page_name) %>% 
  filter(n()>1)
# ==> Pernille Skipper og Alex Vanopslagh har fx begge kørt annoncer fra to forskellige sider (page_id).
# Det udgør ikke et problem for merging baseret på navne, men for en god ordens skyld, rydder jeg op i det ved at give dem 1 page_id hver.

election_all <- election_all %>%
  mutate(page_id = replace(page_id, page_name == "Pernille Skipper", 547840621916967),
         page_id = replace(page_id, page_name == "Alex Vanopslagh", 671022293105597),
         page_id = replace(page_id, page_name == "Danske Bank", 13038884034274),
         page_id = replace(page_id, page_name == "eToro", 193863317737))
         

# Tjekker om det virker
election_all %>%
  select(page_name, page_id) %>%
  distinct() %>%
  group_by(page_name) %>%
  filter(n()>1) # Det virker. = Alle sider i data har nu også unikke id'er.

# OPSUMMERING: Der er nu ingen dubletter på page_name og page_id i Facebook data for politikere.

# Opdaterer listen med unikke FB_navne, så den er klar til at blive merged.
FB_navne <- election_all %>%
  select(page_name, page_id) %>%
  distinct() # 1.532 unikke FB_navne.

# Sletter den midlertidige liste med navne
rm(FB_navne_temp)


### 1b. Dubletter på stemmesedler? ############################# 

# Liste med opstillede folketingskandidater
FT_navne <- ft19kandidat %>%
  select(STEMMESEDDELNAVN, PARTI)

# Er der nogle dubletter på listen over folketingskandidater?
FT_navne %>%
  group_by(STEMMESEDDELNAVN) %>% 
  filter(n()>1)

# JA DER ER 3 DUBLETTER. 
  # Casper Pedersen fra V og Casper Pedersen fra Ø
  # Henrik Boye fra LA og Henrik Boye fra SF
  # Kim Christiansen fra O og Kim Christiansen fra Å.

election_all %>%
  filter(page_name %in% c("Casper Pedersen", "Henrik Boye", "Kim Christiansen" )) %>%
  select(page_name, page_id) %>%
  distinct() # 1 Henrik Boye og 1 Casper Pedersen har kørt annoncer på Facebook.

# Jeg har tjekket deres Facebook-sider og kan se, at det er Casper Pedersen fra V og Henrik Boye fra LA, der har kørt annoncer.

# Sletter de to kandidater, som ikke har kørt annoncer, fra listen over FT_kandidater, så de ikke fejlagtigt bliver merged med FB_data
FT_navne <- FT_navne %>%
  filter(!(STEMMESEDDELNAVN == "Casper Pedersen" & PARTI == "Ø" |
           STEMMESEDDELNAVN == "Henrik Boye" & PARTI == "F"))


# Liste med opstillede europa-palamentsmedlemmer
EP_navne <- ep19kandidat %>%
  select(Stemmeseddelnavn, Parti) # 135 opstillede kandidater

# Er der nogle dubletter på listen over EP-kandidater?
EP_navne %>%
  group_by(Stemmeseddelnavn) %>% 
  filter(n()>1) # Nej det er der ikke. Fint!

# Er der nogle navne, som både optræder på listen over EP og FT kandidater?
intersect(FT_navne$STEMMESEDDELNAVN, EP_navne$Stemmeseddelnavn) # Ja 31 optræder begge steder. 
# Det gør ikke noget at kandidaterne er opstillede til begge valg, så længe de kun har kørt annoncer fra 1 Facebookside.

# Laver en samlet liste med FT og EP kandidater, som skal merges med Facebook-data
alle_kandidater <- full_join(select(FT_navne, STEMMESEDDELNAVN), select(EP_navne, Stemmeseddelnavn), by = c("STEMMESEDDELNAVN" = "Stemmeseddelnavn"))
# Resultat: 1.002 kandidater. Det stemmer! 898 (FT) + 135 (EP) - 31 (kandidater opstillet til både FT og EP).

# Oprydning: Sletter de enkelte lister med FT og EP navne.
rm(FT_navne, EP_navne)

### 2. Stemmeseddelnavn optræder præcist i FB-navn ############################# 

# Match alle kandidater HVIS FULDE NAVNE FREMGÅR AF PAGE_NAME PRÆCIST SOM DE FREMSTÅR PÅ STEMMESEDDELNAVN, uanset om FB-navn indeholder mere end det.
# Dvs. jeg også matcher fx: "Niels Arbøl - Folketingskandidat for Radikale Venstre"
# inner join: "include only rows with mathces in each"

regex_match <- regex_inner_join(FB_navne, alle_kandidater, by = c("page_name" = "STEMMESEDDELNAVN"),
                                  ignore_case = TRUE)
# Result: 407 matches
dim(regex_match)

# 2: Laver en liste med de resterende kandidater, så jeg kan søge videre efter dem i FB data
kandidater_rest <- anti_join(alle_kandidater, select(regex_match, STEMMESEDDELNAVN)) # 598 tilbage
nrow(kandidater_rest) # 598

# Resultat: 407 matches og 598 resterende kandidater. = 1.005. dvs. 3 for meget.

# Identificerer de tre dubletter i de matchede kandidater:
regex_match %>%
  group_by(page_name) %>% 
  filter(n()>1) # Resultat: Her er de 3 dubletter. 

# Kandidaten Klaus Riskær Pedersen er blevet mathed med hans egen FB-side og 2 af hans kandidater, bl.a. Kim Durups FB-side, 
# fordi Klaus Riskærs navn også er navnet på partiet, og derfor fremgår af Kim Durups FB-side.
# I det andet tilfælde er kandidat Hans Henrik Sørensen både blevet matched med ham selv og en anden kandidat ved navn
# Henrik Sørensen, fordi dette navn indgår i Hans Henrik Sørensens Facebookside. Løser disse problemer:

regex_match <- regex_match %>%
  filter(!(page_name == "Martin Nordstjerne Rasmussen - Partiet Klaus Riskær Pedersen - liste E" & STEMMESEDDELNAVN == "Klaus Riskær Pedersen"|
             page_name == "Partiet : Klaus Riskær Pedersen - Kandidat Kim Durup" & STEMMESEDDELNAVN == "Klaus Riskær Pedersen"|
             page_name == "Hans Henrik Sørensen" & STEMMESEDDELNAVN == "Henrik Sørensen"))

nrow(regex_match) # RESULTAT: 404 matches og 598 resterende kandidater ud af i alt 1.002 kandidater. FINT!

# Der er altså stadig 598 umatchede kandidater.
# Jeg fortsætter med fuzzy_join

### 3. Fuzzy join ############################# 

# Identificerer tilfælde, hvor der næsten er match mellem navnene (men ikke 100%)

# Metode: stringdist_innter_join: "Strings that are similiar in Levenshtein/cosine/Jaccard distance, 
# or other metrics from the stringdist package."

fuzzy_match <- stringdist_inner_join(FB_navne, kandidater_rest, 
                                     by = c("page_name" = "STEMMESEDDELNAVN"),
                                     distance_col = "distance",
                                     method = "dl") # The full Damerau-Levenshtein distance
                                     # metoden tæller hvor mange ændringer der skal til, før der er præcist match
fuzzy_match <-  fuzzy_match %>%
  arrange(distance)

head(fuzzy_match,10) # Den finder 5 kandidater med nærmest navne-match.
# I de tilfælde, hvor der skal 2 ændringer til, er der intet match, men i de tilfælde, 
# der kun skal 1 ændring til, er der match. Navnene er de samme, de er blot stavet en smule forskelligt.

# Udvælger kandidaterne som skal mathces manuelt
fuzzy_kandidater <- fuzzy_match %>%
  filter(distance == 1) %>%
  select(page_name, STEMMESEDDELNAVN)

# Tilføjer de tre kandidater til listen over matchede kandidater
matched_kandidater <- bind_rows(select(regex_match, page_name, STEMMESEDDELNAVN), fuzzy_kandidater) # 407 kandidater matched. 

# Opdaterer listen med resterende kandidater
kandidater_rest <- anti_join(alle_kandidater, select(matched_kandidater, STEMMESEDDELNAVN)) # 595 remaining

# Sorterer matchede FB-sider fra listen, så jeg ikke risikerer at matche dem igen neden for.
FB_navne_rest <- anti_join(select(FB_navne, page_name), select(matched_kandidater, page_name)) 
# 1.125 sider tilbage. Dvs. 407 mathede FB-sider fjernet. fint!

# Oprydning
rm(fuzzy_match, fuzzy_kandidater, regex_match)

### 4. Regular expressions #############################

# Her søger jeg videre blandt uidentificerede kandidater i FB-data vha. regular expressions.

### A. Søger efter FB-sider, hvor kandidaternes første og sidste navn på stemmesedlen optræder (uanset mellemnavne mv.)

# Laver en df med fornavn (første navn) og efternavn (sidste navn)
regex_kandidater <- kandidater_rest %>%
  mutate(lastname = word(STEMMESEDDELNAVN,-1),
         firstname = word(STEMMESEDDELNAVN, 1))

# Tilføjer regular expression baseret på for- og efternavn 
regex_kandidater <- regex_kandidater %>%
  mutate(regex_name = str_glue("\\b{firstname}\\b.*\\b{lastname}\\b"))
         # Betyder: # fornavn og alt efter det + efternavn, og kun funde ord, så fx Lene ikke matcher Helene.
  
regex_matchb <- regex_inner_join(select(FB_navne_rest, page_name), select(regex_kandidater, regex_name, STEMMESEDDELNAVN), by = c("page_name" = "regex_name"),
                                 ignore_case = TRUE) # 17 matches
regex_matchb %>%
  select(-regex_name) %>%
  print(n = 25)
# De fleste af dem er rigtige. De identificerer fx flere tilfælde hvor kandidaten har et mellemnavn på FB, men ikke på stemmesedlen.

#Sorterer de forkerte matches fra manuelt
NOT_STEMMESEDDELNAVN <- c("Anne Rasmussen", "Søren Sørensen")

# Matcher 15 ud af de 17 identificerede kandidater

regexb_candidates <- regex_matchb %>%
  filter(!STEMMESEDDELNAVN %in% NOT_STEMMESEDDELNAVN) %>%
  select(page_name, STEMMESEDDELNAVN)

# Tilføjer de 15 kandidater til listen over matchede kandidater
matched_kandidater <- bind_rows(matched_kandidater, regexb_candidates) # 422 kandidater matched (407 + 15)

# Opdaterer listen over resterende kandidater
kandidater_rest <- anti_join(alle_kandidater, select(matched_kandidater, STEMMESEDDELNAVN)) # 580 unidentified.

# Opdaterer listen over resterende FB-sider
FB_navne_rest <- anti_join(FB_navne_rest, select(matched_kandidater, page_name)) # 1.110 tilbage.


### B. Splitter kandidaternes navne op og søger efter dem i FB-data i på andre måder.

# Laver en df, hvor de resterende kandidaters navne er opdelt:
split_kandidater_rest <- kandidater_rest %>% # BRUg DENNE KODE HVIS DU VIL SPLITTE ALLE NAVNE OP I STUMPER!
  separate(STEMMESEDDELNAVN, into = c("første","andet","tredje","fjerde", "femte"), sep = " ",
           fill = "right",
           remove = F)
# Der er 1 kandidat med 5 navne.

# Tilføjer last name til split_kandidater_rest
split_kandidater_rest <- split_kandidater_rest %>%
  mutate(lastname = word(STEMMESEDDELNAVN,-1))

regex_kandidater_split <- split_kandidater_rest %>%
  # 1.Kandidatens første navn, alt efter det (fx bare et mellemrum) og så kandidatens andet navn
  mutate(regex = str_glue("\\b{første}\\b.*\\b{andet}\\b"))
  # 2. Kandidatens første navn og så det andet, tredje fjerde eller femte navn
  #       regex_first = str_glue("^(?=.*\\b{første}\\b)(?=.*\\b{andet}|{tredje}|{fjerde}|{femte})"),
  # 3. Søger bagfra startede med kandidatens sidste navn og derefter 1 af kandidatens andre navne
  #       regex_last = str_glue("$(?=.*\\b{lastname}\\b)(?=.*\\b{fjerde}|{tredje}|{andet}|{første})"))



# Ser om den finder noget
regex_matchc <- regex_inner_join(select(FB_navne_rest, page_name), select(regex_kandidater_split, regex, STEMMESEDDELNAVN), by = c("page_name" = "regex"),
                                 ignore_case = TRUE) # 5 ekstra kandidater identificeret
# Resultat: 1 og 2 gav samme resultat. 3 gav ikke noget.

head(regex_matchc)
# 1 af kandidaterne er ikke et match. Anne Rasmussen skal ikke matches med Anne-Mette Rasmussen.

regexc_candidates <- regex_matchc %>%
  filter(!STEMMESEDDELNAVN %in% NOT_STEMMESEDDELNAVN) %>%
  select(page_name, STEMMESEDDELNAVN)

# Tilføjer de 4 kandidater til listen over matchede kandidater
matched_kandidater <- bind_rows(matched_kandidater, regexc_candidates) # 426 kandidater matched.

# Opdaterer listen over resterende kandidater
kandidater_rest <- anti_join(alle_kandidater, select(matched_kandidater, STEMMESEDDELNAVN)) # 576 unidentified.

# Opdaterer listen over resterende FB-sider
FB_navne_rest <- anti_join(FB_navne_rest, select(matched_kandidater, page_name)) # 1.106 tilbage.

# SLUT ! Jeg kan ikke matche flere kandidater i Facebook-data.

### 5. Gem liste over matchede kandidater #############################

# write_csv(matched_kandidater, "matched_kandidater.csv")

# Oprydning:
rm(FB_navne, FB_navne_rest, kandidater_rest, matched_kandidater, split_kandidater_rest, NOT_STEMMESEDDELNAVN)
rm(list = ls(pattern = "regex")) 

#Næste skridt: importer kandidatvariabe fra EP og FT til data - se httr text analysis koden. 

# MERGER FB DATA OG KANDIDAT-DATA ---------------------------------

# Indlæser listen over identificerede kandidater i FB-data
matched_kandidater <- read_csv("matched_kandidater.csv")

# OBS! Vær opmærksom på at der 2 dubletter på stemmeseddelnavn! Dem skal jeg sortere fra igen. 

# 1. Giv FT og EP dataframes samme kolonne-navne for samme variable
glimpse(ep19kandidat)
glimpse(ft19kandidat)

ep19kandidat <- ep19kandidat %>%
  rename(PARTI = Parti,
         STEMMESEDDELNAVN = Stemmeseddelnavn,
         KØN = Køn,
         ALDER = Alder,
         STILLING = Stilling,
         POSTNR = Postnr) # kun denne variabel har listen med FT-kandidater ikke.

# 2. Merger listen med stemmeseddelnavn til FB-data, så jeg kan merge resten af data på den
  # Må ikke tilføje nogen rows. Kun 1 kolonne "STEMMESEDDELNAVN"
election <- election_all %>%
  left_join(matched_kandidater) # Succes.

# 3. Tilføjer variable om FT-kandidater

# 3a. Først skal jeg fjerne de to kandidater til FT, som har identiske navne med andre kandidater, og som IKKE har kørt nogen annoncer,
#     så de ikke fejlagtigt bliver merged.

  # SKulle gerne tilføje 7 kolonner og 0 rows
election <- election %>%
  left_join(ft19kandidat %>%
              filter(!(STEMMESEDDELNAVN == "Casper Pedersen" & PARTI == "Ø" |
                       STEMMESEDDELNAVN == "Henrik Boye" & PARTI == "F")))
# Succes. 

# Markerer alle FT-kandidater i data 
election <- election %>%
  mutate(FT = if_else(!is.na(PARTI), 1, 0)) # parti er kun udfyldt for FT-kandidater.

# 4. Tikføjer variable om EP-kandidater
  # Skulle gerne tilføje 1 kolonne og 0 rows.

election <- election %>%
  left_join(ep19kandidat)
# SUcces. 1 kolonne med postnummer tilføjet

# Markerer alle EP-kandidater i data
election <- election %>%
  mutate(EP = if_else(!is.na(POSTNR), 1, 0)) # postnummer er kun udfyldt for EP-kandidater.

# Tjekker hvor mange kandidater, som har været opstillet til både EP og FT, der har kørt annoncer:

election %>%
  select(STEMMESEDDELNAVN, FT, EP) %>%
  group_by(STEMMESEDDELNAVN) %>%
  filter(FT == 1 & EP == 1) %>%
  count() %>%
  arrange(desc(n))

dim(election) # 22843 &  28

# Oprydning
rm(matched_kandidater, ep19kandidat, ft19kandidat)

# Identificerer partier i data ---------------------------------

# Det er ikke kun politikere som enkeltpersoner, der har kørt annoncer. Her identificerer jeg parti-siderne.

partier <- c("Socialdemokratiet", "Socialdemokraterne", "SF", "Socialistisk Folkeparti", "Det Konservative Folkeparti", "Konservative", "Enhedslisten", "EL", "Venstre, Danmarks Liberale Parti", "Venstre", "Dansk Folkeparti", "DF", "Liberal Alliance", "LA", "Alternativet", "Å", "Radikale Venstre", "RV", "Nye Borgerlige", "Parti Klaus Riskaer Pedersen", "Parti Klaus Riskær Pedersen", "Kristendemokraterne", "KD", "Stram Kurs") 

# Laver en parti-søgestring med en regular expression regel: Søg kun efter hele ord, ellers får jeg fx alle page_names, hvor "vu" eller "el" optræder et sted.
partier_regex <- str_glue("\\b{partier}\\b")
# Laver det om til en vektor
partier_regex <- read_lines(partier_regex)

# Ser hvilke sider, jeg finder på den måde
election %>%
  filter(is.na(STEMMESEDDELNAVN)) %>%
  filter(str_detect(page_name, regex(paste(partier_regex, collapse = '|'), ignore_case = T))) %>%
  distinct(page_name) %>%
  print(n=50) # 118 sider identificeret.

# Der er mange lokalforeninger og også nogle ungdomsforeninger. Men også nogle sider, der ikke er partier. Dem sorterer jeg fra manuelt.
           

# Leder efter ungdomspartier

ung_string <- c("ung", "ungdom", "dsu", "vu")

election %>%
  filter(str_detect(page_name, regex(paste(ung_string, collapse = '|'), ignore_case = T))) %>%
  distinct(page_name) %>% pull()


# Der var nogle stykker. Tilføjer dem til listen
ungdomspartier <- c("Nye Borgerliges Ungdom", "SF Ungdom", "Konservativ Ungdom", "Alternativets Unge", "DSU Bornholm", "Liberal Alliances Ungdom", "DSU Aalborg", "DSU Viborg", "Liberal Alliance Ungdom Sønderborg", "Radikal Ungdom", "Venstres Ungdom - VU", "Ringsted Konservativ Ungdom", "Danmarks Socialdemokratiske Ungdom (DSU)", "DSU Thy-Mors", "DSU Lolland", "DSU Vesterbro")

# Identificerer alle FB-sider, hvor partinavnet indgår, men som ikke er en kandidat, der allerede er identificeret
regex_parti <- regex_inner_join(election %>%
                                  filter(is.na(STEMMESEDDELNAVN)),
                                alle_kandidater, by = c("page_name" = "STEMMESEDDELNAVN"),
                                ignore_case = TRUE)

election %>%
  filter(is.na(STEMMESEDDELNAVN)) %>%
  group_by(page_name) %>%
  summarise(n=n()) %>%
  arrange(desc(n)) %>%
  print(n=50)

# Når jeg har identificeret alle parti-sider
#election1 <- election_all %>%
  filter(page_name %in% partier| # partiernes egne sider står for 3.141 ad buys.
           funding_entity %in% partier| # når vi lægger partierne som betaler af ads til får vi 5.110.
           page_name %in% ftkandidater| # 12.354 får vi i alt, når alle opstillede kandidaters ad buys tæller med.
           page_name %in% epkandidater) # Tilføjede 1.169.

##### PUBLISHER PLATFORMS #####

glimpse(nov_data)

# Ser hvordan jeg kan extracte publisher_platform
nov_data %>%
  pull(publisher_platforms) %>%
  unique()
# Der er facebook og Instagram i forskellige kombinationer.

# Laver en ny variabel for hvilke platforme, ads har kørt på.
nov_data <- nov_data %>%
  mutate(plat1 = map_chr(publisher_platforms, 1, .default = NA),
         plat2 = map_chr(publisher_platforms, 2, .default = NA))

# Tjekker mulige kombinationer af platforme
nov_data %>%
  select(starts_with("plat")) %>%
  unique()
# Resultat: Ads kan have kørt på facebook, instagram og facebook eller kun instagram.

# Laver en variabel for hhv. facebook og instagram
nov_data <- nov_data %>%
  mutate(facebook = if_else(plat1 == "facebook" |
                              plat2 == "facebook", 
                            1,0),
         instagram = if_else(plat1 == "instagram" |
                               plat2 == "instagram",
                             1,0))
# Rydder lidt op.
nov_data$plat1 <- NULL
nov_data$plat2 <- NULL

# Hvor mange ads kørte på facebook og instagram eller begge?
nov_data %>%
  group_by(facebook, instagram) %>%
  count() # Overraskende mange ads har kørt på begge platforme!

##### NY OVERSKRIFT #####

# Se Rolands kode ift. at extracte impressions, demographics og alt det. 





