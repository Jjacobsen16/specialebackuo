# Indlæser pakker og data ####################

library(pacman)
p_load(tidyverse, readtext, quanteda, xtable, tictoc) 
# måske lubridate, topicmodels

fb_ads <- read_rds("FB API data/data/fb_ads.rds")

# temp: Skal lige finde ud af, hvorfor der er 592 unikke page_names og kun 427 unikke stemmedesselnavne
# SVAR: Det er selvfølgelig fordi, der er 166 partisider med i data. 

# QUANTEDA ----------------------------------------------------------------
## 1. Forberedelse og konstruktion af corpus =================================

# Hvor mange tekstmæssigt unikke annoncer er der?
# fb_ads %>%
#   distinct(ad_creative_body) %>%
#   nrow() # 7.174 ud af de 14.190 dvs. 50 %.

# Giver hver ad et unikt id

# Jeg vil gerne have parti-navner med i hver id.
# Udfordring: parti-sider har ikke noget parti-bogstav.
fb_ads %>%
  count(PARTI)
       
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

# Giver hver ad unikt id hvor partinavn indgår
                         
fb_ads <- fb_ads %>%
  group_by(page_name) %>%
  mutate(ad_id = paste(PARTI,
                       str_trunc(str_remove_all(page_name, "[:space:]"), 20, "center", ellipsis = "."), 
                       # fjerner mellemrum og sætter max længde på page_name
                       row_number(), sep = "-")) %>%
  ungroup() # NB! funktionen virker ikke hvis plyr er loaded!

# Virker det?
n_distinct(fb_ads$ad_id)
# Ja. Og kun med længden 20 virker det! Mindre vil give dubletter.

# Udvælger 14 variable, som er relevante at have med i corpus, så jeg ikke har alt med:
corpus_data <- select(fb_ads, c(ad_id, ad_creative_body, page_name, PARTI, parti_navn, KØN, ALDER, spend_mid, spend_interval, impressions_mid, impressions_interval, kandidat_d, facebook, instagram))

corpus_body <- corpus(corpus_data, 
                      docid_field = "ad_id",
                      text_field = "ad_creative_body")

corpus_info <- summary(corpus_body, n = 14190, showmeta = F, tolower = T) # Smukt.

# Lav gerne et histogram over, hvor lange teksterne er.
sum(ntoken(corpus_body)) # 1.033.644
sum(ntoken(corpus_body, remove_punct = T)) # 885.905. Med numbers, uden punctuation og alle lower case.
sum(corpus_info$Sentences) # 66.243 sentences.

ndoc(corpus_body) # 14.190

# OBS! Der er enkelte ads uden en tekst.

# Fjern evt. annoncer uden tekstmæssigt indhold fra corpus_body, hvis de ikke skal medtages i analysen. skip for now.
# corpus_body <- corpus_subset(corpus_body, ntoken(corpus_body) > 0)

ndoc(corpus_body) # 296 annoncer fjernet. 13.890 tilbage i corpus_body.


# corp_longsent <- corpus_subset(corp_sent, ntoken(corp_sent) >= 10)

## 2. Deskriptiv statistik fra corpus =================================

# Man kan gemme output fra summary command i en df og lave deskriptiv statisik og plots
# tokeninfo <- summary(corpus_body, 100) #  Kigger kun på 100 ads by default

# Hvem af de 100 første ads ideholdt flest ord?
# tokeninfo[which.max(tokeninfo$Tokens),] # text id F-MarianneBigum

# Sådan viser jeg teksten fra en bestemt ad vha. ad_id. 
# writeLines(texts(corpus_body)['F-MarianneBigum-1']) # writeLines fed command til at komme af med /n nye linjer!

## 4. Corpus analyse =================================

# Brug kwic (keywords-in-context) til at se, i hvilken kontekst ord optræder.
# kwic(corpus_body, pattern = "elsk*") %>%
#   nrow() # 285 annoncer indeholder noget med indvandr*.
# 
# # Kan også tage flere ord i en vektor:
# kwic(corpus_body, pattern = c("indvandr*", "immigr*", "flygtn*"), window = 4) %>%
#   head(5)
# 
# # Look up multi-word expressions using phrase()
# kwic(corpus_body, pattern = phrase("Så fedt")) %>%
#   nrow() # Der er blevet skrevet "Så fedt" i 8 annoncer.

# Når jeg subsetter corpus for hvert parti kan jeg også sammenligne, hvilke ord de bruger mest visuelt:
# http://vanatteveldt.com/p/welbers-text-r.pdf (eksempel med Obama vs. Trump) se også Zotero.

## 5. Tokenizing =================================

# Splitter corpus op i enkelte ord (tokens).
# Fjerner tal og tegn i processen

tokens_body <- tokens(corpus_body, what = "character",
                       remove_numbers = F,
                       remove_punct = T,
                       include_docvars = F) # 10 MB stort!

char_body <- tokens(corpus_body, what = "character",
                      remove_numbers = F,
                      remove_punct = T,
                      include_docvars = F)
 
 # Laver alle ord om til lower case
tokens_body <- tokens_tolower(tokens_body) # 9.8 MB

sum(ntoken(tokens_body)) # 885.905 uden punctuation, men med numbers. Stemmer.

sum(ntoken(char_body)) # 4.326.536
.Last.value/2400 # svarende til 1.803 normalsider. 

# Andre options, hvis jeg får brug for dem:
  # tokens_compound(c("Rasmus Paludan", "lavere skat") tilføj denne, hvis jeg vil beholde multi-word expressions as single features
  # tokens_body <- tokens_remove(tokens_body, berteltorp_stopord) # Hvis jeg vil fjerne stopord
  # STEM tokens med tokens_wordstem # tokens_body_stem <- tokens_wordstem(tokens_body, language = "da")

#Her er en omfattende liste med danske stopord: 
berteltorp_link <- "https://gist.githubusercontent.com/berteltorp/0cf8a0c7afea7f25ed754f24cfc2467b/raw/dbe856b10c9cc18f173011f2802074ce3cdbb90e/stopord.txt"
berteltorp_stopord <- read_lines(berteltorp_link) 
# 
# tokens_body_nostopstem <- tokens_remove(tokens_body, berteltorp_stopord)
# tokens_body_nostopstem <- tokens_wordstem(tokens_body_nostopstem, language = "da")

# Sådan ser jeg annonceteksten i fuld længde
# writeLines(texts(corpus_body)['F-MarianneBigum-1'])

# Sådan ser jeg annonceteksten opdelt i ord:
# print(tokens_body[['F-MarianneBigum-1']]) # print baseret på ad_id

## 5. Document feature matrix =================================

# Så kommer vi til det interessante: Document feature matrix (dfm<<)

# Laver en fuld dfm. 
  # Stopord er ikke fjernet og stammen af ordene er ikke fundet.
# dfm_full <- dfm(tokens_body) # dfm kan også laves direkte på corpus_body med samme til/fravalg.
# # 12.8 MB
# 
# ndoc(dfm_full) # 14.190 dokumenter
# nfeat(dfm_full) # 30.180 ord

# dfm_nostopstem <- dfm(tokens_body_nostopstem)
# 
# ndoc(dfm_nostopstem) # 13.890 dokumenter
# nfeat(dfm_nostopstem) # 20.604 ord med stopord fjernet og stammen for hvert ord.
# 
# head(rowSums(dfm_full), 10) # 1 dokument = 1 række. Rækkesum er således antal ord i et dokument.
# head(colSums(dfm_full), 10) # 1 ord = 1 kolonne. Kolonnesum er således antal gange et ord opstræder i data.

# De 1000 hyppigste ord:
# pop1000<- names(topfeatures(dfm_nostopstem, 1000))
# mat <- as.matrix(pop1000)
# matrix <- matrix(mat, nrow = 100, ncol = 10, byrow = T)
# write_excel_csv(as.data.frame(matrix), "matrix1000.csv")
# Skriver til Excel, så jeg kan identificere ord til ordbøger der.

# Hvis jeg ikke er interesseret i count, kan jeg lave en dfm med proportions i stedet
# proportions = hvor stor en andel ordet udgør INDEN FOR ET DOKUMENT. Det skal jeg nok ikke bruge.
# dfm_full_prop <- dfm_weight(dfm_full, scheme = "prop") # 18.5 MB
# topfeatures(dfm_full_prop[1,]) # hyppigste ord i det første dokument.

# Jeg kan vægte hyppighed (count) i forhold til hvor unikke ordene er i forhold til alle dokumenter. Skal jeg nok heller ikke bruge.
# Det er smart, hvis man vil sortere efter hvilken "informationsværdi" et ord har (http://vanatteveldt.com/p/welbers-text-r.pdf)
# dfm_full_tfidf <- dfm_tfidf(dfm_full, scheme_tf = "count", base = 10) 
# topfeatures(dfm_full_tfidf[1,]) # Viser det første dokument.

# Når jeg skal lave output er det dog textstat_frequency, jeg skal bruge.
# Den laver data.frame output og har flere funktioner end topfeatures().

### 5.a Wordcloud #############################

# Her skal stopord være fjernet.

# set.seed(841) # så jeg kan genskabe wordcloud.
# textplot_wordcloud(dfm_full) # wordcloud
# textplot_wordcloud(dfm_full, max_words = 250, random_order = FALSE,
#                    rotation = .25,
#                    color = RColorBrewer::brewer.pal(8, "Dark2")) # Flot!

## 6. DICTIONARIES =================================

# Så kommer vi ind til kernen: dictionaries.

# Strategi: Lav en liste over de 1.000 hyppigst forekomne ord (ud af ca. 30.000)
# Identificer ord, der passer i de forskellige dictionaries.

# Laver liste med de 1.000 hyppigste ord (stemmed og uden stopord)
# tokens_nostopstem_freq <- textstat_frequency(dfm_nostopstem)
# 
# hyppig1000 <- tokens_nostopstem_freq[1:1000,] %>%
#   pull(feature) # 1000 mest anvendte ord.

# Laver en liste med 500 populære ord som har højest 'informationsværdi'
# NEJ DET VIRKER IKKE LIGE. 
# dfm_nostopnostem_tfidf <- dfm_tfidf(dfm_nostopstem, scheme_tf = "count", base = 10) 
# dfm_nostopnostem_tfidf <- dfm_sort(dfm_nostopnostem_tfidf, margin = "features")
# head(dfm_nostopnostem_tfidf, n = 5, nf = 10)
# writeLines(nostopstem_tfidf, "vigtig500.txt")

# hyp1000 <- tokens_nostopstem_freq[1:1000,] %>%
#   select(feature)
# write_csv(hyp1000, "hyp1000.csv") # gemmer ikke æøå

# Dotplot over de mest 15 mest anvendte ord
# dfm_full %>% 
#   textstat_frequency(n = 15) %>% 
#   ggplot(aes(x = reorder(feature, frequency), y = frequency)) +
#   geom_point() +
#   coord_flip() +
#   labs(x = NULL, y = "Frequency") +
#   theme_minimal()

# Konstruerer ordbøger:

# Jeg anvender * i stedet for at finde ordenes stamme.
# Jeg behøver ikke fjerne stopord.

personlig_gammel <- c("føl","ven", "nær", "stolt", "tvivl", "mor", "far", "bedste*", "lyk", "drøm", "bekymr", "pårør", "svigt", "elsk", "frygt", "børnebørn", "hyg", "ærl",
  "bekend", "leg", "forstår", "taknem", "ansgst", "forhåbent", "ægtefæl", "skam", "trist", "vred", "indrøm")
# slettet: ægtefælde, leg, tvivl, nær
følelser_ny <- c("håb*", "ønsk*", "glæd*", "desværre", "glad", "fantastisk*", "stolt*",
                   "drøm*","lykkelig*", "hjert*", "elsk","taknem*", "ærlig*", "trist", "vred", "indrøm*",
              "forhåbent*", "skam*", "forstå*","frygt*", "elsk*", "svigt", "bekymr*") # tvivl fjernet
# slettet: ønsk, tryg, træt
privatliv_ny <- c("min mand", "min kone","min kæreste", "mine børn*", "mit barn*", "min familie*", "mine forældre*", "min far*", "min mor*", "min ven", "gode ven", "hyg*")
# slettet fra privatliv: spis, overvej
# kompetencer <- c("dygt", ) dur ikke!
personlig_ny <- c("håb*", "ønsk*", "glæd*", "desværre", "glad", "fantastisk*", "stolt*",
                  "drøm*","lykkelig*", "hjert*", "elsk","taknem*", "ærlig*", "trist", "vred", "indrøm*",
                  "forhåbent*", "skam*", "forstå*","frygt*", "elsk*", "svigt", "bekymr*", "tvivl*", "hyg*",
                  "min mand", "min kone","min kæreste", "mine børn*", "mit barn*", "min familie*",
                  "mine forældre*", "min far*", "min mor*", "min ven", "gode ven") # multi-word expressions her til sidst!


# OBS! Multi-word expressions i dictionary virker ikke på dfm objekter. Her skal jeg bruge tokens med tokens_lookup
# "Yet, dfm() can handle multi-word expressions because dictionary lookup is performed internally on tokens using tokens_lookup()""

kwic(corpus_body, pattern = phrase("min mand*"), window = 2)

kwic(corpus_body, "føle*", window = 5) %>%
  as_tibble() %>%
  select(pre:post) %>% # tiføjer de to linjer for at skjule doc_id
  sample_n(10) # tilfældigt udtræk



#bagefter bruger jeg en paste til at smide * bag på alle ord.

# "børn er de 4. mest anvendte ord. Skal sikker ud!
# familie  og forældre er også populært. skal måske ud. 

# NY ORDBOG
personlig_ny <- dictionary(list(
  personlig_ny = personlig_ny
))

aktørindeks_dict <- dictionary(list(
  parti = c("parti", "partiet*"), # uden "vi". Undgår partier eller partiernes (flertal)
  kandidat = c("jeg", "mig", "min", "mit", "mine", "personlig*") # kwic viser, at det fungerer fint!
))

# klima_kwic <- kwic(tokens_body, personlig_dict['privatliv']) # For at se privatliv-dictionary in-context!

# Personfokus vs. partifokus

personfokus <- c("jeg", "mig", "min", "mit", "mine") #kræver at stopord IKKE er sorteret fra!
partifokus_A <- c("socialdemokrati*", "vi", "os", "vores") # skal anvendes på corupus_A

### 6.a Group_by parti ###################################################################################

# Jeg kan også gruppere en dfm i forhold til en anden variabel i data, fx parti eller køn

dfm_parti <- dfm(tokens_body, groups = "parti_navn") # 3MB

# Smart! Hver række repræsenterer et parti og hver kolonne et ord.
# Kan fx også bruge sammen med subset parti for at vise, hvilke page_names inden for hvert parti, der bruger hvilke ord.

# Da dfm_parti er et almindeligt dfm object, kan jeg også bruge weight på den.
# Laver en ny dfm som viser, hvor stor en andel
dfm_parti_prop <- dfm_weight(dfm_parti, scheme = "prop") 
topfeatures(dfm_parti_prop[1,]) # hyppigste ord i det første parti
# Fjern [] hvis det skal være ift. gennemsnittet = alle partier.

### 6.b Dictionary count grouped by parti ####################################################################

# Denne metode skal jeg bruge til personlig_ordbogen og emne_ordbogen. 
# Overvej, om jeg skal bruge andele med noget dfm_weight. 

# Dictionary count grouped by parties
# dfm_parties_dict <- dfm(corpus_body, groups= "PARTI", dictionary = personlig_ny,
  #                        remove = berteltorp_stopord, remove_punct = T) # NOT STEMMED

#### NYT LØRDAG ####

# Tester først om det personlig_ny virker på dfm
dfm_personlig_ny <- dfm(corpus_body, dictionary = personlig_ny)
#                        remove = berteltorp_stopord, remove_punct = T) # NOT STEMMED
df_personlig_ny <- convert(dfm_personlig_ny, to = "data.frame")
df_personlig_ny %>%
  count(personlig_ny)
# Får jeg samme resultat med tokens? (spoilter: JA. Derfor skip)
# dfm_personlig_nyt <- dfm(tokens_body, dictionary = personlig_ny) 
#                         #remove = berteltorp_stopord, remove_punct = T) # NOT STEMMED
# df_personlig_nyt <- convert(dfm_personlig_nyt, to = "data.frame")
# df_personlig_nyt %>%
#   count(personlig_ny)
# # RESULTAT: Det giver identisk resultat.
# 
# # Tjekker om den har brugt multi-word expressions
# multi_ord <- c("min mand", "min kone","min kæreste", "mine børn*", "mit barn*", "min familie*",
# "mine forældre*", "min far*", "min mor*", "min ven", "gode ven")
#   # dvs. jeg laver tokens_compound først
# toks_comp <- tokens_compound(tokens_body, pattern = phrase(multi_ord))
# 
# # kører ny dictionary på tokens. får jeg samme resultat?
# dfm_personlig_nym <- dfm(toks_comp, dictionary = personlig_ny) 
# #remove = berteltorp_stopord, remove_punct = T) # NOT STEMMED
# df_personlig_nym <- convert(dfm_personlig_nym, to = "data.frame")
# df_personlig_nym %>%
#   count(personlig_ny)
# Det giver OGSÅ samme resultat! corpus og tokens er altså lige godt til multi-word expressions!

# Merger med fb_data
glimpse(df_personlig_ny)
fb_ads <- fb_ads %>%
  left_join(df_personlig_ny, by = c("ad_id" = "document")) %>%
  mutate(pers_d = if_else(personlig_ny > 0, 1, 0)) # dikotomisering


glimpse(fb_ads)
fb_ads %>%
  group_by(kandidat_d) %>%
  count(pers_d) # Umiddelbart 3.469 personaliserede annoncer (haraf 3.015 fra kandidater)


# Ser hvordan fordelingen mellem partier er

pers_df <- fb_ads %>%
  # filter(kandidat_d == 1) %>% # ser kun på kandidater her.
  group_by(PARTI) %>%
  summarise(n_ads = n(),
            total_spend = sum(spend_mid))
            
# Hvor mange personaliserede ads har partierne kørt?
pers_ads <- fb_ads %>%
  filter(pers_d == 1) %>%
  group_by(PARTI) %>%
  summarise(n_pers = n(),
            pers_spend = sum(spend_mid))

pers_df <- pers_df %>%
  left_join(pers_ads) %>%
  replace(is.na(.), 0) # laver NA om til 0.

# Laver personaliseringsindeks og vægtet personaliseringsindeks
# I dette tilfælde er det blot andelen af personaliserede annoncer, ikke som Rahat & Kenig
pers_df <- pers_df %>%
  mutate(share_pers_n = round(n_pers/n_ads,2),
         share_pers_spend = round(pers_spend/total_spend,2))

# Succes. Gemmer resultatet
# write_csv(pers_df, "personlig_ny_resultat.csv")

# Barplot 1: Baseret på antal ad buys

pers_df %>%
  ggplot(aes(x = reorder(PARTI, share_pers_n), y = share_pers_n)) +
  geom_bar(stat = "identity") + 
  xlab("Parti") +
  theme_minimal() +
  labs(title = "Selvpersonaliseringsindeks",
       subtitle = "Viser andelen af annoncer med personaliseret indhold \nfor hvert parti")

ggsave("pers_indx.png")

# Barplot 2: Baseret på spend

pers_df %>%
  ggplot(aes(x = reorder(PARTI, share_pers_spend), y = share_pers_spend)) +
  geom_bar(stat = "identity") + 
  xlab("Parti") +
  theme_minimal() +
  labs(title = "Selvpersonaliseringsindeks",
       subtitle = "Viser andelen af annoncekroner brugt på annoncer \nmed personaliseret indhold for hvert parti")

ggsave("pers_indx_spend.png")

fb_ads2 %>%
  filter(str_detect(STEMMESEDDELNAVN, "Susanne Eilersen")) %>%
  select(ad_snapshot_url)

glimpse(fb_ads)
# dfm_parties_dict <- dfm_sort(dfm_parties_dict, margin = "both") #sorting dfm by both features and documents.
# # Virker vist også! Der er dog ikke mange indvandrere her.
# 
# # Eksporter!
# dfm_parties_dict <- convert(dfm_parties_dict, to = "matrix")
# 
# dfm_parties_dict_xt <- print(xtable(dfm_parties_dict), type = "latex")

### 6.c Parti vs. person ordbog resultater ###################################################################

# Konstruerer ordbøger

# Personfokus vs. partifokus

personfokus <- c("jeg", "mig", "min", "mit", "mine", "personlig*") #kræver at stopord IKKE er sorteret fra!
partifokus_A <- c("socialdemokrati*", "vi", "os", "vores") # skal anvendes på corupus_A


aktørindeks_dict <- dictionary(list(
  parti = c("parti", "partiet*"), # uden "vi". Undgår partier eller partiernes (flertal)
  kandidat = c("jeg", "mig", "min", "mit", "mine", "personlig*") # kwic viser, at det fungerer fint!
))


# kwic viser, at parti-ordbogen med "vi", "os", og "vores" kan henvise til partiet lige så vel som til os "danskere", vores "generation", vores "børnebørn"
# Kun 271 annoncer har brugt ordet "parti". "Vi" og "vores dur nok ikke, men overvej "os" - se evt 50 tilfældige og tjek, hvor mange, der henviser til partiet.
  # Resultat = LANGT de fleste annoncer med "os" henviser til "os" danskere.

# aktørindeks_dict['parti']
kwic(corpus_body, pattern = phrase("la"), window = 4)

kwic(corpus_Ø, pattern = "el", window = 5)


kwic(corpus_body, aktørindeks_dict['parti'], window = 5) %>% # For at se privatliv-dictionary in-context!
    as_tibble() %>%
    select(pre:post) %>%
    sample_n(50) %>% # tilfældigt udtræk
    print(n=50)

## 7. Corpus subset: parti-ordbog  =================================

# Laver et særskilt corpus for hvert parti.
corpus_A <- corpus_subset(corpus_body, PARTI == "A") # 2181 ads.
corpus_B <- corpus_subset(corpus_body, PARTI == "B") # 1113 ads.
corpus_C <- corpus_subset(corpus_body, PARTI == "C") # 2035 ads.
corpus_D <- corpus_subset(corpus_body, PARTI == "D") # 245 ads.
corpus_E <- corpus_subset(corpus_body, PARTI == "E") # 546 ads.
corpus_F <- corpus_subset(corpus_body, PARTI == "F") # 1266 ads.
corpus_I <- corpus_subset(corpus_body, PARTI == "I") # 1923 ads.
corpus_K <- corpus_subset(corpus_body, PARTI == "K") # 158 ads.
corpus_N <- corpus_subset(corpus_body, PARTI == "N") # 51 ads.
corpus_O <- corpus_subset(corpus_body, PARTI == "O") # 511 ads.
corpus_P <- corpus_subset(corpus_body, PARTI == "P") # 9 ads.
corpus_V <- corpus_subset(corpus_body, PARTI == "V") # 3053 ads.
corpus_Ø <- corpus_subset(corpus_body, PARTI == "OE") # 495 ads.
corpus_Å <- corpus_subset(corpus_body, PARTI == "AA") # 304 ads.

# Laver en dictionary med partinavne
dict_parti <- dictionary(list(
  A = c("socialdemokrat*"),
  B = c("radikal*", "rv*"), #enkelte fejl på b, men det henviser hovedsageligt til liste b.
  C = c("konservativ*"),
  D = c("nye borgerlige*"),
  E = c("klaus riskær pedersen"),
  F = c("sf*", "socialistisk folkeparti*"), # skift navn hvis F <- giver problemer
  I = c("liberal alliance*", "la", "las", "la'S"), # undgår * på la fordi det ellers fanger [LA]ngt mere end ventet.
  K = c("kristendemokrater*"),
  N = c("folkebevægelsen*"), # bruges kun om folkebevægelsen mod eu.
  O = c("dansk folkeparti*", "df*"),
  P = c("stram kurs*"),
  V = c("venstre", "venstres"), # NB! denne vil fange alle gange "radikale venstre" bliver nævnt. Men det er ikke et problem,
                     # når jeg kun bruger den på et subset af venstre annoncer.
  OE = c("enhedslisten*", "el"), 
  AA = c("alternativet*")
))

kwic(corpus_Å, dict_parti['AA'], window = 4) # Der er en del negative kamapgner, hvor S bliver nævnt af andre kandidater. 
# De negative kampagner sorteres fra, når jeg kun anvender de enkelte partiordbøger på partiet selv.
# Jeg har tjekket alle ordbøgerne og de fanger de annoncer de skal uden fejl.

# Tjekker om jeg kan tilføje "parti" til hver ordbog = Altså om partierne henviser til sig selv, når de anvender ordet:
kwic(corpus_A, "parti", window = 4) # Ja det kan jeg gdt. Annoncer, som nævner "parti" handler om partiet selv. Gør det senere.

# Anvender parti-ordbøgerne på de enkelte partiers annoncer
dfm_A <- dfm(corpus_A, dictionary = dict_parti['A'])
dfm_B <- dfm(corpus_B, dictionary = dict_parti['B'])
dfm_C <- dfm(corpus_C, dictionary = dict_parti['C'])
dfm_D <- dfm(corpus_D, dictionary = dict_parti['D'])
dfm_E <- dfm(corpus_E, dictionary = dict_parti['E'])
dfm_F <- dfm(corpus_F, dictionary = dict_parti['F'])
dfm_I <- dfm(corpus_I, dictionary = dict_parti['I'])
dfm_K <- dfm(corpus_K, dictionary = dict_parti['K'])
dfm_N <- dfm(corpus_N, dictionary = dict_parti['N'])
dfm_O <- dfm(corpus_O, dictionary = dict_parti['O'])
dfm_P <- dfm(corpus_P, dictionary = dict_parti['P'])
dfm_V <- dfm(corpus_V, dictionary = dict_parti['V'])
dfm_OE <- dfm(corpus_Ø, dictionary = dict_parti['OE'])
dfm_AA <- dfm(corpus_Å, dictionary = dict_parti['AA'])

# Laver en df med oversigt over, i hvor mange ads, partierne nævner dem selv, og hvor mange gange de nævner dem selv i alt.
stats_parti <- bind_rows(
  textstat_frequency(dfm_A),
  textstat_frequency(dfm_B),
  textstat_frequency(dfm_C),
  textstat_frequency(dfm_D),
  textstat_frequency(dfm_E),
  textstat_frequency(dfm_F),
  textstat_frequency(dfm_I),
  textstat_frequency(dfm_K),
  textstat_frequency(dfm_N),
  textstat_frequency(dfm_O),
  textstat_frequency(dfm_P),
  textstat_frequency(dfm_V),
  textstat_frequency(dfm_OE),
  textstat_frequency(dfm_AA))

# Fjerner overflødige kolonner
stats_part <- stats_parti %>%
  select(-rank,-group)

# Tilføjer en ny kolonne: Antal annoncer i alt (med tekstindhold)

stats_parti <- stats_parti %>%
  mutate(n_ads = case_when(
    feature == "A" ~ ndoc(corpus_A),
    feature == "B" ~ ndoc(corpus_B),
    feature == "C" ~ ndoc(corpus_C),
    feature == "D" ~ ndoc(corpus_D),
    feature == "E" ~ ndoc(corpus_E),
    feature == "F" ~ ndoc(corpus_F),
    feature == "I" ~ ndoc(corpus_I),
    feature == "K" ~ ndoc(corpus_K),
    feature == "N" ~ ndoc(corpus_N),
    feature == "O" ~ ndoc(corpus_O),
    feature == "P" ~ ndoc(corpus_P),
    feature == "V" ~ ndoc(corpus_V),
    feature == "OE" ~ ndoc(corpus_Ø),
    feature == "AA" ~ ndoc(corpus_Å),
  ))

# Tilføjer nye kolonner: 

stats_parti <- stats_parti %>%
  mutate(share_ads = round(docfreq/n_ads,2), # andel annoncer hvor partiet nævnes
         mean_freq = round(frequency/n_ads,2)) # bliver i gennemsnit nævnt så mange gange per annonce

# Sorter efter andel annoncer, hvor partiet nævnes

stats_parti <- stats_parti %>%
  arrange(desc(share_ads))

stats_parti

# Merger resultaterne af parti-ordbogen på fb_ads
  # konverterer dfm's til dataframes først

df_A <- convert(dfm_A, to = "data.frame")
df_B <- convert(dfm_B, to = "data.frame")
df_C <- convert(dfm_C, to = "data.frame")
df_D <- convert(dfm_D, to = "data.frame")
df_E <- convert(dfm_E, to = "data.frame")
df_F <- convert(dfm_F, to = "data.frame")
df_I <- convert(dfm_I, to = "data.frame")
df_K <- convert(dfm_K, to = "data.frame")
df_N <- convert(dfm_N, to = "data.frame")
df_O <- convert(dfm_O, to = "data.frame")
df_P <- convert(dfm_P, to = "data.frame")
df_V <- convert(dfm_V, to = "data.frame")
df_OE <- convert(dfm_OE, to = "data.frame")
df_AA <- convert(dfm_AA, to = "data.frame")
# liste over dataframes
list_df_parti <- list(df_A, df_B, df_C, df_D, df_E, df_F, df_I, df_K, df_N, df_O, df_P, df_V, df_OE, df_AA)

# parti_df <- join_all(list_df_parti, by = "document", type = "full", match = "all") 
# plyr ødelægger min kode andre steder, bruger purrr i stedet.
parti_df <- list_df_parti %>% 
  reduce(full_join, by = "document") 

glimpse(parti_df)
# Virker fint. cellerne er kun udfyldt i kombinationer af samme parti (række) og partiets ordbog (kolonne)
dim(parti_df) # 13.890. Alle ads er med. Fint.

# Laver en samlet variabel til at merge på fb_ads, som viser, hvor mange gange eget parti er nævnt i en annonce:
parti_df <- parti_df %>%
  mutate(parti_selv_freq = rowSums(.[,2:15],na.rm = T))


# Ser fordelingen over antal gange et parti nævner sig selv
parti_selv_freq <- parti_df %>%
  count(parti_selv_freq)

# bar chart
parti_selv_freq %>%
  filter(parti_selv_freq > 0) %>% # sorterer de 10.582 0'er fra
  ggplot(aes(x = parti_selv_freq, y = n)) +
  geom_bar(stat = "identity") + 
  scale_x_continuous(breaks = 1:6, limits = c(0, 6))
  #scale_y_continuous(breaks = seq(0, 6000, 200), limits = c(0,6000))

# Merger med fb_ads
dim(fb_ads) # 14.190 og 41. Tilføjer 1 kolonne

fb_ads <- fb_ads %>%
  left_join(select(parti_df, document, parti_selv_freq), by = c("ad_id" = "document"))

fb_ads %>%
  filter(PARTI == "OE") %>%
  select(ad_id, parti_selv_freq) %>%
  head()

parti_df %>%
  filter(!is.na(OE)) %>%
  select(document, OE) %>%
  head()

# Succes!

# Antal gange et parti nævner sig selv i en ad
fb_ads %>%
  count(parti_selv_freq) # Bemærk at der er 0 NA's nu. Der er 300 når jeg sorterer ads uden tekst fra.

# Fordelt på kandidat- og partisider.
fb_ads %>%
  group_by(kandidat_d) %>%
count(parti_selv_freq)

# Laver en dikotomiseret version af variablen

fb_ads <- fb_ads %>%
  mutate(parti_selv_d = if_else(parti_selv_freq>0, 1, 0))

fb_ads %>%
  count(parti_selv_d) # eget parti nævnt i 2.934 ud af 14.190 ads.

# Og opdelt på kandait- og partisider:
fb_ads %>%
  group_by(PARTI) %>%
  count(parti_selv_d)

fb_ads %>%
  group_by(PARTI) %>%
  count(parti_selv_freq) %>%
  print(n=100) # ser fint ud!


fb_ads %>%
  filter(PARTI == "V") %>%
  count(parti_selv_freq) %>%
  print(n=100) # korrekt!


### 7. Corpus subset: jeg-ordbog  ############################################

# FORÆLDET: BRUGTE KUN "JEG" i ORDBOGEN

# # Bruger jeg-ordbogen og ser resultater
# # jeg_dict <- dictionary(list(
# #   jeg = "jeg"))
# dfm_jeg <- dfm(corpus_body, dictionary = jeg_dict)
# stats_jeg <- textstat_frequency(dfm_jeg, groups = "PARTI")
# 
# # OBS! rodet kode. denne funktion virker kun, hvis jeg danner stats_parti først (neden for)
# stats_jeg <- stats_jeg %>%
#   left_join(select(stats_parti, feature, n_ads), by = c("group" = "feature"))
# 
# stats_jeg <- stats_jeg %>%
#   arrange(desc(share_ads)) # begynder at ligne noget
# 
# stats_jeg <- stats_jeg %>%
#   mutate(share_ads = round(docfreq/n_ads,2), # andel annoncer hvor partiet nævnes
#          mean_freq = round(frequency/n_ads,2)) # bliver i gennemsnit nævnt så mange gange per annonce
# 
# # Sorter efter andel annoncer, hvor partiet nævnes
# stats_jeg <- stats_jeg %>%
#   arrange(desc(share_ads))
# 
# stats_parti
# stats_jeg

##### NY KANDIDATORDBOG ##########################################

# Opdateret kandidat-ordbog.
aktør_dict <- dictionary(list(
  parti = c("parti", "partiet*"), # uden "vi", "os", "vores". Undgår partier eller partiernes (flertal)
  kandidat = c("jeg", "mig", "min", "mit", "mine", "personlig*") # kwic viser, at det fungerer fint!
))

# 1. Kandidatordbog for sig
dfm_kandidat <- dfm(corpus_body, dictionary = aktør_dict['kandidat'])
stats_kandidat <- textstat_frequency(dfm_kandidat, groups = "PARTI")

# tilføjer antal annoncer per parti fra stats_parti df
stats_kandidat <- stats_kandidat %>%
  left_join(select(stats_parti, feature, n_ads), by = c("group" = "feature"))

stats_kandidat <- stats_kandidat %>%
  mutate(share_ads = round(docfreq/n_ads,2), # andel annoncer hvor kandidat nævnes
         mean_freq = round(frequency/n_ads,2)) # bliver i gennemsnit nævnt så mange gange per annonce

# 2. Parti-ordbog for sig
dfm_partier <- dfm(corpus_body, dictionary = aktør_dict['parti'])
stats_partier <- textstat_frequency(dfm_partier, groups = "PARTI")

# tilføjer antal annoncer per parti fra stats_parti df
stats_partier <- stats_partier %>%
  left_join(select(stats_parti, feature, n_ads), by = c("group" = "feature"))

stats_partier <- stats_partier %>%
  mutate(share_ads = round(docfreq/n_ads,2), # andel annoncer hvor partier
         mean_freq = round(frequency/n_ads,2)) # bliver i gennemsnit nævnt så mange gange per annonce

# 3. Merger stas_kandidat og stats_partier med stats_parti = danner stats_aktør
  # skifter docfreq kolonnenavn undervejs, så den bliver unik og kan merges.

stats_aktør <- stats_parti %>%
  # kandidat
  left_join(select(rename(stats_kandidat, docfreq_kandidat = docfreq), group, docfreq_kandidat), by = c("feature" = "group")) %>%
  mutate(docfreq_kandidat = replace_na(docfreq_kandidat, 0)) # erstatter Stram Kurs' NA med 0.

# parti
stats_aktør <- stats_aktør %>%
  left_join(select(rename(stats_partier, docfreq_partier = docfreq), group, docfreq_partier), by = c("feature" = "group")) %>%
  mutate(docfreq_partier = replace_na(docfreq_partier, 0)) # erstatter NA med 0
  
dim(stats_aktør) # 14, 10 = 2 kolonner tilføjet. fint!

# Skrumper stats_aktør lidt
stats_aktør <- stats_aktør %>%
  select(-rank, -group, -share_ads, -mean_freq)

# JEG ANVENDER KUN docfreq (partier der nævner dem selv) og IKKE doc_freq partier (partier, der nævner partier) neden for.
  # fordi de to parti-ordbøger kan ikke lægges sammen. Så skal jeg merge dem på annonce niveau først (i fb_ads).

#Udregner nyt aktør-indeks: Antal kandidatannoncer divideret med antal partiannoncer. Højere værdi = kandidaterne selv i fokus
stats_aktør <- stats_aktør %>%
  mutate(aktør_indeks = docfreq_kandidat/docfreq)

# arrangerer efter aktør-indekset
stats_aktør <- stats_aktør %>%
  arrange(desc(aktør_indeks))

stats_aktør
# Stik mod forventningerne nævner har Ø kandidatfokus i 17 gange så mange annoncer som de har partifokus. = Højt personaliseret. Venstre har 1:1
# Men her har jeg ikke skelnet mellem partisider og kandidat-sider etc. Det kan jeg gøre, når jeg merger det på fb_ads

### merger jeg-resultaterne på fb_ads som freq og som dummy ###

# Konverterer dfm til df
df_kandidat <- convert(dfm_kandidat, to = "data.frame")
df_kandidat <- rename(df_kandidat, jeg = kandidat) # ændrer navn til 'jeg'


# Ser fordelingen over antal gange ads henviser til en kandidat
df_kandidat %>%
  count(kandidat) # 7.358 ads = 0.

# Merger med fb_ads
dim(fb_ads) # 14.190 og 43. Tilføjer 2 kolonner

glimpse(df_kandidat)

fb_ads <- fb_ads %>%
  left_join(df_kandidat, by = c("ad_id" = "document")) %>%
  mutate(jeg_d = if_else(jeg>0, 1, 0))
dim(fb_ads) # 14.190 og 45. fint.

# Er de gensidigt udelukkende? Ja det kunne godt tyde på det!!! Kun 136 (NY: 181) parti-sider skriver "jeg" (3,61 %)
fb_ads %>%
  group_by(kandidat_d) %>%
  count(jeg_d)

# I 2.061 (NY: 2.549) af kandidaternes annoncer bliver deres parti nævnt. I 8.321 (NY 7.833) gør det ikke (24,77 %)
# Andelen af parti-sider, som nævner deres eget navn, er lidt større: 1.059 ud af 3.808 (27,8 %)
fb_ads %>%
  group_by(kandidat_d) %>%
  count(parti_selv_d)

# Afgørende spørgsmål ift. gensidigt udelukkende: Hvor mange af annoncerne nævner begge dele?
fb_ads %>%
  filter(parti_selv_d == 1 & jeg_d == 1) %>%
  group_by(kandidat_d) %>%
  count()# Det gør kun 1.049 (NU 1.670) ud af 14.190. Det er da meget godt?
  # heraf er 75 parti-sider og 1.595 kandidat-sider

# og ingen af delene?
fb_ads %>%
  filter(parti_selv_d == 0 & jeg_d == 0) %>%
  group_by(kandidat_d) %>%
  count() # Det gør 5.420 ud af 14.190.
# heraf er 2.850 (nu 2.643) parti-annoncer og 3.774 kandidat-annoncer (nu 2.777)

# Hvor mange ads nævner det ene?
fb_ads %>%
  filter(jeg_d == 1) %>%
  count() # 6.832

### Merger afsender- og aktørindeks (se dec_analyse for afsenderindekset)
aktør_afsender <- stats_aktør %>%
  select(feature, n_ads, docfreq, docfreq_kandidat, docfreq_partier, aktør_indeks) %>%
  left_join(afsender_indeks, by = c("feature" = "PARTI"))

# write_csv(aktør_afsender, "indeks.csv")

# indeks_resultat <- read_csv("indeks.csv")

indeks_resultat <- aktør_afsender %>%
  # Jeg sætter den højeste værdi i nævneren så alle værdier er mellem 0-1
  mutate(akt_indx = case_when(
    docfreq_kandidat < docfreq ~ docfreq_kandidat/docfreq,
    docfreq_kandidat > docfreq ~ docfreq/docfreq_kandidat,
    TRUE ~ NA_real_),
         afs_indx = case_when(
    n_kandidat < n_parti ~ n_kandidat/n_parti,
    n_kandidat > n_parti ~ n_parti/n_kandidat,
    TRUE ~ NA_real_)
    )
    
# Partier med flere parti-annoncer end kandidat-annoncer gør giver jeg negative værdier.
indeks_resultat <- indeks_resultat %>%
  mutate(akt_indx = case_when(
    docfreq_kandidat < docfreq ~ -abs(akt_indx),
    TRUE ~ akt_indx),
    afs_indx = case_when(
      n_kandidat < n_parti ~ -abs(afs_indx),
      TRUE ~ afs_indx)
  )

# fint! Nu er den skaleret fra -1 til 1 med høje værdier = mere personalisering.
indeks_resultat

# tilføjer lige det originale afsender_indeks igen (ved ikke hvorfor det ikke er der) og så gemmer jeg resultatet [på trods af at ordbog_partier mangler]
indeks_resultat <- indeks_resultat %>%
  mutate(afsender_indeks = n_kandidat/ n_parti)

# ændrer rækkefølge af kolonner, så afsender er først og aktør sidst:
indeks_resultat1 <- indeks_resultat[c("feature","n_kandidat", "n_parti", "afsender_indeks", "afs_indx", "docfreq_kandidat", "docfreq", "aktør_indeks", "akt_indx")]

# write_csv(indeks_resultat1, "indeks_resultat1.csv")

cor(indeks_resultat$akt_indx, indeks_resultat$afs_indx) # -0.01 (altså ingen korrelation). før jeg ændrede parti-ordbøgerne var den 0.0995847. 
# Det var 0.58 inden jeg skalerede den om til -1 til 1 (hvor jeg fjernede Folkebevægelsen og Stram Kurs pga. inf.) ødelægger det korrelationen?
cor(indeks_resultat$aktør_indeks, indeks_resultat$akt_indx) # JA! dette er det samme indeks, men korrelatioen går i stykker når jeg skalerer den på den måde?

# Men! Jeg har ikke vægtet resultaterne ift. spend endnu. 

#### Vægter på spend ####################################################################################################

glimpse(fb_ads)

# Tilføjer "partier" ordbogen til fb_ads så jeg kan samle de to ordbøger
  # 1) partierne nævner deres eget navn (parti_selv)
  # 2) partierne nævner "parti". 
  # Merger dem sammen på en ny mit_parti_d (så jeg ikke forveksler den med parti_d, som viser, om det er en partiside eller ej)


# Konverterer først dfm til df
df_partier <- convert(dfm_partier, to = "data.frame")
df_partier <- rename(df_partier, partier_ordbog = parti) # ændrer navn til partier_ordbog, så det er til at holde styr på.


# Merger med fb_ads
dim(fb_ads) # 14.190 og 45. Tilføjer 2 kolonner

glimpse(df_partier)

fb_ads <- fb_ads %>%
  left_join(df_partier, by = c("ad_id" = "document")) %>%
  mutate(mit_parti_d = if_else(partier_ordbog>0, 1, 0))
dim(fb_ads) # 14.190 og 47. fint.

# NU kan jeg lave en samlet parti-dummy:

  # Ser først, om der er overhovedet er partier, som nævner "parti" uden partiets navn
fb_ads %>%
  filter(mit_parti_d == 1 & parti_selv_d == 1) %>%
  nrow() # jeg får 211 annoncer mere til parti dummyen (ud af 319 fra partier_ordbogen). Fint!

# SAMLET PARTI-DUMMY

fb_ads <- fb_ads %>%
  mutate(parti_selv_d_samlet = replace(parti_selv_d, mit_parti_d == 1, 1))

# tester om det virker

fb_ads %>%
  filter(parti_selv_d == 1 | mit_parti_d == 1) %>%
  nrow() # 3.716

fb_ads %>%
  filter(parti_selv_d_samlet == 1) %>%
  nrow() # 3.716. Det virker.

# write_rds(fb_ads, "fb_ads2.rds") gemmer fb_ads med nyeste Quanteda resulteter 06.12 kl. 15.30.
