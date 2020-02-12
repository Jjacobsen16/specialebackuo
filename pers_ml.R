# 1. Forberedelse til Quanteda ---------------------------------
## 1.1 Indlæser pakker og data =================================

library(pacman)
p_load(tidyverse, quanteda, xtable, tictoc, caret, readxl, estimatr) # måske lubridate, topicmodels, readtext, agendacodeR


fb_ads <- read_rds("FB API data/data/fb_ads.rds")

## 1.2 Tjekker data ============================================

# Undersøger populationen af kandidat-annoncer med tekstmæssigt unikt indhold
fb_ads %>%
  filter(!ad_creative_body =="") %>% # sorterer 2 tekstmæssigt tomme annoncer fra
  # filter(kandidat_d == T) %>% # Bruger kun personlige sider, ikke parti-sider
  distinct(ad_creative_body) %>% # Beholder kun tekstmæssigt unikke kandidat-annoncer 
  nrow() # Det giver n=5.518. Det er denne population jeg tager en stikprøve af i udvælgelsen af training data. N=7.172 inkl. partisider.

# Tjekker hvor mange tekstmæssigt unikke annoncer hvert partis kandidater har kørt:
fb_ads %>%
  filter(!ad_creative_body =="") %>% 
  filter(kandidat_d == T) %>% 
  distinct(ad_creative_body, .keep_all = T) %>%
  group_by(PARTI) %>%
  count() %>%
  arrange(n)
# Det strækker sig fra 39 (Partiet Klaus Riskær Pedersen) til 1.072 (Venstre).

# Laver et corpus med alle annoncer i fb_ads, fuld population
corpus_data <- dplyr::select(fb_ads, c(ad_id, ad_id_copy, ad_creative_body, page_name, PARTI, parti_navn, KØN, ALDER, spend_mid, spend_interval, 
                                       impressions_mid, impressions_interval, kandidat_d, facebook, instagram)) #pers_d)) # OBS! pers_d skal laves først. og tilføjes til fb_ads

corpus_body <- corpus(corpus_data, 
                      docid_field = "ad_id_copy", # sætter dokumentid = ad_id
                      text_field = "ad_creative_body")

# 2. Udvælger stikprøve til manuel kodning ---------------------------------

set.seed(6867) # For replicerbarhed 6867

### SIMPEL TILFÆLDIG UDVÆLGELSE ###

# Her laver jeg et andet udtræk vha. simpel tilfældig udvælgelse, så stikprøven bliver repræsentativ for populationen af annoncer.
# Lige med det OBS at jeg undgår dubletter, som der selvfølgelig er i populationen.
# random_id <- fb_ads %>%
#   filter(!ad_creative_body =="") %>% # undgår 2 tekstmæssigt tomme annoncer.
#   # filter(kandidat_d == T) %>% # Fjernet! Jeg medtager alle annoncer, også partisider.
#   distinct(ad_creative_body, .keep_all = T) %>% # Medtager kun tekstmæssigt unikke annoncer
#   sample_n(1000) %>% # Koder 1.000 til at starte med.
#   pull(ad_id)
# 
# # Trækker de 1000 annoncer ud som en Excel-fil, så jeg kan kode dem manuelt.
# random1000 <- fb_ads %>%
#   filter(ad_id %in% random_id) %>%
#   select(ad_id, kandidat_d, ad_creative_body, starts_with("ad_creative_link"))
# write_excel_csv(random1000, "random1000.csv") # Eksporterer til csv
# # OBS! jeg koder IKKE parti-siderne. Kun de 756 kandidat-sider.

# 3. Inddeler i training og test set og laver CORPUSES ---------------------------------

# Importerer det kodede training subset med 756 kodede annoncer
kodet1000 <- read_excel("random1000 kodet.xlsx")

# Beholder kun kandidatannoncer
kodet756 <- kodet1000 %>%
  filter(kandidat_d == 1) %>% # Resulterer i 756 annoncer.
  mutate(ad_id_copy = ad_id) # kopierer id variablen.

# Labelled subset med alle 756 kodede annoncer
label756_corpus<- corpus(kodet756, docid_field = "ad_id_copy", # sætter dokumentid = ad_id
                         text_field = "ad_creative_body")
# Training subset 500 annoncer
training500_corpus <- corpus(sample_n(kodet756, 500), docid_field = "ad_id_copy", # sætter dokumentid = ad_id
                             text_field = "ad_creative_body")
# Test subset 256 annoncer
test256_corpus <- corpus_subset(label756_corpus, !ad_id %in% docvars(training500_corpus, "ad_id"))

# Rest subset med 13.434 annoncer, som ikke er labelled manuelt
nolabel_corpus <- corpus_subset(corpus_body, !ad_id %in% pull(kodet756, ad_id))
ndoc(nolabel_corpus) # 13.434. De 756 annoncer er fratrukket.

# Tjekker andelen af personaliserede annoncer i de to sæt (tjekker randomiseringen)
prop.table(table(docvars(training500_corpus, "selvpersonaliseret")))*100 # 31.4 pct. selvpersonaliseret
prop.table(table(docvars(test256_corpus, "selvpersonaliseret")))*100 # 35.9 pct. selvpersonaliseret, udmærket.
# randomiseringen er repliceret. 

# 3. TEST OG TRAINING DFM ---------------------------------

# Laver en training dfm med ALLE manuelt kodede annoncer
training756_dfm <- dfm(label756_corpus,
                       remove_numbers = T,
                       remove_punct = T)
nfeat(training756_dfm) # 9447
training756_dfm <- dfm_wordstem(training756_dfm, language = "da")
nfeat(training756_dfm) # 7065, fint.

# Rest dfm med alle andre annoncer, som skal kodes af maskinen:
nolabel_dfm <- dfm(nolabel_corpus,
                   remove_numbers = T,
                   remove_punct = T) %>%
  dfm_wordstem(language = "da")
nfeat(nolabel_dfm) # 19.689

# TRAINING set med 500 annoncer

training500_dfm <- dfm(training500_corpus,
                       remove_numbers = T,
                       remove_punct = T) %>%
  dfm_wordstem(language = "da")
nfeat(training500_dfm) # 5492, fint.

# TEST set med 256 annoncer

test256_dfm <- dfm(test256_corpus,
                   remove_numbers = T,
                   remove_punct = T) %>%
  dfm_wordstem(language = "da")
nfeat(test256_dfm) # 3548

# Træner modellen på de 500 annoncer i training set
NB500 <- textmodel_nb(training500_dfm, docvars(training500_dfm, "selvpersonaliseret"), prior = "docfreq")
# Med prior antager jeg en fordeling af personaliserede/ ikke personaliserede lig den i training set. Fin antagelse.
summary(NB500)

# Tjekker in-sample performance
# ... altså hvor mange af de 500 kodede annoncer, den ville kode rigtigt.
prop.table(table(predict(NB500) == docvars(training500_dfm, "selvpersonaliseret"))) * 100 # Resultat: 97.2 % rigtige. ligegyldigt.
# - forventeligt, når det jo er de annoncer, modellen er trænet på.

# Sammenligner det med et tilfældigt resultat, hvor jeg kun fortæller algoritmen fordelingen mellem selv-personaliserede, og ikke selvpersonaliserede annoncer.
prop.table(table(sample(predict(NB500)) == docvars(training500_dfm, "selvpersonaliseret"))) * 100 # Resulat: 57.6 % rigtige.
# FALSE 38 %, TRUE 62 % ligegyldigt.

# 4. TESTER PERFORMANCE PÅ TEST SET ---------------------------------

# NB can only take features into consideration that occur both in the training set and the test set
# tilføjer derfor alle ord til en samlet dfm
dfm_matched256 <- dfm_match(test256_dfm, features = featnames(training500_dfm)) # OMVENDT I FORHOLD TIL DOCUMENTATION!!!
ndoc(dfm_matched256) # 256 rækker fra test set. 
nfeat(dfm_matched256) # 5492. Alle ord fra training set er inkluderet.

# Laver en confusion matrix for at tjekke, hvordan algoritmen klarede sig.
# De 256 manuelt kodede annoncer
actual_class256 <- docvars(dfm_matched256, "selvpersonaliseret") # indeholder kun de 256 fra test data

# Maskinens prediction af de 256 annoncer
predicted_class256 <- predict(NB500, newdata = dfm_matched256, force = F) # newdata = "dfm on which prediction should be made". 
tab_class256 <- table(actual_class256, predicted_class256) # VIRKER KUN HVIS DE HAR SAMME LÆNGE (500 vs. 256).
tab_class256
# Resultat: 145 TRUE NEGATIVES, 42 TRUE POSITIVES, 50 FALSE NEGATIVES og 19 FALSE POSITIVES.

# Laver en confusion matrix med en masse stats om resultatet
# ... kan også bruges til visualisering senere.
confusion256 <- confusionMatrix(tab_class256, mode = "everything")
confusion256
# Precision: 88 %
# Recall: 74 %
# F1: 81 %
# Ikke dårligt!

## 4.1 Ekstra test: Er performance bedre med et dikotomiseret training set? =====
predicted_class256_01 <- predict(NB500_01, newdata = dfm_matched256, force = F)
tab_class256_01 <- table(actual_class256, predicted_class256_01)
tab_class256_01
# Resultat: 159 TRUE NEGATIVES, 22 TRUE POSITIVES, 70 FALSE NEGATIVES og 5 FALSE POSITIVES
#... med andre ord et langt dårligere resultat når det er dikotomiseret.
#... fortsætter med count matrix.

# 5. Fuldt træningssæt 756 og kodning af resten af data --------------

# FITTER EN NY NB MODEL BASERET PÅ HELE TRAINING SET - OG LADER DEN KØRE PÅ RESTEN AF DATASÆTTET!
NB756 <- textmodel_nb(training756_dfm, docvars(training756_dfm, "selvpersonaliseret"), prior = "docfreq")

# Tjekker hvilke ord, der ifølge modellen skaber størst sandsynlighed for at annoncen er personaliseret.
ord_coef <- as.data.frame(coef(NB756)) %>%
  rownames_to_column("ord") %>%
  arrange(desc(`1`)) # "mærkesag" på 4. pladsen.

# Laver en dfm for de resterende 13.434 annoncer, som skal kodes af maskinen.
  # NB can only take features into consideration that occur both in the training set and the test set
ndoc(training756_dfm) # 756 rows
nfeat(training756_dfm) # 7065 ord
ndoc(nolabel_dfm) # 13.434
nfeat(nolabel_dfm) # 19.689
dfm_matched_rest <- dfm_match(nolabel_dfm, features = featnames(training756_dfm)) #
ndoc(dfm_matched_rest) # 13.434 rækker. dem, der skal predictes.
nfeat(dfm_matched_rest) # 7065. Alle ord, som optræder i training OG test set beholder vi. Giver mening!

predict_nolabel <- predict(NB756, newdata = dfm_matched_rest) # virker!


# 6. Merger resultaterne med fb data og gemmer --------------
# Laver en dataframe med predicted resultater.
predict_nolabel_df <- as.data.frame(predict_nolabel) %>%
  rownames_to_column("ad_id") %>% 
  rename(selvpersonaliseret = predict_nolabel) %>% # omdøber så den er klar til merge. 
  # OBS! R gemmer faktor variabel som 1 og 2. Konverterer det til 0 og 1 i neden for!
  mutate(selvpersonaliseret = as.numeric(selvpersonaliseret)) %>%
  mutate(selvpersonaliseret = case_when(selvpersonaliseret == 2 ~ 1,
                                        selvpersonaliseret == 1 ~ 0,
                                        T ~ NA_real_))

# Samler de to (predicted og labelled) i en df, så den kan merges med fb_ads
selvpersonaliseret <- bind_rows(predict_nolabel_df, select(kodet756, ad_id, selvpersonaliseret))
glimpse(selvpersonaliseret) # alle 14.190 annoncer. Fint!

# gemmer lige selvpersonaliseret.
# write_csv(selvpersonaliseret, "pers_predicted.csv")

fb_ads <- fb_ads %>%
  left_join(selvpersonaliseret)


# 7. Analyse og signifikanstest ########################################

# Hvordan er fordelingen mellem partierne?
fb_ads %>%
  filter(kandidat_d == 1) %>% # kun blandt kandidaternes sider.
  group_by(PARTI) %>%
  summarise(n_ads = n(),
            n_pers = sum(selvpersonaliseret),
            share_pers = n_pers/n_ads) %>%
  arrange(share_pers) # VÆR SÅ GOD! Her har vi så resultatet .

# Og hvis jeg ser på ad spend i stedet for number of ads?
parti_resultat <- fb_ads %>%
  filter(kandidat_d == 1) %>%
  group_by(parti_navn) %>%
  summarise(spend = sum(spend_mid),
            spend_pers = sum(selvpersonaliseret*spend_mid),
            # spend_notpers = spend-spend_pers,
            share_pers_spend = spend_pers/spend) %>%
  arrange(desc(share_pers_spend))

# Visualisering af parti resultat
parti_resultat %>%
  ggplot(aes(x = reorder(parti_navn, spend), y = spend, fill = share_pers_spend)) + 
  geom_bar(position = "stack", stat = "identity") + coord_flip() + theme_minimal()

# Ser spændende ud!

# Lad os definere et par variable og udføre TESTEN: two-sample t-test.

# Centraliseret nomineringsproces
fb_ads <- fb_ads %>%
  mutate(centralisering = case_when(PARTI %in% c("A", "OE", "F", "O", "I", "D") ~ 1,
                                    PARTI %in% c("AA", "B", "V", "C", "K") ~ 0,
                                    T ~ NA_real_)) # E og N er unknown! - tjek deres hjemmesider. 
fb_ads %>%
  distinct(PARTI, centralisering)

# HV-placering (overvej at tilføje)
fb_ads <- fb_ads %>%
  mutate(Højreorienteret = case_when(PARTI %in% c("A", "F", "OE", "B", "AA", "N") ~ 0,
                                    PARTI %in% c("V", "C", "I", "K", "D", "E", "P") ~ 1,
                                    T ~ NA_real_)) # Klaus Riskær Parti kodes som højreorienteret
# Yderligere kontrolvariable, overvej (Pedernsen & VanHeerde 2019):
  # Seat share
  # Governing party
  # Rural district
glimpse(fb_ads)

# two-sample t-test
# Bruger estimatr-pakken

# VÆGTE: Jeg skal lige være sikker på, hvad de gør. De vægter vel hver enkelt observation, 
# så en med spend 0-100 (50) tæller 50, en med spend 100-500 (300) tæller 300 osv.
# Der skal således 6 ads af de billigste til at have lige så meget vægt som den næst dyreste.
# OG! Den DYRESTE ad (150k) tæller i princippet for 3.000 ads.
# Ja. Det giver fin mening.

# laver PARTI om til factor, så jeg kan ændre referencekategori
fb_ads <- fb_ads %>%
  mutate(PARTI = as.factor(PARTI))

lmrobust <- lm_robust(selvpersonaliseret ~ centralisering + KØN + ALDER + Højreorienteret, cluster = PARTI,
                    data = fb_ads, subset = kandidat_d == 1, weights = spend_midf, se_type = "stata")
  # Jeg vægter observationerne i forhold til annoncepris.
# Jeg er lidt i tvivl om jeg kan have både centralisering og parti med som predictors.  
# Jeg kan tage hv-placering i stedet (ideologi)


summary(lmrobust) # såå... Hypotesen er vist afkræftet. Jo mere centraliserede partierne er, 
# desto mere selvpersonaliserede er deres kandidaters annoncer! 
# NEJ. Det er et INSIGNIFIKANT resultat. 

# Hvis vi nu leger, at selvpersonaliseret = et politisk emne, hvordan vil testen så se ud?
# = Meget på samme måde!
lm_emnetest <- lm_robust(selvpersonaliseret ~ relevel(PARTI, ref = "v"), cluster = PARTI,
                         data = fb_ads, se_type = "stata", weights = spend_mid)
  # Brug evt. relevel(PARTI, ref = "v"), hvis der skal ændres referencekategori
  # højreorienteret er vel den egentlige test af hypotesen.

summary(lm_emnetest)

fb_ads %>%
  count(as.factor(spend_mid))

# Hvordan ser kønsfordelingen ud?
fb_ads %>%
  filter(kandidat_d == 1) %>%
  group_by(KØN) %>%
  summarise(n = n(),
            pers = sum(selvpersonaliseret),
            pers_share = pers/n)


# Model performance test ---------------------------------------------------------

# I dette afsnit tester jeg modellens test mere præcis. 2 formål:
  # 1) Kør flere identiske tests (fx 3) med forskellige training/ test sets og beregn et gennemsnit
  # 2) Vis at større training set = bedre performance

# [2] Nyt sample - ny randomisering
set.seed(6789)

# [2] Training subset 500 annoncer
training500_corpus2 <- corpus(sample_n(kodet756, 500), docid_field = "ad_id_copy", # sætter dokumentid = ad_id
                             text_field = "ad_creative_body")

# [3] Nyt sample - ny randomisering
set.seed(4849)

# [3] Training subset 500 annoncer
training500_corpus3 <- corpus(sample_n(kodet756, 500), docid_field = "ad_id_copy", # sætter dokumentid = ad_id
                              text_field = "ad_creative_body")

# [2] Test subset 256 annoncer
test256_corpus2 <- corpus_subset(label756_corpus, !ad_id %in% docvars(training500_corpus2, "ad_id"))
# [3] Test subset 256 annoncer
test256_corpus3 <- corpus_subset(label756_corpus, !ad_id %in% docvars(training500_corpus3, "ad_id"))

# [2] TRAINING set med 500 annoncer
training500_dfm2 <- dfm(training500_corpus2,
                       remove_numbers = T,
                       remove_punct = T) %>%
  dfm_wordstem(language = "da")
nfeat(training500_dfm2) # 5543

# [3] TRAINING set med 500 annoncer
training500_dfm3 <- dfm(training500_corpus3,
                        remove_numbers = T,
                        remove_punct = T) %>%
  dfm_wordstem(language = "da")
nfeat(training500_dfm3) # 5544

# [2] TEST set med 256 annoncer
test256_dfm2 <- dfm(test256_corpus2,
                   remove_numbers = T,
                   remove_punct = T) %>%
  dfm_wordstem(language = "da")
nfeat(test256_dfm2) # 3423

# [3] TEST set med 256 annoncer
test256_dfm3 <- dfm(test256_corpus3,
                    remove_numbers = T,
                    remove_punct = T) %>%
  dfm_wordstem(language = "da")
nfeat(test256_dfm3) # 3450

# Træner modellere
NB500.2 <- textmodel_nb(training500_dfm2, docvars(training500_dfm2, "selvpersonaliseret"), prior = "docfreq")
NB500.3 <- textmodel_nb(training500_dfm3, docvars(training500_dfm3, "selvpersonaliseret"), prior = "docfreq")

# [2] Matcher ordene i test dfm med dem i training dfm, så testen virker.
dfm_matched2 <- dfm_match(test256_dfm2, features = featnames(training500_dfm2)) 
ndoc(dfm_matched2) # 256 rækker fra test set. 
nfeat(dfm_matched2) # 5543. Alle ord fra training set er inkluderet.

# [3] Matcher ordene i test dfm med dem i training dfm, så testen virker.
dfm_matched3 <- dfm_match(test256_dfm3, features = featnames(training500_dfm3)) 
ndoc(dfm_matched3) # 256 rækker fra test set. 
nfeat(dfm_matched3) # 5544. Alle ord fra training set er inkluderet.

# [2] Model 2 performance på test set
actual_class2 <- docvars(dfm_matched2, "selvpersonaliseret") # indeholder kun de 256 fra test data
predicted_class2 <- predict(NB500.2, newdata = dfm_matched2, force = F) # newdata = "dfm on which prediction should be made". 
tab_class2 <- table(actual_class2, predicted_class2) # VIRKER KUN HVIS DE HAR SAMME LÆNGE (500 vs. 256).
tab_class2
# Laver confusion matrix og stats
confusion2 <- confusionMatrix(tab_class2, mode = "everything")
confusion2 # F1 = 82 %
# Resultat: 148 TRUE NEGATIVES, 42 TRUE POSITIVES, 47 FALSE NEGATIVES og 19 FALSE POSITIVES.
# Kommentar: Præcist resultat meget lig første kørsel. Overvej at bruge større training set, fx 80/20.


# [3] Model 3 performance på test set
actual_class3 <- docvars(dfm_matched3, "selvpersonaliseret") # indeholder kun de 256 fra test data
predicted_class3 <- predict(NB500.3, newdata = dfm_matched3, force = F) # newdata = "dfm on which prediction should be made". 
tab_class3 <- table(actual_class3, predicted_class3) # VIRKER KUN HVIS DE HAR SAMME LÆNGE (500 vs. 256).
tab_class3
# Laver confusion matrix og stats
confusion3 <- confusionMatrix(tab_class3, mode = "everything")
confusion3 # F1 = 82,58 %
# Resultat: 147 TRUE NEGATIVES, 47 TRUE POSITIVES, 40 FALSE NEGATIVES og 22 FALSE POSITIVES.
# Kommentar: Bedste model indtil videre. Resultat ikke langt fra de to første modeller.


## 80/20 TRAINING TEST PERFORMANCE ==========================================

# Nu til 80/20 tests

# Laver 3 forskellige tilfældige udtræk af 604 annoncer (80 %)

# [1]
set.seed(1000)
training604_corpus1 <- corpus(sample_n(kodet756, 604), docid_field = "ad_id_copy", # sætter dokumentid = ad_id
                              text_field = "ad_creative_body")

# [2] 
set.seed(2000)
training604_corpus2 <- corpus(sample_n(kodet756, 604), docid_field = "ad_id_copy", # sætter dokumentid = ad_id
                              text_field = "ad_creative_body")

# [3]
set.seed(3000)
training604_corpus3 <- corpus(sample_n(kodet756, 604), docid_field = "ad_id_copy", # sætter dokumentid = ad_id
                              text_field = "ad_creative_body")

# Laver 3 tilsvarende test sets (20 %)
test152_corpus1 <- corpus_subset(label756_corpus, !ad_id %in% docvars(training604_corpus1, "ad_id"))
test152_corpus2 <- corpus_subset(label756_corpus, !ad_id %in% docvars(training604_corpus2, "ad_id"))
test152_corpus3 <- corpus_subset(label756_corpus, !ad_id %in% docvars(training604_corpus3, "ad_id"))

# Laver 3 training sets (DFM)
training604_dfm1 <- dfm(training604_corpus1,
                        remove_numbers = T,
                        remove_punct = T) %>%
  dfm_wordstem(language = "da")
nfeat(training604_dfm1) # 5557

training604_dfm2 <- dfm(training604_corpus2,
                        remove_numbers = T,
                        remove_punct = T) %>%
  dfm_wordstem(language = "da")
nfeat(training604_dfm2) # 5346

training604_dfm3 <- dfm(training604_corpus3,
                        remove_numbers = T,
                        remove_punct = T) %>%
  dfm_wordstem(language = "da")
nfeat(training604_dfm3) # 5389

# Laver 3 test sets (DFM)
test152_dfm1 <- dfm(test152_corpus1,
                    remove_numbers = T,
                    remove_punct = T) %>%
  dfm_wordstem(language = "da")
nfeat(test152_dfm1) # 3402

test152_dfm2 <- dfm(test152_corpus2,
                    remove_numbers = T,
                    remove_punct = T) %>%
  dfm_wordstem(language = "da")
nfeat(test152_dfm2) # 3694

test152_dfm3 <- dfm(test152_corpus3,
                    remove_numbers = T,
                    remove_punct = T) %>%
  dfm_wordstem(language = "da")
nfeat(test152_dfm3) # 3625

# Træner modellerne
NB604.1 <- textmodel_nb(training604_dfm1, docvars(training604_dfm1, "selvpersonaliseret"), prior = "docfreq")
NB604.2 <- textmodel_nb(training604_dfm2, docvars(training604_dfm2, "selvpersonaliseret"), prior = "docfreq")
NB604.3 <- textmodel_nb(training604_dfm3, docvars(training604_dfm3, "selvpersonaliseret"), prior = "docfreq")

# Matcher ordene i test dfm med dem i training dfm
dfm_match_8020_1 <- dfm_match(test152_dfm1, features = featnames(training604_dfm1)) 
dfm_match_8020_2 <- dfm_match(test152_dfm2, features = featnames(training604_dfm2)) 
dfm_match_8020_3 <- dfm_match(test152_dfm3, features = featnames(training604_dfm3)) 


# Model performance resultater

  # Model 1
actual_8020_1 <- docvars(dfm_match_8020_1, "selvpersonaliseret") # indeholder kun de 256 fra test data
predicted_8020_1 <- predict(NB604.1, newdata = dfm_match_8020_1, force = F) # newdata = "dfm on which prediction should be made". 
tab_8020_1 <- table(actual_8020_1, predicted_8020_1) # VIRKER KUN HVIS DE HAR SAMME LÆNGE (500 vs. 256).
tab_8020_1
# Confusion matrix og stats 
confusion8020_1 <- confusionMatrix(tab_8020_1, mode = "everything")
confusion8020_1 # F1 = 80,00 %
# Resultat: 88 TRUE NEGATIVES, 20 TRUE POSITIVES, 27 FALSE NEGATIVES og 20 FALSE POSITIVES.

# Model 2
actual_8020_2 <- docvars(dfm_match_8020_2, "selvpersonaliseret") # indeholder kun de 256 fra test data
predicted_8020_2 <- predict(NB604.2, newdata = dfm_match_8020_2, force = F) # newdata = "dfm on which prediction should be made". 
tab_8020_2 <- table(actual_8020_2, predicted_8020_2) # VIRKER KUN HVIS DE HAR SAMME LÆNGE (500 vs. 256).
tab_8020_2
# Confusion matrix og stats 
confusion8020_2 <- confusionMatrix(tab_8020_2, mode = "everything")
confusion8020_2 # F1 = 83,5 %
# Resultat: 86 TRUE NEGATIVES, 32 TRUE POSITIVES, 23 FALSE NEGATIVES og 11 FALSE POSITIVES.

# Model 3
actual_8020_3 <- docvars(dfm_match_8020_3, "selvpersonaliseret") # indeholder kun de 256 fra test data
predicted_8020_3 <- predict(NB604.3, newdata = dfm_match_8020_3, force = F) # newdata = "dfm on which prediction should be made". 
tab_8020_3 <- table(actual_8020_3, predicted_8020_3) # VIRKER KUN HVIS DE HAR SAMME LÆNGE (500 vs. 256).
tab_8020_3
# Confusion matrix og stats 
confusion8020_3 <- confusionMatrix(tab_8020_3, mode = "everything")
confusion8020_3 # F1 = 80,2 %
# Resultat: 79 TRUE NEGATIVES, 34 TRUE POSITIVES, 22 FALSE NEGATIVES og 17 FALSE POSITIVES.

# Alt i alt ligner det ikke, der var den store forbedring ved training på 604 i stedet


# Samler model performance resultaterne -----------------------------------

confusion256
confusion2
confusion3

confusion8020_1
confusion8020_2
confusion8020_3

samlet_performance <- bind_rows(
  c(accuracy = 0.7305, precision = 0.8841, recall = 0.7436, f = 0.8078),
  c(accuracy = 0.7422, precision = 0.8862, recall = 0.7590 ,f =0.8177 ),
  c(accuracy = 0.7578, precision = 0.8698, recall = 0.7861 ,f = 0.8258),
  c(accuracy = 0.7105, precision = 0.8381, recall = 0.7652,f = 0.8000),
  c(accuracy = 0.7763, precision = 0.8866, recall = 0.7890,f = 0.8350 ),
  c(accuracy = 0.7434, precision = 0.8229, recall = 0.7822,f = 0.8020)
) # Der er sgu ikke stor forskel i model performance! Godnat.

samlet_performance$model
samlet_performance$model[1:3] <- 500
samlet_performance$model[4:6] <- 640

# Laver lige en gennemsnitsberegning af performance for 500 og 640 training modellerne
samlet_performance %>%
  group_by(model) %>%
  summarise_all(.funs = c(mean="mean"))
# Det ser ikke ud som om mere training data (20 % mere) har hjuloet modellen så meget.


# Hvilke politikere har kørt mest personaliseret kampagne? ----------------

kandidat_pers <- fb_ads %>%
  filter(kandidat_d == 1) %>% # ikke-personlige sider sorteres fra.
  group_by(STEMMESEDDELNAVN, PARTI) %>% 
 summarise(total_ads = n(),
            n_pers = sum(selvpersonaliseret),
            total_spend = sum(spend_mid),
           pers_spend = sum(selvpersonaliseret*spend_mid),
           pers_spend_share = pers_spend/total_spend) %>%
  arrange(desc(total_spend))


# Jeg skal bruge 1 kategori for selvpersonalisering (0/1) dvs. 2 rows for hver kandidat
pers_vis <- fb_ads %>%
  filter(kandidat_d == 1) %>%
  group_by(STEMMESEDDELNAVN, selvpersonaliseret) %>%
  summarise(total_spend = sum(spend_mid)) %>%
  arrange(desc(total_spend))
  
# Ja noget i den her stil ser fint ud!
pers_vis %>%
  filter(STEMMESEDDELNAVN %in% pull(head(kandidat_pers,30), STEMMESEDDELNAVN)) %>% # Udvælger de 30 største spenders (total)
ggplot(aes(x = STEMMESEDDELNAVN, y = total_spend, fill = selvpersonaliseret)) + 
  geom_bar(position = "stack", stat = "identity") + coord_flip() + theme_minimal()
    

# Lav gerne sådan et plot https://www.r-graph-gallery.com/299-circular-stacked-barplot.html