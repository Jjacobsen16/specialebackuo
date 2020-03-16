# 1. Forberedelse til Quanteda ---------------------------------
## 1.1 Indlæser pakker og data =================================

library(pacman)
p_load(tidyverse, quanteda, xtable, tictoc, caret, readxl, estimatr, texreg, broom, miceadds, ggbeeswarm) # måske lubridate, topicmodels, readtext, agendacodeR


fb_ads <- read_rds("FB API data/data/fb_ads.rds")

## 1.2 Tjekker data ============================================

# # Undersøger populationen af kandidat-annoncer med tekstmæssigt unikt indhold
# fb_ads %>%
#   filter(!ad_creative_body =="") %>% # sorterer 2 tekstmæssigt tomme annoncer fra
#   # filter(kandidat_d == T) %>% # Bruger kun personlige sider, ikke parti-sider
#   distinct(ad_creative_body) %>% # Beholder kun tekstmæssigt unikke kandidat-annoncer
#   nrow() # Det giver n=5.518. Det er denne population jeg tager en stikprøve af i udvælgelsen af training data. N=7.172 inkl. partisider.
# 
# # Tjekker hvor mange tekstmæssigt unikke annoncer hvert partis kandidater har kørt:
# fb_ads %>%
#   filter(!ad_creative_body =="") %>%
#   filter(kandidat_d == T) %>%
#   distinct(ad_creative_body, .keep_all = T) %>%
#   group_by(PARTI) %>%
#   count() %>%
#   arrange(n)
# # Det strækker sig fra 39 (Partiet Klaus Riskær Pedersen) til 1.072 (Venstre).
# 
# # Laver et corpus med alle annoncer i fb_ads, fuld populationo
# corpus_data <- dplyr::select(fb_ads, c(ad_id, ad_id_copy, ad_creative_body, page_name, PARTI, parti_navn, KØN, ALDER, spend_mid, spend_interval,
#                                        impressions_mid, impressions_interval, kandidat_d, facebook, instagram)) #pers_d)) # OBS! pers_d skal laves først. og tilføjes til fb_ads
# 
# corpus_body <- corpus(corpus_data,
#                       docid_field = "ad_id_copy", # sætter dokumentid = ad_id
#                       text_field = "ad_creative_body")
# 
# # 2. Udvælger stikprøve til manuel kodning ---------------------------------
# 
# set.seed(6867) # For replicerbarhed 6867
# 
# ### SIMPEL TILFÆLDIG UDVÆLGELSE ###
# 
# # Her laver jeg et andet udtræk vha. simpel tilfældig udvælgelse, så stikprøven bliver repræsentativ for populationen af annoncer.
# # Lige med det OBS at jeg undgår dubletter, som der selvfølgelig er i populationen.
# # random_id <- fb_ads %>%
# #   filter(!ad_creative_body =="") %>% # undgår 2 tekstmæssigt tomme annoncer.
# #   # filter(kandidat_d == T) %>% # Fjernet! Jeg medtager alle annoncer, også partisider.
# #   distinct(ad_creative_body, .keep_all = T) %>% # Medtager kun tekstmæssigt unikke annoncer
# #   sample_n(1000) %>% # Koder 1.000 til at starte med.
# #   pull(ad_id)
# #
# # # Trækker de 1000 annoncer ud som en Excel-fil, så jeg kan kode dem manuelt.
# # random1000 <- fb_ads %>%
# #   filter(ad_id %in% random_id) %>%
# #   select(ad_id, kandidat_d, ad_creative_body, starts_with("ad_creative_link"))
# # write_excel_csv(random1000, "random1000.csv") # Eksporterer til csv
# # # OBS! jeg koder IKKE parti-siderne. Kun de 756 kandidat-sider.
# 
# # 3. Inddeler i training og test set og laver CORPUSES ---------------------------------
# 
# # Importerer det kodede training subset med 756 kodede annoncer
# kodet1000 <- read_excel("random1000 kodet.xlsx")
# 
# # Beholder kun kandidatannoncer
# kodet756 <- kodet1000 %>%
#   filter(kandidat_d == 1) %>% # Resulterer i 756 annoncer.
#   mutate(ad_id_copy = ad_id) # kopierer id variablen.
# 
# # Labelled subset med alle 756 kodede annoncer
# label756_corpus<- corpus(kodet756, docid_field = "ad_id_copy", # sætter dokumentid = ad_id
#                          text_field = "ad_creative_body")
# # Training subset 500 annoncer
# training500_corpus <- corpus(sample_n(kodet756, 500), docid_field = "ad_id_copy", # sætter dokumentid = ad_id
#                              text_field = "ad_creative_body")
# # Test subset 256 annoncer
# test256_corpus <- corpus_subset(label756_corpus, !ad_id %in% docvars(training500_corpus, "ad_id"))
# 
# # Rest subset med 13.434 annoncer, som ikke er labelled manuelt
# nolabel_corpus <- corpus_subset(corpus_body, !ad_id %in% pull(kodet756, ad_id))
# ndoc(nolabel_corpus) # 13.434. De 756 annoncer er fratrukket.
# 
# # Tjekker andelen af personaliserede annoncer i de to sæt (tjekker randomiseringen)
# prop.table(table(docvars(training500_corpus, "selvpersonaliseret")))*100 # 31.4 pct. selvpersonaliseret
# prop.table(table(docvars(test256_corpus, "selvpersonaliseret")))*100 # 35.9 pct. selvpersonaliseret, udmærket.
# # randomiseringen er repliceret.
# 
# # 3. TEST OG TRAINING DFM ---------------------------------
# 
# # Laver en training dfm med ALLE manuelt kodede annoncer
# training756_dfm <- dfm(label756_corpus,
#                        remove_numbers = T,
#                        remove_punct = T)
# nfeat(training756_dfm) # 9447
# training756_dfm <- dfm_wordstem(training756_dfm, language = "da")
# nfeat(training756_dfm) # 7065, fint.
# 
# # Rest dfm med alle andre annoncer, som skal kodes af maskinen:
# nolabel_dfm <- dfm(nolabel_corpus,
#                    remove_numbers = T,
#                    remove_punct = T) %>%
#   dfm_wordstem(language = "da")
# nfeat(nolabel_dfm) # 19.689
# 
# # TRAINING set med 500 annoncer
# 
# training500_dfm <- dfm(training500_corpus,
#                        remove_numbers = T,
#                        remove_punct = T) %>%
#   dfm_wordstem(language = "da")
# nfeat(training500_dfm) # 5492, fint.
# 
# # TEST set med 256 annoncer
# 
# test256_dfm <- dfm(test256_corpus,
#                    remove_numbers = T,
#                    remove_punct = T) %>%
#   dfm_wordstem(language = "da")
# nfeat(test256_dfm) # 3548
# 
# # Træner modellen på de 500 annoncer i training set
# NB500 <- textmodel_nb(training500_dfm, docvars(training500_dfm, "selvpersonaliseret"), prior = "docfreq")
# # Med prior antager jeg en fordeling af personaliserede/ ikke personaliserede lig den i training set. Fin antagelse.
# summary(NB500)
# 
# # Tjekker in-sample performance
# # ... altså hvor mange af de 500 kodede annoncer, den ville kode rigtigt.
# prop.table(table(predict(NB500) == docvars(training500_dfm, "selvpersonaliseret"))) * 100 # Resultat: 97.2 % rigtige. ligegyldigt.
# # - forventeligt, når det jo er de annoncer, modellen er trænet på.
# 
# # Sammenligner det med et tilfældigt resultat, hvor jeg kun fortæller algoritmen fordelingen mellem selv-personaliserede, og ikke selvpersonaliserede annoncer.
# prop.table(table(sample(predict(NB500)) == docvars(training500_dfm, "selvpersonaliseret"))) * 100 # Resulat: 57.6 % rigtige.
# # FALSE 38 %, TRUE 62 % ligegyldigt.
# 
# # 4. TESTER PERFORMANCE PÅ TEST SET ---------------------------------
# 
# # NB can only take features into consideration that occur both in the training set and the test set
# # tilføjer derfor alle ord til en samlet dfm
# dfm_matched256 <- dfm_match(test256_dfm, features = featnames(training500_dfm)) # OMVENDT I FORHOLD TIL DOCUMENTATION!!!
# ndoc(dfm_matched256) # 256 rækker fra test set.
# nfeat(dfm_matched256) # 5492. Alle ord fra training set er inkluderet.
# 
# # Laver en confusion matrix for at tjekke, hvordan algoritmen klarede sig.
# # De 256 manuelt kodede annoncer
# actual_class256 <- docvars(dfm_matched256, "selvpersonaliseret") # indeholder kun de 256 fra test data
# 
# # Maskinens prediction af de 256 annoncer
# predicted_class256 <- predict(NB500, newdata = dfm_matched256, force = F) # newdata = "dfm on which prediction should be made".
# tab_class256 <- table(actual_class256, predicted_class256) # VIRKER KUN HVIS DE HAR SAMME LÆNGE (500 vs. 256).
# tab_class256
# # Resultat: 145 TRUE NEGATIVES, 42 TRUE POSITIVES, 50 FALSE NEGATIVES og 19 FALSE POSITIVES.
# 
# # Laver en confusion matrix med en masse stats om resultatet
# # ... kan også bruges til visualisering senere.
# confusion256 <- confusionMatrix(tab_class256, mode = "everything")
# confusion256
# # Precision: 88 %
# # Recall: 74 %
# # F1: 81 %
# # Ikke dårligt!
# 
# ## 4.1 Ekstra test: Er performance bedre med et dikotomiseret training set? =====
# predicted_class256_01 <- predict(NB500_01, newdata = dfm_matched256, force = F)
# tab_class256_01 <- table(actual_class256, predicted_class256_01)
# tab_class256_01
# # Resultat: 159 TRUE NEGATIVES, 22 TRUE POSITIVES, 70 FALSE NEGATIVES og 5 FALSE POSITIVES
# #... med andre ord et langt dårligere resultat når det er dikotomiseret.
# #... fortsætter med count matrix.
# 
# # 5. Fuldt træningssæt 756 og kodning af resten af data --------------
# 
# # FITTER EN NY NB MODEL BASERET PÅ HELE TRAINING SET - OG LADER DEN KØRE PÅ RESTEN AF DATASÆTTET!
# NB756 <- textmodel_nb(training756_dfm, docvars(training756_dfm, "selvpersonaliseret"), prior = "docfreq")
# 
# # Tjekker hvilke ord, der ifølge modellen skaber størst sandsynlighed for at annoncen er personaliseret.
# ord_coef <- as.data.frame(coef(NB756)) %>%
#   rownames_to_column("ord") %>%
#   arrange(desc(`1`)) # "mærkesag" på 4. pladsen.
# 
# # Laver en dfm for de resterende 13.434 annoncer, som skal kodes af maskinen.
#   # NB can only take features into consideration that occur both in the training set and the test set
# ndoc(training756_dfm) # 756 rows
# nfeat(training756_dfm) # 7065 ord
# ndoc(nolabel_dfm) # 13.434
# nfeat(nolabel_dfm) # 19.689
# dfm_matched_rest <- dfm_match(nolabel_dfm, features = featnames(training756_dfm)) #
# ndoc(dfm_matched_rest) # 13.434 rækker. dem, der skal predictes.
# nfeat(dfm_matched_rest) # 7065. Alle ord, som optræder i training OG test set beholder vi. Giver mening!
# 
# predict_nolabel <- predict(NB756, newdata = dfm_matched_rest) # virker!
# predict_nolabel_prob <- predict(NB756, newdata = dfm_matched_rest, type = "probability") %>%
#   round(.,3) # afrunder.
# # Smelter dem sammen i en df
# 
# glimpse(predict_nolabel)
# 
# hist(predict_nolabel_prob)
# 
# 
# Model performance test ---------------------------------------------------------

# I dette afsnit tester jeg modellens test mere præcis. 2 formål:
# 1) Kør flere identiske tests (fx 3) med forskellige training/ test sets og beregn et gennemsnit
# 2) Vis at større training set = bedre performance

# # [2] Nyt sample - ny randomisering
# set.seed(6789)
# 
# # [2] Training subset 500 annoncer
# training500_corpus2 <- corpus(sample_n(kodet756, 500), docid_field = "ad_id_copy", # sætter dokumentid = ad_id
#                               text_field = "ad_creative_body")
# 
# # [3] Nyt sample - ny randomisering
# set.seed(4849)
# 
# # [3] Training subset 500 annoncer
# training500_corpus3 <- corpus(sample_n(kodet756, 500), docid_field = "ad_id_copy", # sætter dokumentid = ad_id
#                               text_field = "ad_creative_body")
# 
# # [2] Test subset 256 annoncer
# test256_corpus2 <- corpus_subset(label756_corpus, !ad_id %in% docvars(training500_corpus2, "ad_id"))
# # [3] Test subset 256 annoncer
# test256_corpus3 <- corpus_subset(label756_corpus, !ad_id %in% docvars(training500_corpus3, "ad_id"))
# 
# # [2] TRAINING set med 500 annoncer
# training500_dfm2 <- dfm(training500_corpus2,
#                         remove_numbers = T,
#                         remove_punct = T) %>%
#   dfm_wordstem(language = "da")
# nfeat(training500_dfm2) # 5543
# 
# # [3] TRAINING set med 500 annoncer
# training500_dfm3 <- dfm(training500_corpus3,
#                         remove_numbers = T,
#                         remove_punct = T) %>%
#   dfm_wordstem(language = "da")
# nfeat(training500_dfm3) # 5544
# 
# # [2] TEST set med 256 annoncer
# test256_dfm2 <- dfm(test256_corpus2,
#                     remove_numbers = T,
#                     remove_punct = T) %>%
#   dfm_wordstem(language = "da")
# nfeat(test256_dfm2) # 3423
# 
# # [3] TEST set med 256 annoncer
# test256_dfm3 <- dfm(test256_corpus3,
#                     remove_numbers = T,
#                     remove_punct = T) %>%
#   dfm_wordstem(language = "da")
# nfeat(test256_dfm3) # 3450
# 
# # Træner modellere
# NB500.2 <- textmodel_nb(training500_dfm2, docvars(training500_dfm2, "selvpersonaliseret"), prior = "docfreq")
# NB500.3 <- textmodel_nb(training500_dfm3, docvars(training500_dfm3, "selvpersonaliseret"), prior = "docfreq")
# 
# # [2] Matcher ordene i test dfm med dem i training dfm, så testen virker.
# dfm_matched2 <- dfm_match(test256_dfm2, features = featnames(training500_dfm2)) 
# ndoc(dfm_matched2) # 256 rækker fra test set. 
# nfeat(dfm_matched2) # 5543. Alle ord fra training set er inkluderet.
# 
# # [3] Matcher ordene i test dfm med dem i training dfm, så testen virker.
# dfm_matched3 <- dfm_match(test256_dfm3, features = featnames(training500_dfm3)) 
# ndoc(dfm_matched3) # 256 rækker fra test set. 
# nfeat(dfm_matched3) # 5544. Alle ord fra training set er inkluderet.
# 
# # [2] Model 2 performance på test set
# actual_class2 <- docvars(dfm_matched2, "selvpersonaliseret") # indeholder kun de 256 fra test data
# predicted_class2 <- predict(NB500.2, newdata = dfm_matched2, force = F) # newdata = "dfm on which prediction should be made". 
# tab_class2 <- table(actual_class2, predicted_class2) # VIRKER KUN HVIS DE HAR SAMME LÆNGE (500 vs. 256).
# tab_class2
# # Laver confusion matrix og stats
# confusion2 <- confusionMatrix(tab_class2, mode = "everything")
# confusion2 # F1 = 82 %
# # Resultat: 148 TRUE NEGATIVES, 42 TRUE POSITIVES, 47 FALSE NEGATIVES og 19 FALSE POSITIVES.
# # Kommentar: Præcist resultat meget lig første kørsel. Overvej at bruge større training set, fx 80/20.
# 
# 
# # [3] Model 3 performance på test set
# actual_class3 <- docvars(dfm_matched3, "selvpersonaliseret") # indeholder kun de 256 fra test data
# predicted_class3 <- predict(NB500.3, newdata = dfm_matched3, force = F) # newdata = "dfm on which prediction should be made". 
# tab_class3 <- table(actual_class3, predicted_class3) # VIRKER KUN HVIS DE HAR SAMME LÆNGE (500 vs. 256).
# tab_class3
# # Laver confusion matrix og stats
# confusion3 <- confusionMatrix(tab_class3, mode = "everything")
# confusion3 # F1 = 82,58 %
# # Resultat: 147 TRUE NEGATIVES, 47 TRUE POSITIVES, 40 FALSE NEGATIVES og 22 FALSE POSITIVES.
# # Kommentar: Bedste model indtil videre. Resultat ikke langt fra de to første modeller.
# 
# 
# ## 80/20 TRAINING TEST PERFORMANCE ==========================================
# 
# # Nu til 80/20 tests
# 
# # Laver 3 forskellige tilfældige udtræk af 604 annoncer (80 %)
# 
# # [1]
# set.seed(1000)
# training604_corpus1 <- corpus(sample_n(kodet756, 604), docid_field = "ad_id_copy", # sætter dokumentid = ad_id
#                               text_field = "ad_creative_body")
# 
# # [2] 
# set.seed(2000)
# training604_corpus2 o<- corpus(sample_n(kodet756, 604), docid_field = "ad_id_copy", # sætter dokumentid = ad_id
#                               text_field = "ad_creative_body")
# 
# # [3]
# set.seed(3000)
# training604_corpus3 <- corpus(sample_n(kodet756, 604), docid_field = "ad_id_copy", # sætter dokumentid = ad_id
#                               text_field = "ad_creative_body")
# 
# # Laver 3 tilsvarende test sets (20 %)
# test152_corpus1 <- corpus_subset(label756_corpus, !ad_id %in% docvars(training604_corpus1, "ad_id"))
# test152_corpus2 <- corpus_subset(label756_corpus, !ad_id %in% docvars(training604_corpus2, "ad_id"))
# test152_corpus3 <- corpus_subset(label756_corpus, !ad_id %in% docvars(training604_corpus3, "ad_id"))
# 
# # Laver 3 training sets (DFM)
# training604_dfm1 <- dfm(training604_corpus1,
#                         remove_numbers = T,
#                         remove_punct = T) %>%
#   dfm_wordstem(language = "da")
# nfeat(training604_dfm1) # 5557
# 
# training604_dfm2 <- dfm(training604_corpus2,
#                         remove_numbers = T,
#                         remove_punct = T) %>%
#   dfm_wordstem(language = "da")
# nfeat(training604_dfm2) # 5346
# 
# training604_dfm3 <- dfm(training604_corpus3,
#                         remove_numbers = T,
#                         remove_punct = T) %>%
#   dfm_wordstem(language = "da")
# nfeat(training604_dfm3) # 5389
# 
# # Laver 3 test sets (DFM)
# test152_dfm1 <- dfm(test152_corpus1,
#                     remove_numbers = T,
#                     remove_punct = T) %>%
#   dfm_wordstem(language = "da")
# nfeat(test152_dfm1) # 3402
# 
# test152_dfm2 <- dfm(test152_corpus2,
#                     remove_numbers = T,
#                     remove_punct = T) %>%
#   dfm_wordstem(language = "da")
# nfeat(test152_dfm2) # 3694
# 
# test152_dfm3 <- dfm(test152_corpus3,
#                     remove_numbers = T,
#                     remove_punct = T) %>%
#   dfm_wordstem(language = "da")
# nfeat(test152_dfm3) # 3625
# 
# # Træner modellerne
# NB604.1 <- textmodel_nb(training604_dfm1, docvars(training604_dfm1, "selvpersonaliseret"), prior = "docfreq")
# NB604.2 <- textmodel_nb(training604_dfm2, docvars(training604_dfm2, "selvpersonaliseret"), prior = "docfreq")
# NB604.3 <- textmodel_nb(training604_dfm3, docvars(training604_dfm3, "selvpersonaliseret"), prior = "docfreq")
# 
# # Matcher ordene i test dfm med dem i training dfm
# dfm_match_8020_1 <- dfm_match(test152_dfm1, features = featnames(training604_dfm1)) 
# dfm_match_8020_2 <- dfm_match(test152_dfm2, features = featnames(training604_dfm2)) 
# dfm_match_8020_3 <- dfm_match(test152_dfm3, features = featnames(training604_dfm3)) 
# 
# 
# # Model performance resultater
# 
# # Model 1
# actual_8020_1 <- docvars(dfm_match_8020_1, "selvpersonaliseret") # indeholder kun de 256 fra test data
# predicted_8020_1 <- predict(NB604.1, newdata = dfm_match_8020_1, force = F) # newdata = "dfm on which prediction should be made". 
# tab_8020_1 <- table(actual_8020_1, predicted_8020_1) # VIRKER KUN HVIS DE HAR SAMME LÆNGE (500 vs. 256).
# tab_8020_1
# # Confusion matrix og stats 
# confusion8020_1 <- confusionMatrix(tab_8020_1, mode = "everything")
# confusion8020_1 # F1 = 80,00 %
# # Resultat: 88 TRUE NEGATIVES, 20 TRUE POSITIVES, 27 FALSE NEGATIVES og 20 FALSE POSITIVES.
# 
# # Model 2
# actual_8020_2 <- docvars(dfm_match_8020_2, "selvpersonaliseret") # indeholder kun de 256 fra test data
# predicted_8020_2 <- predict(NB604.2, newdata = dfm_match_8020_2, force = F) # newdata = "dfm on which prediction should be made". 
# tab_8020_2 <- table(actual_8020_2, predicted_8020_2) # VIRKER KUN HVIS DE HAR SAMME LÆNGE (500 vs. 256).
# tab_8020_2
# # Confusion matrix og stats 
# confusion8020_2 <- confusionMatrix(tab_8020_2, mode = "everything")
# confusion8020_2 # F1 = 83,5 %
# # Resultat: 86 TRUE NEGATIVES, 32 TRUE POSITIVES, 23 FALSE NEGATIVES og 11 FALSE POSITIVES.
# 
# # Model 3
# actual_8020_3 <- docvars(dfm_match_8020_3, "selvpersonaliseret") # indeholder kun de 256 fra test data
# predicted_8020_3 <- predict(NB604.3, newdata = dfm_match_8020_3, force = F) # newdata = "dfm on which prediction should be made". 
# tab_8020_3 <- table(actual_8020_3, predicted_8020_3) # VIRKER KUN HVIS DE HAR SAMME LÆNGE (500 vs. 256).
# tab_8020_3
# # Confusion matrix og stats 
# confusion8020_3 <- confusionMatrix(tab_8020_3, mode = "everything")
# confusion8020_3 # F1 = 80,2 %
# # Resultat: 79 TRUE NEGATIVES, 34 TRUE POSITIVES, 22 FALSE NEGATIVES og 17 FALSE POSITIVES.
# 
# # Alt i alt ligner det ikke, der var den store forbedring ved training på 604 i stedet
# 
# 
# # Samler model performance resultaterne -----------------------------------
# 
# confusion256
# confusion2
# confusion3
# 
# confusion8020_1
# confusion8020_2
# confusion8020_3
# 
# samlet_performance <- bind_rows(
#   c(accuracy = 0.7305, precision = 0.8841, recall = 0.7436, f = 0.8078),
#   c(accuracy = 0.7422, precision = 0.8862, recall = 0.7590 ,f =0.8177 ),
#   c(accuracy = 0.7578, precision = 0.8698, recall = 0.7861 ,f = 0.8258),
#   c(accuracy = 0.7105, precision = 0.8381, recall = 0.7652,f = 0.8000),
#   c(accuracy = 0.7763, precision = 0.8866, recall = 0.7890,f = 0.8350 ),
#   c(accuracy = 0.7434, precision = 0.8229, recall = 0.7822,f = 0.8020)
# ) # Der er sgu ikke stor forskel i model performance! Godnat.
# 
# samlet_performance$model
# samlet_performance$model[1:3] <- 500
# samlet_performance$model[4:6] <- 640
# 
# # Laver lige en gennemsnitsberegning af performance for 500 og 640 training modellerne
# samlet_performance %>%
#   group_by(model) %>%
#   summarise_all(.funs = c(mean="mean"))
# # Det ser ikke ud som om mere training data (20 % mere) har hjuloet modellen så meget.



 # 6. Merger resultaterne med fb data og gemmer --------------
#  # Laver en dataframe med predicted resultater.
#  predict_nolabel_df <- as.data.frame(predict_nolabel) %>%
#    rownames_to_column("ad_id") %>% 
#    rename(selvpersonaliseret = predict_nolabel) %>% # omdøber så den er klar til merge. 
#    # OBS! R gemmer faktor variabel som 1 og 2. Konverterer det til 0 og 1 i neden for!
#    mutate(selvpersonaliseret = as.numeric(selvpersonaliseret)) %>%
#    mutate(selvpersonaliseret = case_when(selvpersonaliseret == 2 ~ 1,
#                                          selvpersonaliseret == 1 ~ 0,
#                                          T ~ NA_real_))
# 
#  # Samler de to (predicted og labelled) i en df, så den kan merges med fb_ads
#  selvpersonaliseret <- bind_rows(predict_nolabel_df, select(kodet756, ad_id, selvpersonaliseret))
#  
#  glimpse(selvpersonaliseret) # alle 14.190 annoncer. Fint!
#  # 756 annoncer er NA's på probability, fordi de er kodet i hånden (trainng set).
# # gemmer lige selvpersonaliseret.
# #  write_csv(selvpersonaliseret, "pers_predicted.csv")
#  
#  ### Nyt: Tilføjer probability fra NB756 ###
# predict_nolabel_prob <- as.data.frame(predict_nolabel_prob) %>%
#     rownames_to_column("ad_id") %>%
#     rename(probability = `1`)
#   
#   # Indsætter en kolonne med probability for de annoncer, maskinen har kodet:
#  selvpersonaliseret_prob <- left_join(selvpersonaliseret, select(predict_nolabel_prob, ad_id, probability))
#  
#  # Tilføjer også annoncetekst for at få eksempler
#  selvpersonaliseret_prob <- left_join(selvpersonaliseret_prob, select(fb_ads, ad_id, ad_creative_body))
#  
#  # Tjekker den ud
#  glimpse(selvpersonaliseret_prob)
#  
#  selvpersonaliseret_prob %>%
#    select(-ad_creative_body) %>%
#    filter(!is.na(probability)) %>%
#    arrange(probability) %>%
#    tail()
#  
#  selvpersonaliseret_prob %>%
#   filter(selvpersonaliseret == 1) %>%
#    select(probability) %>%
#    summary()# Alle annoncer med en probability > 0.5 er blevet labelled "selvpersonaliseret". Fint!
 
 # Gemmer lige selvpersonaliseret_prob.
# write_csv(selvpersonaliseret_prob, "pers_predicted_prob.csv")

# Indlæser resultatet
selvpersonaliseret <- read_csv("pers_predicted.csv") 

fb_ads <- fb_ads %>%
  left_join(selvpersonaliseret)

#######################################################################
# 7. Analyse og signifikanstest ########################################
########################################################################


# Hvor mange af kandidatannoncerne er selvpersonaliserede?
fb_ads %>% 
  filter(kandidat_d == 1) %>%
  count(selvpersonaliseret) # 35 % 

# Hvor mange penge brugte politikerne på hhv. selvpersonaliserede og ikke-selvpersonalisede annoncer?
fb_ads %>%
  filter(kandidat_d == 1) %>%
  group_by(selvpersonaliseret) %>%
  summarise(min_spend = sum(spend_lower_bound),
            mid_spend = sum(spend_mid),
            max_spend = sum(spend_upper_bound)) #%>%
  #colSums()

# Hvordan varierer kandidaternes annoncer i pris? (evt. HISTOGRAM)
fb_ads %>%
  filter(kandidat_d == 1) %>%
  count(spend_interval)

# Og hvor mange har set disse annoncer?
fb_ads %>%
  filter(kandidat_d == 1) %>%
  count(impressions_interval)

glimpse(fb_ads)
# Hvordan er fordelingen mellem partierne?
parti_pers_resultat <- fb_ads %>%
  filter(kandidat_d == 1) %>% # kun blandt kandidaternes sider.
  group_by(parti_navn) %>%
  summarise(n_ads = n(),
            n_pers = sum(selvpersonaliseret),
            share_pers = n_pers/n_ads*100) %>%
  arrange(desc(share_pers)) # VÆR SÅ GOD! Her har vi så resultatet .

# Eksporter tab:selvpersonaliseret_partier
xtable(parti_pers_resultat, digits = 1,
       auto = TRUE, type = "latex")

colSums(parti_pers_resultat[,2:4])

fb_ads %>%
  filter(PARTI == "E") %>%
  group_by(STEMMESEDDELNAVN) %>%
  count()

fb_ads %>%
  filter(ad_id =="D5482") %>%
  select(STEMMESEDDELNAVN,ad_creative_body)

# Og hvis jeg ser på ad spend i stedet for number of ads?
parti_resultat <- fb_ads %>%
 filter(kandidat_d == 1) %>%
 mutate(spend_pers = selvpersonaliseret*spend_mid,
         spend_pers_min = selvpersonaliseret*spend_lower_bound) %>%
  group_by(parti_navn) %>%
  summarise(spend = sum(spend_mid),
            pers_spend = sum(spend_pers),
            share_pers_spend = pers_spend/spend*100,
            spend_min = sum(spend_lower_bound),
            spend_pers_min2 = sum(spend_pers_min)) %>% 
  arrange(desc(share_pers_spend))
            #spend_perstest = sum(selvpersonaliseret*spend_mid))
colSums(parti_resultat[,2:6]) # Nej det kan ikke passe at de har brugt min. 3.75 mio. kroner?
# Update: Jo, det kan det sagtens. I valgkampens sidste 4 uger brugte kandidaterne lidt over 6 mio. kroner.
# colSums driller med scientific notation. slukker det:
# options(scipen = 500)

#Eksport tab:selvpersonaliseret_spend
xtable(parti_resultat[1:4], digits = 1,
       auto = TRUE, type = "latex")
            
# marts NYT: Jeg visualiserer resultatet i et stacked bar plot i stedet.
glimpse(parti_resultat)

# Skal have det i langt format, så jeg får en kategori for spend: personal/ not personal
parti_resultat_lang <- parti_resultat %>%
mutate(not_pers_spend = spend-pers_spend) %>%
  pivot_longer(cols = c(pers_spend, not_pers_spend), names_to = "pers", values_to = "lang_spend")
glimpse(parti_resultat_lang) # fint!

# Visualisering af parti resultat
parti_resultat_lang %>%
  ggplot(aes(x = reorder(parti_navn, -spend), y = lang_spend/1000, fill = pers)) + 
  geom_bar(position = "stack", stat = "identity") + coord_flip() + theme_minimal() +
  geom_text(size = 3, aes(label = ifelse(pers == "pers_spend", paste0(round(share_pers_spend,1), "%"), ""),
                          hjust = case_when(spend < 100000 ~ -.2, # E, N
                                            spend < 111000 ~ -.4, # Å
                                            spend < 112000 ~ -.7, # D
                                            spend < 460000 ~ -1.05, # K og Ø
                                            spend < 470000 ~ -.75,
                                            TRUE ~ -.1))) + # Resten
  scale_fill_manual(values = c("grey72", ku_rød), labels = c("Ikke selvpersonaliseret","Selvpersonaliseret")) +
  labs(x = element_blank(), y = element_blank(), fill = "Selvpersonaliseret",title = "Kandidaternes annonceforbrug (tusinde kroner)",
       caption = "Note: Andel selvpersonaliseret angivet i procent") +
  #scale_y_continuous(limits = c(100, 800), breaks = c(seq(100,800,100))) +
  theme(legend.position = "bottom", legend.title = element_blank())
ggsave("stack_pers.pdf", width = 6, height = 4.5)
# Der var den sgu!

# Lad os definere et par variable og udføre TESTEN: two-sample t-test.

# Centraliseret nomineringsproces
fb_ads <- fb_ads %>%
  mutate(centralisering = case_when(PARTI %in% c("A", "OE", "F", "O", "I", "D") ~ 1,
                                    PARTI %in% c("AA", "B", "V", "C", "K", "E", "N") ~ 0,
                                    T ~ NA_real_))
fb_ads %>%
  distinct(PARTI, centralisering)

# HV-placering (overvej at tilføje)
fb_ads <- fb_ads %>%
  mutate(Højreorienteret = case_when(PARTI %in% c("A", "F", "OE", "B", "AA", "N") ~ 0,
                                    PARTI %in% c("V", "C", "I", "K", "D", "E", "P", "O") ~ 1,
                                    T ~ NA_real_)) # Klaus Riskær Parti kodes som højreorienteret

# Annoncer, som ikke har optrådt på fb eller instagram er kodet til NA. Laver det om til 0.
fb_ads%>%
  count(instagram)

fb_ads$facebook <- fb_ads$facebook %>%
replace_na(0) 
fb_ads$instagram <- fb_ads$instagram %>%
  replace_na(0) 
# fint.

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

### VIGTIG ###
glimpse(fb_ads)
fb_ads %>%
  filter(kandidat_d == 1 & FT == 0 & EP == 0) %>%
  distinct(page_name, parti_navn)

# Af en eller anden grund er 4 politikere kodet som hverken opstillet ved FT eller EP.
# De var alle opstillet til EP. Retter fejlen

fb_ads <- fb_ads %>%
  mutate(EP = case_when(kandidat_d == 1 & FT == 0 & EP == 0 ~ 1,
         T ~ EP))
### RETTET ###

fb_ads %>%
  filter(kandidat_d == 1) %>%
  count(STORKREDS) # De 1.339 annoncer kommer fra kandidater, som kun stiller op til EP og IKKE FT.

# Jeg omkoder derfor STORKREDS til "Hele landet" for de 1.339 EP-kandidater.
fb_ads <- fb_ads %>%
  mutate(STORKREDS = case_when(kandidat_d == 1 & FT == 0 ~ "Hele landet (EP)",
                               T ~ as.character(STORKREDS)))
# Sådan! Ingen missing values på storkreds blandt kandidater nu. kan bruge variablen i regressionen. 

# MODELLERNE ################################################################################

# Intro: Jeg tvivler på, at lineær model er det bedste valg, når jeg nu har en dikotom afhængig variabel!
# Jeg burde jo nok køre en logistisk regression med cluster robuste standardfejl. Jeg kører lige lidt forskelligt her for at se, hvad det viser.
# Så må mit spørgsmål til Frederik være, hvilke af modellerne, jeg skal præsentere. 

### Model -1 : Den helt simple comparison of two proportions (two proportions z-test) -------------------
  # Identisk med en chi^2 test.
  # Er decentraliserede partier mere selvpersonaliserede?

# finder antal selvpersonaliserede i hver kategori (centralisering)
fb_ads %>%
  filter(kandidat_d == 1) %>%
  group_by(centralisering) %>%
  count(selvpersonalisering)

# Sætter det på matrix form
selvp.matrix <- matrix(c(1452, 2208, 3064, 3658), ncol = 2)
colnames(selvp.matrix) <- c("selvpersonaliseret", "ikke selvpersonaliseret")
rownames(selvp.matrix) <- c("centraliseret", "decentraliseret")
prop.table(selvp.matrix, 1) # Viser andel selvpersonaliserede annoncer inden for de to grupper (centralisering vs. decentralisering) [dvs. row = 100%]

prop.test(selvp.matrix, correct = F, alternative = "two.sided") # med correct = FALSE fjerner jeg Yates continuity correction.
# RESULTAT: Andelen af selvpersonaliserede annoncer blandt kandidater fra decentraliserede partier er 37.64 pct.
          # Andelen af selvpersonaliserede annoncer blandt kandidater fra centraliserede partier er 32.15 pct.
          # Forskelen er statistisk signifikant, siger testen. 

### Model 0: Den helt simple two-way t-test via lineær regression (lm) og uden cluster robuste standardfejl -------------
lm.centralisering.0 <- lm(selvpersonaliseret ~ centralisering, data = fb_ads, subset = kandidat_d == 1)
summary(lm.centralisering.0) 
# RESULTAT ER IDENTISK MED OVENSTÅENDE teST OF PROPORtIONS (t-test). Perfekt. 

# Model 0.1: Samme som ovensteånde, men vægtet efter annoncepris
lm.centralisering.01 <- lm(selvpersonaliseret ~ centralisering, data = fb_ads, subset = kandidat_d == 1,
                          weights = spend_mid)
summary(lm.centralisering.01)

summary(lm.centralisering.0) 
confint(lm.centralisering.0, level = 0.95) # konfidensinterval
# RESULTAT: Stadig signifikant forskel, omend koefficienterne har ændret sig lidt.
  # Andelen af selvpersonaliserede annoncer blandt kandidater fra decentraliserede partier er nu 35.50 pct. (vægtet for annoncepris)
  # Andelen af selvpersonaliserede annoncer blandt kandidater fra centraliserede partier er nu 33.20 pct. (vægtet for annoncepris)

# Jeg fortsætter med at vægte for pris i de følgende tests.

### Model 1: lm test med cluster robuste standardfejl for partier, uden kontrolvariable (uk)
lmr.centralisering.uk <- lm_robust(selvpersonaliseret ~ centralisering, clusters = PARTI, se_type = "stata",
                                   data = fb_ads, subset = kandidat_d == 1, weights = spend_mid)
summary(lmr.centralisering.uk)
# Resulat: Fortsat INSIGNIFIKANT FORSKEL med robuste standardfejl for partierne.
# Koefficienterne (estimaterne) er UÆNDREDE i forhold til lm.centralisering.0 oven for. Kun standardfejl er vokset.
  # decentraliserede partier er UÆNDRET 35.50 
  # centraliserede partier er UÆNDRET 33.20. 

# Johan to hurtige ekstratests: Bliver effekten også insignifikant, når der kun anvendes kontrolvariable,
# og ikke clusterrobsute standardfejl?
lm.centralisering.ak <- lm(selvpersonaliseret ~ centralisering + ALDER + KØN + EP + instagram,
                                  data = fb_ads, subset = kandidat_d == 1, weights = spend_mid)
summary(lm.centralisering.ak)
### Model 2: lm_robust for partier, med få kontrolvariable (fk) =====================================
lmr.centralisering.fk <- lm_robust(selvpersonaliseret ~ centralisering + KØN + ALDER, clusters = PARTI, se_type = "stata",
                                   data = fb_ads, subset = kandidat_d == 1, weights = spend_mid)
summary(lmr.centralisering.fk)
# RESULTAT: Centralisering FORTSAT INSIGNIFIKANT med inklusion af køn og alder som kontrolvariable.

# Model 2.1: lm_robust med alle kontrolvariable (ak)

lmr.centralisering.ak <- lm_robust(selvpersonaliseret ~ centralisering + KØN + ALDER + EP + instagram, clusters = PARTI,
                    data = fb_ads, subset = kandidat_d == 1, se_type = "stata", weights = spend_mid)
summary(lmr.centralisering.ak) # instagram gør det lidt bedre! OVERVEJ AT TAGE STORKREDS MED (og så måske udelade EP)
# Resultat: Centralisering FORTSAT INSIGNIFIKANT med inklusion af alle kontrolvariable.
  # EP og instagram er de eneste signifikante variable i modellen. 

### Model 3: Logistisk regression med cluster robuste standardfejl for partierne =================
# Bruger pakken miceadds, som bygger på multiwayvcov fra sandwich pakken. Nævnt her http://discuss.declaredesign.org/t/cluster-robust-logistic-regression/122/2 
# dokumentation for glm cluster her https://www.rdocumentation.org/packages/miceadds/versions/3.5-14/topics/lm.cluster 
  # glm cluster er vist det samme som vce(cluster) i stata. 

glmr.centralisering <- miceadds::glm.cluster(formula = selvpersonaliseret ~ centralisering + ALDER + KØN + EP + instagram, cluster = fb_ads$PARTI, family = "binomial",
            data = fb_ads) # weights = fb_ads$spend_mid)
summary(glmr.centralisering)
# FORDI Jeg får noget fejlmeddelelse fra modellen oven for, hvis jeg har de forkerte vægte med, prøver lige en clean glm model (uden cluster robuste standardfejl):

glm.centralisering <- glm(selvpersonaliseret ~ centralisering + ALDER + KØN + EP + instagram, data = fb_ads, subset = kandidat_d == 1,
                          family = binomial("logit"), weights = NULL) # spend_mid/sum(spend_mid))
summary(glm.centralisering)

# FINT! Koefficienterne er ens i de to modeller. I den første (alm glm uden robuste standardfejl) er alle variable signifikante. 
#   I den næste (glm med cluster robuste standardfejl) er ingen variable signifikante. 

### STATUS EFTER EN FORMIDDAGS TJEK AF DENNE FEJL - OPSUMMERERING AF DET NEDEN FOR #######################
# I en binomial glm kan man kun bruge frekvensvægte - altså vægte, som fortæller hvor mange observationer, der har proportion of succes x.
# Problemet er, at min Y ikke er proportion of succes mellem 0 og 1, men min Y er binær 0/1. Derfor kan jeg ikke bruge vægte med alm binomial glm.
  # Nyt: Når jeg bruger cluster robuste standardfej (af typen "stata"), fortæller jeg R, at jeg faktisk kun har 13 (partierne) uafhængige observationer, 
# og at meget af data dermed er overflødigt. Standard errors bliver pustet op for at reflektere dette,
# Clyde Schnecther fra STATA anbefaler faktisk, at jeg bruger en hierarkisk model, fordi en one-level model IKKE tager højde for den hierarkiske struktur i data.
  # Resultaterne kunne måske blive mere efficiente ved at fortælle R, at det er en hierarisk model.
  # https://www.statalist.org/forums/forum/general-stata-discussion/general/1407698-clustering-in-logistic-regression 


# Underligt nok viser problemet (warning) sig først, når vægten og KØN er med i modellen samtidigt.

summary((predict(glm.centralisering, type = "response")))
predict(glm.centralisering, type="response")[1] # første observation 
# PROBLEM: Jeg får predicted values 0 eller 1. Hvordan kan det være???

# The strange thing is that when I scale the weights, such that the total equals one, the probability is correctly estimated = spend_mid/sum(spend_mid)
# However scaling of the weights should, as far as I am aware, not have an effect on the estimated parameters. I also tried some other
# scalings. And, for example scaling the weights by 20 also gives me the correct result. PRÆCIS! https://www.mail-archive.com/r-help@r-project.org/msg92655.html 

# "For a binomial GLM prior weights are  used to give the number of trials when the response is the proportion of successes". Står der i dokumentation. 
# Det betyder: "weights are used to give the number of trials when the response is the proportion of successes." ==> Det er det IKKE i mit data. 
  # altså skal det være en vector of integers - det kan IKKE være decimaltal. 
# Det betyder måske, at jeg slet ikke kan bruge den slags vægte i binomial glm.
# Scaling the weights will change the results because you change the NUMBER OF TRIALS.
# More trials = more information = lower variances.
# ... So you only need to give the weights when the response is expressed as a ratio. If you have it as a binary variable or as cbind(NummerOfSuccesses,NumberOfFailures) then you don't need weights.
# fra https://www.mail-archive.com/r-help@r-project.org/msg92681.html 
# Der er en, der siger, at data enten skal være struktureret på den "proportion of succes måde" eller at jeg skal give glm en starting value, fx beta=1 eller whatever # her https://www.mail-archive.com/r-help@r-project.org/msg92706.html 

# "For true likelihood based binomial GLMs, the weight argument is determined by the number of trials and cannot be varied."
  # Ellers skulle jeg bruge quasi likelihood eler pseudo binomial ! kilde: https://stats.stackexchange.com/questions/386675/what-are-weights-in-a-binary-glm-and-how-to-calculate-them 

# augment(glm.centralisering)
glm.resids <- augment(glm.centralisering) %>%
  mutate(p = 1 / (1 + exp(-.fitted)),
         predicted = predict(glm.centralisering, type = "response")) %>%
  arrange(desc(p))

summary(glm.resids$predicted)

# Jeg mistænker vægtene for at være for store og uhåndterbare. Normaliserer spend_mid (så den er skaleret 0-1):
fb_ads <- fb_ads %>%
  mutate(spend_mid_norm = (spend_mid-min(spend_mid))/(max(spend_mid)-min(spend_mid)))

# tjekker om det virker
par(mfrow=c(1,2))
hist(fb_ads$spend_mid, xlab="Data",  col="lightblue", main="")
hist(fb_ads$spend_mid_norm, xlab="Normaliseret Data", col="lightblue", main="")
summary(fb_ads$spend_mid)
summary(fb_ads$spend_mid)
fb_ads %>%
  count(spend_mid_norm)
fb_ads %>%
  count(spend_mid)

fb_ads %>%
  filter(kandidat_d == 1) %>%
  count(spend_mid)

# Undersøger de forudsagte værdier
predict(glm.centralisering, type = "response")
# Mange af dem viser probability = 1 ! Hvordan kan det være?
eps(1)

augment(glm.centralisering)
glm.resids <- augment(glm.centralisering) %>%
  mutate(p = 1 / (1 + exp(-.fitted)),
         predicted = predict(glm.centralisering, type = "response")) %>%
  arrange(desc(p))

glm.resids %>%
  count(.fitted)

# prøver lige en anden version (update: det her er noget fis)
scalar1 <- function(x) {x / sqrt(sum(x^2))}
scalar1(distinct(fb_ads$spend_mid))
summary(scalar1(fb_ads$spend_mid))
# tjekker om det virker
par(mfrow=c(1,2))
hist(fb_ads$spend_mid, xlab="Data",  col="lightblue", main="")
hist(scalar1(fb_ads$spend_mid), xlab="Normaliseret Data", col="lightblue", main="")

summary(glm.centralisering)
glm.centralisering$eps

fb_ads%>%
  count(ALDER) %>%
  tail()


################# VIDERE (ikke mere vægt-problem) ########################

# Eksporterer resultat til latex via texreg 

texreg(list(lm.centralisering.0, lm.centralisering.01, lmr.centralisering.uk, lmr.centralisering.fk, lmr.centralisering.ak), 
       dcolumn = TRUE, booktabs = TRUE, use.packages = FALSE, label = "tab:pers_lin", caption = "Lineære regressionermodeller", float.pos = "hb!",
       include.rmse = F, include.ci = F, include.rsquared = F, custom.note = "En lille forklaring", stars = 0.05,
       custom.gof.rows = list("Klyngerobust" = c("Nej", "Nej", "Ja", "Ja", "Ja"), 
                              "Vægtet" = c("Nej", "Ja","Ja", "Ja", "Ja"))
       #model.names = c("t-test", "cluster robust", "vægtet", "få kontrolvariable", "alle kontrolvariable"))
)


# Test, som evt. kan komme på https://github.com/leifeld/texreg/issues/128 ? vent med det lort. skriv specialet. 
x <- rnorm(100, 5)
y <- rnorm(100, 10)
lm.a <- lm(y ~ x)
lm.b <- lm(y ~ x -1)
texreg(list(lm.a, lm.b),
       custom.gof.rows = list("Intercept" = c("Yes", "No")))
       

texreg(list(lm.centralisering.0, lm.centralisering.01),
       custom.gof.rows = list("Cluster" = c("Nej", "Nej"), "Vagtet" = c("Nej", "Ja"))) # konklusion: custom.gof.rows virker vist ikke. 
  # Jeg kan selv skrive linjerne i latex. 

list("Random effects" = c("YES", "YES", "NO"), Observations = c(25, 25, 26))
# tester texre
extr_centralisering<- extract.lm_robust(lm.centralisering, include.ci = F,
                                        include.rmse = F, include.fstatistic = F,
                                        include.rsquared = F)

texreg.centralisering <- texreg(extr_centralisering, dcolumn = TRUE, booktabs = TRUE, 
                                use.packages = FALSE, 
                                label = "tab:pers_model", caption = "Selvpersonalisering statistiske tests.",
                                float.pos = "hb!", custom.note = "En lille forklaring",
                                digits = 2, leading.zero = T, #omit.coef = "Inter",
                                stars = 0.05)


# Det kunne godt tyde på en konkav sammenhæng mellem alder og personalisering?
fb_ads %>%
  filter(kandidat_d == 1) %>%
  count(ALDER) %>%
  tail()

fb_ads %>%
  filter(kandidat_d == 1) %>%
ggplot(aes(ALDER, selvpersonaliseret)) +
  stat_smooth(formula = selvpersonaliseret ~ poly(ALDER,2)) +
  theme_bw()
# OVervej poly(ALDER, 2) # OBS! missing values are not allowed in 'poly'! ikke når jeg bruger orthogonal. brug måske bare raw = T.

# Klar til eksort
tidy_centralisering <- tidy(lm.centralisering) %>%
  select(-starts_with("conf"), -df, -outcome) %>% 
  rename("t value" = "statistic")

# Eksport
xtable(tidy_centralisering, auto = T, label = "tab:lm centralisering", caption = "Signifikanstest centraliseringshypotesen",
       digits = 2)

plot(lm.centralisering, 1)
# FHJ: Det ser fint ud med "stata". 
# Overvej at have hv-placering, men lad være, hvis det er balanceret på tværs af højre/venstre.
# overvej subset kun på billige7 dyre annoncer i stedet for weight. 
# vægte giver fin mening . 
# okay det ser fint. 
  # Jeg vægter observationerne i forhold til annoncepris.
# Jeg er lidt i tvivl om jeg kan have både centralisering og parti med som predictors.  
# Jeg kan tage hv-placering i stedet (ideologi)



summary(lmrobust) # såå... Hypotesen er vist afkræftet. Jo mere centraliserede partierne er, 
# desto mere selvpersonaliserede er deres kandidaters annoncer! 
# NEJ. Det er et INSIGNIFIKANT resultat. 

# Hvis vi nu leger, at selvpersonaliseret = et politisk emne, hvordan vil testen så se ud?
# = Meget på samme måde!
lm_emnetest <- lm_robust(selvpersonaliseret ~ Højreorienteret, cluster = PARTI,
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
ggplot(aes(x = reorder(STEMMESEDDELNAVN, total_spend), y = total_spend, fill = selvpersonaliseret)) + 
  geom_bar(position = "stack", stat = "identity") + coord_flip() + theme_minimal()
    

# Lav gerne sådan et plot https://www.r-graph-gallery.com/299-circular-stacked-barplot.html


# Johan vejledning noter: Det ser super ud. Skriv det færdigt. 
# 1) Tjek face-value: top 3 og bund 3 personaliserede og ikke-personaliserede annoncer. (ideal types?) valider!
    # note to self: Det ser også bedre ud end at tage de ord, det er stimeret på baggrund af. 



# Nyt: Personalisering for partierne  -------------------------------------

# Her laver jeg regressionsmoddelerne med PARTI som den forklarende variabel

# Vigtigt: Ændrer rækkefølgen af partierne, så V bliver ref kategori og partierne kommer i rækkefølgen 1:7 decentraliseret og 9:14 centraliseret. P er NULL for kandidat_d == 1
fb_ads <- fb_ads %>%
  mutate(PARTI = fct_relevel(fb_ads$PARTI, "V", "AA", "B", "C", "K", "E", "N", "P", "A", "OE", "F", "O", "I", "D"))
fb_ads$PARTI

# Model 0: lm uden vægt
m0.parti <- lm(selvpersonaliseret ~ PARTI -1, data = fb_ads, subset = kandidat_d == 1)

# Model 0.1: lm vægtet
m01.parti <- lm(selvpersonaliseret ~ PARTI, data = fb_ads, subset = kandidat_d == 1,
                           weights = spend_mid)

# Model 1: få kontrolvariable
m1.parti <- lm(selvpersonaliseret ~ PARTI + KØN + ALDER, data = fb_ads, subset = kandidat_d == 1,
               weights = spend_mid)

# Model 2: Alle kontrolvariable
m2.parti <- lm(selvpersonaliseret ~ PARTI + KØN + ALDER + EP + instagram, data = fb_ads, subset = kandidat_d == 1,
                       weights = spend_mid)

# Og en uvægtet glm binomial for sjov
glm.parti <- glm(selvpersonaliseret ~ PARTI + KØN + ALDER + EP + instagram, data = fb_ads, subset = kandidat_d == 1,
                 weights = NULL, family = "binomial")


# Eksporterer med texreg
# lave factor grupe variabel
g.centraliseret <- gl(2,10,20, labels = c("Ctl","Trt"))

f.centraliseret <- fb_ads %>%
  filter(!PARTI %in% "P") %>%
  distinct(PARTI) %>%
  pull()


f.centralisering2 <- fct_collapse(f.centraliseret, centralisderet = c("A", "OE", "F", "O", "I", "D"), decentraliseret = c("AA", "B", "V", "C", "K", "E", "N"), NULL = "P")


texreg(list(m0.parti, m01.parti, m1.parti, m2.parti, glm.parti), 
       dcolumn = TRUE, booktabs = TRUE, use.packages = FALSE, label = "tab:pers_parti", caption = "Parti pers", float.pos = "hb!",
       include.rmse = F, include.ci = F, include.rsquared = F, custom.note = "Model 1 til 4 er lineære regressioner. Model 2 til 4 er vægtet.\nModel 5 adskiller sig ved at være en uvægtet logistisk regression", stars = 0.05,
       groups = list("Decentraliseret nominering" = 1:7, "Centraliseret nominering" = 8:13),
       include.aic = F, include.bic = F, include.loglik = F, include.dev = F,
       custom.gof.rows = list("Model" = c("lm uvægtet", "lm vægtet", "lm vægtet", "lm vægtet", "glm uvægtet")))
       # model.names = c("Uvægtet", "Vægtet", "Vægtet ", "få kontrolvariable", "alle kontrolvariable"))) # custom række virker stadig ikke. 

# Centraliseret nomineringsproces - BAre til at huske! 
#fb_ads <- fb_ads %>%
#  mutate(centralisering = case_when(PARTI %in% c("A", "OE", "F", "O", "I", "D") ~ 1,
#                                    PARTI %in% c("AA", "B", "V", "C", "K", "E", "N") ~ 0,
#                                    T ~ NA_real_))

# NYT: Personalisering for kandidaterne #########################


# Tilføjer et en grænseværdi i data: Hvorvidt andelen af annoncekroner er over eller under kandidaternes gennemsnit totalt set
kandidater <- fb_ads %>%
    filter(kandidat_d == 1) %>%
    mutate(spend_pers = selvpersonaliseret*spend_mid,
         spend_pers_min = selvpersonaliseret*spend_lower_bound) %>%
    group_by(STEMMESEDDELNAVN, PARTI) %>%
  summarise(spend = sum(spend_mid),
            pers_spend = sum(spend_pers),
            share_pers_spend = pers_spend/spend,
            spend_min = sum(spend_lower_bound),
            spend_pers_min2 = sum(spend_pers_min)) %>% 
  arrange(desc(pers_spend))
#spend_perstest = sum(selvpersonaliseret*spend_mid))
colSums(parti_resultat[,2:6]) # Nej det kan ikke passe at de har brugt min. 3.75 mio. kroner?
# Update: Jo, det kan det sagtens. I valgkampens sidste 4 uger brugte kandidaterne lidt over 6 mio. kroner.
# colSums driller med scientific notation. slukker det:
# options(scipen = 500)

# Hvor meget har partiernes kandidater brugt i alt?
kandidat_spend_by_parti <- kandidater %>%
  group_by(PARTI) %>%
  summarise(parti_spend = sum(spend)) %>%
  arrange(desc(parti_spend))

# tilføjer til kandidater df
kandidater <- left_join(kandidater, kandidat_spend_by_parti)

kandidater %>%  
  mutate(farve = if_else(share_pers_spend > .345656, "Over gennemsnit", "Under gennemsnit")) %>%
  ggplot(aes(x= PARTI, y = share_pers_spend)) +
  geom_segment(aes( x = spend, xend = spend, y = .345656, yend = share_pers_spend, color = farve)) +
  geom_linerange(aes(x = spend, ymin = .345656, ymax = share_pers_spend, color = farve)) +
  theme_minimal() +
  theme(
    legend.position = "none",
    panel.border = element_blank(),
  ) +
  xlab("Totalt annonceforbrug per kandidat (DKK)") +
  ylab("Andel kroner brugt på personaliserede annoncer") +
  facet_wrap(~reorder(PARTI, -parti_spend)) +
  scale_x_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
                              labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  scale_y_continuous(labels = scales::percent) 
# Jeg kan også bruge geom_linerange() med følgende info:
   #ggplot(aes(x= reorder(STEMMESEDDELNAVN, spend), y = share_pers_spend, ymin = .345656, ymax = share_pers_spend, group = PARTI)) +
   #  geom_linerange(aes(x = log(spend), color = farve))

#ggsave("kandidat_pers_figur.pdf", width = 7, height = 5)

# Prøver lige at lave et boxlot
kandidater %>%
  mutate(farve = if_else(share_pers_spend > .345656, "Over gennemsnit", "Under gennemsnit")) %>%
  ggplot(aes(x = reorder(PARTI, -parti_spend), y = spend)) + 
  geom_boxplot(aes(colour = farve), varwidth = F) + # position = position_dodge2(preserve = "single")) + 
  scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
                labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  # annotation_logticks(sides = "b") + virker ikke
  theme_minimal() +
  theme(legend.position = "bottom") +
  coord_flip() +
  xlab("Partibogstav") +
  ylab("log(kandidaternes totale annonceforbrug") +
  geom_jitter(size=0.1, alpha=0.9)

# Jeg synes ikke, boxplot giver så meget mening, fordi det vægter de enkelte observationer ens, når det beregner et gennemsnit?
# Jeg kører lige en graf uden det. 


kandidater %>%
  mutate(Selvpersonaliseret = if_else(share_pers_spend > .345656, "Over gennemsnit", "Under gennemsnit")) %>%
  ggplot(aes(x = reorder(PARTI, -parti_spend), y = spend)) + 
  geom_point((aes(colour = Selvpersonaliseret)), position = position_jitter(w = 0.2, h = 0.02), size = 3) + # position = position_dodge2(preserve = "single")) + 
  # pas på med jitter h = fordi den rykker på prikkerne horisontalt. Giver den kun en smule jitter.
  scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
                labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  # annotation_logticks(sides = "b") + virker ikke
  theme_minimal() +
  theme(legend.position = "none") +
  coord_flip() +
  xlab("Partibogstav") +
  ylab("log(kandidaternes totale annonceforbrug") 
#ggsave("kandidat_pers_prik.pdf", width = 7, height = 6)

kandidater %>%
  filter(PARTI == "OE") %>%
  arrange(desc(spend))

fb_ads %>%
  filter(STEMMESEDDELNAVN == "Rosa Lund") %>%
  select(ad_id, selvpersonaliseret, ad_creative_body)



# Marts nyt ---------------------------------------------------------------

# Jeg forbedrer visualiseringerne i analyseafsnittet. 
  # Laver et stacked bar plot i stedet for tabellen med spend
  # Laver et swarm plot i stedet for det rodede dotplot. 

# SWARM PLOT
# Anvender ggbeeswarm som er en udvidelse til ggplot2.

set.seed(1603)

# Sætter KU farver
ku_rød <- rgb(144, 26, 30, maxColorValue=255, alpha=255)
ku_grå <- rgb(102, 102, 102, maxColorValue=255, alpha=255)

# For dem, der har brugt max 100.000 kroner
kandidater %>%
  mutate(pers_gns = if_else(share_pers_spend > .345656, "Over gns.", "Under gns.")) %>%
  ggplot(aes(x = reorder(PARTI, -parti_spend), y = spend/1000, colour = pers_gns)) + # spend i tusinde kroner
  geom_beeswarm(dodge.width = -.75) +
  theme_minimal() +
  coord_flip() +
  labs(x = "Partibogstav", y = element_blank(), color = "Andel selvpersonaliseret",
       title = "Kandidaternes annonceforbrug (tusinde kroner)") + #,
       # caption = "Note: Hver prik repræsenterer 1 kandidat") +
  scale_y_continuous(limits = c(0, 100)) +
  theme(legend.position = "bottom") +
  scale_color_manual(values = c(ku_rød, "#4E84C4"))
ggsave("pers_spend_dot100.pdf", width = 6, height = 5)

# For dem, der har brugt over 100.000 kroner
kandidater %>%
  mutate(pers_gns = if_else(share_pers_spend > .345656, "Over gns.", "Under gns.")) %>%
  ggplot(aes(x = reorder(PARTI, -parti_spend), y = spend/1000, colour = pers_gns)) + # spend i tusinde kroner
  geom_beeswarm(dodge.width = -.75) +
  theme_minimal() +
  coord_flip() +
  labs(x = "Partibogstav", y = "Kandidaternes totale annonceforbrug (tusinde kroner)", color = "Andel selvpersonaliseret") +
  scale_y_continuous(limits = c(100, 800), breaks = c(seq(100,800,100))) +
  theme(legend.position = "bottom", legend.justification='right') +
  scale_color_manual(values = c(ku_rød, "#4E84C4"))
ggsave("pers_spend_dot800.pdf", width = 6, height = 5)

# Overvej at navngive nogle dots. 

# De ser fine ud nu. Nr. 2 ryger i bilag.