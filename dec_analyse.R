library(pacman)
p_load(tidyverse)
fb_ads2 <- read_rds("FB API data/data/fb_ads2.rds")

#tjekker lige deskriptive ting

glimpse(fb_ads2)

fb_ads2 %>%
  filter(parti_selv_d_samlet ==1 & jeg_d ==1) %>%
  select(ad_creative_body) %>%
  sample_n(5)

glimpse(fb_ads)

bib <- fb_ads %>%
          count(spend_interval)
xtable(bib, type = "latex")

# Afsenderindekset: Hvor mange annoncer kommer fra hhv. parti- og kandidatsider?
fb_ads2 %>%
  group_by(kandidat_d) %>%
  count()
# Resulat: 3.808 fra partisider og 10.382 fra kandidatsider. Umiddelbart meget personaliseret.

# Centraliseret personalisering: Hvor stor en andel udgør partilederne?
partiledere_n <- fb_ads2 %>%
  filter(STEMMESEDDELNAVN %in% partileder) %>%
  group_by(PARTI) %>%
  summarise(n_partileder = n()) # 1.001 annoncer kommer fra partiledern (heraf 502 fra Klaus Riskær Pedersen)
# Pia O. Dyhr kommer på en 2. plads med 156 ad buys.

# Tilføjer vægtet parti-leder kolonne (efter spend)

partiledere_spend <- fb_ads2 %>%
  filter(STEMMESEDDELNAVN %in% partileder) %>%
  group_by(PARTI) %>%
  summarise(spend_partileder = sum(spend_mid))

# Laver også lige en total n_ads kolonne.
n_ads <- fb_ads2 %>%
  group_by(PARTI) %>%
  summarise(n_ads = n())

# samler de tre kolonner i en df

partiledere_df <- left_join(partiledere_n, partiledere_spend) %>%
  left_join(n_ads)

# tilføjer de to kolonner til afsender-indeks

glimpse(afsender_indeks)

# Laver en total spend og tilføjer den til vægtet_indeks
total_spend <- fb_ads2 %>%
  group_by(PARTI) %>%
  summarise(spend = sum(spend_mid))

# tilføjer også en spend_partileder
spend_partileder <-  fb_ads2 %>%
  filter(STEMMESEDDELNAVN %in% partileder) %>%
  group_by(PARTI) %>%
  summarise(spend_partileder = sum(spend_mid))

vægtet_indeks <- uvægtet_indeks %>%
  left_join(partiledere_df, by = c("feature" = "PARTI")) %>%
  replace(is.na(.), 0) # erstatter NA med 0.

vægtet_indeks <- vægtet_indeks %>%
  left_join(total_spend) %>%
  left_join(spend_partileder) %>%
  replace(is.na(.), 0)

# Laver en første deskriptiv df til print:
antal_ads <- uvægtet_indeks %>%
  select(feature, n_ads, n_parti, n_kandidat, n_partileder, afs_indx)

spend_ads <- vægtet_indeks %>%
  select(PARTI, spend, spend_parti, spend_kandidat, spend_partileder, wny_afs_indx)

# Laver rigtige resultater til aktørindeks og får dem ud i en df
  # Dvs. resultater, som bygger på parti_selv_samlet og ikke kun parti-ordbøgerne.

glimpse(fb_ads2)
aktør_resultat <- fb_ads2 %>%
  group_by(PARTI) %>%
  summarise(n_ads = n(),
            kandidat_aktør = sum(jeg_d),
            parti_aktør = sum(parti_selv_d_samlet))
# Tilføjer "ingen aktør" og "begge aktører"

ingen_aktør <- fb_ads2 %>%
  filter(jeg_d == 0 & parti_selv_d_samlet == 0) %>%
  group_by(PARTI) %>%
  summarise(ingen_aktør = n())

# kandidat og parti som aktør
begge_aktør <- fb_ads2 %>%
  filter(jeg_d == 1 & parti_selv_d_samlet == 1) %>%
  group_by(PARTI) %>%
  summarise(begge_aktør = n())

begge_aktør <- left_join(begge_aktør, ingen_aktør)

# tilføjer til aktør df

aktør_uvægtet <- left_join(aktør_resultat, begge_aktør) %>%
  replace(is.na(.), 0)

# tilføjer uvægtet aktørindeks til df
aktør_uvægtet <- left_join(aktør_uvægtet, select(uvægtet_indeks, feature, akt_indx), by = c("PARTI" = "feature"))


personalisering_resultat <- read_csv("personlig_ny_resultat.csv")

# SÅ ER VI VIST KLAR TIL PRINT. Sørg for, at partierne optræder i samme rækkefølge.
# ønsket rækkefølge:
række <- c("A", "B", "C", "D", "E", "F", "I", "K", "N", "O", "P", "V", "OE", "AA")
length(række) # 14 partier. 

# ordner rækkefølgen i afsender, antal og spend:
p_antal_ads <- antal_ads %>%
  slice(match(række, feature))

p_spend_ads <- spend_ads %>%
  slice(match(række, PARTI))

# Ordner rækkefølgen i aktør uvægtet og personalisering resultat

p_aktør_uvægtet <- aktør_uvægtet %>%
  slice(match(række, PARTI))

p_personalisering_resultat <- personalisering_resultat %>%
  slice(match(række, PARTI))

# SUcces. Så laver jeg det om til latex, så jeg kan printe:
xtable(p_antal_ads, type = "latex")
xtable(p_spend_ads, type = "latex")
xtable(p_aktør_uvægtet, type = "latex")
xtable(p_personalisering_resultat, type = "latex")



# tilføjer kolonne med antal partileder

# Objective: Lav en stor infotabel og tag med i dag. Også med penge. 

# Hvor mange kandidatannoncer har de enkelte partier promoveret?
n_kandidat <- fb_ads %>%
  filter(kandidat_d == 1) %>%
  group_by(PARTI) %>%
  summarise(n_kandidat = n())

# Hvor mange parti-annoncer har de enkelte partier promoveret?
n_parti <- fb_ads %>%
  filter(kandidat_d == 0) %>%
  group_by(PARTI) %>%
  summarise(n_parti = n())

afsender_indeks <- full_join(n_kandidat, n_parti) %>%
  mutate(n_parti = replace_na(n_parti, 0),
         n_kandidat = replace_na(n_kandidat, 0)) # erstatter NA med 0

afsender_indeks <- afsender_indeks %>%
  mutate(afs_indx = n_kandidat/n_parti) %>%
  arrange(desc(afs_indx))

# Tilføjer parti-bogstav så jeg kan merge

# afsender_indeks <- afsender_indeks %>%
#   semi_join(select(fb_ads, parti_navn, PARTI), by = "par")
# 
# afsender_indeks

# Merger med aktør_indeks

#################### ARBEJDER MED AKTØR OG AFSENDER-INDEKS FRA fb_ads2 #######################

# fb_ads2 <- read_rds("fb_ads2.rds")

# OBS! jeg kører bare videre med fb_ads her, fordi de er identiske lige nu.

# Her har jeg kopieret noget kode fra Quanteda script til at se resultaterne

# Er de gensidigt udelukkende? Ja det kunne godt tyde på det!!! Kun 181 parti-annoncer ud af 3.808 skriver "jeg" (4.75 %)
# I modsætning hertil skriver 6.651 ud af 10.382 kandidat-annoncer om sig selv (64 %)
fb_ads %>%
  group_by(kandidat_d) %>%
  count(jeg_d)

# I 2.061 (NY: 2.542) af kandidaternes annoncer bliver deres parti nævnt (24.49 %)
# I 8.321 (NY 7.740) gør det ikke.
# Andelen af parti-sider, som nævner deres eget parti, er lidt større: 1.074 ud af 3.808 (28.20 %)
fb_ads %>%
  group_by(kandidat_d) %>%
  count(parti_selv_d_samlet)

# FORLØBIGT RESULTAT: KANDIDATERNE SNAKKER OFTE OM DEM SELV OG PARTIET. PARTIET SNAKKER LIGE MEGET OM BEGGE DELE

# Afgørende spørgsmål ift. gensidigt udelukkende: Hvor mange af annoncerne nævner begge dele?
fb_ads %>%
  filter(parti_selv_d_samlet == 1 & jeg_d == 1) %>%
  group_by(kandidat_d) %>%
  count()# Det gør kun 1.049 (NU 1.741) ud af 14.190. Det er da meget godt?
# heraf er 77 parti-sider og 1.664 kandidat-sider

# og ingen af delene?
fb_ads %>%
  filter(parti_selv_d_samlet == 0 & jeg_d == 0) %>%
  group_by(kandidat_d) %>%
  count() # Det gør 5.3.83 ud af 14.190.
# heraf er 2.630 parti-annoncer og 2.753 kandidat-annoncer 
  # GODT SPØRGSMÅL: Hvad handler disse annoncer om, hvis ikke kandidaterne eller partierne?

fb_ads %>%
  filter(parti_selv_d_samlet == 0 & jeg_d == 0) %>%
  select(ad_creative_body) %>%
  print(n=50) %>%
  writeLines()

# HEY ! Det ser faktisk ud som en rigtig fin inddeling!!
# De fleste af disse annoncer handler direkte om politik, "vi" eller "Danmark". 
# Måske jeg skulle lave en "vi" "danmark" ordbog og sammenligne med - måske har jeg nogle forskellige dimensioner ift. aktør her!


######### NYE, VÆGTEDE INDEKS ##############

glimpse(fb_ads)

# STRATEGI: Jeg vægter ift. midtpunktet i spend intervallet for hver annonce. 
# Dvs. jeg erstatter 1-taller med spend for at få en vægtet variabel.
fb_ads %>%
  count(impressions_spend)

cor(fb_ads$spend_mid, fb_ads$impressions_mid) # 0.83. Der er meget stærk sammenhæng mellem spend og hvor mange, der ser en ad.
# imponerende korrelation, når vi tænker på at data er så upræcis? I virkeligheden må den være højere?

fb_ads <- fb_ads %>%
  mutate(jeg_d_spendw = jeg_d * spend_mid, # spend_mid for hver kandidatannonce (aktør)
         parti_selv_d_spendw = parti_selv_d_samlet * spend_mid) # spend_mid for hver parti-annonce (aktør)

# Hvor mange penge har parierne brugt på kandidatannoncer?
spend_kandidat <- fb_ads %>%
  filter(kandidat_d == 1) %>%
  group_by(PARTI) %>%
  summarise(spend_kandidat = sum(spend_mid))

# Hvor mange penge har partierne brugt på partiannoncer?
spend_parti <- fb_ads %>%
  filter(kandidat_d == 0) %>%
  group_by(PARTI) %>%
  summarise(spend_parti = sum(spend_mid))

# Hvor mange penge har partierne brugt på at promovere partiannoncer relativt ift. kandidatannoncer? (det vægtede afsenderindeks):    
w_afsender_indeks <- full_join(spend_kandidat, spend_parti) %>%
  mutate(spend_parti = replace_na(spend_parti, 0),
         spend_kandidat = replace_na(spend_kandidat, 0)) # erstatter NA med 0

w_afsender_indeks <- w_afsender_indeks %>%
  mutate(w_afs_indx = spend_kandidat/spend_parti) %>%
  arrange(desc(w_afs_indx))

### Ser fint ud so far. Tilføjer det vægtede aktørindeks

# Hvor mange penge har parierne brugt på annoncer OM kandidater (aktøren i annoncen)?
spend_jeg <- fb_ads %>%
  filter(jeg_d == 1) %>%
  group_by(PARTI) %>%
  summarise(spend_jeg = sum(spend_mid))

# Hvor mange penge har partierne brugt på annoncer OM partier (aktøren i annoncen)?
spend_partiet <- fb_ads %>%
  filter(parti_selv_d_samlet == 1) %>%
  group_by(PARTI) %>%
  summarise(spend_partiet = sum(spend_mid))

# Hvor mange penge har partierne brugt på at promovere annoncer om partier relativt ift. annoncer om kandidater? (det vægtede aktørindeks):    
w_aktør_indeks <- full_join(spend_jeg, spend_partiet) %>%
  mutate(spend_partiet = replace_na(spend_partiet, 0),
         spend_jeg = replace_na(spend_jeg, 0)) # erstatter NA med 0

w_aktør_indeks <- w_aktør_indeks %>%
  mutate(w_akt_indx = spend_jeg/spend_partiet) %>%
  arrange(desc(w_akt_indx))

# Samler det vægtede afsender- og aktørindeks i en df

w_indeks_samlet <- left_join(w_afsender_indeks, w_aktør_indeks)

# Tilføjer samme sammme type indeks, som Rahat & Kenig bruger (skaleret fra -1 til 1 hvor 1 er max personfokus)

w_indeks_samlet1 <- w_indeks_samlet %>%
  # Jeg sætter den højeste værdi i nævneren så alle værdier er mellem 0-1
  mutate(wny_akt_indx = case_when(
    spend_jeg < spend_partiet ~ spend_jeg/spend_partiet,
    spend_jeg > spend_partiet ~ spend_partiet/spend_jeg,
    TRUE ~ NA_real_),
    wny_afs_indx = case_when(
      spend_kandidat < spend_parti ~ spend_kandidat/spend_parti,
      spend_kandidat > spend_parti ~ spend_parti/spend_kandidat,
      TRUE ~ NA_real_)
  )

# Partier med flere parti-annoncer end kandidat-annoncer gør giver jeg negative værdier.
w_indeks_samlet1 <- w_indeks_samlet1 %>%
  mutate(wny_akt_indx = case_when(
    spend_jeg < spend_partiet ~ -abs(wny_akt_indx),
    TRUE ~ wny_akt_indx),
    wny_afs_indx = case_when(
      spend_kandidat < spend_parti ~ -abs(wny_afs_indx),
      TRUE ~ wny_afs_indx)
  )

# ændrer rækkefølge af kolonner, så afsender er først og aktør sidst:
w_indeks_samlet1 <- w_indeks_samlet1[c(1,2,3,4,9,5,6,7,8)]

# Succes. Gemmer resultatet
# write_csv(w_indeks_samlet1, "w_aktør_indeks_resultat.csv")

vægtet_indeks <- read_csv("w_aktør_indeks_resultat.csv")

# og den uvægtede:
uvægtet_indeks <- read_csv("indeks_resultat1.csv")

# Laver 2 bar charts for at illustrere det

# Afsender indeks uvægtet
indeks_resultat1 %>%
ggplot(aes(x = reorder(feature, afs_indx), y = afs_indx)) +
  geom_bar(stat = "identity") + 
  xlab("Parti") +
  theme_minimal() +
  labs(title = "Afsender indeks",
       subtitle = "Viser det relative forhold mellem antal kandiat-\nannoncer og partiannoncer for hvert parti",
       caption = "Højere værdi = højere grad af personalisering")
ggsave("afs_indx.png")

# AKtør indeks uvægtet
indeks_resultat1 %>%
  ggplot(aes(x = reorder(feature, akt_indx), y = akt_indx)) +
  geom_bar(stat = "identity") + 
  xlab("Parti") +
  theme_minimal() +
  labs(title = "Aktør indeks",
       subtitle = "Viser forholdet mellem antal annoncer om kandidater \nog annoncer om partier for hvert parti",
       caption = "Højere værdi = højere grad af personalisering")
ggsave("akt_indx.png")

# Afsender indeks vægtet
w_indeks_samlet1 %>%
  ggplot(aes(x = reorder(PARTI, wny_afs_indx), y = wny_afs_indx)) +
  geom_bar(stat = "identity") + 
  xlab("Parti") +
  theme_minimal() +
  labs(title = "Afsender indeks vægtet",
       subtitle = "Afsenderindeks vægtet efter annoncepris",
       caption = "Højere værdi = højere grad af personalisering")
ggsave("w_afs_indx.png")

# Aktør indeks vægtet
w_indeks_samlet1 %>%
  ggplot(aes(x = reorder(PARTI, wny_akt_indx), y = wny_akt_indx)) +
  geom_bar(stat = "identity")  + 
  xlab("Parti") +
  theme_minimal() +
  labs(title = "Aktør indeks vægtet",
       subtitle = "Aktørindeks vægtet efter annoncepris",
       caption = "Højere værdi = højere grad af personalisering")
ggsave("w_akt_indx.png")

# tesseract sjov
# jeg kan whiteliste så den kun søger efter bogstaver:
a_z <- paste0(letters, LETTERS, collapse ="")
a_å <- paste0(a_z, c("æÆøØåÅ "))
# Update: Det her med whitelist ødelagde resultatet. væk med det igen. 
eilersen <- ocr("https://bit.ly/2RAadFG", 
                engine = tesseract("dan",
                options = list(tessedit_char_whitelist = a_å)))
ocr_eilersen <- ocr_data("https://bit.ly/2RAadFG", 
                         engine = tesseract("dan",
                         options = list(tessedit_char_whitelist = a_å)))
print(ocr_eilersen, n = 50)
# Der er meget støj med, men det virker, hvis jeg sorterer efter confindence:
ocr_eilersen %>%
  filter(confidence > 90) %>%
  select(word) %>%
  print(n=25)

writeLines(eilersen)
