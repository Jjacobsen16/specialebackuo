# 1 Indlæser pakker og data ####################

library(pacman)
p_load(tidyverse, readtext, quanteda, xtable, tictoc, estimatr, texreg, miceadds, sjPlot, sjmisc, sjlabelled, effects, hrbrthemes, ggthemes, ggridges, scales)
# måske lubridate, topicmodels
options(scipen = 1) # skru ned for brugen af scientific notation
# theme_set(theme_sjplot()) # et lidt flottere standard tema


fb_ads <- read_rds("FB API data/data/fb_ads.rds") 


# Udvælger variable, som er relevante at have med i corpus, så jeg ikke har alt med:
corpus_data <- select(fb_ads, c(ad_id, ad_creative_body, page_name, PARTI, kandidat_d)) # parti_navn, KØN, ALDER, spend_mid, spend_interval, impressions_mid, impressions_interval, facebook, instagram))

corpus_body <- corpus(corpus_data, 
                      docid_field = "ad_id",
                      text_field = "ad_creative_body")

# Lav gerne et histogram over, hvor lange teksterne er.
sum(ntoken(corpus_body)) # 1.033.644
sum(ntoken(corpus_body, remove_punct = T)) # 885.905. Med numbers, uden punctuation og alle lower case.

# tokenizing
tokens_body <- tokens(corpus_body,
                      remove_numbers = T,
                      remove_punct = T,
                      remove_symbols = T,
                      remove_twitter = T, # For at fjerne hashtags
                      include_docvars = T) # 9.5 MB stort!

tokens_sentence <- tokens(corpus_body,
                      what = "sentence",
                      remove_numbers = T,
                      remove_punct = T,
                      remove_symbols = T,
                      remove_twitter = T, # For at fjerne hashtags
                      include_docvars = F) # 9.5 MB stort!

# Laver alle ord om til lower case
tokens_body <- tokens_tolower(tokens_body) # 9.2 MB
ndoc(tokens_body) # 14.190

# Antal ord i de 14.190 annoncer
sum(ntoken(tokens_body)) # 873877
# Antal sætninger i de 14.190 annoncer
sum(ntoken(tokens_sentence)) # 66.243 sætninger

# Antal ord per annonce
sum(ntoken(tokens_body))/ nrow(fb_ads) # 62 ord. 
summary(ntoken(tokens_body)) # gode stats

# Antal sætninger per annonce
sum(ntoken(tokens_sentence)) / nrow(fb_ads) # 4,7 sætninger
summary(ntoken(tokens_sentence)) # gode stats

# Operationalisering af ordbøger ------------------------------------------


kwic(tokens_body, phrase("*energi*")) # For at se privatliv-dictionary in-context!

# Emne dictiionary
emne_dict <- dictionary(list( # Skal IKKE bruge STEM!
  .okonomi = c("*økonomi*", "samfundsøkonomi*", "velstand*", "vækst*", "erhverv", "erhvervsliv*", "beskæftigelse*", "arbejdsudbud*",
              "arbejdskraft*", "arbejdsplads*", "konkurrenceevne*", "*virksomhed*",
              # nyt
              "afskrivning*", "aktie*", "*skat*", "bank*", "betalingsring*", "bruttonationalprodukt*", "bnp*", "brugerbetaling", "*budget*", "skattely*",
              "børs*", "offentlig* forbrug*", "finanssektor*", "eksport", "generationsskifte", "grænsehandel*", "gæld", "import*", "*obligation*", 
              "*investering*", "iværksæt*", "kapital*", "konkurs*", "*kredit*", "*fradrag*", "offentlig* sektor*", "offentlig* udgift*", "offentlig* ydelse*", 
              "overførselsindkomst*", "privatiser*", "*regnskab*", "likivd*", "*lån*", "arbejdsmarked*", "rente*", "udligningsreform*", "valuta*"
  ),
  # Ækonomi er svær at indkredse, fordi det er de færreste annoncer, som "alene" handler om økonomi. De handler for det meste om noget andet, men 
  # så omtales det økonomiske aspekt af det. Annoncer med "samfundsøkonomi handler fx ofte om "den samfundsøkonomisk bedste grønne omstilling" eller 
  # "stop letbanen - et samfundsøkonomisk underskud". 
  # Erhverv* er fjernet, fordi flere annoncer handler om erhvervsskoler eller om kandidaternes erhvervserfaring.
  # KOMMENTAR: Jeg er i tvivl om ordbogen er god nok til at blive brugt.
  # TIP! Jeg kan gøre sådan her: global* opvarmning*" for at gøre det mere specifikt. Måske jeg kan bruge her? 
  # Jeg undgår beskæftigelse*sminister/fradrag osv.
  # "kvalificeret arbejdskraft" i stedet for blot arbejdskraft, fordi det også handler om fx billig udl. arbejdskraft (social dumping). 
  .klima_miljo = c("klima*", "grøn*", "omstilling", "miljø*", "parisaftale*", "drikkevand*", "*forurening*", "natur", "naturen",
                  "greta", "thunberg*", "bæredyg*", "co2*", "c02*", "klode*", "*udledning*", "biodiversitet*", "global* opvarmning*",
                  # nye
                  "*affald*", "benzin*", "diesel*", "drivhus*", "*energi*", "genbrug*", "giftgrund*", "grundvand*", "*kraftværk*", "kystsikr*", "nordsø*", "olie*", "pesticid*", 
                  "sprøjtemid*", "vindmølle*", "økologi*", "økosystem*", "fossil*", "*brænds*", "solcelle*"
  ),
  # Ser rigtig fin ud. "miljø kan godt fange fx "miljø-og fødevareminister", men generelt rammer den godt.
  # Natur der er fjernet, da den fanger andre ting en lige præcis klima og miljø. Fx urørt skov, naturparker og natufredningsforeningen. Ikke helt det,
    # Det kommer på bekostning af enkelte annoncer, fx forslag om forbud mod bidræberting i O. Men sådan er det!
    # ja natur giver også "naturligvis" og "naturister". 
  # Klimahasttags: "#stemgrønnest", "#klimavalg", "#grøntvalg" er fjernet, da de er partispecifikke og kan give bias. Jeg fanger dem på mere "neutrale" stikord.
  # "gift" kunne godt være med, men det giver også bryllupper. "ret til at gifte sig og få børn"... # GODT EKS #
  .flygninge_indvandrere = c("indvandr*", "udlænding*", "flygtning*", "asyl*", "immigra*", "migra*", "migrer*", "kvoteflygtning*", 
                            "udrejsecent*", "sjælsmark*", "lindholm", "nærområde*", "paradigme", "paradigmeskifte*", "udvis*",
                            "islam*", "international* konvention*", "fattigdomsydelse*",
                            # neden for er ord inspireret af Hansen2013
                            "24-årsregl*", "familiesammenføring*", "apartheid", "ghetto*", "hjemsend*", "send* hjem*", 
                            "humanitær*", "menneskesmugling*", "racis*", "statsborgerskab", "tvangsægteskab*", "opholdstillad*", "uland*"
  ),
                            # noter:
                              # "diskrimination fravalgt, fordi det handler mest om handicappede.
                              # ghetto overlapper med boligpolitik, men sådan er det.
  # Ser fint ud, men der er en del snak om "kriminelle udlændinge". Har det mere med retspolitik og straf at gøre, eller overlapper emnet her?
  # "muslim" er fjernet, fordi den ikke var så præcis. Fx med muslimske friskoler, som er lidt en anden sag.
  # Af sammen grund har jeg også slettet "islam". Der går for meget religionsdebat i den, hvilket ikke har så meget med flygtninge og indvandrere at gøre?
    # ISLAM er tilføjet igen. Hansen13 har den også. 
  # MEN! Tjek om jeg fanger fx Nye Borgerliges annoncer om emnet - om deres sprogbrug er inkluderet i ordbogen. 
  # SJÆLSMARK handler meget om børn, som har det skidt. Overvej om det ER indvandre/ flygtningepolitik? Ja det er det jo.. 
  # "Lindholm" giver et par annoncer med en politiker, som deler morgenbrød ud i Nørresundby på Lindholm Station. Men det er så få! Det tager vi med. 
  # Ligeledes er der enkelte politiker, som "arbejder for flere behandlingsmuligheder i nærområdet" - altså lokalt. Men ganske få. 
  # "udvis" fjernet. Handler meget om kriminalitet. # UDVIS er tilføjet igen. Det er et overlap med kriminalitet, som jeg accepterer. 
  .integration = c("integr*", "velintegr*", "uintegr*", "parallelsamfund", "lære* sprog*", "danskundervisning*"),
  
  # UPDATE: Jeg SAMLER flygtninge_indvandrere og integration. Jeg forsøger blot at holde dem adskilt her for at vise forskelle i diskursen omkring flygtninge/ indvandrere, som netop er politisk bestemt.
  # I Hansen 2013 indgår "integration" fx også i deres flygtninge- og indvandrepolitik-ordbog. 
  
  .sundhed = c("sygehus*", "*hospital*", "*patient*", "*læge*", "*sygepleje*", "*operation*", "*sundhed*", "syg", "syge", "*sygdom*", "helbred*", "behandling*", "*psykiatri*", "*medicin*", 
              "familielæge*", "ambulance*", "akut*", "tandpleje*", "jord?m*", "psykolog*", "*støj", "*støjen", "støjbekæmpelse*", "støjsikring*", "støjmur*", "støjværn*",
    # TILFØJELSER
              "abort*", "adhd", "alkohol*", "cigaret*", "allergi*", "autisme*", "demens*", "depression*", "bipolar*", "drikkevand*", "dødshjælp*", "epilepsi*", "fysioterapi*",
    "genoptræning*", "gravid*", "hjerneskade*", "høreapparat*", "*kræft", "levealder*", "menstruation*", "prævention", "organdon*", "*rygning", "*skadestue*", "skizofreni", "stofmisbrug", 
    "sukkersyg*", "tilsætningsstof*", "*indlæggelse*", "udviklingshæmme*", "*vaccin*", "venteliste*", "ventetid*", "kemikalie*"
  ),
  # "sygeplejerske" handler en enkelt gang om professionsuddannelser (fx sygeplejerske). men ser fint ud. 
  # Nogle annoncer handler om udkanstdanmark, der bliver forsømt: "der er blevet meget længere til biblioteket, politistationen, sygehus og mange skoler lukker"... - sådan en kan jeg ikke undgå. 
  # "syg" er opdelt for at undgå annoncer med fx "sygelig" og "sygedagpenge". 
  # "akut" ser fin ud. Der er kun en enkelt dyrevelfærdsannonce
  # OBS! Både MILJØ og SUNDHED indeholder ordet "drikkevand" - men sådan er det jo! godt eksempel! 
  # Der er en lidt sjov "klima-vaccine". Sådan en kan man ikke undgå. 
  # "kemikalie" kommer med i sundhedsordbogen og ikke i miljø-ordbogen, fordi annoncer handler om fx babyers sundhed. 
  .aeldre = c("ældre*", "plejehjem*", "hjemmehjælp*", "hospice*", "alderdom*"),
  .eu = c("eu*", "*forbehold*", "brexit*", "schengen*", "ministerråd*", "*traktat*", "unionen*", "bruxelles", "strasbourg"), # "eu*" fanger også alt, der har med europa at gøre. # OBS! "ep" fjernet, fordi det meget er "husk at stemme til ep". Handler ikke om EU. 
  # "kommission" udeladt. Der er bl.a. tængsels- og trivselskommission, og den er ofte benævnte eu-kommissionen aligevel.
  .born = c("børn*", "vuggestue*", "*normering*", "daginstitution*", "barn*", "*barndom*", "*pædagog*", "opvækst", "dagpleje*", "sfo*", "barsel*", "tvangsfjerne*", "adopt*"), 
  # pædagog* fjernet, fordi det handler meget om professionen og deres forhold. TILFØJET igen. 
  # børn* giver lidt støj, fordi "kloden også skal være god at bo på for vores børn og børnebørn" etc. Jeg gør den mere specifik
  # Det hjælper vist lidt med børne*, fordi så er der mindre omtale af "børn" i almindelighed. "En ny regering, der tager børnene alvorligt", 
  # "børnene har brug for en stemme.. 
  .pension = c("*pension*", "tilbagetrækning*", "nedslid*", "slid* ned*", "senior*", "værdig")
  # nedslid* giver lidt støj med fx sygeplejersker og pædagoger, der bliver nedslidt.
)) # Overvej at adskille integration

set.seed(12) # for replicerbarhed

kwic(tokens_body, phrase("børn*")) # For at se privatliv-dictionary in-context!


kwic(tokens_body, phrase("børn*"), window = 7) %>%
  as_tibble() %>%
  select(pre:post) %>% # tiføjer de to linjer for at skjule doc_id
  sample_n(10) # 
# EVALUER ORDBOGEN JOHAN HER ER VI NÅET TIL
  # A) Evaluer de enkelte ord i ordbøgerne: Fanger de det, de skal, eller for lidt? Inspiration til nye ord i ordbogen?
  # B) Evaluer ordbøgernes resultater som helhed. Tag et udtræk af annoncer og undersøg ordbøgerne precision (andel true positives).

     # for at se ordbogen i aktion: emne_dict['okonomi'])

# OBS! Jeg har nu tjekket 100 annoncer uden emne ud. Det ser fint ud! Kun få skulle tilføjes. (False negatives)


# Anvender ordbogen
dfm_emne <- dfm(tokens_body, dictionary = emne_dict)# not_stemmed
dfm_emne <- dfm_sort(dfm_emne, margin = "features") #sorting dfm by both features and documents.
# Og hvis jeg gerne vil se antal unmatched features. 
# emne_resultat <- dfm_lookup(dfm_emne, dictionary = emne_dict, nomatch = "_UNMATCHED")
  # Jeg forstår ikke helt hvad den "unmatched" kolonne betyder. 

stats_emne <- textstat_frequency(dfm_emne) # Fordelingen af emner.
stats_parti <- textstat_frequency(dfm_emne, groups = "PARTI")

# Hvor stor en andel af de 14.190 ads kodes som klima?
emne_df <- dfm_emne %>%
  convert("data.frame")
glimpse(emne_df)

# VIGTIG: Omkoder smelter flygtninge_indvandrere og integration sammen
emne_df <- emne_df %>%
  mutate(.indvandrere = .flygninge_indvandrere + .integration) %>%
  select(-.flygninge_indvandrere, -.integration) # fjerner de gamle

# tilføjer total count/ zero count
emne_df <- emne_df %>%
  mutate(total_count = rowSums(select(.,-"document")))
glimpse(emne_df)

# Hvor mange af annoncerne er kategoriseret?
emne_df %>%
  filter(total_count> 0) %>%
  nrow() # 8531.

# Tjekker fordelingne af count INDEN jeg dikotomiserer
emne_df %>%
  count(.klima_miljo)

# Klima og miljø histogram
emne_df %>%
  filter(.klima_miljo >0) %>%
ggplot(aes(x = .klima_miljo)) +
  geom_density() +
  xlim(.1,10) + theme_minimal()

# Økonomi histogram
emne_df %>%
  filter(.okonomi >0) %>%
  ggplot(aes(x = .okonomi)) +
  geom_histogram(binwidth = 1) +
  xlim(1,20) + theme_minimal()

# VISUALISERING AF WORD COUNT FORDELING: RIDGEPLOT - viser density på flere variable samtidigt.
emne_df_long <- emne_df %>%
  pivot_longer(cols = starts_with("."), "total_count", names_to = "emne")

# Sætter KU farver
ku_rød <- rgb(144, 26, 30, maxColorValue=255, alpha=255)
ku_grå <- rgb(102, 102, 102, maxColorValue=255, alpha=255)

# Den her er flot 
emne_df_long %>%
  #filter(total_count > 0) %>% # Sorterer annoncer fra, der ikke tilhører nogen af de 8 emner (ligegyldig)
  ggplot(aes(x = value, y = emne)) +
  geom_density_ridges(fill=ku_rød) + # stat = "binline", bins = 10) + # stat="binline", bins=100) +
  theme_ridges(grid = F, center_axis_labels = T) +
  scale_x_continuous(breaks = c(1:19),
                     limits = c(.1,10)) +
  scale_y_discrete(labels= c("Ældre","Børn","EU","Indvandrere", "Klima og miljø", "Økonomi", "Pension", "Sundhed")) +
  labs(x = "Antal matches i ordbogen",
       y = NULL, # "Emneordbøger",
       title = "Fordeling for hver ordbog") 
       # fill = "Andel",
       # caption = "Figuren viser ...") +
#  ggsave("emne_density.pdf", width = 5.5, height = 5)
# OVERVEJ at tilføje streger for gennemsnit og sådan. 

# Eller jeg kan lave et samlet overblik: Hvor stor en del af annoncerne er kategoriseret på baggrund af 1 match?
emne_df_long %>%
  filter(total_count >0) %>% # kun kategoriserede annoncer
  # group_by(emne) %>%
  ggplot(aes(x = value)) +
  geom_histogram(binwidth = .5) +
  xlim(.1,20) + theme_minimal()

# NY: Sorterer alle ugyldige annonce-emne kombinationer fra
emne_df_long <- emne_df_long %>%
  filter(value > 0)

# Hvor 

# Undersøger hvor stor en andel af de kategoriserede annoncer, der er kategoriseret på baggrund af 1 match
glimpse(emne_df_long)
emne_df_long %>%
  filter(value >0) %>% # kun kategoriserede annoncer
  #group_by(emne) %>%# kun kategoriserede annoncer
  count(value) # 1 = 3.612 - hvordan fik jeg det tal??

# Ud af hvor mange kategoriserede annoncer?
glimpse(emne_df)
emne_df %>%
  filter(total_count>0) %>%
  nrow() # 8.531

# Laver en dikotom version, så det er docfreq i stedet for count
emne_01df <- select(emne_df, -total_count)
emne_01df[,2:length(emne_01df)][emne_01df[,2:length(emne_01df)] > 1] <- 1
emne_01df %>%
  summary() # Nu kun 0 og 1. Fint!

# Tilføjer kolonne "antal emner"
emne_01df <- emne_01df %>%
  mutate(antal_emner = rowSums(select(.,-"document")),
         ingen_emner = if_else(antal_emner < 1, 1, 0))

summary(emne_01df) # 40 pct. er uden emne. Ser fint ud. 

# Hvor mange annoncer har hvilket antal emner?
emne_01df %>%
  count(antal_emner) # 5659 (40 %) har intet emne. 38 % har 1 emne osv.

# Hvordan er fordelingen mellem emner?
# colSums(select(emne_01df,-"document")) - den viser bare docfreq, samme som "stats_emne" oven for. 

# Merger resultaterne til fb_ads
fb_ads <- fb_ads %>%
  left_join(emne_01df, by = c("ad_id" = "document"))

# FOrdelingen på tværs af partier?

emne_count <- fb_ads %>%
  group_by(PARTI) %>%
  summarise(n_ads = n(),
            klima_miljø = sum(.klima_miljo),
            børn = sum(.born),
            eu = sum(.eu),
            sundhed = sum(.sundhed),
            indvandrere = sum(.indvandrere),
            økonomi = sum(.okonomi),
            pension = sum(.pension),
            ældre = sum(.aeldre),
            ikke_kategoriseret = sum(ingen_emner),
            kategoriseret = n_ads-ikke_kategoriseret
  ) %>%
  arrange(desc(n_ads))
# tilføj evt. colSums til df

# Laver det til langt format, så jeg kan lave plots
emne_count_long <- emne_count %>%
  select(-n_ads, -kategoriseret, -ikke_kategoriseret) %>%
  pivot_longer(-PARTI, names_to = "emne", values_to = "antal") %>%
  mutate(emne = as.factor(emne))
# super.

# Hvilke emner var mest populære (count) [kan fx sammenlignes med dotplot for s]
emne_count_long %>%
  group_by(emne) %>%
  summarise(sum = sum(antal)) %>%
  arrange(desc(sum))

#### OVERORDNET deskriptivt ----------------------------------------------------
# Overordnet: Hvor mange af annoncerne promoverede forskellige emner?
fb_ads %>%
  count(ingen_emner) # 8531 ud af 14.190 annoncer er kategoriseret.
fb_ads %>%
  count(antal_emner)
fb_ads %>%
  filter(antal_emner > 1) %>%
  nrow() # 2.953 annoncer handler om mere end et emne. 


# Og samme tabel i procent (andel af alle partiets annoncer)

emne_prop<- fb_ads %>%
  group_by(PARTI) %>%
  summarise(n_ads = n(),
            klima_miljø = sum(.klima_miljo)/n_ads,
            børn = sum(.born)/n_ads,
            eu = sum(.eu)/n_ads,
            sundhed = sum(.sundhed)/n_ads,
            indvandrere = sum(.indvandrere)/n_ads,
            økonomi = sum(.okonomi)/n_ads,
            pension = sum(.pension)/n_ads,
            ældre = sum(.aeldre)/n_ads,
            kategoriseret = 1-mean(ingen_emner),
            ikke_kategoriseret = mean(ingen_emner)
  ) %>%
  arrange(desc(n_ads))
# tilføj evt. colSums til df

# Laver det til langt format, så jeg kan lave plots
emne_prop_long <- emne_prop %>%
  select(-n_ads, -kategoriseret, -ikke_kategoriseret) %>%
  pivot_longer(-PARTI, names_to = "emne", values_to = "andel") %>%
  mutate(emne = as.factor(emne))
# super.

# partiernes hv-placering 2019 ifølge Johan inspireret af
# https://www.altinget.dk/christiansborg/artikel/nyt-politisk-kompas-saadan-placerer-partierne-sig 
hv <- c("OE", "N", "F", "A", "AA", "B", "K", "E", "O", "V", "C", "I", "D", "P")

# Heatmap med andel annoncer
ggplot(emne_prop_long, aes(x = reorder(emne,-andel), y = fct_relevel(PARTI, hv), fill= andel)) + 
  geom_tile() +
  #scale_fill_distiller(palette = "RdPu") +
  scale_fill_gradient(low = "white", high = "steelblue") +
  theme_minimal() +
  labs(x = "Emne",
       y = "Partibogstav",
       fill = "Andel",
       caption = "Figuren viser andelen af partiernes annoncer, som promoverer hvilke emner") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
        #legend.position = 
        #legend.direction = "horizontal")
# ggsave("emne_heatmap.png", width = 7, height = 5)

### VÆGTET EFTER ANNONCEPRIS #########################################
  # Her ser jeg på, hvor mange annoncekroner, de enkelte partier har brugt på at
  # promovere politiske annoncer ud af deres samlede annoncebudget.

# Og hvis jeg ser på ad spend i stedet for number of ads?
emne_spend <- fb_ads %>%
  group_by(PARTI) %>%
  summarise(total_spend = sum(spend_mid),
            klima_miljø = sum(.klima_miljo*spend_mid),
            børn = sum(.born*spend_mid),
            eu = sum(.eu*spend_mid),
            sundhed = sum(.sundhed*spend_mid),
            indvandrere = sum(.indvandrere*spend_mid),
            økonomi = sum(.okonomi*spend_mid),
            pension = sum(.pension*spend_mid),
            ældre = sum(.aeldre*spend_mid),
            ikke_kategoriseret = sum(ingen_emner*spend_mid),
            kategoriseret = total_spend-ikke_kategoriseret
  ) %>%
  arrange(desc(total_spend))

emne_spend %>%
  arrange(desc(børn))
# Kolonnerne kategoriseret/ ikke kategoriseret skal summe op til total_spend. Tjekker det.
emne_spend %>%
  mutate(total_spend2 = ikke_kategoriseret+kategoriseret) %>%
  select(starts_with("total")) # Det stemmer. 
# Vær opmærksom på, at forbrug på de enkelte emner ikke summer op til total_spend, fordi 1 annonce
# med flere emner tæller flere gange. 

# Laver det til langt format, så jeg kan lave plots
emne_spend_long <- emne_spend %>%
  select(-total_spend, -kategoriseret, -ikke_kategoriseret) %>%
  pivot_longer(-PARTI, names_to = "emne", values_to = "spend") %>%
  mutate(emne = as.factor(emne))

# Deskriptivt: Hvor mange penge har partierne brugt på at promovere de forskellige emner:
emne_spend_dot<- emne_spend_long %>%
  group_by(emne) %>%
  summarise(spend = sum(spend)) %>%
  arrange(desc(spend))

# Vis i et dotplot
ggplot(data = emne_spend_dot, aes(x = reorder(emne, spend), y = spend/1000000)) + #ymin = lower, ymax = upper, colour = sex)) +
  geom_point(size = 2) +#position = position_dodge(width = 0.2)) +
  ylim(0, 3.5) +
  # geom_errorbar(position = position_dodge(width = 0.2), width = 0.1) +
  coord_flip() +
  # scale_colour_manual(values = c("blue", "red")) +
  theme_sjplot() +
  labs(x = NULL, 
       y = "Annoncekroner (mio.)") + 
      # caption = "Beregningen er baseret på medianen af det interval for annoncens pris,\n som Facebook giver til rådighed") +
  theme(axis.text.x = element_text(angle = NULL, hjust = NULL))
ggsave("emne_dot.pdf", width = 6, height = 4)

### Og samme tabel i procent (andel af partiets samlede annonceforbrug) ###

emne_spend_prop<- fb_ads %>%
  group_by(PARTI) %>%
  summarise(total_spend = sum(spend_mid),
              klima_miljø = sum(.klima_miljo*spend_mid) / total_spend,
              børn = sum(.born*spend_mid)/ total_spend,
              eu = sum(.eu*spend_mid)/ total_spend,
              sundhed = sum(.sundhed*spend_mid)/ total_spend,
              indvandrere = sum(.indvandrere*spend_mid)/ total_spend,
              økonomi = sum(.okonomi*spend_mid)/ total_spend,
              pension = sum(.pension*spend_mid)/ total_spend,
              ældre = sum(.aeldre*spend_mid)/ total_spend,
              ikke_kategoriseret = sum(ingen_emner*spend_mid)/ total_spend,
              kategoriseret = 1-ikke_kategoriseret
  ) %>%
  arrange(desc(total_spend))
# tilføj evt. colSums til df
fb_ads %>%
  group_by(KØN) %>%
  distinct(STEMMESEDDELNAVN, .keep_all = TRUE ) %>%
  count(Højreorienteret)
# Laver det til langt format, så jeg kan lave plots
emne_spend_prop_long <- emne_spend_prop %>%
  select(-total_spend, -kategoriseret, -ikke_kategoriseret) %>%
  pivot_longer(-PARTI, names_to = "emne", values_to = "Andel") %>%
  mutate(emne = as.factor(emne))
# super.

# NU kan jeg lave et tilsvarende heatmap med annoncekroner i stedet for anatal ads. 
ggplot(emne_spend_prop_long, aes(x = reorder(emne,-Andel), y = fct_relevel(PARTI, hv), fill= Andel)) + 
  geom_tile() +
  #scale_fill_distiller(palette = "RdPu") +
  scale_fill_gradient(low = "white", high = "steelblue") +
  theme_minimal() +
  labs(x = "Emne",
       y = "Partibogstav",
       caption = "Figuren viser andelen af partiernes annoncekroner brugt på at promovere hvilket emne") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#legend.position = 
#legend.direction = "horizontal")
# ggsave("emne_heatmap_spend.png", width = 7, height = 5)

#########################
  
  
  
  group_by(PARTI) %>%
  summarise(total_spend = sum(spend_mid),
            klima_miljø = sum(.klima_miljo),
            børn = sum(.born),
            eu = sum(.eu),
            sundhed = sum(.sundhed),
            indvandrere = sum(.indvandrere),
            økonomi = sum(.okonomi),
            pension = sum(.pension),
            ældre = sum(.aeldre),
            ikke_kategoriseret = sum(ingen_emner),
            kategoriseret = n_ads-ikke_kategoriseret
  ) %>%
  arrange(desc(n_ads))

fb_ads %>%
  select(PARTI, starts_with(".")) %>%
  pivot_longer(-PARTI, names_to = "emne", values_to = "andel") %>%
  mutate(emne = as.factor(emne))
  
  


fb_ads %>%
  mutate(spend_klima = selvpersonaliseret*spend_mid,
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


arrange(desc(share_pers_spend))

# Visualisering af parti resultat
parti_resultat %>%
  ggplot(aes(x = reorder(parti_navn, spend), y = spend, fill = share_pers_spend)) + 
  geom_bar(position = "stack", stat = "identity") + coord_flip() + theme_minimal()

### UDTRÆK AF ANNONCER UDEN EMNE ###
# Laver et udtræk af 100 annoncer uden emne for at se, om der er nogle relevante annoncer, der gemmer sig
set.seed(12)
intet_emne100 <- fb_ads %>%
   filter(antal_emner == 0) %>%
  distinct(ad_creative_body, .keep_all = T) %>% # Medtager kun tekstmæssigt unikke annoncer
   select(ad_id, kandidat_d, ad_creative_body) %>%
  sample_n(100)

 # write_excel_csv(intet_emne100, "intetemne100.csv") # Eksporterer til csv
 # Update: De er hermed kodet. Og det ser fint ud. Skriv om det i teksten.
 
# Et andet slags udtræk. De annoncer, som ER kodet (true/ false positives?)
 emne_100 <- fb_ads %>%
   filter(antal_emner > 0) %>% # skal omhandle mindst 1 emne
   distinct(ad_creative_body, .keep_all = T) %>% # Medtager kun tekstmæssigt unikke annoncer
   select(ad_id, kandidat_d, ad_creative_body, starts_with(".")) %>%
   sample_n(100)
 
# write_excel_csv(emne_100, "emne100.csv") # gennemgå bare dem derhjemme. 
 

# 2 Statistiske tests -------------------------------------------------------

 # HV-placering
 fb_ads %>%
   distinct(parti_navn, Højreorienteret)

# Her ved de statistiske test opstår samme problem som tidligere.
# Jeg bør estimere det som en logistisk regression, men så kan jeg ikke få vægte på.
# Jeg laver det som udgangspunkt som lineær, men tester også glm uden vægte. 
glimpse(fb_ads)
 
# lidt guidance her
# https://stats.idre.ucla.edu/r/dae/multinomial-logistic-regression/

# mlogit eksempel her
# https://cran.r-project.org/web/packages/mlogit/vignettes/e1mlogit.html
# er vist i en anden version her https://mran.microsoft.com/snapshot/2016-10-12/web/packages/mlogit/vignettes/Exercises.pdf 

# lidt diskussion om mlogit her
# https://stackoverflow.com/questions/43623076/multinomial-logit-in-r-mlogit-versus-nnet/43697814 

# Jeg kunne måske kode den gensidigt udelukkende: kategori x, kategoi y eller "flere kategorier" eller "ingen kategorier". hvis det giver mening, 

### RÅD TIL FREMGANGSMÅDE: Vent lidt med multinomial og start blot med at modellere 1 af de dikotome emner. Bare for at se fremgangsmåden. 


## KLIMA_MILJØ -----------------------------------------------------------
### Lineære modeller =====================================================

# Ændrer "Højreorienteret" til faktor variabel (fra nuværende numeric)
# fb_ads$Højreorienteret <- as.factor(fb_ads$Højreorienteret) [UDELADT]

# 0 TEST; Estimerer alle dummy DV's i samme model
# inspireret af multivariate multiple regression her https://stats.idre.ucla.edu/r/whatstat/what-statistical-analysis-should-i-usestatistical-analyses-using-r/#mmreg 

mTEST.emne <- lm(cbind(.klima_miljo, .okonomi, .born, .sundhed, .eu, .flygninge_indvandrere, .pension, .aeldre, .integration) ~ Højreorienteret, data = fb_ads)
#m1.emne <- lm(cbind(tidyselect::vars_select(starts_with("."))) ~ Højreorienteret, data = fb_ads) # virker vist
#m1.emne <- lm(cbind(emner) ~ Højreorienteret, data = fb_ads) # virker vist også
summary(mTEST.emne)
# virker selvfølgelig ikke med texreg.
# MEN: Man kan altså lave alle regressionerne simultant. 

m1.klima <- lm(.klima_miljo ~ Højreorienteret, data = fb_ads)

summary(m1.klima)
# 11,1 % af de højreorienterede partiers annoncer er klima-miljø annoncer.
# 31,4 % af de venstreorienterede partiers annoncer er klima-miljø annoncer.
# SIGNIFIKANT forskel.

fb_ads %>%
  cbind(select(.,starts_with("."))) %>%
          glimpse() # endte med ikke at bruge. skrev dem ud i cbind.
# 2 lineær med vægt
m2.klima <- lm(.klima_miljo ~ Højreorienteret, data = fb_ads,
                     weights = spend_mid)
summary(m2.klima)
# 9,8 % af de højreorienterede partiers annoncekroner er gået til klima-miljø-annocer.
# 39,6 % af de venstreorienterede partiers annoncekroner er gået til klima-miljø annoncer.
# Koefficient (forskel): 29,9 pp.
# SIGNIFIKANT forskel.

# ... nu til klyngerobuste standardfejl

# 3 lineær med vægt og klyngerobuste standardfejl
m3.klima <- lm_robust(.klima_miljo ~ Højreorienteret, data = fb_ads,
                      weights = spend_mid, cluster = PARTI, se_type = "stata")
summary(m3.klima)
# Estimaterne er de samme som model 2. Kun standardfejlene er vokset. 
# Men det er STADIG signifikant. 

# 4 lineær med få kontrolvariable
m4.klima <- lm_robust(.klima_miljo ~ Højreorienteret + KØN + ALDER, data = fb_ads,
                      weights = spend_mid, cluster = PARTI, se_type = "stata")
summary(m4.klima) # OBS! Der er lidt bedre modelfit med ordinary polynomial term + I(ALDER^2) - overvej at tilføje.
                 # (i mangel på bedre nu når poly() ikke virker?)
#  Koefficient: 29,4 pp.
# Fortsat SIGNIFIKANT forskel
# Kvinder mere klima end mænd (signifikant?)
# Ikke nogen lineær effekt af alder.

## Ekstra: Tjekker den bivariate sammenhæng mellem ALDER og Pr(klima=1)
ggplot(fb_ads)+
  geom_smooth(aes(x= ALDER, y= .klima_miljo)) + theme_minimal() 

fb_ads %>%
  ggplot(aes(x = ALDER, y = .klima_miljo)) +
  geom_smooth(col = "blue", se = F) +
  geom_smooth(col = "red", method = "loess", se = F) +
  ggtitle("Sammenhæng mellem alder og andel klimaannoncer") +
  labs(x = "Alder",
       y = "Andel klimaannoncer",
       caption = "Note: ") +
  theme_sjplot()

# Resultat: Sammenhængen mellem alder og klima-miljø er alt andet end lineær. 
# Jeg ville gerne transformere den, men poly() siger, at der er missing values. Fortsætter uden.

# 5 Lineær med alle kontrolvariable
m5.klima <- lm_robust(.klima_miljo ~ Højreorienteret + KØN + ALDER + EP + instagram, data = fb_ads,
                     weights = spend_mid, cluster = PARTI, se_type = "stata")
summary(m5.klima)
# Koefficient: 28,9 %
# Fortsat SIGNIFIKANT forskel (men KÆMPE konfidensinterval på forskellen: højreorienterede promoverer 18 til 40 pp mere klima).
# instgram og EP har ikke noget at byde på.

# TIL LATEX
texreg(list(m1.klima, m2.klima, m3.klima, m4.klima, m5.klima), 
       dcolumn = TRUE, booktabs = TRUE, use.packages = FALSE, label = "tab:klima_hv", caption = "Klima- og miljø", float.pos = "hb!",
       include.rmse = F, include.ci = F, include.rsquared = F, custom.note = "Standardfejl i parantes", stars = 0.05,
       include.aic = F, include.bic = F, include.loglik = F, include.dev = F,
       custom.gof.rows = list("Model" = c("lm a", "lm b", "lm c", "lm d", "lm e")))
# model.names = c("Uvægtet", "Vægtet", "Vægtet ", "få kontrolvariable", "alle kontrolvariable"))) # custom række virker stadig ikke. 

### TEST: Jeg laver lige en logistisk version DOG uden vægte. Til sammenligning. ### 

# Brug evt. relevel(PARTI, ref = "v"), hvis der skal ændres referencekategori
# højreorienteret er vel den egentlige test af hypotesen.

# ... Her kunne jeg lave regressionerne for de enkelte partier - men det vil jeg ikke. Den statistiske test skal være TESTEN på hypotesen.
# Ellers så opgiver jeg bare deskriptivt, hvor mange annoncer de forskellige partier har haft. Nu videre til logistisk. 
#   Update på logistisk: Viser overordnet samme resultat: Signifikant forskel - også med robuste standardfejl. 

#### Visualisering ------------------------------------------
set_theme(theme_sjplot(base_size = 10))
# Visualisering

# PLot marginale effekter fra regressionsmodellen
# "pred" er når øvrige variable er på 0 (som i normalt regtabel output) "eff" er hvor de er holdt på deres gennemsnit.
plot_model(m5.emne.alderx2, type = "eff", terms = c("ALDER [19:74]", "Højreorienteret"),
           axis.title = c("Alder", "Forudsagt andel klima- og miljøannoncer"), title = "Promovering af klima- og miljøannoncer") +
  theme(legend.position = "none") 
           # 19-74 er det spænd, der er observationer inden for. 
           # OBS! Jeg har ladet alder være en ordinært kvadreret led her.
# ggsave("klima_alderx2.pdf", width = 5, height = 4)

# 2: Bare højreorientering (det min hypotese går på)
# med plot_model https://strengejacke.github.io/sjPlot/articles/plot_model_estimates.html 
plot_model(m5.emne, type = "est", sort.est = T, show.values = T, value.offset = .3,
           axis.title = c("Estimater", ""), title = "Promovering af klima- og miljøannoncer",
           axis.labels = c("Højreorienteret", "Mand", "Instagram", "Alder", "EP"))#+
  # labs(x = "Alder", y = "Pr(klima- og miljø = 1)", caption = "Note: Konfidensintervallet markeret omkring de forudsagte værdier") +
  #ggtitle("Sandsynligheden for at promovere annoncer om klima- og miljø",
         # subtitle = "For politikere på venstrefløjen (rød) og højrefløjen (blå) efter alder") +
 # theme(legend.position = "none") 
# ggsave("klima_hv_plot.pdf", width = 5, height = 4)

fb_ads %>%
  count(ALDER) %>%
  tail()
# plot_model(ki_ltflygt_pooled, type = "eff", terms = c("Arbejdsløshedspct", "Flygt_pr_indb"), mdrt.values = "meansd",
#           title = "B: Forudsagte værdier (kun for pooled OLS)",
#           axis.title = c("Arbejdsløshedsprocent i kommunen", "Social tillid lokalt"))
# Af typen "eff" for at holde de kategoriske variable i modellen på deres andele i stedet for på deres referencekategori. 


### Logistiske modeller =========================================================

# Logistiske modeller med robuste standardfejl, men UDEN vægte, fordi man ikke kan med glm binomial. 
# Bruger glm.cluster fra miceadds pakken. 

###  1 logistisk uden noget som helst ###

glm1.klima <- glm(.klima_miljo ~ Højreorienteret, family = binomial(link = "logit"), data = fb_ads)

# laver krydstabulering for at se, hvad der foregår
CrossTable(fb_ads$.klima_miljo,fb_ads$Højreorienteret,
           prop.c = F, prop.t = F, prop.chisq=F, prop.r = F)
# Her kan jeg selv udregne odds for, at en højreorienteret annonce er en klima-annonce:
  # Pr(1) / Pr(0)
(916/8660)/(7744/8660) # odds for h = 0.1182851
# og en venstreorienteret:
(1734/5530)/(3796/5530) # odds for v = 0.4567966

# ratio of the odds for, at venstreorienterede promoverer klima ift. en højreorienteret:
(1734/3796)/(916/7744) # odds ratio er 3.861827
# eller odds for at højreorienterede promoverer mere:
(916*3796)/(1734*7744) # 0.2589448
# odds for, at venstreorienterede promoverer klimaannoncer er altså 383 % højere end odds for, at højreorienterede gør det.

# ... og nu kan vi se det i glm outputtet:
summary(glm1.emne)
# konstanten er log odds for venstreorienterede.
log(0.46)
# Koefficienten for h er the log of odds ratio mellem venstreorienterede og højreorienterede
log(0.2589448) # -1.35114
log(3.861827) # samme resultat. Perfekt. 
# exponentials er det modsatte:
exp(1.35114)
# så koefficienten er altså log af odds ratioen for, at en højreorienteret promoverer en klimaannonce sammenlignet med en venstreorienteret.

# log af odds for ratioen mellem sandsynligheden for at højerientede promoverer en klimaannonce sammenlignet med en venstreorienteret er (+/-) 1.35114. 
# log af odds for at vvenstreorienterede annoncer promoverer en klimaannonce er -0.7765288 (konstanten).

# SIGNIFIKANT forskel.


# 2 logistisk med klyngerobuste standardfejl
glm2.klima <- glm.cluster(.klima_miljo ~ Højreorienteret, family = binomial(link = "logit"), data = fb_ads,
                          cluster = fb_ads$PARTI)
summary(glm2.klima)
# Estimaterne er de samme som model 1. Kun standardfejlene er vokset. 
# stadig signifikant forskel.
extract(glm2.klima) # texreg virker IKKE til dette formål. Fortsætter uden. 


# 3 logistisk med klyngerobuste standardfejl og flere kontrolvariable
glm3.klima <- glm.cluster(.klima_miljo ~ Højreorienteret + KØN + ALDER, family = binomial(link = "logit"), data = fb_ads,
                          cluster = fb_ads$PARTI)
glm3.klima
# koefficinet -1.20615, stadig signifikant.

# 4 logistisk med klyngerobuste standardfejl og alle kontrolvariable
glm4.klima <- glm.cluster(.klima_miljo ~ Højreorienteret + KØN + ALDER + EP + instagram, family = binomial(link = "logit"), data = fb_ads,
                          cluster = fb_ads$PARTI)
glm4.klima
summary(glm4.klima)
#  Koefficient: -1.18854640, stadig signifikant. 


# TIL LATEX
# ... ikke muligt med glm.cluster. 



# NYT Emnedata lang form - samlet test ----------------------------------------

glimpse(fb_ads)

# Og hvilke partier er kodet som hhv. højre- og venstreorienteret?
fb_ads %>%
  distinct(parti_navn, Højreorienteret) # Fint. 

## 1. Udvælger emnedata og vender det til langt format
# Laver emnedata til langt format. Antallet af rækker stiger med faktor 8. 
  # Det betyder, at hver annonce har 8 rækker - 1 for hvert muligt emne.
lang_emne <- fb_ads %>%
  select(ad_id, page_name, STEMMESEDDELNAVN, KØN, ALDER, PARTI, parti_navn, kandidat_d, FT, EP, starts_with("spend"), 
         starts_with("impressions"), instagram, facebook, Højreorienteret, starts_with("."), antal_emner, ingen_emner) %>%
  pivot_longer(cols = starts_with("."), names_to = "emne", values_to = "emne_til_stede") %>%
  mutate(emne = as.factor(emne)) %>%
  filter(emne_til_stede == 1)  %>% # Resucerer antallet af rækker, så kun annonce-emne kombinationer, der eksisterer, medtages.
  select(-emne_til_stede) # alle har nu værdien 1 og den er overflødig

dim(lang_emne) # Antal rækker er nu 12.984 (gyldige annonce-emne kombinationer) ud af 113.520 mulige.
head(lang_emne)

## 2. Laver en ny variabel som viser, om partiet har ejerskab over det pågældende emne
højre_emner <- c(".indvandrere", ".okonomi", ".eu")
venstre_emner <- c(".klima_miljo", ".born", ".sundhed", ".pension", ".aeldre" )

lang_emne <- lang_emne %>%
  mutate(emne_ejerskab = case_when(Højreorienteret == 1 & emne %in% højre_emner ~ 1,
                                   Højreorienteret == 1 & emne %in% venstre_emner ~ 0,
                                   Højreorienteret == 0 & emne %in% højre_emner ~ 0,
                                   Højreorienteret == 0 & emne %in% venstre_emner ~ 1,
                                   TRUE ~ NA_real_),
         Venstreorienteret = if_else(Højreorienteret == 1, 0, 1)) # Identisk af højreorienteret, blot vendt om.
glimpse(lang_emne)
# Tilføjer emnerejerskab ift. emne. 
lang_emne <- lang_emne %>%
  mutate(Højre_emne = case_when(emne %in% højre_emner ~ 1,
                                emne %in% venstre_emner ~ 0,
                                TRUE ~ NA_real_),
         Venstre_emne = if_else(Højre_emne == 1, 0, 1)) # Dette er præcis den samme variabel som højre_emne.
                                                        # Den er blot vendt om for at lette fortolkningen.

dim(lang_emne_test) # 113.520 og 26
lang_emne_test %>%
  count(emne_ejerskab, emne_til_stede)


# Hvor mange annoncer er blevet kategoriseret?
fb_ads %>%
  filter(antal_emner>0) %>%
  nrow()

# Hvor mange annoncer handler om mere end 1 emne?
fb_ads %>%
  filter(antal_emner>1) %>%
  nrow()

# Så hvor mange unikke annoce-emne kombinationer er der?
fb_ads%>%
  select(antal_emner) %>%
  sum() # 12.984. 

lang_emne %>%
  count(emne_ejerskab)

glimpse(lang_emne)
# 5.217 annoncer om emner, som afsender ikke har ejerskab over, er blevet promoveret.
# 7.767 annoncer om emner, som afsender har ejerskab over, er blevet promoveret. 
# Er forskellen statistisk signifikant? Også når vi vægter for pris og tilføjer kontrolvariable?

# Og hvilken fløj promoverer mest de emner, de har ejerskab over?
lang_emne %>%
  count(Højreorienteret, emne_ejerskab)
# Det ser meget lige ud. Men begge fløje har deres favorit-emner - ser det ud til.

### 2. Blandet deksriptivt og visuelt. (wait for it) ------------------------------------------

# Lige en hurtig tanke: I stedet for at tælle den samme annonce som 7*150 kroner, hvis den handler om 7 emner,
# burde jeg så dele den op, så hvert emne er 150/7? Ja måske. Hvordan ville jeg gøre det?
# Simpelt. Sådan:
glimpse(lang_emne)
lang_emne <- lang_emne %>%
  mutate(emne_prioritet = spend_mid/antal_emner)

summary(lang_emne$emne_prioritet)

## Ekstra: Tjekker den bivariate sammenhæng mellem ALDER og Pr(emne_ejerskab = 1)
# Tjekker lige aldersfordelingen
fb_ads %>%
  distinct(STEMMESEDDELNAVN, .keep_all = T) %>%
  count(ALDER)

fb_ads %>%
  distinct(STEMMESEDDELNAVN, .keep_all = T) %>%
  # count(ALDER) %>%
  ggplot(aes(x = ALDER, fill = "Højreorienteret")) +
  geom_histogram(binwidth = 1) +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  theme_sjplot() +
  labs(fill="")
  
fb_ads %>%
  count(Højreorienteret)

ggplot(lang_emne)+
  geom_smooth(aes(x= ALDER, y= emne_ejerskab)) + theme_minimal() 

lang_emne %>%
  ggplot(aes(x = ALDER, y = emne_ejerskab)) +
  geom_smooth(col = "blue", se = F) +
  geom_count() + 
  ggtitle("Sammenhæng mellem alder og promovering af emner partiet har ejerskab over") +
  labs(x = "Alder",
       y = "Andel annoncer om emner partiet har ejerskab over",
       caption = "Note: ") +
  theme_sjplot()

# Resultat: Det ser ikke ud til, at der er en store sammenhæng mellem alder og sandsynlighed for at promovere annoncer, man har ejerskab over.
# Ingen klar tendens. 


## 3. [SLET] Emneejerskab test højrefløjen -------------------------------------------
# Hvor meget har højrefløjen brugt på annoncer, de har ejerskab over?
lang_emne %>%
  filter(emne %in% højre_emner) %>%
  group_by(Højreorienteret) %>%
  summarise(sum(emne_prioritet))

# Hvor meget har venstrefløjen brugt på annoncer, de har ejerskab over?
lang_emne %>%
  filter(emne %in% venstre_emner) %>%
  group_by(Højreorienteret) %>%
  summarise(sum(emne_prioritet))

# NY LØSNING: Subset på højrefløjens emner og dernæst på venstrefløjens emner. 
glimpse(lang_emne)
lang_emne %>%
  filter(emne %in% højre_emner) %>%
  glimpse()
# 0 Simpel lineær regression
m0.højre <- lm_robust(Højre_emne ~ Højreorienteret, data = lang_emne)
summary(m0.højre)
summary(m0.højre.ni) # no intercept

# Tolknigen skal være således:
# 58,6 % af af annoncerne om eu/indvandrere/økonomi er promoveret af højreorienterede kandidater.
# 41,4 % af annoncerne om eu/indvandrere/økonomi er promoveret af venstreorienterede kandidater.
  # SUMMEN er her promovering af annoncer om eu/indvandrere/økonomi.

# Vender regressionen i håb om, at andelen af emnerne summerer til 100, så de kan sammenlignes på tværs af fløje.
mt.højre <- lm_robust(Højreorienteret ~ Højre_emne, data = lang_emne)
summary(mt.højre)
summary(mt.højre.ni)
# 60,2 % af annoncerne om eu/indvandring/økonomi er promoveret af højreorienterede kandidater.               #KOEF#
  # (39.8 % af annoncerne om eu/indvandring/økonomi er således promoveret af venstreorienterede kandidater).          #KOEF#
# 40,5 % af annoncerne om    venstrefløjens emner er promoveret af højreorienterede kandidater.              #KOEF#
  # (59.5 % af annoncerne om venstrefløjens emner er således promoveret af venstreorienterede kandidater.)            #KOEF#
# NYT NYT: Det jeg jo i virkeligheden skal sammenligne er, om de 60,2 og 40.5 % HVER ISÆR er forskellige fra 50 %


# Venstrefløjens emner
mt.venstre <- lm_robust(Venstreorienteret ~ Venstre_emne, data = lang_emne)
summary(mt.venstre)
summary(mt.venstre.ni)
# 59.5 % af annoncerne om venstrefløjens emner er promoveret af venstreorienterede kandidater.
# 39.8 % af annoncerne om højrefløjens emner er promoveret af venstreorienterede kandidater.

# Chi^2 test. Hvad siger den?
# finder antal annoncer om højrefløjens emner i hver kategori (højrefløj/ venstrefløj)

lang_emne %>%
  count(Højreorienteret, Højre_emne)

# Sætter det på matrix form
højre.matrix <- matrix(c(3812, 2696, 2512, 3955), ncol = 2)
colnames(højre.matrix) <- c("Højrefløj", "Venstrefløj")
rownames(højre.matrix) <- c("Højre emne", "Venstre emne")
prop.table(højre.matrix, 1) # Viser andel selvpersonaliserede annoncer inden for de to grupper (centralisering vs. decentralisering) [dvs. row = 100%]

prop.test(højre.matrix, correct = F, p = c(.5,.5)) # med correct = FALSE fjerner jeg Yates continuity correction.
# Med p =0.5 specificerer jeg nulhypotesen: At andelen af annoncer om højrefløjens emner promoveret af højrefløjen (60,2 %),
  # og andelen af annoncer om venstrefløjens emner promoveret af højreorienterede kandidater (40.5 %) ER FORSKELLIG fra 50 % !

# Løsningen må være subset efter emne?

glimpse(lang_emne)
# HØJRE-EMNER
lang_emne %>%
  #filter(emne %in% højre_emner) %>% # økonomi, eu og indvandring
  count(Højreorienteret, Højre_emne) # Hvor stor en andel af annoncer om disse emner, har højrefløjen promoveret (vs. venstrefløjen)

# VENSTRE-EMNER
lang_emne %>%
  filter(!emne %in% højre_emner) %>%
  group_by(Højreorienteret) %>%
  count(emne_ejerskab)

# Sætter det på matrix form
emner.m <- matrix(c(3812, 3955, 2512, 2696), ncol = 2)
colnames(emner.m) <- c("Ejerskab", "Ikke ejerskab")
rownames(emner.m) <- c("Højrefløj", "Venstrefløj")

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Lige nu er tolkningen desværre:
# 61.1 % af venstrefløjens annoncer har promoveret venstrefløjens emner
# 41.4 % af højrefløjens annoncer har promoveret venstrefløjens emner. 
# Forskellen er 19.7 pp.
  # = venstrefløjen promoverer signifikant flere annoncer om venstrefløjens emner sammenlignet med højrefløjen

# Og:
# 58.6 % af højrefløjens annoncer har promoveret højrefløjens emner.
# 38.9 % af venstrefløjens annoncer har promoveret højrefløjens emner.
# Forskellen er 19.7 pp.
# = Højrefløjen promoverer signifikant flere annoncer om højrefløjens emner sammenlignet med venstrefløjen. 
# Dette er sandt og alt andet er forkert. Se det markeret med gult i Johans udredning fra 7. klasse.


# Problem: Fløjenes andel af hhv. venstrefløjen og højrefløjens emner summerer ikke til 100 %.
# Problemet er, at testen viser andelen af FLØJENS annoncer om højrefløjens emner.
  # I stedet for målet: Andelen af annoncer om højrefløjens emner, som promoveres af højrefløen.
  # Og er den signifikant forskellig fra andelen af annoncer om højrefløjens emner, som promoveres af venstrefløjen? 
  # H0: ANdelen af annoncer om højrefløjens emner er ikke forskellig for højrefløjen og venstrefløjen (p = 0.5)
  # Ha: Andelen af annoncer om højrefløjens emner er signifikant forskellige for højre- og venstrefløjen. (P ≠ 0.5)

# LØSNING: Jeg tester jo ikke om andelen af annoncer om højrefløjens emner, som promoveres af højrefløjen (60.2 %)
# er forskellig fra andelen af annoncer om venstrefløjens emner, som promoveres af højrefløjen (40.5 %)
# DERIMOD tester jeg, om andelene hver især er signifikant forskellige fra H0 = 50 %.
  # H0: Andelen af annoncer om højrefløjens emner, som promoveres af af en fløj er ikke forskellige fra 50 %
  # med andre ord: Den ene fløj promoverer IKKE en større andel annoncer om de emner, den selv ejer.
  # Ha: Den ene fløj promoverer en større andel af annoncerne om de emner, den selv har ejerskab over.

# Nyt problem: Hvordan fanden behandler jeg det i en lineær regression?
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# 58,6 % af højrefløjens (kategoriserede) annoncer har promoveret emner, højrefløjen har ejerskab over.
# (100-58.6) 41,4 % af højrefløjens (kategoriserede) annoncer har således promoveret emner, venstrefløjen har ejerskab over.
# = Viser andelen af højrefløjens annoncer, der har promoveret emner, de selv har ejerskab over

# [slet] Det betyder at 100-58.6 = 41,4 % af venstrefløjens (kategoriserede) annoncer har promoveret emner, højrefløjen har ejerskab over.

# 100-61,1 = 38.9 % af venstrefløjens (kategoriserede) annoncer har promoveret emner, højrefløjen har ejerskab over.
# Og de resterende 61,1 % af venstrefløjens (kategoriserede) annoncer har promoveret emner, venstrefløjen har ejerskab over.
# = Viser andelen af venstrefløjens annoncer, der har promoveret emner, de selv har ejerskab over.

# FOrskel på 19 procentpoint.
# = Forskellen på de to viser, at Højreorienterede har promoveret signifikant flere annoncer om emner, de selv har ejerskab over

summary(m0.venstre)
summary(m0.venstre.ni)
# 61,1 % af venstrefløjens (kategoriserede) annoncer har promoveret emner, de selv har ejerskab over.
# 38.9 % af venstrefløjens (kategoriserede) annoncer har promoveret emner, højrefløjen har ejerskab over.

# Forslag? 
# Jeg skal bruge andelen af penge (ud af penge, højrefløjen har brugt i alt), som højrefløjen har brugt på at promovere højre-emner,
# og sammenligne om andelen af penge, som venstrefløjen har brugt på at promovere højre-emner. !! Ja mon ikke!?

##### HER ER DEN #####
# y = højrefløjsemne
# x = højrefløj
# fordi så har vi alle emner med (0/1 - og kan dermed se andelen af annoncer som promoverer et emne)
# og vi har både højre- og venstrefløjen med i modellen.
# vægt = emne_prioritet
##### ##### ##### ##### 

# y = er det et højre-emne eller ej?
# x = penge brugt på at promovere annoncen (emne_prioritet)

# Eller er det omvendt:
# y = penge brugt på at promovere annoncen (emne_prioritet)
# x = Er det et højre-emne eller ej?

# Subset: Højrefløjen == 1/0
# Testen skal laves først for højrefløjen: Hvor stor en andel af deres annoncekroner bruger de på, at promovere et emne, de har ejerskab over?
  # (i forhold til hvor stor en andel af deres annoncekroner, de bruger på at promovere et emne, de ikke har ejerskab over)
# Tilsvarende test for venstrefløjen. ==> Eller overvej, om jeg kan bruge vægte i stedet for subset?

# Så ville det være y = Er det et højre-emne eller ej,
                  # x = Højrefløjen eller ej
                  # vægtet efter annoncepris. Ville det være det samme? Ja det samme. 


# plot_model(m0.højre)

# 1 Lineær regression med vægte 
m1.højre <- lm_robust(Højre_emne ~ Højreorienteret, data = lang_emne, weights = emne_prioritet)
summary(m1.højre)

# 2 Lineær regression med vægte og klyngerobuste std. fejl.
m2.højre <- lm_robust(Højre_emne ~ Højreorienteret, data = lang_emne, weights = emne_prioritet,
                      cluster = ad_id, se_type = "stata")
summary(m2.højre)

# 3 med 1 kontrolvariabel
m3.højre <- lm_robust(Højre_emne ~ Højreorienteret + KØN, data = lang_emne, weights = emne_prioritet,
                     cluster = ad_id, se_type = "stata")
summary(m3.højre)

# 4 Lineær med 2 kontrolvariable
m4.højre <- lm_robust(Højre_emne ~ Højreorienteret + KØN + ALDER, data = lang_emne, weights = emne_prioritet,
                      cluster = ad_id, se_type = "stata")
summary(m4.højre)

# 5 Lineær med alle predictors
m5.højre <- lm_robust(Højre_emne ~ Højreorienteret + KØN + ALDER + EP + instagram, data = lang_emne, weights = emne_prioritet,
                      cluster = ad_id, se_type = "stata")
# Bemærk 6593 freedom degrees. Det er de 6594-1 ads, kandidaterne har kørt:
lang_emne %>%
  filter(kandidat_d == 1) %>%
  distinct(ad_id) %>%
  nrow()

summary(m5.højre)
m5.højre$N # antal observationer
m5.højre$N_clusters # antal klynger.

# KOMMENTAR TIL RESULTAT: M4 er nok det endelige resultat. M5 med variablene EP og instagram giver ikke meget mening, fordi de ikke kan påvirke x.
# Interessant at KØNS-variablen æder så stor en del af emneejerskabets koefficinet. 
# Valg af emne (ejerskab eller ej) ser altså ud til at have meget med køn at gøre. Ikke kun emneejerskab. 

# Plot koefficineterne
plot_model(m4.højre, type = "est", sort.est = T, show.values = T, value.offset = NULL, 
           axis.title = "Estimater", title = "Promovering af emner, højrefløjen har ejerskab over",
           axis.labels = c("Alder", "Højrefløj", "Mand")) + theme_sjplot(base_size = 10) +
  # labs(x = "Alder", y = "Pr(klima- og miljø = 1)", 
  labs (caption = "Note: Konfidensintervallet markeret omkring de forudsagte værdier")
  # subtitle = "For politikere på venstrefløjen (rød) og højrefløjen (blå) efter alder"
  # theme(legend.position = "none") 
ggsave("højre_emneejerskab.pdf", width = 4.5, height = 3.5)
  
# Kan jeg plotte det på anden vis?

# Tjekker lige først sammenhængen mellem alder og promoveringen af højrefløjs-emne
lang_emne %>%
  ggplot(aes(x = ALDER, y = Højre_emne)) +
  geom_smooth(col = "blue", se = F) +
  geom_smooth(col = "red", method = "loess", se = F) +
  geom_count() +
  labs(title = "Promovering af emner, højrefløjen har ejerskab over",
       subtitle = "På tværs af alder",
       x = "Alder",
       y = "Andel højre-emner",
       caption = "Note: ") +
  theme_sjplot()
# Resultat: Ingen lineær sammenhæng mellem de to variable.


# Koefficient: -0.101784 altså 10,2 pp. lavere. for højreoriente
# signifikant forskel.

# TIL LATEX
texreg(list(m1.emne, m2.emne, m3.emne, m4.emne, m5.emne), 
       dcolumn = TRUE, booktabs = TRUE, use.packages = FALSE, label = "tab:klima_hv", caption = "Klima- og miljø", float.pos = "hb!",
       include.rmse = F, include.ci = F, include.rsquared = F, custom.note = "Standardfejl i parantes", stars = 0.05,
       include.aic = F, include.bic = F, include.loglik = F, include.dev = F,
       custom.gof.rows = list("Model" = c("lm a", "lm b", "lm c", "lm d", "lm e")))

## 3. [SLET OGSÅ] Emneejerskab test ----------------------------------------

# 0 En simpel lineær regression (z-test of difference in proportions)
m0.venstre. <- lm_robust(Venstre_emne ~ Venstreorienteret, data = lang_emne)
summary(m0.venstre)
summary(m0.højre.ni) # Ja det er jo en identisk regression. No need. 

# 1 Lineær regression med vægte 
m1.højre <- lm_robust(Højre_emne ~ Højreorienteret, data = lang_emne, weights = emne_prioritet)
summary(m1.højre)

# 2 Lineær regression med vægte og klyngerobuste std. fejl.
m2.højre <- lm_robust(Højre_emne ~ Højreorienteret, data = lang_emne, weights = emne_prioritet,
                      cluster = ad_id, se_type = "stata")
summary(m2.højre)

# 3 med 1 kontrolvariabel
m3.højre <- lm_robust(Højre_emne ~ Højreorienteret + KØN, data = lang_emne, weights = emne_prioritet,
                      cluster = ad_id, se_type = "stata")
summary(m3.højre)

# 4 Lineær med 2 kontrolvariable
m4.højre <- lm_robust(Højre_emne ~ Højreorienteret + KØN + ALDER, data = lang_emne, weights = emne_prioritet,
                      cluster = ad_id, se_type = "stata")
summary(m4.højre)

# 5 Lineær med alle predictors
m5.højre <- lm_robust(Højre_emne ~ Højreorienteret + KØN + ALDER + EP + instagram, data = lang_emne, weights = emne_prioritet,
                      cluster = ad_id, se_type = "stata")
# Bemærk 6593 freedom degrees. Det er de 6594-1 ads, kandidaterne har kørt:
lang_emne %>%
  filter(kandidat_d == 1) %>%
  distinct(ad_id) %>%
  nrow()

summary(m5.højre)
m5.højre$N # antal observationer
m5.højre$N_clusters # antal klynger.

# 0 simpel z-test proportions
m0.emne <- lm_robust(emne_ejerskab ~ Højreorienteret, data = lang_emne)
summary(m0.emne)

# 1 z-test med vægte
m1.emne <- lm_robust(emne_ejerskab ~ Højreorienteret, data = lang_emne,
                     weights = spend_mid)
summary(m1.emne)

# 2 med klyngerobuste standardfejl på annonce-niveau
m2.emne <- lm_robust(emne_ejerskab ~ Højreorienteret, data = lang_emne,
                     weights = spend_mid, cluster = ad_id, se_type = "stata")
summary(m2.emne)

# 4 Lineær med få kontrolvariable
m4.emne <- lm_robust(emne_ejerskab ~ Højreorienteret + KØN + ALDER, data = lang_emne,
                     weights = spend_mid, cluster = ad_id, se_type = "stata")
summary(m4.emne)

# 5 Lineær med alle kontrolvariable
m5.emne <- lm_robust(emne_ejerskab ~ Højreorienteret + KØN + ALDER + EP + instagram, data = lang_emne,
                     weights = spend_mid, cluster = ad_id, se_type = "stata")
summary(m5.emne)

# Kommentar: Interessant at se, at højreorienteret er insignifikant uden kontrolvariable, men signifikant når de tilføjes!
# Hvad kan der tolkes ud af det? Se på gennemsnittene på de øvrige variable ligesom A&F. 

# Plot koefficineterne

plot_model(m5.emne, type = "est", sort.est = T, show.values = T, value.offset = NULL, 
           axis.title = "Estimater", title = "Promovering af emner partiet har ejerskab over",
           axis.labels = c("Højreorienteret", "Mand", "Instagram", "EP", "Alder")) + #+ theme_sjplot()
# labs(x = "Alder", y = "Pr(klima- og miljø = 1)", caption = "Note: Konfidensintervallet markeret omkring de forudsagte værdier") +
# subtitle = "For politikere på venstrefløjen (rød) og højrefløjen (blå) efter alder"
# theme(legend.position = "none") 
# ggsave("klima_hv_plot.pdf", width = 5, height = 4)


plot_model(m5.emne)

# Koefficient: -0.101784 altså 10,2 pp. lavere. for højreoriente
# signifikant forskel.

# TIL LATEX
texreg(list(m1.emne, m2.emne, m3.emne, m4.emne, m5.emne), 
       dcolumn = TRUE, booktabs = TRUE, use.packages = FALSE, label = "tab:klima_hv", caption = "Klima- og miljø", float.pos = "hb!",
       include.rmse = F, include.ci = F, include.rsquared = F, custom.note = "Standardfejl i parantes", stars = 0.05,
       include.aic = F, include.bic = F, include.loglik = F, include.dev = F,
       custom.gof.rows = list("Model" = c("lm a", "lm b", "lm c", "lm d", "lm e")))

# Efterfølgende ting, jeg har tjekket -----------------------------------

# 1) Tjekker alder som kontrolvariabel i emneejerskab.

# 1a) Tjekker sammenhæng mellem alder og sandsynlighed for at promovere et emne, man har ejerskab over
glimpse(lang_emne)
lang_emne %>%
  # filter(Højreorienteret == 1) %>%
  ggplot(aes(x = ALDER, y = emne_ejerskab)) +
  geom_smooth(method = "loess", col = "blue", se = F) +
  geom_smooth(method ="lm", col = "red", se = F) +
  geom_smooth(col = "green", se = F) +
  geom_count() +
  ggtitle("Alder og emneejerskab") +
  labs(x = "Alder",
       y = "Andel ejerskab",
       caption = "Note: ") +
  theme_sjplot()

  # 1b) Gennemsnitsalder for kandidater på højre- og venstrefløjen
# Gennemsnitsalderen er højere på højrefløjen. 
fb_ads %>%
  distinct(STEMMESEDDELNAVN, .keep_all = T) %>%
  select(Højreorienteret, ALDER) %>%
  split(.$Højreorienteret) %>% map(summary)

# 2) Gennemsnitsalderen blandt kandidater, der promoverer forskellige emner
fb_ads %>%
  filter(.eu== 1) %>% # medtager kun annoncer, som har promoveret dette emne
  distinct(STEMMESEDDELNAVN, .keep_all = T) %>% # Hver kandidat tæller kun 1 gang
  select(ALDER) %>%
  summary()
  
 

# 3. EMNE EJERSKAB TEST ENDELIG ============================================

# Laver højreorienteret om til faktorvariabel 
lang_emne <- lang_emne %>%
  mutate(Højreorienteret = set_labels(Højreorienteret, labels = c(`Venstrefløj` = 0, `Højrefløj` = 1)),
         Højreorienteret = as_factor(Højreorienteret1))

get_labels(lang_emne$Højreorienteret1) # Det ser ud til at virke,

lang_emne$Højreorienteret <- as.numeric(lang_emne$Højreorienteret)
# -1 chi^2 test
# finder antal annoncer om højrefløjens emner i hver kategori (højrefløj/ venstrefløj)
lang_emne %>%
  count(Højreorienteret, Højre_emne)
# table(lang_emne$Højreorienteret, lang_emne$Højre_emne)

# Sætter det på matrix form
højre.matrix <- matrix(c(3812, 2512, 2696, 3955), ncol = 2)
rownames(højre.matrix) <- c("Højrefløj", "Venstrefløj")
colnames(højre.matrix) <- c("Højre emne", "Venstre emne")
prop.table(højre.matrix, 1) # Viser andel selvpersonaliserede annoncer inden for de to grupper (centralisering vs. decentralisering) [dvs. row = 100%]

prop.test(højre.matrix, correct = F, alternative = "two.sided") # med correct = FALSE fjerner jeg Yates continuity correction.
# Med alternative = "two.sided" specificerer jeg nulhypotesen: 
  # H0: Andelen af højreføjens annoncer om eu, økonomi og indvandring er IKKE forskellig fra andelen af venstrefløjens annoncer om samme emner. 
  # Ha: Andelen af højreføjens annoncer om eu, økonomi og indvandring ER forskellig fra end andelen af venstrefløjens annoncer om samme emner. 

# 0 Chi^2 testen foretaget gennem lineær regression: 
m0.højre <- lm_robust(Højre_emne ~ Højreorienteret, data = lang_emne)
summary(m0.højre)
summary(m0.højre.ni) # no intercept
# Samme resultat som chi^2. Koefficienten viser forskellen mellem andelen af højrefløjens annoncer om emner, de har ejerskab over, 
  # sammenlignet med andelen af venstrefløjens annoncer om emner, de IKKE har ejerskab over. 
# Fint. Udbygger testen med vægte, klyngerobuste standardfejl og kontrolvariable på individniveau. 

# 1 Lineær regression med vægte 
m1.højre <- lm_robust(Højre_emne ~ Højreorienteret, data = lang_emne, weights = emne_prioritet)
summary(m1.højre)

# 2 Lineær regression med vægte og klyngerobuste std. fejl.
m2.højre <- lm_robust(Højre_emne ~ Højreorienteret, data = lang_emne, weights = emne_prioritet,
                      cluster = ad_id, se_type = "stata")
summary(m2.højre)

# 3 med 1 kontrolvariabel
m3.højre <- lm_robust(Højre_emne ~ Højreorienteret + KØN, data = lang_emne, weights = emne_prioritet,
                      cluster = ad_id, se_type = "stata")
summary(m3.højre)

# 4 Lineær med 2 kontrolvariable
m4.højre <- lm_robust(Højre_emne ~ Højreorienteret + KØN + ALDER, data = lang_emne, weights = emne_prioritet,
                      cluster = ad_id, se_type = "stata")
summary(m4.højre)

# 5 Lineær med alle predictors
m5.højre <- lm_robust(Højre_emne ~ Højreorienteret + KØN + ALDER + EP + instagram, data = lang_emne, weights = emne_prioritet,
                      cluster = ad_id, se_type = "stata")
# Bemærk 6593 freedom degrees. Det er de 6594-1 ads, kandidaterne har kørt:
lang_emne %>%
  filter(kandidat_d == 1) %>%
  distinct(ad_id) %>%
  nrow()

summary(m5.højre)
m5.højre$N # antal observationer
m5.højre$N_clusters # antal klynger.

# Plotter koefficienterne
plot_model(m4.højre, type = "est", sort.est = T, show.values = T, value.offset = NULL, 
           axis.title = "Estimater", title = "Promovering af emner, partiet har ejerskab over",
           axis.labels = c("Alder", "Højrefløj", "Mand")) + theme_sjplot(base_size = 11) +
  labs (caption = "Note: 95 % konfidensinterval markeret omkring koefficienterne")
  # labs(x = "Alder", y = "Pr(klima- og miljø = 1)", caption = "Note: Konfidensintervallet markeret omkring de forudsagte værdier") +
  # subtitle = "For politikere på venstrefløjen (rød) og højrefløjen (blå) efter alder"
  # theme(legend.position = "none") 
ggsave("hojre_emne.pdf", width = 5, height = 4)
  
# Plotter forudsagte værdier (marginale effekter)
  # med type = "eff" holder jeg kategoriske predictors (køn) på gennemsnit frem for referencekategori (kvinde)
plot_model(m4.højre, type = "eff", terms = "Højreorienteret",
           title = "Marginale effekter af emneejerskab", axis.title = c("", "Promovering af emner, højrefløjen har ejerskab over"),
           grid.breaks =.05, ci.lvl = .95) +
  # grid.breaks og show.values virker ikke. 
  theme_sjplot(base_size = 11) + coord_flip() +
  labs (caption = "Note: 95 % konfidensinterval markeret omkring de forudsagte værdier")
ggsave("marginal_emne.pdf", width = 5, height = 2)


# TIL LATEX
texreg(list(m2.højre, m3.højre, m4.højre), 
       dcolumn = F, booktabs = TRUE, use.packages = FALSE, label = "tab:ejerskab_hvfløj", caption = "Promovering af højrefløjens emner", float.pos = "hb!",
       include.rmse = F, include.ci = F, include.rsquared = F, custom.note = "Standardfejl i parantes", stars = 0.05,
       include.aic = F, include.bic = F, include.loglik = F, include.dev = F, include.adjrs = F)
       # custom.gof.rows = list("Model" = c("lm a", "lm b", "lm c")))
