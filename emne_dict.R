# 1 Indlæser pakker og data ####################

library(pacman)
p_load(tidyverse, readtext, quanteda, xtable, tictoc, estimatr, texreg, miceadds, sjPlot, sjmisc, effects)
# måske lubridate, topicmodels
options(scipen = 1) # skru ned for brugen af scientific notation
theme_set(theme_sjplot()) # et lidt flottere standard tema


fb_ads <- read_rds("FB API data/data/fb_ads.rds") # se rettelser sidst i DEC_ALT script. 

# Rettelse: Annoncer, som ikke har optrådt på fb eller instagram er kodet til NA. Laver det om til 0.
fb_ads%>%
  count(instagram)

fb_ads$facebook <- fb_ads$facebook %>%
  replace_na(0) 
fb_ads$instagram <- fb_ads$instagram %>%
  replace_na(0) 
glimpse(fb_ads)

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

# Laver alle ord om til lower case
tokens_body <- tokens_tolower(tokens_body) # 9.2 MB
ndoc(tokens_body) # 14.190

kwic(tokens_body, phrase("*gas*")) # For at se privatliv-dictionary in-context!

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
  .klima_miljo = c("klima*", "grøn*", "omstilling", "miljø*", "parisaftale*", "drikkevand*", "*forurening*", 
                  "greta", "thunberg*", "bæredyg*", "co2*", "klode*", "*udledning*", "biodiversitet*", "global* opvarmning*",
                  # nye
                  "*affald*", "atomkraft*", "benzin*", "diesel*", "drihus*", "eliktiricet*", "*energi*", "forbrænding*", "*fredning*",
                  "genbrug*", "giftgrund*", "grundvand*", "*kraftværk*", "kystsikr*", "losseplads*", "nordsø*", "olie*", "pesticid*", 
                  "sprøjtemid*", "vindmølle*", "økologi*", "økosystem*", "fossil*", "*brænds*"
  ),
  # Ser rigtig fin ud. "miljø kan godt fange fx "miljø-og fødevareminister", men generelt rammer den godt.
  # Natur er fjernet, da den fanger andre ting en lige præcis klima og miljø. Fx urørt skov, naturparker og natufredningsforeningen. Ikke helt det,
    # Det kommer på bekostning af enkelte annoncer, fx forslag om forbud mod bidræberting i O. Men sådan er det!
    # ja natur giver også "naturligvis" og "naturister". 
  # Klimahasttags: "#stemgrønnest", "#klimavalg", "#grøntvalg" er fjernet, da de er partispecifikke og kan give bias. Jeg fanger dem på mere "neutrale" stikord.
  # "gift" kunne godt være med, men det giver også bryllupper. "ret til at gifte sig og få børn"... # GODT EKS #
  .flygninge_indvandrere = c("indvandr*", "udlænding*", "flygtning*", "asyl*", "immigra*", "migra*", "migrer*", "kvoteflygtning*", 
                            "udrejsecent*", "sjælsmark*", "lindholm", "nærområde*", "paradigme", "paradigmeskifte*", "udvis*",
                            "islam*", "international* konvention*",
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

kwic(tokens_body, phrase("medborger*")) # For at se privatliv-dictionary in-context!


kwic(tokens_body, phrase("*tvangsfjer*"), window = 7) %>%
  as_tibble() %>%
  select(pre:post) %>% # tiføjer de to linjer for at skjule doc_id
  sample_n(16) # 
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

# Hvor stor en andel af de 13.747 ads kodes som klima?
emne_df <- dfm_emne %>%
  convert("data.frame")
glimpse(emne_df)

# Laver en dikotom version, så det er docfreq i stedet for count
emne_01df <- emne_df
emne_01df[,2:length(emne_df)][emne_df[,2:length(emne_df)] > 1] <- 1
emne_01df %>%
  summary() # Nu kun 0 og 1. Fint!

# Tilføjer kolonne "antal emner"
emne_01df <- emne_01df %>%
  mutate(antal_emner = rowSums(select(.,-"document")),
         ingen_emner = if_else(antal_emner < 1, 1, 0))

summary(emne_01df) # 40 pct. er uden emne. Ser fint ud. 

# Hvor mange annoncer har hvilket antal emner?
emne_01df %>%
  count(antal_emner) # 5713 (40 %) har intet emne. 38 % har 1 emne osv.

# Hvordan er fordelingen mellem emner?
# colSums(select(emne_01df,-"document")) - den viser bare docfreq, samme som "stats_emne" oven for. 

# Merger resultaterne til election dataframe
fb_ads <- fb_ads %>%
  left_join(emne_01df, by = c("ad_id" = "document"))

glimpse(fb_ads)

# FOrdelingen på tværs af partier?

fb_ads %>%
  group_by(parti_navn) %>%
  summarise(n_ads = n(),
            klima_miljø = sum(.klima_miljo),
            børn = sum(.born),
            eu = sum(.eu),
            sundhed = sum(.sundhed),
            flygninge_indvandrere = sum(.flygninge_indvandrere),
            økonomi = sum(.okonomi),
            pension = sum(.pension),
            ældre = sum(.aeldre),
            integration = sum(.integration),
            ikke_kategoriseret = sum(ingen_emner),
            kategoriseret = n_ads-ikke_kategoriseret
  ) %>%
  arrange(desc(n_ads))
# tilføj evt. colSums til df


# Og samme tabel i procent (andel af alle partiets annoncer)

fb_ads %>%
  group_by(parti_navn) %>%
  summarise(n_ads = n(),
            klima_miljø = sum(.klima_miljo)/n_ads,
            børn = sum(.born)/n_ads,
            eu = sum(.eu)/n_ads,
            sundhed = sum(.sundhed)/n_ads,
            flygninge_indvandrere = sum(.flygninge_indvandrere)/n_ads,
            økonomi = sum(.okonomi)/n_ads,
            pension = sum(.pension)/n_ads,
            ældre = sum(.aeldre)/n_ads,
            integration = sum(.integration)/n_ads,
            kategoriseret = 1-mean(ingen_emner),
            ikke_kategoriseret = mean(ingen_emner)
  ) %>%
  arrange(desc(n_ads))
# tilføj evt. colSums til df



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

 # HV-placering tilføjes
 fb_ads <- fb_ads %>%
   mutate(Højreorienteret = case_when(PARTI %in% c("A", "F", "OE", "B", "AA", "N") ~ 0,
                                      PARTI %in% c("V", "C", "I", "K", "D", "E", "P", "O") ~ 1,
                                      T ~ NA_real_)) # Klaus Riskær Parti kodes som højreorienteret. Alternativet som venstreorienteret.

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

# 0 TEST; Estimerer alle dummy DV's i samme model
# inspireret af multivariate multiple regression her https://stats.idre.ucla.edu/r/whatstat/what-statistical-analysis-should-i-usestatistical-analyses-using-r/#mmreg 

mTEST.emne <- lm(cbind(.klima_miljo, .okonomi, .born, .sundhed, .eu, .flygninge_indvandrere, .pension, .aeldre, .integration) ~ Højreorienteret, data = fb_ads)
#m1.emne <- lm(cbind(tidyselect::vars_select(starts_with("."))) ~ Højreorienteret, data = fb_ads) # virker vist
#m1.emne <- lm(cbind(emner) ~ Højreorienteret, data = fb_ads) # virker vist også
summary(mTEST.emne)
# virker selvfølgelig ikke med texreg.
# MEN: Man kan altså lave alle regressionerne simultant. 

m1.emne <- lm(.klima_miljo ~ Højreorienteret, data = fb_ads)

summary(m1.emne)
# 11,1 % af de højreorienterede partiers annoncer er klima-miljø annoncer.
# 31,4 % af de venstreorienterede partiers annoncer er klima-miljø annoncer.
# SIGNIFIKANT forskel.

fb_ads %>%
  cbind(select(.,starts_with("."))) %>%
          glimpse() # endte med ikke at bruge. skrev dem ud i cbind.
# 2 lineær med vægt
m2.emne <- lm(.klima_miljo ~ Højreorienteret, data = fb_ads,
                     weights = spend_mid)
summary(m2.emne)
# 9,8 % af de højreorienterede partiers annoncekroner er gået til klima-miljø-annocer.
# 39,6 % af de venstreorienterede partiers annoncekroner er gået til klima-miljø annoncer.
# Koefficient (forskel): 29,9 pp.
# SIGNIFIKANT forskel.

# ... nu til klyngerobuste standardfejl

# 3 lineær med vægt og klyngerobuste standardfejl
m3.emne <- lm_robust(.klima_miljo ~ Højreorienteret, data = fb_ads,
                      weights = spend_mid, cluster = PARTI, se_type = "stata")
summary(m3.emne)
# Estimaterne er de samme som model 2. Kun standardfejlene er vokset. 
# Men det er STADIG signifikant. 

# 4 lineær med få kontrolvariable
m4.emne <- lm_robust(.klima_miljo ~ Højreorienteret + KØN + ALDER, data = fb_ads,
                      weights = spend_mid, cluster = PARTI, se_type = "stata")
summary(m4.emne)
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
  ggtitle("Sammenhæng mellem alder og andel klimaannoncer") +
  labs(x = "Alder",
       y = "Andel klimaannoncer",
       caption = "Note: ") +
  theme_sjplot()

# Resultat: Sammenhængen mellem alder og klima-miljø er alt andet end lineær. 
# Jeg ville gerne transformere den, men poly() siger, at der er missing values. Fortsætter uden.

# 5 Lineær med alle kontrolvariable
m5.emne <- lm_robust(.klima_miljo ~ Højreorienteret + KØN + ALDER + EP + instagram, data = fb_ads,
                     weights = spend_mid, cluster = PARTI, se_type = "stata")
summary(m5.emne)
# Koefficient: 28,9 %
# Fortsat SIGNIFIKANT forskel (men KÆMPE konfidensinterval på forskellen: højreorienterede promoverer 18 til 40 pp mere klima).
# instgram og EP har ikke noget at byde på.

# TIL LATEX
texreg(list(m1.emne, m2.emne, m3.emne, m4.emne, m5.emne), 
       dcolumn = TRUE, booktabs = TRUE, use.packages = FALSE, label = "tab:klima_hv", caption = "Klima- og miljø", float.pos = "hb!",
       include.rmse = F, include.ci = F, include.rsquared = F, custom.note = "Lineære regressionsmodeller", stars = 0.05,
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

# Visualisering
plot_model(m5.emne, type = "eff") # hold fast hvor er det en fed pakke! Ind i overleaf med noget af det. 


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