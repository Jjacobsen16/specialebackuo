# 1. Forberedelse til Quanteda ---------------------------------

## 1.1 Indlæser pakker og data =================================

library(pacman)
p_load(tidyverse, readtext, quanteda, xtable, tictoc) 
# måske lubridate, topicmodels

fb_ads <- read_rds("FB API data/data/fb_ads.rds")

## 1.2 Datamanipulation og ID =================================

## 1.3 Konstruktion af corpus =================================

### Section Three #############################

# 2. Naive Bayes estimation ---------------------------------

## 2.1 Opsætning af trænings- og testdata =================================
## 2.2 Laver DFM baseret på de to datasæt =================================
## 2.3 Træner algoritmen og tester dens performance =================================
## 2.4 Sammenligner med random prediction =================================
