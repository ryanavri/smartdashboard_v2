#Preparation----

#load library
library(tidyverse)
library(lubridate)
library(chron)
library(anytime)

library(sf)

#Load and clean dataset

##Ringkasan Patroli----
###Load shapefile for Ringkasan Patroli----
ringkasan_patroli <- st_read("source/KSL/patrol_tracks.shp")

CRP <- ringkasan_patroli %>%  #name of data stands for "Cleaned Ringkasan Patroli"
  mutate(Landscape = "Kerinci-Seblat") %>% #Fill this value correctly!
  mutate(Jarak = st_length(ringkasan_patroli)) %>%
  select(-Patrol_L_1, -Patrol_L_2, -Armed, -Patrol_Leg) %>%
  as.data.frame()

### merge new data to the old one----
ringkasan_patroli <- st_read("source/KSL/Ringkasan_Jalur_Patroli_000251.shp")

NRP <- ringkasan_patroli %>%
  mutate(Landscape = "Kerinci-Seblat") %>% #Fill this value correctly!
  mutate(Jarak = st_length(ringkasan_patroli)) %>%
  select(-Patrol_L_1, -Patrol_L_2, -Armed, -Patrol_Leg) %>%
  as.data.frame()

CRP <- bind_rows(CRP, NRP) #this is your newest data

##Aktivitas Manusia----
###Load dataset (csv) for Aktivitas Manusia----
aktivitas_manusia <- read.csv("source/KSL/Aktivitas_Manusia_000053.csv")

CAM <- aktivitas_manusia %>%
  mutate(Landscape = "Kerinci-Seblat") %>% #Fill this value correctly!
  rename('Patrol_ID' ='Patrol.ID') %>%
  rename('Kategori_temuan' ='Observation.Category.1') %>%
  rename('Tanggal' = 'Waypoint.Date') %>%
  mutate(Tanggal = anytime(Tanggal)) %>%
  select(-`Observation.Category.0`)

### merge new data to the old one----


##Perjumpaan Satwa----
###Load dataset (csv) for Perjumpaan Satwa----
satwa_liar <- read.csv("source/KSL/Tabel_dan_sebaran_satwa_000001.csv")

CSL <- satwa_liar %>%
  mutate(Landscape = "Kerinci-Seblat") %>% #Fill this value correctly!
  rename('Patrol_ID' ='Patrol.ID') %>%
  rename('Kategori_temuan' ='Observation.Category.1') %>%
  rename('Tanggal' = 'Waypoint.Date') %>%
  mutate(Tanggal = anytime(Tanggal)) %>%
  select(-`Observation.Category.0`) %>%
  separate(`Jenis.satwa`, sep = ' - ', c("Jenis.satwa", "Scientific.Name")) %>% 
  drop_na(Scientific.Name) %>%
  mutate(Scientific.Name = case_when(#in the model, the data was slightly typos, maybe we`ll find more in the future
    Scientific.Name == "Panthera tigris sumatrae" ~ "Panthera tigris",
    Scientific.Name == "Hylobates syndactylus" ~ "Symphalangus syndactylus",
    Scientific.Name == "Catopuma teminckii" ~ "Catopuma temminckii",
    TRUE ~ Scientific.Name
  ))

###merge new data to the old one----
satwa_liar <- read.csv("input/Satwa_Liar_000248.csv")

NSL <- satwa_liar %>%
  mutate(Landscape = "Kerinci-Seblat") %>% #Fill this value correctly!
  mutate(Site = "Taman Nasional Kerinci Seblat") %>% #Fill this value correctly!
  rename('Patrol_ID' ='Patrol.ID') %>%
  rename('Kategori_temuan' ='Observation.Category.1') %>%
  rename('Tanggal' = 'Waypoint.Date') %>%
  mutate(Tanggal = anytime(Tanggal)) %>%
  select(-`Observation.Category.0`) %>%
  separate(`Jenis.satwa`, sep = ' - ', c("Jenis.satwa", "Scientific.Name")) %>% 
  drop_na(Scientific.Name)

CSL <- bind_rows(CSL, NSL) #this is your newest data






##Perjumpaan Tumbuhan----
###Load dataset (csv) for Tumbuhan----
tumbuhan <- read.csv("source/KSL/Tumbuhan_000089.csv")

CTH <- tumbuhan %>%
  mutate(Landscape = "Kerinci-Seblat") %>% #Fill this value correctly!
  rename('Patrol_ID' ='Patrol.ID') %>%
  rename('Kategori_temuan' ='Tipe.temuan') %>%
  rename('Tanggal' = 'Waypoint.Date') %>%
  mutate(Tanggal = anytime(Tanggal)) %>%
  select(-`Observation.Category.0`) %>%
  separate(`Jenis.tumbuhan`, sep = ' - ', c("Jenis.tumbuhan", "Scientific.Name")) %>% 
  drop_na(Scientific.Name) 

# Export all ---- 
# Save the variables into an RData file
save(CAM, CRP, CSL, CTH, file = "source/smart_patrol_data.RData")

