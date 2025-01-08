# Preparation ----
# Load libraries
library(tidyverse)
library(lubridate)
library(chron)
library(anytime)
library(sf)

# Load and clean dataset ----

## Ringkasan Patroli ----
# Function to clean Ringkasan Patroli shapefiles
clean_ringkasan_patroli <- function(file_path, landscape) {
  st_read(file_path) %>%
    mutate(
      Landscape = landscape,
      Jarak = st_length(.)
    ) %>%
    select(-Patrol_L_1, -Patrol_L_2, -Armed, -Patrol_Leg) %>%
    as.data.frame() %>%
    select(-geometry)
}

# Clean and combine datasets
CRP <- clean_ringkasan_patroli("source/KSL/patrol_tracks.shp", "Kerinci-Seblat")
NRP <- clean_ringkasan_patroli("source/Riau/Jalur_kegiatan_000038.shp", "Riau")
CRP <- bind_rows(CRP, NRP)  # Combine old and new data

## Aktivitas Manusia ----
# Function to clean Aktivitas Manusia CSVs
clean_aktivitas_manusia <- function(file_path, landscape) {
  read.csv(file_path) %>%
    mutate(
      Landscape = landscape,
      Tanggal = anytime(Waypoint.Date)
    ) %>%
    rename(
      Patrol_ID = Patrol.ID,
      Kategori_temuan = Observation.Category.1
    ) %>%
    select(-`Observation.Category.0`, -`Patrol.Leg.ID`, -`Waypoint.ID`)
  
}

# Clean and combine datasets
CAM <- clean_aktivitas_manusia("source/KSL/Aktivitas_Manusia_000053.csv", "Kerinci-Seblat")
NAM <- clean_aktivitas_manusia("source/Riau/Aktivitas_manusia__pelanggaran__000053.csv", "Riau")
CAM <- bind_rows(CAM, NAM)  # Combine old and new data

## Perjumpaan Satwa ----
# Function to clean Perjumpaan Satwa CSVs
clean_satwa <- function(file_path, landscape, site = NA) {
  read.csv(file_path) %>%
    mutate(
      Landscape = landscape,
      Site = site,
      Tanggal = anytime(Waypoint.Date)
    ) %>%
    rename(
      Patrol_ID = Patrol.ID,
      Kategori_temuan = Observation.Category.1
    ) %>%
    select(-Observation.Category.0) %>%
    separate(Jenis.satwa, into = c("Jenis.satwa", "Scientific.Name"), sep = " - ") %>%
    drop_na(Scientific.Name) %>%
    mutate(Scientific.Name = case_when(
      Scientific.Name == "Panthera tigris sumatrae" ~ "Panthera tigris",
      Scientific.Name == "Hylobates syndactylus" ~ "Symphalangus syndactylus",
      Scientific.Name == "Catopuma teminckii" ~ "Catopuma temminckii",
      TRUE ~ Scientific.Name
    ))
}

# Clean and combine datasets
CSL <- clean_satwa("source/KSL/Tabel_dan_sebaran_satwa_000001.csv", "Kerinci-Seblat")
NSL <- clean_satwa("source/Riau/Tabel_dan_sebaran_satwa_000001.csv", "Riau")
CSL <- bind_rows(CSL, NSL)  # Combine old and new data

# Data availability
DAVAIL <- CRP %>%
  mutate(
    Patrol_Sta = lubridate::mdy(Patrol_Sta),
    Patrol_End = lubridate::mdy(Patrol_End),
    Patrol_Days = as.numeric(Patrol_End - Patrol_Sta)
  ) %>%
  group_by(Landscape) %>%
  summarise(
    "Total Patrols" = n(),
    "Start Date" = min(Patrol_Sta, na.rm = TRUE),
    "End Date" = max(Patrol_End, na.rm = TRUE),
    "Patrol Days" = sum(Patrol_Days, na.rm = TRUE)
  ) %>%
  mutate(
    PIC = case_when(
      Landscape == "Kerinci-Seblat" ~ "Doddy Saputra, Luri Ikhsan",
      Landscape == "Riau" ~ "Dwiyanto, Yogi Satrio",
      Landscape == "Kalbar" ~ "Jarian, Tutus",
      TRUE ~ "Unknown"
    ),
    Link = case_when(
      Landscape == "Kerinci-Seblat" ~ "https://example.com/kerinci",
      Landscape == "Riau" ~ "https://example.com/riau",
      Landscape == "Kalbar" ~ "https://example.com/kalbar",
      TRUE ~ NA_character_
    )
  ) %>%
  arrange(Landscape)

# other argument
datEff <- CRP %>%
  group_by(Patrol_ID) %>%
  summarise(effort = sum(Jarak)) %>%
  inner_join(CRP %>% distinct(Patrol_ID, Patrol_Sta), by = "Patrol_ID")   %>%
  as.data.frame() %>%
  select(-geometry)

# Export all ----
# Save the cleaned datasets to an RData file
save(CAM, CRP, CSL, DAVAIL, datEff, taxon, file = "source/smart_patrol_data.RData")
