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
    as.data.frame()
}

# Clean and combine datasets
CRP <- clean_ringkasan_patroli("source/KSL/patrol_tracks.shp", "Kerinci-Seblat")
# NRP <- clean_ringkasan_patroli("source/KSL/Ringkasan_Jalur_Patroli_000251.shp", "Kerinci-Seblat")
# CRP <- bind_rows(CRP, NRP)  # Combine old and new data

## Aktivitas Manusia ----
# Load and clean Aktivitas Manusia CSV
CAM <- read.csv("source/KSL/Aktivitas_Manusia_000053.csv") %>%
  mutate(
    Landscape = "Kerinci-Seblat",
    Tanggal = anytime(Waypoint.Date)
  ) %>%
  rename(
    Patrol_ID = Patrol.ID,
    Kategori_temuan = Observation.Category.1
  ) %>%
  select(-Observation.Category.0)

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
# NSL <- clean_satwa("input/Satwa_Liar_000248.csv", "Kerinci-Seblat", "Taman Nasional Kerinci Seblat")
# CSL <- bind_rows(CSL, NSL)  # Combine old and new data

## Perjumpaan Tumbuhan ----
# Load and clean Perjumpaan Tumbuhan CSV
CTH <- read.csv("source/KSL/Tumbuhan_000089.csv") %>%
  mutate(
    Landscape = "Kerinci-Seblat",
    Tanggal = anytime(Waypoint.Date)
  ) %>%
  rename(
    Patrol_ID = Patrol.ID,
    Kategori_temuan = Tipe.temuan
  ) %>%
  select(-Observation.Category.0) %>%
  separate(Jenis.tumbuhan, into = c("Jenis.tumbuhan", "Scientific.Name"), sep = " - ") %>%
  drop_na(Scientific.Name)

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
      Landscape == "Kerinci-Seblat" ~ "Ican, Doddy",
      Landscape == "Riau" ~ "Yogi, Dwi",
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

# Export all ----
# Save the cleaned datasets to an RData file
save(CAM, CRP, CSL, CTH, DAVAIL, file = "source/smart_patrol_data.RData")
