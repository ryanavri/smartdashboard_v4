# Preparation ----
# Load libraries
library(tidyverse)
library(lubridate)
library(chron)
library(anytime)
library(sf)

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

## Aktivitas Manusia ----
cam_cols <- names(CAM)
# Function to clean Aktivitas Manusia by KLHK data model
clean_aktivitas_manusia <- function(file_path, landscape, sel_cols) {
  dat <- read.csv(file_path) %>%
    mutate(
      Landscape = landscape,
      Tanggal = anytime(Waypoint.Date)
    ) %>%
    filter(Observation.Category.0 %in% c("Aktivitas Manusia")) %>%
    rename(
      Patrol_ID = Patrol.ID,
      Kategori_temuan = Observation.Category.1
    )
  
  missing_cols <- setdiff(sel_cols, names(dat))
  
  dat[missing_cols] <- NA
  
  dat %>%
    select(all_of(sel_cols))
}

# Function to clean Aktivitas Manusia by DLHK data model
clean_aktivitas_manusia_DLHK <- function(file_path, landscape, sel_cols) {
  dat <- read.csv(file_path) %>%
    mutate(
      Landscape = landscape,
      Tanggal = anytime(Waypoint.Date)
    ) %>%
    rename(
      Patrol_ID = Patrol.ID,
      Kategori_temuan = Observation.Category.0
    ) %>%
    filter(!Kategori_temuan %in% c("Satwa Liar", "Posisi", "Monitoring Orangutan", "Tumbuhan"))
  
  missing_cols <- setdiff(sel_cols, names(dat))
  
  dat[missing_cols] <- NA
  
  dat %>%
    select(all_of(sel_cols))
}

## Perjumpaan Satwa ----
NSL_cols <- names(CSL)
# Function to clean Perjumpaan Satwa by KLHK data model
clean_satwa <- function(file_path, landscape, sel_cols, site=NA) {
  dat <- read.csv(file_path) %>%
    mutate(
      Landscape = landscape,
      Site = site,
      Tanggal = anytime(Waypoint.Date)
    ) %>%
    filter(Observation.Category.0 %in% c("Satwa Liar")) %>%
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
  
  missing_cols <- setdiff(NSL_cols, names(dat))
  
  dat[missing_cols] <- NA
  
  dat %>%
    select(all_of(sel_cols))
}

# Function to clean Perjumpaan Satwa by DLHK data model
clean_satwa_DLHK <- function(file_path, landscape, sel_cols, site = NA) {
  dat <- read.csv(file_path) %>%
    mutate(
      Landscape = landscape,
      Site = site,
      Tanggal = anytime(Waypoint.Date)
    ) %>%
    rename(
      Patrol_ID = Patrol.ID,
      Kategori_temuan = Observation.Category.0
    ) %>%
    separate(Jenis.TSL, into = c("Jenis.satwa", "Scientific.Name"), sep = " - ") %>%
    drop_na(Scientific.Name) %>%
    mutate(Scientific.Name = case_when(
      Scientific.Name == "Panthera tigris sumatrae" ~ "Panthera tigris",
      Scientific.Name == "Hylobates syndactylus" ~ "Symphalangus syndactylus",
      Scientific.Name == "Catopuma teminckii" ~ "Catopuma temminckii",
      TRUE ~ Scientific.Name
    )) %>%
    filter(Kategori_temuan %in% c("Satwa Liar", "Monitoring Orangutan"))
  
  missing_cols <- setdiff(NSL_cols, names(dat))
  
  dat[missing_cols] <- NA
  
  dat %>%
    select(all_of(sel_cols))
}

# Memperbarui dataset----

## Dataset usaha patroli----
NRP1 <- clean_aktivitas_manusia(
  file_path = "source/Kalbar/2026_q1/Observasi_Query_2020_2024.shp",
  landscape = "Kalbar",
)

#CRP <- bind_rows(CRP, NRP1, NRP2)  # Combine old and new data

## Dataset aktivitas manusia----
NAM1 <- clean_aktivitas_manusia(
  file_path = "source/Kalbar/2026_q1/Observasi_Query_2020_2024.csv",
  landscape = "Kalbar",
  sel_cols = cam_cols
)

NAM2 <- clean_aktivitas_manusia_DLHK(
  file_path = "source/Kalbar/2026_q1/Observasi_Query_CFES.csv",
  landscape = "Kalbar",
  sel_cols = cam_cols
)

#CAM <- bind_rows(CAM, NAM1, NAM2)  # Combine old and new data

## Dataset satwa liar----
NSL1 <- clean_satwa(
  file_path = "source/Kalbar/2026_q1/Observasi_Query_2020_2024.csv",
  landscape = "Kalbar",
  sel_cols = NSL_cols
)


NSL2 <- clean_satwa_DLHK(
  file_path = "source/Kalbar/2026_q1/Observasi_Query_CFES.csv",
  landscape = "Kalbar",
  sel_cols = NSL_cols
)

#CSL <- bind_rows(CSL, NSL1, NSL2)  # Combine old and new data

# Data availability----
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
      Landscape == "Aceh" ~ "Muhammad Akbar", 
      Landscape == "Kerinci-Seblat" ~ "Wido Albert, Luri Ikhsan",
      Landscape == "Riau" ~ "Dwiyanto, Yogi Satrio",
      Landscape == "Kalbar" ~ "Jarian, Tutus",
      TRUE ~ "Unknown"
    ),
    Link = case_when(
      Landscape == "Aceh" ~ "01_Biodive_IP_allsite/00_SMART/Aceh",
      Landscape == "Kerinci-Seblat" ~ "01_Biodive_IP_allsite/00_SMART/KSL",
      Landscape == "Riau" ~ "01_Biodive_IP_allsite/00_SMART/Riau",
      Landscape == "Kalbar" ~ "01_Biodive_IP_allsite/00_SMART/Kalbar",
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
save(CAM, CRP, CSL, DAVAIL, datEff, taxon, cam_cols, NSL_cols, file = "source/smart_patrol_data7.RData")
