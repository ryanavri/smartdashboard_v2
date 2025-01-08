library(tidyverse)
library(rredlist)
library(rcites)

load("source/smart_patrol_data.RData")

#load database for Indonesian protected species
urlfile<-'https://raw.githubusercontent.com/ryanavri/GetTaxonCS/main/PSG_v3.csv'
db <- read.csv(urlfile)

set_token("kUydW4HMDXY9AvDFSThxMwtt") #This is Ryan Tokens

#set token from IUCN
Sys.setenv(IUCN_KEY = "9bb4facb6d23f48efbf424bb05c0c1ef1cf6f468393bc745d42179ac4aca5fee") #This is Ryan Tokens # nolint
apikey <- Sys.getenv("IUCN_KEY")


#Set argument for the functions#### 
retrieve_IUCN_data <- function(speciesList){
  IUCN_status <- data.frame(Species = character(), Status = character(), 
                            Trend = character(), Family = character(), Order = character(), 
                            Class = character(), stringsAsFactors=FALSE)
  for(sp in speciesList){
    IUCN_search <- rl_search(name = sp, key = apikey)
    if (length(IUCN_search$result) == 0){
      IUCN_status_sp <- data.frame(Species = sp, 
                                   Status = 'NA', 
                                   Trend = 'NA', 
                                   Family ='NA', 
                                   Order ='NA',
                                   Class = 'NA',
                                   stringsAsFactors=FALSE)
      IUCN_status <- rbind(IUCN_status, IUCN_status_sp)
      cat(sp,'----- CHECK\n')
    }
    else {
      IUCN_status_sp <- data.frame(Species = IUCN_search$result$scientific_name, 
                                   Status = IUCN_search$result$category, 
                                   Trend = IUCN_search$result$population_trend,
                                   Order = IUCN_search$result$order,
                                   Family = IUCN_search$result$family,
                                   Class = IUCN_search$result$class,
                                   stringsAsFactors=FALSE)
      IUCN_status <- rbind(IUCN_status, IUCN_status_sp)
    }
  }
  return(IUCN_status)
}

retrieve_CITES_data <- function(speciesList){
  CITES_status <- data.frame(Species = character(), taxon_id = character(), 
                             stringsAsFactors=FALSE)
  for(sp in speciesList){
    CITES_search <- spp_taxonconcept(query_taxon = sp)
    if (length(CITES_search$all_id) == 0){
      CITES_status_sp <- data.frame(Species = sp, 
                                    taxon_id = 'NA', 
                                    stringsAsFactors=FALSE)
      CITES_status <- rbind(CITES_status, CITES_status_sp)
      cat(sp,'----- CHECK\n')
    }
    else {
      CITES_status_sp <- data.frame(Species = CITES_search$all_id$full_name, 
                                    taxon_id = CITES_search$all_id$id, 
                                    stringsAsFactors=FALSE)
      CITES_status <- rbind(CITES_status, CITES_status_sp)
    }
  }
  return(CITES_status)
}

# Function for data validator
between <- function(a, b) {
  function(x) {
    ifelse(!is.na(x), a <= x & x <= b, FALSE)
  }
}


Species_df <- CSL %>%
  distinct(Species = Scientific.Name)

sp1 <- retrieve_IUCN_data(Species_df$Species)
sp2 <- retrieve_CITES_data(Species_df$Species) %>%
  distinct(Species, .keep_all = TRUE)

# Check if sp2 'taxon_id' column contains only the string "NA"
if (all(sp2$taxon_id == "NA")) {
  # Proceed without sp2 and sp3 data
  result <- left_join(sp1, db, by = "Species") %>%
    select(Class, Order, Family, Species, Status, Trend, Protected, Endemic, Migratory) %>%
    mutate(across(c(Class, Order, Family), tolower)) %>%
    mutate(across(c(Class, Order, Family), tools::toTitleCase)) %>%
    arrange(Order, Family, Species)
} else {
  # Proceed with additional data processing involving sp2 and sp3
  sp3 <- spp_cites_legislation(taxon_id = sp2$taxon_id, verbose = FALSE)
  sp3 <- as.data.frame (sp3[["cites_listings"]])
  sp3 <- sp3 %>%
    distinct(taxon_id, .keep_all = TRUE)
  
  result <- left_join(sp1, sp2, by='Species') %>%
    left_join(., sp3, by='taxon_id') %>%
    left_join(., db, by="Species") %>%
    select(Class, Order, Family, Species, Status, Trend, appendix, Protected, Endemic, Migratory) %>%
    mutate(across(c(Class, Order, Family), tolower)) %>%
    mutate(across(c(Class, Order, Family), tools::toTitleCase)) %>%
    rename(Appendix = appendix) %>%
    arrange(Order, Family, Species)
}

# remove undeteced species
taxon <- result %>%
  filter(Class!="Na") %>%
  rename(Scientific.Name = Species)

  