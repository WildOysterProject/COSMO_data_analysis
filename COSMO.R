library(tidyverse)
library(ggplot2)
library(readxl)


####Input####
COSMO_Input <-
  read_xlsx("Input/COSMO Data Entry MASTER 08.06.2026.xlsx", sheet = 1)

####Variables and Functions####




####Processing####

COSMO_Processed <- COSMO_Input %>%
  mutate(Live_Oysters = as.numeric(Live_Oysters),
         oys_per_m2 = Live_Oysters*4,
         Box_Oysters = as.numeric(Box_Oysters),
         box_per_m2 = as.numeric(Box_Oysters)*4,
         Mussel_Count = as.numeric(Live_Oysters),
         mussel_per_m2 = as.numeric(Mussel_Count)*4,
         Primary_percent_cover = as.numeric(Primary_percent_cover),
         Total_percent_cover = as.numeric(Total_percent_cover),
         Meter = as.numeric(Meter),
         Site = factor(Site, levels = Site_List),
         Year = format(as.Date(Date, format="%d/%m/%Y"),"%Y"),
         totalLiveCover = Primary_percent_cover-as.numeric(Bare),
         across(Live_Oysters:UNID_spp., ~as.numeric(.x))) %>% 
  unite("Transect_Meter", Transect_Replicate, Meter, sep = ".") %>% 
  relocate(oys_per_m2, .after = Live_Oysters) %>% 
  relocate(box_per_m2, .after = Box_Oysters) %>% 
  relocate(mussel_per_m2, .after = Mussel_Count) %>% 
  relocate(Year, .after = Date) %>% 
  relocate(totalLiveCover, .after = `Total_percent_cover`)

COSMO_Processed_Filtered <- COSMO_Processed %>%
  filter(Data_Recorder == "SUM") %>% 
  mutate(SampleID = row_number()) %>% 
  relocate(SampleID, .before = Date)

COSMO_Normalized_Percentage <- COSMO_Processed_Filtered %>% 
  mutate(Error_Score = 25/Primary_percent_cover) %>% 
  mutate(across(Primary_percent_cover:UNID_spp., ~.x*(Error_Score)*4),
         Primary_percent_cover_unscaled = Primary_percent_cover/4/Error_Score) %>% 
  relocate(Error_Score, .before = Primary_percent_cover) %>% 
  relocate(Primary_percent_cover_unscaled, .before = Primary_percent_cover)
  
Percent_Cover_unpivoted <- COSMO_Normalized_Percentage %>% 
  dplyr::select(SampleID:`Error_Score_(Qualitative)`, Error_Score, Bare:UNID_spp.) %>% 
  mutate(across(Bare:UNID_spp., ~replace_na(as.numeric(.x), 0))) #Maybe not needed?

Percent_Cover_aggregated <- Percent_Cover_unpivoted %>%
  mutate(
    `Green Algae spp.` = rowSums(dplyr::select(., all_of(`Green Algae spp.`)), na.rm = T),
    `Green Algae spp. canopy` = rowSums(dplyr::select(., all_of(`Green Algae spp. canopy`)), na.rm = T),
    `Red Algae spp.` = rowSums(dplyr::select(., all_of(`Red Algae spp.`)), na.rm = T),
    `Red Algae spp. canopy` = rowSums(dplyr::select(., all_of(`Red Algae spp. canopy`)), na.rm = T),
    `Bryozoan spp.` = rowSums(dplyr::select(., all_of(`Bryozoan spp.`)), na.rm = T),
    `Bryozoan spp. canopy` = rowSums(dplyr::select(., all_of(`Bryozoan canopy spp.`)), na.rm = T),
    `Tunicate spp.` = rowSums(dplyr::select(., all_of(`Tunicate spp.`)), na.rm = T),
    `Tunicate spp. canopy` = rowSums(dplyr::select(., all_of(`Tunicate canopy spp.`)), na.rm = T)) %>% 
  dplyr::select(-c(
    all_of(`Green Algae spp.`), 
    all_of(`Green Algae spp. canopy`), 
    all_of(`Red Algae spp.`), 
    all_of(`Red Algae spp. canopy`),
    all_of(`Bryozoan spp.`), 
    all_of(`Bryozoan canopy spp.`), 
    all_of(`Tunicate spp.`), 
    all_of(`Tunicate canopy spp.`), 
    )) %>% 
  relocate(`Green Algae spp.`:`Tunicate spp. canopy`, .after = Bare)

# Percent_Cover_aggregated_pivoted <- Percent_Cover_aggregated %>% 
#   pivot_longer(cols = bare:`Tunicate spp. Canopy`,
#                names_to = "response_variable",
#                values_to = "percent_cover") %>% 
#   mutate(Native = case_when(
#     response_variable == "bare" ~ "Bare",
#     response_variable %in% c("barnacles", "barnacle_canopy", "oysters", "oyster_canopy", "Green Algae spp.", "Green Algae spp. Canopy", "Red Algae spp.", "Red Algae spp. Canopy"
#     ) ~ "Native",
#     response_variable %in% c("Tunicate spp.", "Tunicate spp. Canopy", "Bryozoan spp.", "Bryozoan spp. Canopy", "tubeworm", "hydroid_canopy", "Hydroid"
#     ) ~ "Non-native",
#     .default = "Unknown"))
# 

####Lists####

#####
Site_List <- c(
  "SP", #Strawberry Point
  "HH", #Herons Head Park
  "PM", #Point Molate
  "PE"  #Point Emery
)

#####Aggregated Taxa#####
`Green Algae spp.` <- c("Ulva", "Green_Algae_sp.", "Green_filamentous_algae", "Cladophora", "Bryopsis", "Eelgrass")
`Green Algae spp. canopy` <- c("Ulva_canopy", "Green_alg_sp._canopy", "Green_filamentous_algae_canopy", "Cladophora_canopy")
`Red Algae spp.` <- c("Mastocarpus", "Red_Algae_sp.", "Caulacanthus", "Chondracanthus", "Polyneura", "Red_filamentous_algae", "Fucus_spp.", "Gracilaria_spp.", "Cryptopleura_violacea", "Gymnogongrus_spp.")
`Red Algae spp. canopy` <- c("Mastocarpus_canopy", "Red_alg_sp._canopy", "Caulacanthus_canopy", "Chondracanthus_canopy", "Polyneura_canopy", "Red_filamentous_algae_canopy", "Crytopleura_canopy", "Mazzaela_canopy", "Fucus_canopy", "Gracilaria_canopy")
`Bryozoan spp.` <- c("Bryozoan", "Bugula_neritina", "Cryptosula_pallasiana")
`Bryozoan canopy spp.` <- c("Bryozoan_canopy", "Bugula_neritina_canopy")
`Tunicate spp.` <- c("Tunicate", "Colonial_tunicate", "Solitary_tunicate")
`Tunicate canopy spp.` <- c("Colonial_tunicate_canopy")


responsevarcolors <- c(
  `bare` = "grey", 
  `native` = "#66C2A5",
  `non-native` = "#BDA37D",
  `barnacles` = "#A5EDFF", `barnacle_canopy` = "#A5EDFF",
  `Bryozoan spp.` = "#FFFF99", `Bryozoan spp. Canopy` = "#FFFF99",
  #`Diatom sp.` = "#FFBF7F", `diatom_canopy` = "#FFBF7F", 
  `Green Algae spp.` =  "#32FF00", `Green Algae spp. Canopy` =  "#32FF00",
  Hydroid = "#FF7F00", hydroid_canopy = "#FF7F00",
  Mussels = "#654CFF", 
  oysters = "#19B2FF", oyster_canopy = "#19B2FF", 
  `Red Algae spp.` = "#E51932", `Red Algae spp. Canopy` = "#E51932", 
  `sponge` = "gold", `sponge_canopy` = "gold", 
  `tubeworm` = "#CCBFFF", 
  `Tunicate spp.` = "#FF99BF", `Tunicate spp. Canopy` = "#FF99BF")


