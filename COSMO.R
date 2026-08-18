library(tidyverse)
library(ggplot2)
library(readxl)
library(openxlsx)
library(viridis)
library(patchwork)
library(vegan)

####Input####
COSMO_Input <-
  read_xlsx("Input/COSMO Data Entry MASTER 08.16.2026.xlsx", sheet = 1)

####Processing####
#####Initial Processing#####
COSMO_Processed <- COSMO_Input %>%
  mutate(Live_Oysters = as.numeric(Live_Oysters),
         oys_per_m2 = Live_Oysters*4,
         Box_Oysters = as.numeric(Box_Oysters),
         box_per_m2 = as.numeric(Box_Oysters)*4,
         Mussel_Count = as.numeric(Mussel_Count),
         mussel_per_m2 = as.numeric(Mussel_Count)*4,
         Primary_percent_cover = as.numeric(Primary_percent_cover),
         Total_percent_cover = as.numeric(Total_percent_cover),
         Meter = as.numeric(Meter),
         Site = factor(Site, levels = Site_List),
         Date = openxlsx::convertToDate(Date),
         Year = format(Date, "%Y"),
         totalPrimaryLiveCover = Primary_percent_cover-as.numeric(Bare)) %>% 
  unite("Transect_Meter", Transect_Replicate, Meter, sep = ".") %>% 
  relocate(oys_per_m2, .after = Live_Oysters) %>% 
  relocate(box_per_m2, .after = Box_Oysters) %>% 
  relocate(mussel_per_m2, .after = Mussel_Count) %>% 
  relocate(Year, .after = Date) %>% 
  relocate(totalPrimaryLiveCover, .after = `Total_percent_cover`)

COSMO_Processed_Filtered <- COSMO_Processed %>%
  filter(Data_Recorder == "SUM") %>% 
  mutate(SampleID = row_number()) %>% 
  relocate(SampleID, .before = Date) %>% 
  select(-c(`Oyster 1 (mm)`:`Oyster 10 (mm)`))

#####Percent Cover#####
COSMO_Normalized_Percentage <- COSMO_Processed_Filtered %>% 
  mutate(across(Live_Oysters:UNID_spp., ~as.numeric(.x)),
         Error_Score = 25/Primary_percent_cover) %>%
  mutate(across(Primary_percent_cover:UNID_spp., ~.x*(Error_Score)*4),
         Primary_percent_cover_unscaled = Primary_percent_cover/4/Error_Score) %>% 
  relocate(Error_Score, .before = Primary_percent_cover) %>% 
  relocate(Primary_percent_cover_unscaled, .before = Primary_percent_cover)
  
Percent_Cover_unpivoted <- COSMO_Normalized_Percentage %>% 
  dplyr::select(SampleID:`Error_Score_(Qualitative)`, Error_Score, Bare:UNID_spp.)

Percent_Cover_unpivoted_trimmed <- Percent_Cover_unpivoted %>% 
  filter(`Error_Score_(Qualitative)` != "High" & between(Error_Score, 0.8, 1.2)) #20% margin of error
  

Percent_Cover_aggregated <- Percent_Cover_unpivoted_trimmed %>%
  mutate(across(Bare:UNID_spp., ~replace_na(as.numeric(.x), 0))) %>% 
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
    `UNID_spp.`
    )) %>% 
  relocate(`Green Algae spp.`:`Tunicate spp. canopy`, .after = Bare)

Percent_Cover_aggregated_pivoted <- Percent_Cover_aggregated %>%
  pivot_longer(cols = Bare:`Diatom_canopy`,
               names_to = "response_variable",
               values_to = "percent_cover") %>%
  mutate(Native = case_when(
    response_variable == "Bare" ~ "Bare",
    response_variable %in% c("Barnacles", "Barnacle_canopy", "Oysters", "Oyster_canopy", "Green Algae spp.", "Green Algae spp. canopy", "Red Algae spp.", "Red Algae spp. canopy"
    ) ~ "Native",
    response_variable %in% c("Tunicate spp.", "Tunicate spp. canopy", "Bryozoan spp.", "Bryozoan spp. canopy", "Tubeworm", "Hydroid_canopy", "Hydroid"
    ) ~ "Non-native",
    .default = "Unknown"))

Primary_Percent_Cover_aggregated_pivoted <- Percent_Cover_aggregated_pivoted %>% 
  filter(!grepl("Canopy|canopy|can", response_variable))

Secondary_Percent_Cover_aggregated_pivoted <- Percent_Cover_aggregated_pivoted %>% 
  filter(grepl("Canopy|canopy|can", response_variable))

#####Per Quadrat Native Summary#####
perquad_Native_Summary <- Primary_Percent_Cover_aggregated_pivoted %>%
  filter(percent_cover != 0) %>% 
  #filter(response_variable != "bare") %>% 
  group_by(Year, Site, Native, SampleID) %>% 
  summarise(MeanPercentCover = mean(percent_cover, na.rm = T),
            se = sd(percent_cover, na.rm = T)/sqrt(n()),
            n = n()) %>% 
  ungroup() %>% 
  tidyr::complete(Site, Year, SampleID, Native, fill = list(`MeanPercentCover` = 0, `se` = NA, n = 0)) %>% 
  pivot_wider(
    names_from = Native,
    values_from = MeanPercentCover
  ) %>% 
  mutate(across(`Native`:`Unknown`, ~replace_na(as.numeric(.x), 0))) %>% 
  mutate(CoverSum = `Native` + `Non-native` + `Unknown`) %>% 
  mutate(NativeProportion = Native/CoverSum,
         `Non-nativeProportion` = `Non-native`/CoverSum,
         UnknownProportion = Unknown/CoverSum) %>% 
  filter(!is.nan(NativeProportion))%>% 
  group_by(Year, Site) %>% 
  summarise(
    MeanNativeProportion = mean(NativeProportion),
    `MeanNon-nativeProportion` = mean(`Non-nativeProportion`),
    UnknownProportion = mean(UnknownProportion),
    seProportion = sd(NativeProportion, na.rm = T)/sqrt(n()),
    n = n()
  ) %>% 
  mutate(across(c(MeanNativeProportion:seProportion), ~(.*100))) %>% 
  pivot_longer(
    cols = c(MeanNativeProportion, `MeanNon-nativeProportion`, UnknownProportion),
    names_to = c("Native"),
    values_to = c("Proportion")
  ) %>% 
  mutate(Native = gsub("Mean|Proportion", "", Native))


#####Oyster Sizes#####
COSMO_OysterSizes <- COSMO_Processed %>%
  dplyr::select(!(oys_per_m2:Notes)) %>%
  filter(Data_Recorder != "SUM") %>% 
  dplyr::select(!c(`Error_Score_(Qualitative)`:Live_Oysters)) %>% 
  mutate(Transect_Meter = replace_values(Transect_Meter, "NA.NA" ~ NA),
         Substrate_Type = replace_values(Substrate_Type, "N/A" ~ NA)) %>%
  fill(Date:Substrate_Type, .direction = "down") %>% 
  mutate(across(`Oyster 1 (mm)`:`Oyster 10 (mm)`, ~as.numeric(.))) %>% 
  pivot_longer(cols = `Oyster 1 (mm)`:`Oyster 10 (mm)`,
               names_to = "Oyster_#",
               values_to = "Size") %>% 
  drop_na(Size) %>% 
  group_by(Site)

####Plotting####

basic_plot_aesthetics <- function(){
  theme_bw(base_size = 15) +
    theme(axis.text = element_text(color = "#000000", angle = 45, vjust = 0.5, size = 16, hjust=1),
          axis.title = element_text(size = 24),
          plot.title = element_text(hjust = 0.5),
          strip.text = element_text(size = 16),
          panel.grid.major.x = element_line("#FFFFFF"))
}

#####Native Summary Barplot#####
Primary_perquad_Percent_Cover_Native_Summary_barplot <- perquad_Native_Summary %>% 
  ggplot(
    mapping = aes(
      x = Site,
      y = Proportion,
      fill = Native
    )) +
  geom_col(aes(group = Native), position=position_dodge(preserve = "single", width = .9), alpha = 0.8) +
  geom_errorbar(aes(ymin = Proportion - seProportion, ymax = Proportion + seProportion), width = 0.1, alpha = 0.5, position=position_dodge(preserve = "single", width = .9), alpha = 0.8) +
  # geom_text(aes(label = n), position=position_dodge(preserve = "single", width = .9), vjust = -1.2, size = 4, alpha = 0.6) +
  facet_wrap(vars(Year)) +
  scale_fill_manual(
    name="Cover Type",
    values = nativecolors) +
  labs(y = "Primary-layer \n proportional cover (%)", x = "Site") +
  basic_plot_aesthetics() +
  theme(axis.text = element_text(size = 12)) +
  coord_cartesian(ylim = c(0,100))
Primary_perquad_Percent_Cover_Native_Summary_barplot

#####Oyster Size Histogram#####
Oyster_Size_Histogram <- COSMO_OysterSizes %>% 
  ggplot(mapping = aes(
    x = Size,
    fill = Site
  )) +
  geom_histogram(color = 'white', binwidth = 10, breaks=c(0, 10, 20, 30, 40, 50, 60)) +
  scale_fill_manual(values = sitecolors) +
  #geom_bar(stat = "identity", position=position_dodge()) +
  labs(y = "Number oysters", x = "Size (mm)") +
  theme_bw(base_size = 15) +
  basic_plot_aesthetics() +
  theme(legend.position = "none") +
  facet_grid(Site ~ Year, scales = "free_y") +
  plot_layout(guides = "collect", axes = "collect")
Oyster_Size_Histogram

#####Oyster Density Boxplot#####
Oys_Density_Boxplot <- COSMO_Processed_Filtered %>% 
  ggplot(mapping = aes(
    x = Site,
    y = oys_per_m2
  )
  ) +
  geom_boxplot(aes(fill = Site), outlier.shape = NA, alpha = 0.7) +
  scale_fill_manual(limits = Site_List, values = sitecolors) +
  geom_jitter(color = "darkgrey", height = 0, width = 0.3, alpha = 0.5) + 
  stat_summary(fun.y="mean", shape = 5, size = 0.4, position = position_dodge(0.55), color = "black")+
  labs(y = bquote("Oysters/"*m^2)) +
  basic_plot_aesthetics() +
  theme(legend.position = "none") +
  facet_grid(~ Year, scales = "free_y") +
  plot_layout(guides = "collect", axes = "collect") +
  coord_cartesian(ylim = c(0, 200))
Oys_Density_Boxplot
ggsave(path = "Plots", filename = "Oyster Abundance vs. Site Boxplot.jpg", width = 10, height = 10)

#####Primary Percent Cover vs Site Boxplot#####
Site_Primary_Percent_Cover_Boxplot <- Primary_Percent_Cover_aggregated_pivoted %>%
  ggplot(mapping = aes(
    x = response_variable,
    y = percent_cover,
  )
  ) +
  geom_boxplot(aes(fill = response_variable), outlier.shape = NA, alpha = 0.8) +
  geom_jitter(height = 0, alpha = 0.2) + 
  stat_summary(fun.y="mean", shape = 5, size = 0.4, position = position_dodge(0.55), color = "black") +
  scale_x_discrete(labels=c("Bare", "Barnacles", "Bryozoans", "Green Algae", "Hydroid", "Mussel", "Oyster", "Red Algae", "Sponges", "Tubeworm", "Tunicates")) +
  labs(y = "Primary-layer cover (%)", x = "Aggregated taxa list") +
  scale_fill_manual(name="Species List", values = responsevarcolors) + 
  basic_plot_aesthetics() +
  theme(legend.position = "none",
        axis.text = element_text(size = 12)) +
  #scale_y_continuous(limits = c(0,40))+
  scale_y_log10(breaks = c(0,5,10,25, 50, 75, 100)) +
  #coord_cartesian(ylim = c(0, 40)) +
  facet_grid(~Site, scales = "free_y")
Site_Primary_Percent_Cover_Boxplot

#####Primary Percent Cover vs Year vs. Site Boxplot#####
Site_Year_Primary_Percent_Cover_Boxplot <- Site_Primary_Percent_Cover_Boxplot + facet_grid(Year~Site) + plot_layout(guides = "collect", axes = "collect")
Site_Year_Primary_Percent_Cover_Boxplot
ggsave(path = "Plots", filename = "Primary Cover vs Year vs Site boxplot.jpg", width = 10, height = 10)

####Diversity####
#####Setup#####
Percent_Cover_diversity <- Percent_Cover_unpivoted_trimmed %>% 
  select(-`Error_Score_(Qualitative)`) %>% 
  mutate(across(Bare:UNID_spp., ~replace_na(as.numeric(.x), 0.0000001))) %>% 
  pivot_longer(cols = Bare:UNID_spp.,
               names_to = "response_variable",
               values_to = "percent_cover") %>%
  mutate(percent_cover = percent_cover/4) %>% 
  rename(spcount = percent_cover) %>%
  group_by(Site, Year, Transect_Meter) %>% 
  summarize(rich = specnumber(spcount),
            shannon = diversity(spcount, index = "shannon")) %>% 
  mutate(evenness = shannon/log(rich))

Percent_Cover_Richness <- Percent_Cover_diversity %>%
  group_by(Site, Year) %>% 
  summarize(mean = mean(rich, na.rm = TRUE),
            sd = sd(rich, na.rm = TRUE),
            n = n()) %>%
  mutate(se = sd/sqrt(n)) %>% 
  cbind(div = "Species Richness")

Percent_Cover_Shannon <- Percent_Cover_diversity %>% 
  group_by(Site, Year) %>% 
  summarize(mean = mean(shannon, na.rm = TRUE),
            sd = sd(shannon, na.rm = TRUE),
            n = n()) %>%
  mutate(se = sd/sqrt(n)) %>% 
  cbind(div = "Shannon Diversity")

Percent_Cover_Evenness <- Percent_Cover_diversity %>% 
  group_by(Site, Year) %>% 
  summarize(mean = mean(evenness, na.rm = TRUE),
            sd = sd(evenness, na.rm = TRUE),
            n = n()) %>%
  mutate(se = sd/sqrt(n)) %>% 
  cbind(div = "Species Evenness")

Percent_Cover_diversity <- Percent_Cover_Richness %>% 
  rbind(Percent_Cover_Shannon) %>% 
  rbind(Percent_Cover_Evenness) 

#####Plotting#####

#####Combined Diversity Plot#####
Diversity_Combined_Plot <- Percent_Cover_diversity %>% 
  ggplot(mapping = aes(
    x = Year,
    y = mean,
    color = Site
  )) +
  geom_line(linewidth = 0.8, aes(group = Site), alpha = 0.7) +
  geom_point(aes(fill = Site), size = 2, alpha = 0.7) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0.1, alpha = 0.5) +
  labs(y = "", x = "Year") +
  scale_fill_manual(values = sitecolors,
                    aesthetics = c("color", "fill")) +
  facet_grid(div ~ ., scales = "free_y") +
  basic_plot_aesthetics() +
  theme(strip.text = element_text(size = 12),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 16)) +
  guides(color = guide_legend(override.aes = list(size=3)))
Diversity_Combined_Plot
ggsave(path = "Plots", filename = "Combined Diversity Indices 2025-2026.jpg", width = 8, height = 6)




####Lists####
#####Sites#####
Site_List <- c(
  "SP", #Strawberry Point
  "HH", #Herons Head Park
  "PM", #Point Molate
  "PE",  #Point Emery
  "YBI" #Yerba Buena Island
)

#####Aggregated Taxa Groups#####
`Green Algae spp.` <- c("Ulva", "Green_Algae_sp.", "Green_filamentous_algae", "Cladophora", "Bryopsis")
`Green Algae spp. canopy` <- c("Ulva_canopy", "Green_alg_sp._canopy", "Green_filamentous_algae_canopy", "Cladophora_canopy")
`Red Algae spp.` <- c("Mastocarpus", "Red_Algae_sp.", "Caulacanthus", "Chondracanthus", "Polyneura", "Red_filamentous_algae", "Fucus_spp.", "Gracilaria_spp.", "Cryptopleura_violacea", "Gymnogongrus_spp.", "Polysiphonia_spp.")
`Red Algae spp. canopy` <- c("Mastocarpus_canopy", "Red_alg_sp._canopy", "Caulacanthus_canopy", "Chondracanthus_canopy", "Polyneura_canopy", "Red_filamentous_algae_canopy", "Crytopleura_canopy", "Mazzaela_splendens_canopy", "Fucus_canopy", "Gracilaria_canopy", "Gymnogongrus_spp_canopy", "Polysiphonia_canopy")
`Bryozoan spp.` <- c("Bryozoan", "Bugula_neritina", "Cryptosula_pallasiana", "Watersipora_spp.", "Schizoporella_spp.", "Encrusting_bryozoan_spp.", "Upright_bryozoan_spp.")
`Bryozoan canopy spp.` <- c("Bryozoan_canopy", "Bugula_neritina_canopy", "Encrusting_bryozoan_canopy", "Upright_bryozoan_canopy")
`Tunicate spp.` <- c("Tunicate", "Colonial_tunicate", "Solitary_tunicate")
`Tunicate canopy spp.` <- c("Colonial_tunicate_canopy")

#####Colors#####
######Aggregated Taxa######
responsevarcolors <- c(
  `Bare` = "grey", 
  `Barnacles` = "#A5EDFF", `Barnacle_canopy` = "#A5EDFF",
  `Bryozoan spp.` = "#FFFF99", `Bryozoan spp. Canopy` = "#FFFF99",
  #`Diatom sp.` = "#FFBF7F", `diatom_canopy` = "#FFBF7F", 
  `Green Algae spp.` =  "#32FF00", `Green Algae spp. Canopy` =  "#32FF00",
  Hydroid = "#FF7F00", Hydroid_canopy = "#FF7F00",
  Mussels = "#654CFF", 
  Oysters = "#19B2FF", Oyster_canopy = "#19B2FF", 
  `Red Algae spp.` = "#E51932", `Red Algae spp. Canopy` = "#E51932", 
  `Sponge` = "gold", `Sponge_canopy` = "gold", 
  `Tubeworm` = "#CCBFFF", 
  `Tunicate spp.` = "#FF99BF", `Tunicate spp. Canopy` = "#FF99BF")

#######Sites######
tempColors3 <- viridis(5, begin = 0.1, end = 0.8)
sitecolors <- c(
  `SP` = tempColors3[1], 
  `HH` = tempColors3[2],
  `PM` = tempColors3[3],
  `PE` = tempColors3[4],
  `YBI` = tempColors3[5])

#######Native vs. Non-native######
tempColors4 <- mako(2, begin = 0.4, end = 0.7)
nativecolors <- c(
  `Bare` = "grey",
  `Native` = tempColors4[1],
  `Non-native` = tempColors4[2],
  `Unknown` = "purple")

#knitr::spin("COSMO.R", format = "Rmd", knit = FALSE)