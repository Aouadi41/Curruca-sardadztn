
library(tidyverse)
library(auk)



# File paths

setwd("C:/Users/DALLA/Desktop/work/Ssarda/REVIEW/data/ebd_TN_marwar1_smp_relOct-2024")

# File paths
ebd_file <- "ebd_TN_marwar1_smp_relOct-2024.txt"
sampling_file <- "ebd_TN_marwar1_smp_relOct-2024_sampling.txt"



# Load auk library
library(auk)

# Create auk_ebd object
ebd <- auk_ebd(ebd_file, file_sampling = sampling_file)

# Filter for Marmora's Warbler presence with taxonomy version
ebd_presence <- ebd %>%
  auk_species(species = "Marmora's Warbler", taxonomy_version = 2024) %>%
  auk_complete()


# Define output file paths
presence_file <- "marmoras_warbler_presence.txt"        # For observations
presence_sampling_file <- "marmoras_warbler_sampling.txt"  # For sampling data






# Apply the filter and save the presence data
auk_filter(
  ebd_presence,
  file = presence_file,
  file_sampling = presence_sampling_file
)



# Load the filtered presence data
presence_data <- read_ebd(presence_file)
sampling_data <- read.csv(presence_sampling_file, sep = "\t")

sampling_data<-as_tibble(sampling_data)

# Load Marmora's Warbler presence data
presence_data <- read_ebd(presence_file)





###########################################################################


f_ebd  <- "ebd_TN_marwar1_smp_relOct-2024.txt"
f_smp    <- "ebd_TN_marwar1_smp_relOct-2024_sampling.txt"





filters <- auk_ebd(f_ebd, file_sampling = f_smp) %>% 
  auk_species(species = "Marmora's Warbler", taxonomy_version = 2024) %>%
  auk_complete()
filters


ebd_sed_filtered <- auk_filter(filters, overwrite = TRUE,
                             file = "ebd-filtered.txt",
                             file_sampling = "sampling-filtered.txt")


ebd_sed_filtered
                               
                               
                        


ebd_zf <- auk_zerofill(ebd_sed_filtered)
ebd_zf



ebd_zf$sampling_events
ebd_zf$observations


head(ebd_zf$observations)
head(ebd_zf$sampling_events)


glimpse(ebd_zf$sampling_events)



ebd_zf_df <- collapse_zerofill(ebd_zf)
class(ebd_zf_df)



ebd_zf_df$observation_count

library(xlsx)

write.xlsx(ebd_zf_df, "data.xlsx")


getwd()








###########################################################################



setwd("C:/Users/DALLA/Desktop/work/Ssarda/REVIEW/data/ebd_DZ_marwar1_smp_relOct-2024")



f_ebd  <- "ebd_DZ_marwar1_smp_relOct-2024.txt"
f_smp    <- "ebd_DZ_marwar1_smp_relOct-2024_sampling.txt"





filters <- auk_ebd(f_ebd, file_sampling = f_smp) %>% 
  auk_species(species = "Marmora's Warbler", taxonomy_version = 2024) %>%
  auk_complete()
filters


ebd_sed_filtered <- auk_filter(filters, overwrite = TRUE,
                               file = "ebd-filtered.txt",
                               file_sampling = "sampling-filtered.txt")


ebd_sed_filtered





ebd_zf <- auk_zerofill(ebd_sed_filtered)
ebd_zf



ebd_zf$sampling_events
ebd_zf$observations


head(ebd_zf$observations)
head(ebd_zf$sampling_events)


glimpse(ebd_zf$sampling_events)



ebd_zf_df <- collapse_zerofill(ebd_zf)
class(ebd_zf_df)



ebd_zf_df$observation_count

library(xlsx)

write.xlsx(ebd_zf_df, "dataDZ.xlsx")


getwd()




