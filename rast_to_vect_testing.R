require(sf)
require(tidyverse)
require(terra)

# Get VRI data in polygon form
vri <- sf::st_read("C:/Users/ccarmour.stu/OneDrive/UBC/phd_data/VRI/VEG_R1_PLY_BDY_R.shp")
# Get PEM data in raster form and resulting map key
pem <- terra::rast("C:/Users/ccarmour.stu/OneDrive/UBC/phd_data/bec_pem/PEM_ss_5m.tif")
key_fact <- readxl::read_xlsx("C:/Users/ccarmour.stu/OneDrive/UBC/phd_data/placeholder_map_key.xlsx") %>%
  mutate(code = as.factor(code))
key_dbl <- readxl::read_xlsx("C:/Users/ccarmour.stu/OneDrive/UBC/phd_data/placeholder_map_key.xlsx")

# Manually review and select a VRI polygon to demonstrate
vri_p <- vri %>% filter(POLYGON_ID == 65835125)
# Mask the PEM raster to the polygon of interest and add labels from map key
pem_p <- pem %>% terra::crop(terra::vect(vri_p), mask = TRUE)
levels(pem_p) <- list(key_dbl)

# Summarize the number of cells corresponding to each site series label and find the proportion of total area
pem_p_df <- as.data.frame(pem_p) %>%
  tibble() %>%
  mutate(ss_name = as.factor(name)) %>%
  group_by(ss_name) %>%
  summarize(count = n()) %>%
  arrange(desc(count)) %>%
  mutate(total_cells = sum(pull(., count)),
         ss_prop = round(count/total_cells, 2)) %>%
  left_join(., key_fact, by = c("ss_name" = "code")) %>%
  dplyr::select(c(ss_name, ss_prop))

# If fewer than 5 site series are present, fill out table to 5 rows regardless to ensure number of columns is consistent.
if(nrow(pem_p_df) < 5){
  pem_p_df_trunc <- tibble(ss_name = rep(NA_character_, times = 5 - nrow(pem_p_df)),
                     ss_prop = rep(NA_integer_, times = 5 - nrow(pem_p_df))) %>%
    rbind(pem_p_df, .)
}

pem_p_df_row <- pem_p_df %>%
  slice_head(n = 5) %>%
  mutate(row = row_number()) %>%
  pivot_wider(
    names_from = row,
    values_from = c(ss_name, ss_prop),
    names_glue = "{toupper(gsub('ss_', 'SS_', .value))}_{row}"
  )

cbind(dplyr::select(vri_p, POLYGON_ID), pem_p_df_row)


pem_p_plot <- as.data.frame(pem_p, xy = TRUE)

ggplot() +
  geom_raster(data = pem_p_plot, aes(x = x, y = y, fill = name)) +
  coord_fixed()

ggsave(filename  = "C:/Users/ccarmour.stu/OneDrive/UBC/agreements_deliverables/2025_figs/cropped_PEM_to_VRI.png")


full_key <- readxl::read_xlsx("C:/Users/ccarmour.stu/OneDrive/UBC/phd_data/bec_pem/tables/PEM_map_key.xlsx") %>%
  tibble() %>%
  select(c(map_label, z_szvp_ss_code)) %>%
  mutate(map_label = as.double(map_label))

levels(pem) <- list(full_key)

pem_agg <- terra::aggregate(pem, fact = 5, fun = "modal")

pem_plot <- pem_agg %>%
  as.data.frame(xy = T) %>%
  tibble()

ggplot() +
  geom_raster(data = pem_plot, aes(x = x, y = y, fill = z_szvp_ss_code)) +
  coord_fixed() +
  labs(fill = "BEC Subzone/Variant") +
  theme(
    legend.position = "bottom",
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  )
