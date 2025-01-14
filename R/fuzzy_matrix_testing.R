library(tidyverse)
library(sf)

nbrs <- read.csv("C:/Users/ccarmour.stu/OneDrive/PEM/edatopic_neighbours.csv") %>%
  as.data.frame()
edat <- readxl::read_xlsx("C:/Users/ccarmour.stu/OneDrive/PEM/Edatopic_v12_12.xlsx") %>%
  as.data.frame()

# param
bgc <- "SBSwk1"

# Get vector of unique site series in BGC for loop
ss_all <- filter(edat, BGC == bgc) %>% pull(SS_NoSpace) %>% unique()

# Get individual ss
i = 1
ss <- ss_all[i]

ss_cells <- filter(edat, SS_NoSpace == ss) %>% pull(Edatopic)
ss_nbrs <- filter(nbrs, target %in% ss_cells) %>% pull(fuzzy)

