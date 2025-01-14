library(tidyverse)
library(sf)


# param
bgc <- "SBSwk1"

# these need to be stored in the package
nbrs <- read.csv("C:/Users/ccarmour.stu/OneDrive/PEM/edatopic_neighbours.csv") %>%
  as.data.frame()
edat <- readxl::read_xlsx("C:/Users/ccarmour.stu/OneDrive/PEM/Edatopic_v12_12.xlsx") %>%
  as.data.frame()

# Get vector of unique site series in BGC for loop
ss_all <- filter(edat, BGC == bgc) %>% pull(SS_NoSpace) %>% unique() %>% sort()

# Initialize a matrix with all of the site series in the BGC as both the row names and column names.
# This is what I like to call the "reverse confusion matrix"
mtx <- matrix(data = NA_integer_,
              nrow = length(ss_all),
              ncol = length(ss_all),
              byrow = FALSE,
              dimnames = list(ss_all, ss_all))


for(i in 1:length(ss_all)){
  # Target site series
  ss_tar <- ss_all[i]
  # Find cells of the target site series
  ss_cells_tar <- filter(edat, SS_NoSpace == ss_tar) %>%
    pull(Edatopic)
  # Find neighbours and remove duplicate neighbours from list
  ss_nbrs <- filter(nbrs, target %in% ss_cells_tar) %>%
    pull(fuzzy)

  # Set score of ss_tar == ss_tar (left-right diagonal) to 1
  mtx[ss_tar, ss_tar] <- 1

  # Extract remaining site series
  # We want two-way comparisons, so leave the same
  ss_rem <- setdiff(ss_all, ss_tar) %>% sort()

  for(j in 1:length(ss_rem)){
    # Choose non-target site series from list
    ss_fuzz <- ss_rem[j]
    # Find cells of the non-target site series that may neighbour the target site series
    ss_cells_fuzz  <-  filter(edat, SS_NoSpace == ss_fuzz) %>%
      pull(Edatopic)

    # Extract which target cells have overlap (share a square) with non-target cells
    sq_count <- ss_cells_tar[ss_cells_tar %in% ss_cells_fuzz] %>% length()
    # Extract which target neighbours have overlap (share a border) with non-target cells
    br_count <- ss_cells_fuzz[ss_cells_fuzz %in% ss_nbrs] %>% length()

    # add up the total score as per rules where shared square = +0.25 and shared border = +0.1
    score = sq_count*0.25 + br_count*0.1

    # Update the relevant cell in the matrix
    mtx[ss_tar, ss_fuzz] <- score
  }
}
mtx
