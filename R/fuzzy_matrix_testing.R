library(tidyverse)

# param
bgc <- "SBSmc2"

# these need to be stored in the package
nbrs <- read.csv("C:/Users/ccarmour.stu/OneDrive/PEM/edatopic_neighbours.csv") %>%
  as.data.frame()
edat <- readxl::read_xlsx("C:/Users/ccarmour.stu/OneDrive/PEM/Edatopic_v12_12.xlsx") %>%
  as.data.frame() %>%
  mutate(SS_NoSpace = stringr::str_replace(SS_NoSpace, pattern = "/", replacement = "_"))

# Get vector of unique site series in BGC for loop
ss_all <- filter(edat, BGC == bgc) %>% pull(SS_NoSpace) %>% unique() %>% sort()

# Initialize a matrix with all of the site series in the BGC as both the row names and column names.
# This is what I like to call the "reverse confusion matrix"
mtx <- matrix(data = NA_integer_,
              nrow = length(ss_all),
              ncol = length(ss_all),
              byrow = FALSE,
              dimnames = list(ss_all, ss_all))

## SCENARIO 1
## All instances of all neighbours

for(i in 1:length(ss_all)){
  # Target site series
  ss_tar <- ss_all[i]
  # ss_tar <- "SBSmc2_01"
  # Find cells of the target site series
  ss_cells_tar <- filter(edat, SS_NoSpace == ss_tar) %>%
    pull(Edatopic) %>% sort()
  # Find neighbours
  ss_nbrs <- filter(nbrs, target %in% ss_cells_tar) %>%
    pull(fuzzy) %>% sort()

  # Set score of ss_tar == ss_tar (left-right diagonal) to 1
  mtx[ss_tar, ss_tar] <- 1

  # Extract remaining site series for one-sided matrix
  # ss_rem <- setdiff(ss_all, ss_all[1:i]) %>% sort()
  # Extract remaining site series for two-sided matrix
  ss_rem <- setdiff(ss_all, ss_tar) %>% sort()

  if(length(ss_rem) < 1){
    return()
  }

  for(j in 1:length(ss_rem)){
    # Choose non-target site series from list
    ss_fuzz <- ss_rem[j]
    # ss_fuzz <- 'SBSmc2_03'
    # Find cells of the non-target site series that may neighbour the target site series
    ss_cells_fuzz  <-  filter(edat, SS_NoSpace == ss_fuzz) %>%
      pull(Edatopic)

    # Extract which target cells have overlap (share a square) with non-target cells
    sq_count <- ss_cells_tar[ss_cells_tar %in% ss_cells_fuzz] %>% length()
    # Extract which target neighbours have overlap (share a border) with non-target cells
    br_count <- ss_nbrs[ss_nbrs %in% ss_cells_fuzz] %>% length()

    # add up the total score as per rules where shared square = +0.25 and shared border = +0.1
    score = sq_count*0.1 + br_count*0.05

    # Update the relevant cell in the matrix
    mtx[ss_tar, ss_fuzz] <- score
  }
}

write.table(as.data.frame(mtx), file = "C:/Users/ccarmour.stu/OneDrive/PEM/fuzzy_S1.txt", quote = FALSE, append = FALSE)

## SCENARIO 2
## Only external neighbours are counted, i.e., the neighbouring cell is not also classified as the target site series

for(i in 1:length(ss_all)){
  # Target site series
  ss_tar <- ss_all[i]
  # ss_tar <- "SBSmc2/01"
  # Find cells of the target site series
  ss_cells_tar <- filter(edat, SS_NoSpace == ss_tar) %>%
    pull(Edatopic) %>% sort()
  # Find neighbours and remove neighbours that are other target cells, e.g., external neighbours only.
  ss_nbrs <- filter(nbrs, target %in% ss_cells_tar) %>%
    filter(., !fuzzy %in% target) %>%
    pull(fuzzy) %>% sort()

  # Set score of ss_tar == ss_tar (left-right diagonal) to 1
  mtx[ss_tar, ss_tar] <- 1

  # Extract remaining site series for one-sided matrix
  # ss_rem <- setdiff(ss_all, ss_all[1:i]) %>% sort()
  # Extract remaining site series for two-sided matrix
  ss_rem <- setdiff(ss_all, ss_tar) %>% sort()

  if(length(ss_rem) < 1){
    return()
  }

  for(j in 1:length(ss_rem)){
    # Choose non-target site series from list
    ss_fuzz <- ss_rem[j]
    # ss_fuzz <- 'SBSmc2/03'
    # Find cells of the non-target site series that may neighbour the target site series
    ss_cells_fuzz  <-  filter(edat, SS_NoSpace == ss_fuzz) %>%
      pull(Edatopic)

    # Extract which target cells have overlap (share a square) with non-target cells
    sq_count <- ss_cells_tar[ss_cells_tar %in% ss_cells_fuzz] %>% length()
    # Extract which target neighbours have overlap (share a border) with non-target cells
    br_count <- ss_nbrs[ss_nbrs %in% ss_cells_fuzz] %>% length()

    # add up the total score as per rules where shared square = +0.25 and shared border = +0.1
    score = sq_count*0.1 + br_count*0.05

    # Update the relevant cell in the matrix
    mtx[ss_tar, ss_fuzz] <- score
  }
}
write.table(as.data.frame(mtx), file = "C:/Users/ccarmour.stu/OneDrive/PEM/fuzzy_S2.txt", quote = FALSE, append = FALSE)


## SCENARIO 3
## Overlap cells are excluded entirely from boundary consideration

for(i in 1:length(ss_all)){
  # Target site series
  ss_tar <- ss_all[i]
  # ss_tar <- "SBSmc2/01"
  # Find cells of the target site series
  ss_cells_tar <- filter(edat, SS_NoSpace == ss_tar) %>%
    pull(Edatopic) %>% sort()


  # Set score of ss_tar == ss_tar (left-right diagonal) to 1
  mtx[ss_tar, ss_tar] <- 1

  # Extract remaining site series for one-sided matrix
  # ss_rem <- setdiff(ss_all, ss_all[1:i]) %>% sort()
  # Extract remaining site series for two-sided matrix
  ss_rem <- setdiff(ss_all, ss_tar) %>% sort()

  if(length(ss_rem) < 1){
    return()
  }

  for(j in 1:length(ss_rem)){
    # Choose non-target site series from list
    ss_fuzz <- ss_rem[j]
    # ss_fuzz <- 'SBSmc2/03'
    # Find cells of the non-target site series that may neighbour the target site series
    ss_cells_fuzz  <-  filter(edat, SS_NoSpace == ss_fuzz) %>%
      pull(Edatopic)

    # Find neighbours and remove neighbours that are other target cells AND remove the neighbours of target cells that overlap with the fuzzy cells
    ss_nbrs <- filter(nbrs, target %in% ss_cells_tar) %>%
      filter(., !fuzzy %in% target) %>%
      filter(., !target %in% ss_cells_tar[ss_cells_tar %in% ss_cells_fuzz]) %>%
      pull(fuzzy) %>% sort()

    # Extract which target cells have overlap (share a square) with non-target cells
    sq_count <- ss_cells_tar[ss_cells_tar %in% ss_cells_fuzz] %>% length()
    # Extract which target neighbours have overlap (share a border) with non-target cells
    br_count <- ss_nbrs[ss_nbrs %in% ss_cells_fuzz] %>% length()

    # add up the total score as per rules where shared square = +0.25 and shared border = +0.1
    score = sq_count*0.1 + br_count*0.05

    # Update the relevant cell in the matrix
    mtx[ss_tar, ss_fuzz] <- score
  }
}
write.table(as.data.frame(mtx), file = "C:/Users/ccarmour.stu/OneDrive/PEM/fuzzy_S3.txt", quote = FALSE, append = FALSE)
