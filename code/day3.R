# part 1
library(tidyverse)

data_raw <- read_csv("inputs/day3.csv", col_names = c("bit"))

data <- data_raw %>% 
  separate(bit, into = c(as.character(0:12)), sep = "") %>% 
  select(-"0") %>% 
  mutate(across(everything(), as.numeric))

sums <- colSums(data)
names(sums) <- NULL
gamma_sums <- ifelse(sums > 500, 1, 0)
epsilon_sums <- ifelse(sums > 500, 0, 1)

gam_binary <- paste(gamma_sums, collapse = "")
ep_binary <- paste(epsilon_sums, collapse = "")

strtoi(gam_binary, 2) * strtoi(ep_binary, 2)

# part 2

most_common_bit <- function(df, nrows) {
  vals <- colSums(df)
  sums <- ifelse(vals >= nrows/2, 1, 0)
  names(sums) <- NULL
  sums
}

least_common_bit <- function(df, nrows) {
  vals <- colSums(df)
  sums <- ifelse(vals >= nrows/2, 0, 1)
  names(sums) <- NULL
  sums
}


data_trimmed <- data
i <- 1

while (nrow(data_trimmed) > 1) {
  most_common <- most_common_bit(data_trimmed, nrow(data_trimmed))    
  mc_for_row <- most_common[i]
  data_trimmed <- data_trimmed[data_trimmed[, i] == mc_for_row, ]
  
  i <- i + 1
}

binary_ox <- unlist(data_trimmed)
names(binary_ox) <- NULL
oxy_gen <- strtoi(paste(binary_ox, collapse = ""), 2)

# ------------------------------------------------------------------------------
data_trimmed2 <- data
j <- 1

while (nrow(data_trimmed2) > 1) {
  least_common <- least_common_bit(data_trimmed2, nrow(data_trimmed2))    
  lc_for_row <- least_common[j]
  data_trimmed2 <- data_trimmed2[data_trimmed2[, j] == lc_for_row, ]
  
  j <- j + 1
}
  
binary_co <- unlist(data_trimmed2)
names(binary_co) <- NULL
co_gen <- strtoi(paste(binary_co, collapse = ""), 2)

co_gen*oxy_gen









