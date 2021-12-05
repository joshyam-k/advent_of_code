library(tidyverse)

# reading data

vals <- c(42,44,71,26,70,92,77,45,6,18,79,54,31,34,64,32,16,55,81,11,90,10,21,87,0,84,8,23,1,12,60,20,57,68,61,82,49,59,22,2,63,33,50,39,28,30,88,41,69,72,98,73,7,65,53,35,96,67,36,4,51,75,24,86,97,85,66,29,74,40,93,58,9,62,95,91,80,99,14,19,43,37,27,56,94,25,83,48,17,38,78,15,52,76,5,13,46,89,47,3)
boards_lines <- readLines("inputs/day4.txt")

boards_lines <- boards_lines[!(boards_lines %in% c(""))]

read_matrix <- function(obj) {
  
  all_vals <- c()
  for (i in 1:5) {
    line_vals <- str_split(obj[i], "\\s+")[[1]]
    line_vals <- line_vals[!(line_vals %in% c(""))]
    all_vals <- c(all_vals, line_vals)
  }
  
  matrix(all_vals, nrow = 5, byrow = T)
}

starting_ids <- seq(1,501, by = 5)

boards <- 
  purrr::map(
    1:20, ~read_matrix(boards_lines[(starting_ids[.x]):(starting_ids[.x + 1]- 1)])
    )

# part 1

replace_val <- function(mat, val) {
  if (val %in% mat) {
    mat[mat == val] <- "*"
  } 
  mat
}

check_bingo <- function(mat) {
  for (i in 1:5){
    filled <- sum(mat[i, ] == "*")
    if  (filled == 5){
      return(mat)
    }
  }
  for (j in 1:5){
    filled <- sum(mat[ ,i] == "*")
    if  (filled == 5){
      return(mat)
    }
  }
  if (sum(diag(mat) == "*")) {
    return(mat)
  }
}



