library(tidyverse)

# part 1
data <- as.integer(readLines("inputs/day1.txt"))
sum(diff(data) > 0)

# part 2
rolling_sums <- c()
for (i in 1:(length(data) - 2)) {
  rolling_sums[i] <- data[i] + data[i + 1] + data[i + 2]
}

sum(diff(rolling_sums) > 0)
