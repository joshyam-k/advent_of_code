# part 1
data_raw <- read_csv("inputs/day2.csv", col_names = c("input"))

data <- data_raw %>% 
  separate(input, into = c("direction", "n"), sep = " ") %>% 
  mutate(n = as.numeric(n))

data_rev <- data %>% 
  mutate(
    n = ifelse(direction == "up", n*(-1), n),
    direction = ifelse(direction == "up", "down", direction)
    )

data_rev %>% 
  group_by(direction) %>% 
  summarise(total = sum(n)) %>% 
  ungroup() %>% 
  pull(total) %>% 
  prod()

# part 2

horiz <- 0
depth <- 0
aim <- 0

for (i in 1:nrow(data)) {
  if (data[i, ]$direction == "down") {
    aim <- aim + data[i, ]$n
  } else if (data[i, ]$direction == "up") {
    aim <- aim - data[i, ]$n
  } else {
    horiz <- horiz + data[i, ]$n
    depth <- depth + aim*data[i, ]$n
  }
}

horiz*depth










