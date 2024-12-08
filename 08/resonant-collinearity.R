library(tidyverse)

imax = 50
jmax = 50

input_orig = read.fwf("08/input", widths = rep(1, imax), comment.char = "") |> 
  as.matrix()

freqs = tibble(freq = (input_orig |> table() |> names())[-1])

antennas = freqs |> 
  rowwise() |> 
  mutate(antennas = list(as_tibble(which(input_orig == freq, arr.ind = TRUE)))) |> 
  unnest(antennas) |> 
  rename(i = row, j = col)

paired = antennas |> 
  full_join(antennas, by = "freq", relationship = "many-to-many") |> 
  filter(!(i.x == i.y & j.x == j.y)) |> # same-same
  mutate(i.dist = i.x - i.y, j.dist = j.x - j.y)

# Part I
antinodes = paired |> 
  mutate(a1.x = i.x + i.dist, a1.y = j.x + j.dist,
         a2.x = i.y - i.dist, a2.y = j.y - j.dist)

antinodes |> 
  pivot_longer(starts_with("a")) |> 
  separate(name, into = c("antenna", "coord"), sep = "\\.") |> 
  filter(value > 0 & value <= imax) |>
  pivot_wider(names_from = coord, values_from = value) |>
  drop_na() |> 
  distinct(x, y) |> 
  nrow()

# debugging
# ggplot() +
#   geom_point(aes(x, y), shape = "#", size = 6) +
#   geom_text(aes(i.x, j.x, label = freq), size = 12) +
#   coord_fixed()


# Part II

harmonics = c(-50:50)

antinodes = paired |> 
  rowwise() |> 
  mutate(a = list(tibble(x = i.x + harmonics*rep(i.dist, 101),
                    y = j.x + harmonics*rep(j.dist, 101)))) |> 
  unnest(a)

antinodes |> 
  filter(x > 0 & x <= imax) |> 
  filter(y > 0 & y <= jmax) |> 
  distinct(x, y) |> 
  nrow()
