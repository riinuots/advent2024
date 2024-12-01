library(tidyverse)

input_orig = read_delim("01/input", delim = "   ", col_names = c("id1", "id2"))

# Part I
bind_cols(
  select(arrange(input_orig, id1), id1),
  select(arrange(input_orig, id2), id2)
  ) |> 
  mutate(diff = abs(id2 - id1)) |>
  summarise(sum(diff))

# Part II
right = as_vector(select(input_orig, id2))

input_orig |> 
  rowwise() |> 
  mutate(score = id1*sum(id1 == right)) |> 
  ungroup() |> 
  summarise(sum(score))



