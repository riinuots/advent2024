library(tidyverse)

# Load data
input_orig = read_lines("03/input")

# Part I 
# write regular expression to extract mul(number, number)
mul_re = "mul\\((\\d+),(\\d+)\\)" # thanks co-pilot
res = str_extract_all(input_orig, mul_re)

tibble(numbers = unlist(res)) |> 
  extract(numbers, c("a", "b"), "(\\d+),(\\d+)", convert = TRUE) |> 
  summarise(sum(a*b))

# Part II
mul_re2 = "do\\(\\)|don't\\(\\)|mul\\((\\d+),(\\d+)\\)"
res2 = str_extract_all(input_orig, mul_re2)

tibble(instructions = unlist(res2)) |> 
  mutate(do = str_extract(instructions, "do\\(\\)|don't\\(\\)")) |> 
  fill(do, .direction = "down") |> 
  filter(do != "don't()" | is.na(do)) |> 
  filter(str_starts(instructions, "mul")) |>
  extract(instructions, c("a", "b"), "(\\d+),(\\d+)", convert = TRUE) |> 
  summarise(sum(a*b))