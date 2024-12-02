library(tidyverse)

input_orig = tibble(report = read_lines("02/input.txt"))

# Part I
input_orig |> 
  rowid_to_column("report_id") |> 
  # shame that separate_longer_delim() doesn't have convert = TRUE, like separate_rows() does
  separate_rows(report, sep = " ", convert = TRUE) |> 
  group_by(report_id) |> 
  mutate(level = report - lag(report)) |>  
  drop_na() |> 
  # running the checks in two parts is easier
  mutate(unsafe = level == 0 | abs(level) > 3) |> 
  filter(all(!unsafe)) |> 
  summarise(safe = all(level < 0) | all(level > 0)) |> 
  count(safe)
