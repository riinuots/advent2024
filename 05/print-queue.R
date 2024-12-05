library(tidyverse)

input_orig = read_lines("05/input-test")
br = which(input_orig == "")
rules = tibble(rule = input_orig[1:(br-1)])

updates = tibble(update = input_orig[(br+1):length(input_orig)]) |>
  rowid_to_column("update_id") |> 
  crossing(rules) |>
  rowwise() |> 
  mutate(associated_numbers = str_extract_all(update, rule),
         associated_numbers =
           if_else(length(associated_numbers) > 1, 
                   paste(associated_numbers, collapse = "|"),
                   NA_character_)) |> 
  filter(associated_numbers != "")

# Part I
valid_updates = updates |> 
  group_by(update_id) |>
  filter(all(rule == associated_numbers)) |> 
  distinct(update_id, update)

valid_updates |> 
  # split update into numeric vector (thanks co-pilot)
  rowwise() |> 
  mutate(update = str_split(update, ",") |> map(~as.numeric(.x)),
         middle = update[ceiling(length(update)/2)]) |> 
  ungroup() |> 
  summarise(sum(middle))

# Part II
invalid_updates = updates |> 
  group_by(update_id) |>
  filter(!all(rule == associated_numbers)) |> 
  mutate(correct = rule == associated_numbers)
