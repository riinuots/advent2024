library(tidyverse)

input_orig =  str_split("2333133121414131402", "")[[1]] |> as.numeric()
#input_orig = str_split(read_lines("09/input"), "")[[1]] |> as.numeric()

input = tibble(input_orig) |> 
  mutate(type = c(rep(c("file", "space"), length(input_orig)/2), "file")) |> 
  group_by(type) |>
  mutate(id =  if_else(type == "space", NA, row_number() - 1)) |>
  rowwise() |> 
  mutate(expand = list(rep(id, input_orig))) |> 
  pull(expand) |> 
  unlist()

# Part I
filesystem = input
spaces = which(is.na(filesystem))
for (i in length(filesystem):1){
  if (is.na(filesystem[i])) next
  space = which(is.na(filesystem))[1]
  if (space > i) break
  filesystem[space] = filesystem[i]
  filesystem[i] = NA
}
(filesystem*(1:length(filesystem)-1)) |> sum(na.rm = TRUE) |> as.character()

