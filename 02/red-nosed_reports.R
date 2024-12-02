library(tidyverse)

input_orig = tibble(report = read_lines("02/input.txt"))

input = input_orig |> 
  rowid_to_column("report_id") |> 
  # newer separate_longer_delim() doesn't have convert = TRUE,
  # so using separate_rows() despite it being 'superseded'
  separate_rows(report, sep = " ", convert = TRUE) 

# Part I
input |> 
  group_by(report_id) |> 
  mutate(level = report - lag(report)) |>  
  drop_na() |> 
  # running the checks in two parts is easier
  mutate(unsafe = level == 0 | abs(level) > 3) |> 
  filter(all(!unsafe)) |> 
  summarise(safe = all(level < 0) | all(level > 0)) |> 
  count(safe)

# Part II

is_safe = function(v){
  d = diff(v)
  all(abs(d) > 0 & abs(d) < 4) & (all(d < 0) | all(d > 0))
}

any_safe = function(v){
  if(is_safe(v)){
    return(TRUE)
  }else{
    for(i in 1:length(v)){
      if(is_safe(v[-i])){
        return(TRUE)
      }
    }
    return(FALSE)  
  }
}



input |> 
  group_by(report_id) |> 
  summarise(report = list(report)) |> 
  rowwise() |> 
  mutate(safe = any_safe(report)) |> 
  count(safe)
