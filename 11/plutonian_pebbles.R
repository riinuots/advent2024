library(tidyverse)
library(memoise)

input_orig = "1950139 0 3 837 6116 18472 228700 45"
#input_orig = "125 17"
input = str_split(input_orig, " ")[[1]] |> map(~parse_number(.x)) |> unlist()

# Part I
blink = function(x){
  len = str_length(x)
  if (x == 0){
    return(list(1))
  } else if (str_length(x) %% 2 == 0){
    x_char = as.character(x)
    return(
      list(parse_number(str_sub(x_char, 1, len/2)),
           parse_number(str_sub(x_char, len/2 + 1, len)))
    )
  } else{
    return(list(x*2024))
  }
}

blink = memoise(blink)
tictoc::tic()
pebbles = input
for (i in 1:25){
  #print(i)
  pebbles = map(pebbles, ~blink(.x)) |> unlist()
}
length(pebbles)
tictoc::toc()

# Part II
# Part I solution is too slow and memory hungry for 75, need to smarter
input_tibble = tibble(input) |> 
  count(input, name = "total")

tictoc::tic()
for (i in 1:75){
  input_tibble = input_tibble |> 
    mutate(pebbles = map(input, ~blink(.x))) |> 
    unnest(pebbles) |> 
    group_by(pebbles) |>
    summarise(total = sum(total)) |> 
    select(input = pebbles, total)
}

input_tibble$total |> sum() |> as.character()
tictoc::toc()