library(tidyverse)

input_orig = "1950139 0 3 837 6116 18472 228700 45"
input = str_split(input_orig, " ")[[1]] |> map(~parse_number(.x)) |> unlist()

# Part I
blink = function(x){
  x_char = as.character(x)
  len = str_length(x_char)
  if (x == 0){
    return(1)
  } else if (str_length(x_char) %% 2 == 0){
    return(
      c(parse_number(str_sub(x_char, 1, len/2)),
        parse_number(str_sub(x, len/2 + 1, len)))
    )
  } else{
    return(x*2024)
  }
}

pebbles = input
for (i in 1:25){
  print(i)
  pebbles = map(pebbles, ~blink(.x)) |> unlist()
}
length(pebbles)

# Part II
# tooooo slow
# pebbles = input
# for (i in 1:75){
#   pebbles = map(pebbles, ~change(.x)) |> unlist()
# }
# length(pebbles)
