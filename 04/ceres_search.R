library(tidyverse)

len = readLines("04/input", 1) |> str_count("")

m = read.fwf("04/input", widths = rep(1, len)) |> 
  as.matrix()

# Part I
regexp = "(?=XMAS|SAMX)" # this includes overlapping instances
n = m |>
  diag() |>
  paste(collapse = "") |> 
  str_count("XMAS|SAMX")

#mt = t(m)
mt = m[, len:1]

n = n + mt |> 
  diag() |>
  paste(collapse = "") |> 
  str_count("XMAS|SAMX")

m_flip = m[, len:1]

for(i in 1:(len-3)){
  #print(m[, -c(1:i)])
  # upper right diagonals
  n = n + m[, -c(1:i)] |>
    diag() |>
    paste(collapse = "") |>
    str_count(regexp)
  # lower left diagonals
  n = n + m[-c(1:i),] |>
    diag() |>
    paste(collapse = "") |>
    str_count(regexp)
  
  # lower right or left, who knows what's what after t()
  n = n + mt[, -c(1:i)] |>
    diag() |>
    paste(collapse = "") |>
    str_count(regexp)
  # lower left or right
  n = n + mt[-c(1:i),] |>
    diag() |>
    paste(collapse = "") |>
    str_count(regexp)
}


# rows
n = n + m |> 
  apply(1, paste, collapse = "") |> 
  str_count(regexp) |> 
  sum()

# columns
n = n + m |> 
  apply(2, paste, collapse = "") |> 
  str_count(regexp) |> 
  sum()


n
# 2572 too low