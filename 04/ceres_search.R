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

# Part II
patterns = c("MMSS", "MSMS", "SSMM", "SMSM")
nn = 0
for (i in 2:(len-1)){
  for (j in 2:(len-1)){
    if(m[i, j] != "A"){
      next
    } else{
      #print(i)
      #print(j)
      # i = 3
      # j = 7
      corners = paste0(m[i-1, j-1], m[i-1, j+1], m[i+1, j-1], m[i+1, j+1])
      #print(corners)
      nn = nn + corners %in% patterns
    }
  }
}

nn
