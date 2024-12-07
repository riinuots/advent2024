library(tidyverse)

imax = 130
jmax = 130

input_orig = read.fwf("06/input", widths = rep(1, imax), comment.char = "") |> 
  as.matrix()

# Part I 
loc = which(input_orig == "^", arr.ind = TRUE)
dir = "^"
move = list("^" = list(-1, 0), "v" = list(1, 0), "<" = list(0, -1), ">" = list(0, 1))
path = loc

while (loc[1] > 0 & loc[1] <= imax & loc[2] > 0 & loc[2] <= jmax) {
  step_to = loc + unlist(move[dir])
  if (input_orig[step_to] == "#") {
    dir = c(">", "v", "<", "^")[c("^", ">", "v", "<") == dir]
  } else {
    path = rbind(path, step_to)
    loc = step_to
  }
}

path |> tibble() |> distinct() |> nrow()
