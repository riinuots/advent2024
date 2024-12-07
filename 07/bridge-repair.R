library(tidyverse)
library(gtools)
options(scipen = 999)

# load in 07/input-test
input_orig = read_delim("07/input", delim = ": ", col_names = c("value", "numbers"))

# Part I
get_perm = function(n){
  perm = permutations(2,n,c("+", "*"),repeats=TRUE)
  return(perm)
}

calc_eq = function(nums, ops){
  res = nums[1]
  for (i in 1:length(ops)){
    if (ops[i] == "+") (res = res + nums[i+1]) else (res =  res * nums[i+1])
  }
  return(res)
}

calibration = function(numbers, value){
  n = length(numbers)-1
  perm = get_perm(n)
  for (i in 1:nrow(perm)){
    # print(paste("n perm: ", i))
    # print(numbers)
    # print(perm[i,])
    res = calc_eq(numbers, perm[i,])
    # print(res)
    if (res == value) return(TRUE)
    if (i == nrow(perm)) return(FALSE)
  }
}


input_orig |> 
  rowwise() |> 
  mutate(numbers = str_split(numbers, " ") |> map(~as.numeric(.x))) |> 
  mutate(result = calibration(numbers, value)) |> 
  filter(result) |>
  ungroup() |> 
  summarise(sum(value))

# Part II
get_perm2 = function(n){
  perm = permutations(3,n,c("+", "*", "|"),repeats=TRUE)
  return(perm)
}

calc_eq2 = function(nums, ops){
  res = nums[1]
  for (i in 1:length(ops)){
    # print(res)
    # print(ops[i])
    # print(nums[i+1])
    if (ops[i] == "+"){
      res = res + nums[i+1]
      } else if (ops[i] == "*"){
      res =  res * nums[i+1]
      } else {
      #print(paste0(res, nums[i+1]))
      res = as.numeric(paste0(res, nums[i+1]))
      }
  }
  return(res)
}

#get_perm2(2)

calibration2 = function(numbers, value){
  n = length(numbers)-1
  perm = get_perm2(n)
  for (i in 1:nrow(perm)){
    # i = 56124
    # print(paste("n perm: ", i))
    # print(numbers)
    # print(perm[i,])
    res = calc_eq2(numbers, perm[i,])
    #print(res, format="fg")
    if (res == value) return(TRUE)
    if (i == nrow(perm)) return(FALSE)
    if (res > value) next
  }
}

final = input_orig |> 
  rowwise() |> 
  mutate(numbers = str_split(numbers, " ") |> map(~as.numeric(.x))) |> 
  mutate(result = calibration2(numbers, value)) |> 
  filter(result) |>
  ungroup() |> 
  summarise(result = sum(value))

final |> pull(result)
