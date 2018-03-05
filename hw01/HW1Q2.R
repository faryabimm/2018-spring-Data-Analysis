find_valid_sequences <- function(n) {
  index_max <- factorial(n)
  i <- 0
  while (i < index_max) {
    permutation <- find_permutation_by_index(n, i)
    if (permutation[1] >= sum(permutation[2:3])) {
      i <- i + factorial(n - 3)
    } else if (sum(permutation[2:3]) ^ 2 != permutation[1] * sum(permutation[3:5])) {
      i <- i + factorial(n - 5)
    } else if (sum(permutation[6:7]) ^ 2 != permutation[6] * sum(permutation[7:9])) {
      i <- i + factorial(n - 9)
    } else if (sum(permutation[7:9]) ^ 2 != sum(permutation[6:7]) * sum(permutation[c(5, 9:10)])) {
      i <- i + factorial(n - 10)
    } else if (permutation[11] >= permutation[13] ||
               sum(permutation[11:12]) * sum(permutation[c(10, 13)]) != sum(permutation[12:13]) ^ 2) {
      i <- i + factorial(n - 13)
    } else {
      i <- i + 1
      print(permutation)
    }
  }
}

find_permutation_by_index <- function(n, index) {
  result <- numeric(n)
  sequence <- 1:n
  first_sequence <- sequence
  changed <- logical(n)

  for (i in 1:n) {
    element_index <- index %/% factorial(n - i) + 1
    element <- sequence[element_index]
    index <- index %% factorial(n - i)
    changed[element] <- TRUE
    sequence <- first_sequence[changed == FALSE]
    result[i] <- element
  }
  return(result)
}


find_valid_sequences(13)

