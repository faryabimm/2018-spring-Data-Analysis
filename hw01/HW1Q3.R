verify_results <- function(bool_vector) {
  result = TRUE
  for (i in seq(1, 15, 2)) {
    if (i == 1) {
      result = (xor(bool_vector[15], bool_vector[3]) && bool_vector[i] ||
                  !xor(bool_vector[15], bool_vector[3]) && !bool_vector[i]) && bool_vector[i+1] ||
        ((!bool_vector[15] && !bool_vector[3]) && bool_vector[i] ||
           (bool_vector[15] || bool_vector[3]) && !bool_vector[i]) && !bool_vector[i+1]
    } else if (i == 15) {
      result = (xor(bool_vector[1], bool_vector[13]) && bool_vector[i] ||
                  !xor(bool_vector[1], bool_vector[13]) && !bool_vector[i]) && bool_vector[i+1] ||
        ((!bool_vector[1] && !bool_vector[13]) && bool_vector[i] ||
           (bool_vector[1] || bool_vector[13]) && !bool_vector[i]) && !bool_vector[i+1]
    } else {
      result = (xor(bool_vector[i-2], bool_vector[i+2]) && bool_vector[i] ||
                  !xor(bool_vector[i-2],  bool_vector[i+2]) && !bool_vector[i]) && bool_vector[i+1] ||
        ((!bool_vector[i-2] && !bool_vector[i+2]) && bool_vector[i] ||
           (bool_vector[i-2] || bool_vector[i+2]) && !bool_vector[i]) && !bool_vector[i+1]
    }
    if (!result) break
  }
  return(result)
}

find_result <- function(bool_vector, index, liers_count) {
  bool_vector[index] = FALSE
  if (sum(bool_vector[seq(2, 16, 2)]) < 4 && index == 16) {
    return()
  }
  if (index == 16) {
    if (verify_results(bool_vector)) {
      print(as.integer(bool_vector))
      liers_count <- union(liers_count, sum(bool_vector[seq(1, 15, 2)]))
    }
  } else {
    liers_count <- find_result(bool_vector, index + 1, liers_count)
  }
  bool_vector[index] = TRUE
  if (sum(bool_vector[seq(2, 16, 2)]) < 4 && index == 16 || sum(bool_vector[seq(2, 16, 2)]) > 4) {
    return(liers_count)
  }
  if (index == 16) {
    if (verify_results(bool_vector)) {
      print(as.integer(bool_vector))
      liers_count <- union(liers_count, sum(bool_vector[seq(1, 15, 2)]))
    }
  } else {
    liers_count <- find_result(bool_vector, index + 1, liers_count)
  }
  return(liers_count)
}

print('possible configurations (liers are zeros):')
liers_count <- integer(0)
bool_vector <- logical(16)
liers_count <- find_result(bool_vector, 1, liers_count)
# print('possible count of liers:')
# print(liers_count)