magic_square_odd <- function(dimention) {
  
  result <- matrix(0, nrow = dimention, ncol = dimention)
  col_index <- dimention %/% 2 + 1
  row_index <- 1
  
  max_i <- dimention ^ 2
  
  for (i in 1:max_i) {
    result[row_index, col_index] <- i
    next_row <- if (row_index > 1) row_index - 1 else dimention
    next_col <- if (col_index < dimention) col_index + 1 else 1
    
    if (result[next_row, next_col] != 0) {
      next_row <- if (row_index < dimention) row_index + 1 else 1
      next_col <- col_index
    }
    row_index <- next_row
    col_index <- next_col
  }
  
  return(result)
}
magic_square_even_four <- function(dimention) {
  result <- matrix(0, nrow = dimention, ncol = dimention)
  max_n = dimention ^ 2
  
  for (i in 1:dimention) {
    for (j in 1:dimention) {
      if ((i <= dimention %/% 4 || i > (dimention * 3) %/% 4) && (j <= dimention %/% 4 || j > (dimention * 3) %/% 4) ||
          (i > dimention %/% 4 && i <= (dimention * 3) %/% 4) && (j > dimention %/% 4 && j <= (dimention * 3) %/% 4)) {
        result[i, j] <- (i - 1) * dimention + j
      } else {
        result[i, j] <- max_n - ((i - 1) * dimention + j) + 1
      }
    }
  }
  return(result)
}
magic_square_even_two <- function(dimention) {
  result <- matrix(0, nrow = dimention, ncol = dimention)
  half_dimention <- dimention / 2
  quarter_index <- half_dimention ^ 2
  quarter_mat <- magic_square_odd(half_dimention)
  
  result[1:half_dimention, 1:half_dimention] <- quarter_mat + quarter_index * 0
  result[1:half_dimention, (half_dimention+1):dimention] <- quarter_mat + quarter_index * 2
  result[(half_dimention+1):dimention, 1:half_dimention] <- quarter_mat + quarter_index * 3
  result[(half_dimention+1):dimention, (half_dimention+1):dimention] <- quarter_mat + quarter_index * 1
  
  lhs_block_width <- (half_dimention - 1) / 2
  rhs_line_width <- lhs_block_width - 1
  rhs_top_center_index <- lhs_block_width + 1
  rhs_bot_center_index <- rhs_top_center_index + half_dimention
  
  temp <- result[1:lhs_block_width, 1:lhs_block_width]
  result[1:lhs_block_width, 1:lhs_block_width] <- result[(half_dimention+1):(half_dimention+lhs_block_width), 1:lhs_block_width]
  result[(half_dimention+1):(half_dimention+lhs_block_width), 1:lhs_block_width] <- temp
  
  temp <- result[(lhs_block_width+2):half_dimention, 1:lhs_block_width]
  result[(lhs_block_width+2):half_dimention, 1:lhs_block_width] <- result[(dimention-lhs_block_width+1):dimention, 1:lhs_block_width]
  result[(dimention-lhs_block_width+1):dimention, 1:lhs_block_width] <- temp
  
  temp <- result[(1+lhs_block_width),2:(lhs_block_width+1)]
  result[(1+lhs_block_width),2:(lhs_block_width+1)] <- result[(1+lhs_block_width + half_dimention),2:(lhs_block_width+1)]
  result[(1+lhs_block_width + half_dimention),2:(lhs_block_width+1)] <- temp
  
  if (rhs_line_width > 0) {
    temp <- result[1:half_dimention, (dimention-rhs_line_width+1):dimention]
    result[1:half_dimention, (dimention-rhs_line_width+1):dimention] <- result[(half_dimention+1):dimention, (dimention-rhs_line_width+1):dimention]
    result[(half_dimention+1):dimention, (dimention-rhs_line_width+1):dimention] <- temp
  }
  
  return(result)
}
magic_square <- function(dimention) {
  if (dimention %% 2 == 1) {
    return(magic_square_odd(dimention))
  } else {
    if (dimention %% 4 == 0) {
      return(magic_square_even_four(dimention))
    } else {
      return(magic_square_even_two(dimention))
    }
  }
}

magic_square(4)
magic_square(5)
magic_square(6)