#--------------------------------
# pm_first
# 1 +  x + 2x^2    1 + 4x^2    3x
#     3x +  x^2    2           0
# 3                    6x^2    1
# degree coef: pm_first_coef_0, pm_first_coef_1, pm_first_coef_2
#--------------------------------
# pm_second
#     0     0     0
#     0     0     1 + 2x + 3x^2
#     0     0     0
#--------------------------------
# pm_third
# 1 + 9x   2 + 7x
#     6x   2 + 4x^2
# 2        9 + x
#--------------------------------
# pm_fourth
# 1 + 2x    3 + x    4
# 2 + 9x^2  3 + x^2  7 + x
#--------------------------------
# pm_small
# 1 + 2x   5 + 6x
# 3 + 4x   7 + 8x
#--------------------------------
# m_first
#      [,1] [,2] [,3]
# [1,]    3    0    2
# [2,]    7    1    0
# [3,]    4    2    8
#--------------------------------
# pmcp
# 1 + x + (x - x^2) l + (-1 + 2 x^2) l^2


p <- function(...) { polynom::polynomial(c(...)) }

.clean_up_data <- function() {
  pm_first_coef_0 <<- NULL
  pm_first_coef_1 <<- NULL
  pm_first_coef_2 <<- NULL
  pm_first <<- NULL
  pm_second <<- NULL
  pm_third <<- NULL
  pm_fourth <<- NULL
  pm_small <<- NULL
  m_first <<- NULL
  pmcp <<- NULL
}

init_data <- function(env = parent.frame()) {
  pm_first_coef_0 <<- matrix(c(1, 1, 0, 0, 2, 0, 3, 0, 1), 3, 3, byrow = TRUE)
  pm_first_coef_1 <<- matrix(c(1, 0, 3, 3, 0, 0, 0, 0, 0), 3, 3, byrow = TRUE)
  pm_first_coef_2 <<- matrix(c(2, 4, 0, 1, 0, 0, 0, 6, 0), 3, 3, byrow = TRUE)
  pm_first <<- polyMatrix(cbind(pm_first_coef_0, pm_first_coef_1, pm_first_coef_2), 3, 3, 2)
  pm_second <<- polyMatrix(matrix(c(0, 0, 0, 0, 0, 0, 0, 0, 0,
                                    0, 0, 1, 0, 0, 2, 0, 0, 3,
                                    0, 0, 0, 0, 0, 0, 0, 0, 0), 3, 9, byrow = TRUE),
                           3, 3, 2)
  pm_third <<- polyMatrix(matrix(c(1, 2, 9, 7, 0, 0,
                                   0, 2, 6, 0, 0, 4,
                                   2, 9, 0, 1, 0, 0), 3, 6, byrow = TRUE), 3, 2, 2)
  pm_fourth <<- polyMatrix(matrix(c(1, 3, 4, 2, 1, 0, 0, 0, 0,
                                    2, 3, 7, 0, 0, 1, 9, 1, 0), 2, 9, byrow = TRUE), 2, 3, 2)
  pm_small <<- polyMatrix(matrix(c(1, 5, 2, 6,
                                   3, 7, 4, 8), 2, 4, byrow = TRUE),
                          2, 2, 1)

  m_first <<- matrix(c(3, 0, 2, 7, 1, 0, 4, 2, 8), 3, 3, byrow = TRUE)

  pmcp <<- polyMatrixCharClass(
    coef=polyMatrix(matrix(c(1, 0, -1, 1, 1, 0, 0, -1, 2), 1, 9, byrow = TRUE),
               1, 3, 2)
  )

  withr::defer(.clean_up_data, envir = env)
}

.clean_up_data()
