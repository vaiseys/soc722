p <- matrix(c(11.00, 
              8.00 ),
            nrow = 2)

N <- matrix( c(1, 2,
               2, 4,
               2, 1,
               8, 8,
               2, 5) ,
             nrow = 5, byrow = TRUE )

s <- N %*% p

N

s
