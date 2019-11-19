library(tidyverse)
library(broom)

### Normal equations for 2 predictors ####

# data set up

set.seed(12)

obs <- 100
a <- 0
b1 <- .2
b2 <- .6

u = rnorm(obs, 0, 1)
x1 = rnorm(obs, u, 1)
x2 = rnorm(obs, u, 1)
y = rnorm(obs, a + b1*x1 + b2*x2, 1 )

d <- tibble(y, x1, x2)


# check right answer

lm(y ~ . , data = d) %>% tidy()

# get needed quantities

## create demeaned variables

d <- d %>% 
  mutate(ys = y - mean(y), 
         x1s = x1 - mean(x1),
         x2s = x2 - mean(x2))

## get product sums, etc.

Pyx1 <- sum(d$ys * d$x1s)
Pyx2 <- sum(d$ys * d$x2s)
Px1x2 <- sum(d$x1s * d$x2s)

Px1sq <- sum( d$x1s^2 )
Px2sq <- sum( d$x2s^2 )

# beta1

b1 <- (Pyx1 * Px2sq - Pyx2 * Px1x2) / (Px1sq*Px2sq - Px1x2^2)

# beta2

b2 <- (Pyx2 * Px1sq - Pyx1 * Px1x2) / (Px2sq*Px1sq - Px1x2^2)

# intercept

a <- mean(d$y) - b1*mean(d$x1) - b2*mean(d$x2)

# put together and display

results <- c(a, b1, b2)
names(results) <- c("a", "b1", "b2")
results

### Normal equations using matrices ####

# get design matrix for X
X <- as.matrix(cbind(1, x1, x2))

# transpose of X
Xt <- t(X)

b <- solve(t(X)%*%X) %*% t(X)%*%y




