library(tidyverse)
library(broom)

data("anscombe")

# the "nice looking" version of the data
d <- tibble(
  y = anscombe$y1 ,
  x = anscombe$x1
)

# right answers with LM
ols <- lm(y ~ x , data = d )
tidy(ols) %>% 
  select(term, estimate, std.error)

# data as matrices
X <- as.matrix(cbind(1, d$x))
y <- d$y

# define Xt for easy reading
Xt <- t(X)

# get betas
b <- solve(Xt%*%X) %*% Xt%*%y

# predictions of the model
yhat <- X %*% b

# residuals
e <- y - yhat

# error variance
evar <-  as.numeric( t(e)%*%e / (length(y) - length(b) ) )

# variance-covariance matrix 
vcov <- evar * solve( Xt %*% X )

# standard errors of betas
se <- sqrt( diag( vcov ) )

# hat matrix 
H <- X %*% solve( Xt %*% X ) %*% Xt
