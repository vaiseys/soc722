### Simple version ####

X <- tibble(
  a = 1,
  x = 1:3
) %>% as.matrix()

y <- c(2,4,6)

Xt <- t(X)

Xty <- Xt %*% y

invXtX <- solve(Xt %*% X)


### Anscombe version ####

# data
data("anscombe")

# lm version
fit <- lm(y1 ~ x1, data = anscombe)
broom::tidy(fit) %>% select(term, estimate, std.error)
broom::augment(fit)

# matrix versions of data
X <- tibble(
  a = 1,
  x = anscombe$x1
) %>% as.matrix()

y <- anscombe$y1

# define all the pieces
Xt <- t(X)
XtX <- Xt %*% X
Xty <- Xt %*% y
invXtX <- solve(XtX)

# solve for beta
b <- invXtX %*% Xty

# get residuals and variance of residuals
yhat <- X %*% b
e <- y - yhat
sigma2 <-  as.numeric( t(e) %*% e / (length(y) - length(b) ) )

# variance-covariance matrix 
vcov <- sigma2 * invXtX

# standard errors of betas
se <- sqrt( diag( vcov ) )

# hat matrix for leverage plot
H <- X %*% invXtX %*% Xt

# leverage
leverage <- diag(H)
