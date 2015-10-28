## Random multivariate normals with random variance covariance
## matrices, using the `mvtnorm` package:
random_mvnorm <- function(n, dim) {
  rmvnorm(n, rnorm(dim), random_vcv(dim))
}

## Random positive definite matrices:
random_vcv <- function(n) {
  decomp <- qr(matrix(ncol=n, rnorm(n^2)))
  O <- qr.Q(decomp) %*% diag(sign(diag(qr.R(decomp))))
  t(O) %*% diag(runif(n, 0, 10)) %*% O
}
