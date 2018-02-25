test_that("%xax% works", {

  a <- matrix(rnorm(9), nc = 3, nr = 3)
  x <- c(1,2,'a')
  expect_error(a %xax% x)

  a <- matrix(rnorm(9), nc = 3, nr = 3)
  a[1] <- 'a'
  x <- c(1,2,3)
  expect_error(a %xax% x)

  a <- matrix(rnorm(9), nc = 3, nr = 3)
  x <- c(1,NA,3)
  expect_error(a %xax% x)

  a <- matrix(rnorm(9), nc = 3, nr = 3)
  a[1] <- NA
  x <- c(1,2,3)
  expect_error(a %xax% x)

  a <- matrix(rnorm(12), nc = 3, nr = 4)
  x <- c(1,2,3)
  expect_error(a %xax% x)

  a <- matrix(rnorm(12), nc = 4, nr = 3)
  x <- c(1,2,3,4)
  expect_error(a %xax% x)

  a <- matrix(c(3,8), nc = 1, nr = 1)
  x <- c(1)
  expect_equal(a %xax% x, matrix(1/3))
})

test_that("ggplot_wrapper works", {
  ggplot_wrapper(food_coded, food_coded$GPA, food_coded$weight, as.factor(food_coded$Gender))
  expect_identical(TRUE, TRUE)
})

test_that("mle works", {
  x <- c(1,2,3)
  oout = optimize(function(alpha) f = sum(dgamma(x, shape = alpha, log = TRUE)), maximum = TRUE, pmax(mean(x) / 1e3, mean(x) + c(-1, 1) * 3 * sd(x)))
  max <- oout$maximum
  expect_equal(mle(x),max)
})

test_that("myapply works", {
  a <- matrix(1:9, nrow = 3, ncol = 3)
  app1 <- apply(a, 1, mean)
  app2 <- apply(a, 2, mean)
  expect_equal(myapply(a, 1, mean), app1)
  expect_equal(myapply(a, 2, mean), app2)

  expect_error(myapply(a, 3, mean))

  a <- array(1:9, c(3,3,3))
  expect_error(myapply(a, 2, mean))
})

test_that("opt works", {
  x <- c(1,2)
  logl <- function(alpha, x) {sum(dgamma(x, shape = alpha, log = TRUE))}
  interval <- mean(x) + c(-1, 1) * 3 * sd(x)
  expect_lt(opt(x, logl, interval), 1.88611)
  expect_gt(opt(x, logl, interval), 1.88609)
})

test_that("slice works", {
  x <- c (1,2,2,3)
  y <- c (2,2,2,2)
  df <- data.frame(x,y)
  filt <- dplyr::filter(df, x == 2)
  filt2 <- dplyr::filter(df, y == 2)
  expect_equal(slice(df, df$x, 2), filt)
  expect_equal(slice(df, df$y, 2), filt2)
})

test_that("standardize works", {
  a <- c(1,2,3)
  expect_error(standardize(a))

  a <- c(1,2,'a')
  expect_error(standardize(a))

  a <- c(1,2,NaN)
  expect_error(standardize(a))

  a <- array(1:9, dim=c(3,3))
  b <- array(c(-1,0,1,-1,0,1,-1,0,1), dim=c(3,3))
  expect_equal(standardize(a), b)
})

test_that("summFunc works", {
  expect_error(summFunc(c(1,2,'a')))
  expect_error(summFunc(c(1,2,NaN)))
  expect_error(summFunc(c()))
  x <- c(-3, 0, 3)
  mean <- sum(x) / length(x)
  var = sum((x - mean)^2)/length(x)
  sd = sqrt(var)
  expect_equal(summFunc(x), list(mean, var, sd))
})

test_that("weightedSumm works", {
  x <- c()
  p <- c(.1,.2,.7)
  expect_error(weightedSumm(x,p))

  x <- c(1,2,3)
  p <- c()
  expect_error(weightedSumm(x,p))

  x <- c(1,'a',3)
  p <- c(.1,.2,.7)
  expect_error(weightedSumm(x,p))

  x <- c(1,2,3)
  p <- c(.1,.2,.6,.1)
  expect_error(weightedSumm(x,p))

  x <- c(1,2,3,4)
  p <- c(.1,.2,.7)
  expect_error(weightedSumm(x,p))

  x <- c(1,2,3)
  p <- c(.1,.2,'b')
  expect_error(weightedSumm(x,p))

  x <- c(1,2,3)
  p <- c(.1,NaN,.7)
  expect_error(weightedSumm(x,p))

  x <- c(NaN,2,3)
  p <- c(.1,NaN,.7)
  expect_error(weightedSumm(x,p))

  x <- c(1,'a',3)
  p <- c(.1,.2,.9)
  expect_error(weightedSumm(x,p))

  x <- c(1,2,3)
  p <- c(.1,.2,.7)
  mean = sum(p * x)
  var = sum(((x - mean) ^ 2) * p)
  sd = sqrt(var)
  expect_equal(weightedSumm(x,p),list(mean, var, sd))
})

test_that("xax works", {
  a <- matrix(rnorm(9), nc = 3, nr = 3)
  x <- c(1,2,'a')
  expect_error(xax(a,x))

  a <- matrix(rnorm(9), nc = 3, nr = 3)
  a[1] <- 'a'
  x <- c(1,2,3)
  expect_error(xax(a,x))

  a <- matrix(rnorm(9), nc = 3, nr = 3)
  x <- c(1,NA,3)
  expect_error(xax(a,x))

  a <- matrix(rnorm(9), nc = 3, nr = 3)
  a[1] <- NA
  x <- c(1,2,3)
  expect_error(xax(a,x))

  a <- matrix(rnorm(12), nc = 3, nr = 4)
  x <- c(1,2,3)
  expect_error(xax(a,x))

  a <- matrix(rnorm(12), nc = 4, nr = 3)
  x <- c(1,2,3,4)
  expect_error(xax(a,x))

  a <- matrix(c(3,8), nc = 1, nr = 1)
  x <- c(1)
  expect_equal(xax(a,x), matrix(1/3))
})
