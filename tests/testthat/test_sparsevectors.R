library(testthat)


## -----------------------------
## Class and validity
## -----------------------------

test_that("validity method exists", {
  validity <- getValidity(getClassDef("sparse_numeric"))
  expect_false(is.null(validity))
})

test_that("valid object passes validity", {
  x <- new("sparse_numeric",
           value = c(1, 2, 3),
           pos = c(1L, 3L, 5L),
           length = 5L)
  expect_true(validObject(x))
})

test_that("invalid object fails validity", {
  expect_error({
    x <- new("sparse_numeric",
             value = c(1, 2),
             pos = c(1L, 3L, 5L),
             length = 5L)
    validObject(x)
  })
})


## -----------------------------
## Coercion
## -----------------------------

test_that("coercion numeric -> sparse_numeric", {
  x <- as(c(0, 4, 0, 5), "sparse_numeric")
  expect_s4_class(x, "sparse_numeric")
  expect_equal(x@value, c(4, 5))
  expect_equal(x@pos, c(2L, 4L))
  expect_equal(x@length, 4L)
})

test_that("coercion sparse_numeric -> numeric", {
  x <- new("sparse_numeric",
           value = c(3, 7),
           pos = c(2L, 5L),
           length = 5L)
  expect_equal(as(x, "numeric"), c(0, 3, 0, 0, 7))
})

## -----------------------------
## Methods exist
## -----------------------------

test_that("show exists", {
  expect_no_error(getMethod("show", "sparse_numeric"))
})

test_that("plot exists", {
  expect_no_error(getMethod("plot", c("sparse_numeric", "sparse_numeric")))
})

test_that("operators exist", {
  expect_no_error(getMethod("+", c("sparse_numeric", "sparse_numeric")))
  expect_no_error(getMethod("-", c("sparse_numeric", "sparse_numeric")))
  expect_no_error(getMethod("*", c("sparse_numeric", "sparse_numeric")))
})

## -----------------------------
## Generics exist
## -----------------------------

test_that("generics exist", {
  expect_true(isGeneric("sparse_add"))
  expect_true(isGeneric("sparse_sub"))
  expect_true(isGeneric("sparse_mult"))
  expect_true(isGeneric("sparse_crossprod"))
  expect_true(isGeneric("norm"))
  expect_true(isGeneric("standardize"))
})

## -----------------------------
## sparse_add
## -----------------------------

test_that("sparse_add returns sparse_numeric", {
  x <- as(c(0, 2, 0, 1), "sparse_numeric")
  y <- as(c(1, 0, 3, 1), "sparse_numeric")
  expect_s4_class(sparse_add(x, y), "sparse_numeric")
})

test_that("sparse_add correct result", {
  x <- as(c(0, 2, 0, 1), "sparse_numeric")
  y <- as(c(1, 0, 3, 1), "sparse_numeric")
  expect_equal(
    as(sparse_add(x, y), "numeric"),
    c(1, 2, 3, 2)
  )
})

test_that("sparse_add length mismatch error", {
  x <- as(rep(0, 5), "sparse_numeric")
  y <- as(rep(0, 4), "sparse_numeric")
  expect_error(sparse_add(x, y))
})

## -----------------------------
## sparse_sub
## -----------------------------

test_that("sparse_sub works", {
  x <- as(c(3, 0, 1), "sparse_numeric")
  y <- as(c(1, 0, 5), "sparse_numeric")
  expect_equal(
    as(sparse_sub(x, y), "numeric"),
    c(2, 0, -4)
  )
})

## -----------------------------
## sparse_mult
## -----------------------------

test_that("sparse_mult only overlaps", {
  x <- as(c(1, 0, 2, 0), "sparse_numeric")
  y <- as(c(0, 5, 2, 0), "sparse_numeric")
  expect_equal(
    as(sparse_mult(x, y), "numeric"),
    c(0, 0, 4, 0)
  )
})

test_that("sparse_mult zero overlap returns empty", {
  x <- as(c(1, 0, 0), "sparse_numeric")
  y <- as(c(0, 2, 3), "sparse_numeric")
  z <- sparse_mult(x, y)
  expect_equal(z@value, numeric(0))
  expect_equal(z@pos, integer(0))
})

## -----------------------------
## sparse_crossprod
## -----------------------------

test_that("sparse_crossprod works", {
  x <- as(c(1, 0, 2), "sparse_numeric")
  y <- as(c(0, 5, 3), "sparse_numeric")
  expect_equal(sparse_crossprod(x, y), 6)
})

test_that("sparse_crossprod no overlap gives zero", {
  x <- as(c(1, 0), "sparse_numeric")
  y <- as(c(0, 2), "sparse_numeric")
  expect_equal(sparse_crossprod(x, y), 0)
})

## -----------------------------
## norm
## -----------------------------

test_that("norm returns correct value", {
  x <- as(c(3, 0, 4), "sparse_numeric")
  expect_equal(norm(x), 5)
})

## -----------------------------
## mean
## -----------------------------

test_that("mean works", {
  x <- as(c(2, 0, 4, 0), "sparse_numeric")
  expect_equal(mean(x), 6 / 4)
})

## -----------------------------
## length
## -----------------------------

test_that("length returns full length", {
  x <- as(c(1, 0, 3, 0, 0), "sparse_numeric")
  expect_equal(length(x), 5)
})

## -----------------------------
## standardize
## -----------------------------

test_that("standardize returns sparse_numeric", {
  x <- as(c(1, 0, 2, 0), "sparse_numeric")
  expect_s4_class(standardize(x), "sparse_numeric")
})

test_that("standardize produces mean near zero and sd near one", {
  x <- as(c(1, 0, 2, 0), "sparse_numeric")
  z <- standardize(x)
  dense <- as(z, "numeric")
  expect_equal(mean(dense), 0, tolerance = 1e-6)
  expect_equal(sd(dense), 1, tolerance = 1e-6)
})

test_that("standardize constant vector errors", {
  x <- as(c(2, 2, 2), "sparse_numeric")
  expect_error(standardize(x))
})

test_that("standardize length <= 1 errors", {
  x <- as(c(5), "sparse_numeric")
  expect_error(standardize(x))
})

## -----------------------------
## helpers: lookup
## -----------------------------
test_that("lookup returns zeros for missing indices", {
  x <- as(c(0, 3, 0, 5), "sparse_numeric")
  expect_equal(lookup(x, c(1, 3, 4)), c(0, 0, 5))
})

test_that("lookup handles empty sparse vector", {
  x <- as(c(0, 0, 0), "sparse_numeric")
  expect_equal(lookup(x, 1:3), c(0, 0, 0))
})

## -----------------------------
## helpers: build_sparse
## -----------------------------
test_that("build_sparse drops zeros correctly", {
  x <- build_sparse(c(0, 2, 0, 4), 1:4, 4)
  expect_equal(as(x, "numeric"), c(0, 2, 0, 4))
})

test_that("build_sparse returns empty vector if all zeros", {
  x <- build_sparse(c(0, 0), 1:2, 2)
  expect_equal(length(x@value), 0)
})

## -----------------------------
## helpers: check_length
## -----------------------------
test_that("check_length passes for equal lengths", {
  x <- as(c(1, 0), "sparse_numeric")
  y <- as(c(0, 1), "sparse_numeric")
  expect_identical(check_length(x, y), invisible(TRUE))
})

test_that("check_length errors for unequal lengths", {
  x <- as(c(1, 0, 2), "sparse_numeric")
  y <- as(c(0, 1), "sparse_numeric")
  expect_error(check_length(x, y))
})

## -----------------------------
## plot method (run for coverage)
## -----------------------------
test_that("plot does not error", {
  x <- as(c(1,0,2), "sparse_numeric")
  y <- as(c(0,3,2), "sparse_numeric")
  expect_no_error(plot(x, y))
})

## -----------------------------
## show method edge case
## -----------------------------
test_that("show works for all-zero vector", {
  x <- as(c(0, 0, 0), "sparse_numeric")
  expect_no_error(show(x))
})
