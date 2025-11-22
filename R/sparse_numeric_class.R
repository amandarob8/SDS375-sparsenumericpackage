## sparse_numeric class, helpers, generics, and methods
## With roxygen2 documentation

#' Homework6: Sparse Numeric Vector Tools
#'
#' Provides an S4 class for sparse numeric vectors and basic operations
#' such as arithmetic, norms, and standardization without converting
#' to dense format.
#'
#' @keywords internal
#' @import methods
#' @importFrom graphics plot grid plot.new title
"_PACKAGE"

# sparse_numeric class

# sparse_numeric class


#' Sparse numeric vector class
#'
#' Stores sparse numeric vectors.
#'
#' @slot value Non-zero values.
#' @slot pos Positions of non-zero values.
#' @slot length Total length including zeros.
#'
#' @exportClass sparse_numeric
setClass(
  Class = "sparse_numeric",
  slots = c(
    value  = "numeric",
    pos    = "integer",
    length = "integer"
  )
)

# Validity

setValidity("sparse_numeric", function(object) {
  if (length(object@value) != length(object@pos)) {
    return("The lengths of value and pos must be equal")
  }
  if (any(object@pos < 1L | object@pos > object@length)) {
    return("The positions in pos must be between 1 and the specified length")
  }
  if (anyDuplicated(object@pos)) {
    return("No duplicate positions in pos")
  }
  TRUE
})

# Coercion (not exported)

# Convert numeric to sparse_numeric (not exported)
setAs("numeric", "sparse_numeric", function(from) {
  new("sparse_numeric",
      value = from[from != 0],
      pos = as.integer(seq_along(from)[from != 0]),
      length = length(from))
})

# Convert sparse_numeric to numeric
setAs("sparse_numeric", "numeric", function(from) {
  out = numeric(from@length)
  out[from@pos] = from@value
  out
})

# Helper functions (not exported)

check_length = function(x, y) {
  if (x@length != y@length)
    stop("arguments must have the same length", call. = FALSE)
  invisible(TRUE)
}

## Safe lookup into a sparse vector's values by position; returns 0 if missing
lookup = function(s, idx) {
  if (!length(s@pos) || !length(idx)) return(numeric(length(idx)))
  m = match(idx, s@pos, nomatch = 0L)
  output = numeric(length(idx))
  non_zero = m != 0L
  output[non_zero] = s@value[m[non_zero]]
  output
}

## Build a sparse vector from positions and values (drop zeros)
build_sparse = function(vals, idx, n) {
  keep = which(vals != 0)
  if (length(keep)) {
    new("sparse_numeric",
        value  = as.numeric(vals[keep]),
        pos    = as.integer(idx[keep]),
        length = as.integer(n))
  } else {
    new("sparse_numeric",
        value  = numeric(0),
        pos    = integer(0),
        length = as.integer(n))
  }
}

# Generics for sparse operations

#' Add two sparse numeric vectors
#'
#' Element-wise addition of two \code{sparse_numeric} vectors.
#'
#' @param x A \code{sparse_numeric} vector.
#' @param y A \code{sparse_numeric} vector of the same length.
#' @param e1 First \code{sparse_numeric} vector (for operator methods).
#' @param e2 Second \code{sparse_numeric} vector (for operator methods).
#'
#' @return A \code{sparse_numeric} vector containing the element-wise sum.
#'
#' @examples
#' x <- as(c(1, 0, 2), "sparse_numeric")
#' y <- as(c(0, 5, 0), "sparse_numeric")
#' sparse_add(x, y)
#'
#' @export
#' @exportMethod sparse_add
setGeneric("sparse_add",  function(x, y) standardGeneric("sparse_add"))

#' Subtract two sparse numeric vectors
#'
#' Element-wise subtraction of two \code{sparse_numeric} vectors.
#'
#' @inheritParams sparse_add
#'
#' @return A \code{sparse_numeric} vector containing \code{x - y}.
#'
#' @examples
#' x <- as(c(1, 0, 2), "sparse_numeric")
#' y <- as(c(0, 5, 1), "sparse_numeric")
#' sparse_sub(x, y)
#'
#' @export
#' @exportMethod sparse_sub
setGeneric("sparse_sub",  function(x, y) standardGeneric("sparse_sub"))

#' Multiply two sparse numeric vectors element-wise
#'
#' Element-wise multiplication of two \code{sparse_numeric} vectors.
#'
#' @inheritParams sparse_add
#'
#' @return A \code{sparse_numeric} vector containing the element-wise product.
#'
#' @examples
#' x <- as(c(1, 0, 2), "sparse_numeric")
#' y <- as(c(0, 5, 3), "sparse_numeric")
#' sparse_mult(x, y)
#'
#' @export
#' @exportMethod sparse_mult
setGeneric("sparse_mult", function(x, y) standardGeneric("sparse_mult"))

#' Crossproduct of two sparse numeric vectors
#'
#' Computes the dot product of two \code{sparse_numeric} vectors.
#'
#' @param x A \code{sparse_numeric} vector.
#' @param y A \code{sparse_numeric} vector of the same length.
#'
#' @return A numeric scalar equal to \code{sum(x * y)}.
#'
#' @examples
#' x <- as(c(1, 0, 2), "sparse_numeric")
#' y <- as(c(0, 5, 3), "sparse_numeric")
#' sparse_crossprod(x, y)
#'
#' @export
#' @exportMethod sparse_crossprod
setGeneric("sparse_crossprod",
           function(x, y) standardGeneric("sparse_crossprod"))

# norm() and standardize() generics

#' Euclidean norm of a sparse numeric vector
#'
#' Computes the Euclidean (L2) norm of a \code{sparse_numeric} vector:
#' the square root of the sum of squared elements.
#'
#' @param x A \code{sparse_numeric} vector.
#'
#' @return A numeric scalar giving the norm.
#'
#' @examples
#' x <- as(c(3, 0, 4), "sparse_numeric")
#' norm(x)
#'
#' @export
#' @exportMethod norm
setGeneric("norm", function(x) standardGeneric("norm"))

#' Standardize a sparse numeric vector
#'
#' Standardizes a \code{sparse_numeric} vector by subtracting the mean
#' (computed over all entries including zeros) and dividing by the sample
#' standard deviation (also over all entries).
#'
#' @param x A \code{sparse_numeric} vector.
#'
#' @return A standardized \code{sparse_numeric} vector with (approximately)
#'   mean 0 and standard deviation 1 when converted to dense form.
#'
#' @examples
#' x <- as(c(1, 0, 2, 0), "sparse_numeric")
#' z <- standardize(x)
#' as(z, "numeric")
#'
#' @export
#' @exportMethod standardize
setGeneric("standardize", function(x) standardGeneric("standardize"))


# Methods for sparse operations

#' @rdname sparse_add
setMethod("sparse_add",
          signature(x = "sparse_numeric", y = "sparse_numeric"),
          function(x, y) {
            check_length(x, y)
            idx = sort(unique(c(x@pos, y@pos)))
            vx  = lookup(x, idx)
            vy  = lookup(y, idx)
            build_sparse(vx + vy, idx, x@length)
          })

#' @rdname sparse_sub
setMethod("sparse_sub",
          signature(x = "sparse_numeric", y = "sparse_numeric"),
          function(x, y) {
            check_length(x, y)
            idx = sort(unique(c(x@pos, y@pos)))
            vx  = lookup(x, idx)
            vy  = lookup(y, idx)
            build_sparse(vx - vy, idx, x@length)
          })

#' @rdname sparse_mult
setMethod("sparse_mult",
          signature(x = "sparse_numeric", y = "sparse_numeric"),
          function(x, y) {
            check_length(x, y)
            ## Only overlapping positions can be non-zero
            idx = intersect(x@pos, y@pos)
            if (!length(idx)) {
              return(new("sparse_numeric",
                         value  = numeric(0),
                         pos    = integer(0),
                         length = x@length))
            }
            vx = lookup(x, idx)
            vy = lookup(y, idx)
            build_sparse(vx * vy, idx, x@length)
          })

#' @rdname sparse_crossprod
setMethod("sparse_crossprod",
          signature(x = "sparse_numeric", y = "sparse_numeric"),
          function(x, y) {
            check_length(x, y)
            idx = intersect(x@pos, y@pos)
            if (!length(idx)) return(0.0)
            vx = lookup(x, idx)
            vy = lookup(y, idx)
            sum(vx * vy)
          })

# Arithmetic operator methods

#' @rdname sparse_add
#' @exportMethod "+"
setMethod("+",
          signature(e1 = "sparse_numeric", e2 = "sparse_numeric"),
          function(e1, e2) sparse_add(e1, e2))

#' @rdname sparse_sub
#' @exportMethod "-"
setMethod("-",
          signature(e1 = "sparse_numeric", e2 = "sparse_numeric"),
          function(e1, e2) sparse_sub(e1, e2))

#' @rdname sparse_mult
#' @exportMethod "*"
setMethod("*",
          signature(e1 = "sparse_numeric", e2 = "sparse_numeric"),
          function(e1, e2) sparse_mult(e1, e2))

# Show and plot methods

#' Display a sparse_numeric object
#'
#' \code{show} method for \code{sparse_numeric} prints the length,
#' number of non-zero entries, and the first few non-zero positions/values.
#'
#' @param object A \code{sparse_numeric} object.
#'
#' @export
#' @exportMethod show
setMethod("show", "sparse_numeric", function(object) {
  cat("A sparse_numeric vector of length", object@length, "\n")
  if (length(object@value) == 0) {
    cat("All the elements are zero \n")
  } else {
    cat("Nonzero values at positions:\n")
    for (i in seq_along(object@value)) {
      cat("  Position", object@pos[i], ":", object@value[i], "\n")
    }
  }
})

#' Plot overlapping non-zero entries of two sparse vectors
#'
#' \code{plot} method for \code{sparse_numeric} vectors. It plots the values
#' of two sparse vectors at their overlapping non-zero positions.
#'
#' @param x A sparse_numeric vector
#' @param y A sparse_numeric vector of same length
#' @param ... Extra plot args
#'
#' @importFrom graphics points legend
#' @export
#' @exportMethod plot
setMethod("plot", c("sparse_numeric", "sparse_numeric"), function(x, y, ...) {
  if (x@length != y@length) stop("Vectors must be the same length")
  all_positions = sort(unique(c(x@pos, y@pos)))
  x_values = numeric(length(all_positions))
  y_values = numeric(length(all_positions))
  x_values[match(x@pos, all_positions)] = x@value
  y_values[match(y@pos, all_positions)] = y@value
  plot(all_positions, x_values, type="h", col="skyblue", lwd=2,
       ylim=range(c(x_values, y_values)),
       xlab="Position", ylab="Value", main="Sparse Numeric Vectors")
  points(all_positions, y_values, type="h", col="orchid3", lwd=2)
  legend("topright", legend=c("Vector x", "Vector y"), col=c("skyblue", "orchid3"), lwd=2)
})

# length() and mean() methods

#' Length of a sparse_numeric vector
#'
#' Returns the full length of the underlying vector, including zeros.
#'
#' @param x A \code{sparse_numeric} object.
#'
#' @return An integer scalar giving the vector length.
#'
#' @export
#' @exportMethod length
setMethod("length", "sparse_numeric", function(x) {
  x@length
})

#' Mean of a sparse_numeric vector
#'
#' Computes the mean of a \code{sparse_numeric} vector, treating
#' missing entries as zeros (i.e., over the full length).
#'
#' @param x A \code{sparse_numeric} object.
#' @param ... Ignored.
#'
#' @return A numeric scalar giving the mean.
#'
#' @examples
#' x <- as(c(1, 0, 2, 0), "sparse_numeric")
#' mean(x)
#'
#' @export
#' @exportMethod mean
setMethod("mean",
          signature(x = "sparse_numeric"),
          function(x, ...) {
            n <- x@length
            if (n == 0L) {
              return(NaN)
            }
            sum(x@value) / n
          })

# norm() and standardize() methods

#' @rdname norm
setMethod("norm",
          signature(x = "sparse_numeric"),
          function(x) {
            sqrt(sum(x@value^2))
          })

#' @rdname standardize
setMethod("standardize",
          signature(x = "sparse_numeric"),
          function(x) {
            n <- x@length
            if (n <= 1L) {
              stop("standard deviation is undefined for length <= 1",
                   call. = FALSE)
            }

            # Compute mean and sd using sparse info only
            S1 <- sum(x@value)
            S2 <- sum(x@value^2)
            mu <- S1 / n

            # Sum of squared deviations
            ss <- S2 - (S1^2 / n)
            if (ss < 0 && abs(ss) < 1e-14) ss <- 0

            if (ss == 0) {
              stop("standard deviation is zero; cannot standardize",
                   call. = FALSE)
            }

            var <- ss / (n - 1)
            sd  <- sqrt(var)

            # Build dense standardized vector then convert back to sparse
            dense <- numeric(n)
            if (length(x@pos)) dense[x@pos] <- x@value
            z <- (dense - mu) / sd

            as(z, "sparse_numeric")
          })
