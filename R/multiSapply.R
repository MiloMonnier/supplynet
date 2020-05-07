# Apply multiples functions to a vector or a list

#' multiSapply function takes a vector as first argument and next one can specify
#' multiple functions that are to be applied to this vector.
#' \href{Solution}{https://www.r-bloggers.com/applying-multiple-functions-to-data-frame/}
#' proposed by Bogumił Kamiński
#'
#' Apply Multiple function to an object
#'
#' @param ... first element is the vector or list to apply the function on.
#' Folliwing arguments are the functions.
#'
#' @return a data.frame with as many columns as functions passed.
#' @export
#'
#' @examples
#' data(cars)
#' multiSapply(cars, min, mean, max)
#'
multiSapply <- function(...)
{
  arglist <- match.call(expand.dots = FALSE)$...
  var.names <- sapply(arglist, deparse)
  has.name <- (names(arglist) != "")
  var.names[has.name] <- names(arglist)[has.name]
  arglist <- lapply(arglist, eval.parent, n = 2)
  x <- arglist[[1]]
  arglist[[1]] <- NULL
  result <- sapply(arglist, function (FUN, x) sapply(x, FUN), x)
  # Personnal add: if list length was 2, the result is a vector
  if (!is.null(dim(result))) {
    colnames(result) <- var.names[-1]
  } else {
    names(result) <- var.names[-1]
  }
  return(result)
}
