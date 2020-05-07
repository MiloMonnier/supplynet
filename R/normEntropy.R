
#' Normalized entropy of a distribution
#'
#' Shannon entropy estimate the average minimum number of bits needed to encode
#' a string of symbols, based on the frequency of the symbols. A perfectly
#' homogeneous distribution will have a normalized entropy of 1. In case of networks,
#' maximizing the entropy of the degree distribution is equivalent to smooth the
#' degree inequalities
#'
#'  @seealso Based on \code{\link[DescTools]{Entropy}}.
#' https://github.com/cran/DescTools/blob/master/R/StatsAndCIs.r#L4776
#'
#' @param x numeric vector; the statistical distribution
#' @param N numeric; expected number of elements of x. Allows to compare
#' entropies of distribution with different number of elements, if an element
#' is deleted from one of the 2, such as in a network rewiring model, entropy
#' remain comparable.
#'
#' @return a numeric between 0 an 1.
#' @export
#'
#' @examples
#' library(igraph)
#' library(DescTools)
#' ## Compare entropy of different distributions
#' x = rep(1, 5)         # Perfectly homogeneous = maximal H
#' y = seq(1, 5)         # Linear ascending
#' z = cumprod(c(1,rep(2,4))) # Scale ascending = minimal H
#' m = cbind(x,y,z)
#' # matplot(m, type="b")
#' ## Raw entropy vs normalized entropy
#' apply(m, 2, Entropy)
#' apply(m, 2, normEntropy)
#' ## Generate scale free networks with different powerlaws
#' powers = seq(1, 2, by=0.2)
#' nets = lapply(powers, function(x) barabasi.game(n=100, power=x))
#' ## High power law exponent gives low normalized entropy
#' sapply(nets, function(x) fit_power_law(degree(x))$alpha)
#' sapply(nets, function(x) normEntropy(degree(x)))
#' ## Same powerlaws but different number of vertices
#' n_vs = seq(1000, 10000, by=1000)
#' nets = lapply(n_vs, function(x) barabasi.game(n=x, power=1.3))
#' sapply(nets, function(x) fit_power_law(degree(x))$alpha)
#' sapply(nets, function(x) normEntropy(degree(x)))
#' #TODO Prove utility of increased v
#'
normEntropy = function(x, N=0)
{
  if (sum(x)==0)
    return(0)
  if (N > length(x)) { # Extend x with "trailing zeros"
    x = c(x, rep(0, N-length(x)))
  } else {
    N = length(x)
  }
  P = x / sum(x)
  H = -sum(ifelse(P>0, P * log(P, base=2), 0))
  H / log(N, base=2)
}
