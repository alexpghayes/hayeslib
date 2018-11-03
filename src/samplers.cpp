#include <RcppArmadillo.h>
// [[Rcpp::depends("RcppArmadillo")]]

// all of the code in this file is from the Rcpp galleries

const double log2pi = std::log(2.0 * M_PI);

//' Sample from the multivariate normal distribution
//'
//' @param n Number of samples to generate.
//' @param mu Center of the distribution.
//' @param sigma Covariance matrix.
//'
//' @return A matrix where each row is a sample.
//' @export
//' @examples
//'
//' mu <- 1:3
//' sigma <- diag(3)
//' rmvn(10, mu, sigma)
//'
// [[Rcpp::export]]
arma::mat rmvn_arma(int n, arma::vec mu, arma::mat sigma) {
  arma::mat z = arma::randn(n, sigma.n_cols);

  arma::vec eigval;
  arma::mat eigvec;

  // use eigendecomp instead of cholesky since cholesky exploded
  // on some GP problems

  eig_sym(eigval, eigvec, sigma);
  return arma::repmat(mu, 1, n).t() + z * eigvec * diagmat(eigval);
}


//' Calculate multivariate normal densities
//'
//' @param x Matrix of multivariate normal samples
//' @param mean Center of the distribution.
//' @param sigma Covariance matrix.
//' @param log Logical indicating if log density should be used.
//'
//' @return Matrix with single column of densities.
//' @export
//'
//' @examples
//'
//' mu <- 1:3
//' sigma <- diag(3)
//' samps <- rmvn(10, mu, sigma)
//' dmvn(samps, mu + 1, sigma)
//'
// [[Rcpp::export]]
arma::vec dmvn_arma(arma::mat x,
               arma::rowvec mean,
               arma::mat sigma,
               bool log = false) {
  int n = x.n_rows;
  int xdim = x.n_cols;
  arma::vec out(n);
  arma::mat rooti = arma::trans(arma::inv(trimatu(arma::chol(sigma))));
  double rootisum = arma::sum(arma::log(rooti.diag()));
  double constants = -(static_cast<double>(xdim)/2.0) * log2pi;

  for (int i=0; i < n; i++) {
    arma::vec z = rooti * arma::trans( x.row(i) - mean) ;
    out(i)      = constants - 0.5 * arma::sum(z%z) + rootisum;
  }

  if (log == false) {
    out = exp(out);
  }
  return(out);
}
