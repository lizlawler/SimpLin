#include <RcppArmadillo.h>   
// [[Rcpp::depends(RcppArmadillo)]]
using namespace arma;

// [[Rcpp::export]]
Rcpp::List SimpLinCpp(arma::vec x, arma::vec y) {
  double n = y.size();
  mat X(n, 2);
  X.col(0) = ones(n);
  X.col(1) = x;
  vec coefs = solve(trans(X) * X, trans(X) * y);
  vec preds = X * coefs;
  vec resids = y - preds;
  double sigma_hat = as_scalar((trans(resids) * resids) / (n - 2));
  vec std_errors = sqrt(sigma_hat * diagvec(inv(trans(X) * X)));
  mat conf_int(2, 2);
  conf_int.row(0) = {coefs(0) - 1.98 * as_scalar(std_errors(0)), coefs(0) + 1.98 * as_scalar(std_errors(0))};
  conf_int.row(1) = {coefs(1) - 1.98 * as_scalar(std_errors(1)), coefs(1) + 1.98 * as_scalar(std_errors(1))};
  return Rcpp::List::create(Rcpp::Named("coefs")=coefs, 
                            Rcpp::Named("std_errors")=std_errors, 
                            Rcpp::Named("conf_int")=conf_int, 
                            Rcpp::Named("preds")=preds, 
                            Rcpp::Named("resids")=resids);
}


// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be autoarma::matically 
// run after the compilation.
//

/*** R
SimpLinCpp(c(1,5,6), c(1,2,3))
*/
