#include <Rcpp.h>
using namespace Rcpp;

// This is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp 
// function (or via the Source button on the editor toolbar). Learn
// more about Rcpp at:
//
//   http://www.rcpp.org/
//   http://adv-r.had.co.nz/Rcpp.html
//   http://gallery.rcpp.org/
//

// [[Rcpp::export]]
NumericVector timesTwo(NumericVector x) {
  return x * 2;
}


// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically 
// run after the compilation.
//

/*** R
n <- 5e4
X <- matrix(rnorm(n*10), ncol = 10)
Y <- X %*% c(1,2,3,0,0,0,.2,0,0,0) + rnorm(n)
i <- 70 # grid point
eta <- 1e-10
max_it <- 1e5
beta.start <- coef(glmnet(x = X, y = Y, alpha=0, lambda=grid[i], intercept=F)) #Beta_0
beta.start <- beta.start[-1,] %>% as.vector()

bridge(      x = X, y = Y, lambda = grid[i], beta.start =  beta.start, q = q_seq[j], eta = eta, max_it = mat_it) %>% round(10)
bridge_arm_1(x = X, y = Y, lambda = grid[i], beta_start =  beta.start, q = q_seq[j], eta = eta, max_it = mat_it) %>% round(10)

*/
