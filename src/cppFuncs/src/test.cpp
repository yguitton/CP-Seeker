#include <Rcpp.h>
using namespace Rcpp;

NumericVector ppm_to_da(NumericVector mass, double ppm) {
  NumericVector result(mass.size());
  for (int i = 0; i < mass.size(); ++i) {
    result[i] = mass[i] * ppm * 1e-6;
  }
  return result;
}

// [[Rcpp::export]]
NumericVector ppm_to_da_vector(NumericVector mass, double ppm) {
  return ppm_to_da(mass, ppm);
}
