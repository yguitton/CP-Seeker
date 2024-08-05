#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector ppm_to_da(NumericVector mass, double ppm) {
  NumericVector result(mass.size());
  for (int i = 0; i < mass.size(); ++i) {
    result[i] = mass[i] * ppm * pow(10, -6);
  }
  return result;
}
