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

// [[Rcpp::export]]
double da_to_ppm(double mass, double da) {
  return da * pow(10, 6) / mass;
}
