#include <Rcpp.h>
#include "is_ordered.h"
using namespace Rcpp;

// [[Rcpp::export]]
bool is_ordered_numeric(NumericVector x) {

  // Setup
  int length_x = x.size();
  int i;
  bool ordered = true;

  // Loop over each element
  // (Nicely won't execute for vectors of length 1)
  for(i = 0; i < (length_x - 1); i++) {

    // Check if diff < 0
    // (< allows duplicates (<= would not))
    if(x[i+1] - x[i] < 0.0) {

      ordered = false;

      // Terminate early
      break;

    }

  }

  return ordered;
}
