#include <Rcpp.h>
#include "is_ordered.h"
using namespace Rcpp;

// [[Rcpp::export]]
LogicalVector sorted_range_search(NumericVector x, double lower, double upper) {
  
  if ( lower > upper ) {
    throw std::range_error( "upper value must be greater than lower value" ) ;
  }
  
  if(!is_ordered_numeric(x)) {
    Rf_warning("Note: Index not ordered. tibbletime assumes index is in ascending order. Results may not be as desired.");
  }
  
  NumericVector::iterator iter_lower;
  NumericVector::iterator iter_upper;
  IntegerVector loc = IntegerVector::create(0, 0);
  
  NumericVector::iterator x_begin = x.begin();
  NumericVector::iterator x_end = x.end();
  
  iter_lower = std::lower_bound(x_begin, x_end, lower);
  loc[0] = std::distance(x_begin, iter_lower);
  
  iter_upper = std::upper_bound(x_begin, x_end, upper);
  loc[1] = std::distance(x_begin, iter_upper); // + 1 - 1, +1 for C++ to R, -1 for upper_bound going too far
  
  LogicalVector filter_criteria(x.size(), false);
  
  // Two cases to return NULL
  // 1) When the upper pos is below the minimum of the series and lower is below that
  // 2) When the lower pos is above the max of the series and upper is above that
  if(loc[1] == 0 | loc[0] > x.size() - 1) {
    return filter_criteria;
  }

  for(int i = loc[0]; i < loc[1]; i++) {
    filter_criteria[i] = true;
  }

  return filter_criteria;
}
