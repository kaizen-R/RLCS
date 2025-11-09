#include <Rcpp.h>
using namespace Rcpp;

bool element_matches(List element, NumericVector ti_cond) {
  List temp_conds = element("condition_list");
  NumericVector temp_conds_0 = temp_conds("0");
  int j;
  for(j = 0; j < temp_conds_0.size(); j++) {
    if(ti_cond[temp_conds_0[j]-1] != 0) { return(false); }
  }
  NumericVector temp_conds_1 = temp_conds("1");
  for(j = 0; j < temp_conds_1.length(); j++) {
    if(ti_cond[temp_conds_1[j]-1] != 1) { return(false); }
  }
  return(true);
}

// [[Rcpp::export]]
Rcpp::NumericVector get_match_set_cpp(List pop, NumericVector ti_cond) {
  NumericVector matches_indices;
  int i;
  for(i = 0; i < pop.length(); i++) {
    if(element_matches(pop[i], ti_cond)) {
      matches_indices.push_back(i+1); // R index is +1
    }
  }
  return(matches_indices);
}


// [[Rcpp::export]]
Rcpp::List update_matched_accuracy_cpp(List pop) {
  int i;
  Rcpp::List L = pop;

  for(i = 0; i < L.length(); i++) {
    Rcpp::List elem = L[i];
    float accuracy = 0.0;
    //x$accuracy <- x$correct_count / x$match_count
    accuracy = float(elem["correct_count"]) / float(elem["match_count"]);
    elem["accuracy"] = accuracy;
    L[i] = elem;
  }
  return(L);
}

// [[Rcpp::export]]
Rcpp::List inc_param_count_cpp(List pop, String param_name) {
  int i;
  Rcpp::List L = pop;

  for(i = 0; i < L.length(); i++) {
    Rcpp::List elem = L[i];
    elem[param_name] = float(elem[param_name])+1;
    L[i] = elem;
  }
  return(L);
}
