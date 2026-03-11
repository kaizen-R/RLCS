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

bool string_matches(List element, Rcpp::StringVector t_class) {
  Rcpp::StringVector t_action = element("action");
  if(t_action(0) == t_class(0)) return(true);
  return(false);
}

// [[Rcpp::export]]
Rcpp::NumericVector get_correct_set_cpp(List match_pop, Rcpp::StringVector t_class) {
  NumericVector matches_indices;
  int i;
  for(i = 0; i < match_pop.length(); i++) {
    // Rcout << t_class(0) << "\n";
    //
    // List element = match_pop[i];
    // Rcout << t_class << "\n";
    // Rcpp::StringVector t_action = element["action"];
    // Rcout << t_action(0) << "\n";
    // std::string t_action2 = Rcpp::as<std::string>(element("action"));
    // Rcout << t_class << "\n";

    // Rcout << t_action << "\n";
    if(string_matches(match_pop[i], t_class)) {
      matches_indices.push_back(i+1); // R index is +1
    }
  }
  return(matches_indices);
}



// [[Rcpp::export]]
Rcpp::List update_matched_accuracy_cpp(List pop) {
  int i;
  Rcpp::List L = pop;
  float accuracy = 0.0;

  for(i = 0; i < L.length(); i++) {
    Rcpp::List elem = L[i];

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
  // print(L);
  return(L);
}


// [[Rcpp::export]]
float min_param_count_cpp(List pop, String param_name) {
  int i;
  Rcpp::List L = pop;
  float new_elem = 0.0;
  float best_val = FLT_MAX;
  for(i = 0; i < L.length(); i++) {
    Rcpp::List elem = L[i];
    new_elem = float(elem[param_name]);
    if(new_elem < best_val) { best_val = new_elem; }
  }
  // print(L);
  return(best_val);
}
