#include <Rcpp.h>
using namespace Rcpp;

// bool element_matches(const List& element, const NumericVector& ti_cond) {
//   List temp_conds = element("condition_list");
//   NumericVector temp_conds_0 = temp_conds("0");
//   int j;
//   for(j = 0; j < temp_conds_0.size(); j++) {
//     if(ti_cond[temp_conds_0[j]-1] != 0) { return(false); }
//   }
//   NumericVector temp_conds_1 = temp_conds("1");
//   for(j = 0; j < temp_conds_1.length(); j++) {
//     if(ti_cond[temp_conds_1[j]-1] != 1) { return(false); }
//   }
//   return(true);
// }

// // [[Rcpp::export]]
// Rcpp::NumericVector get_match_set_cpp(const List& pop, const NumericVector& ti_cond) {
//   //Rcpp::NumericVector get_match_set_cpp(List pop, NumericVector ti_cond) {
//   NumericVector matches_indices;
//   int i;
//   for(i = 0; i < pop.length(); i++) {
//     if(element_matches(pop[i], ti_cond)) {
//       matches_indices.push_back(i+1); // R index is +1
//     }
//   }
//   return(matches_indices);
// }

// bool string_matches(List element, Rcpp::StringVector t_class) {
//   Rcpp::StringVector t_action = element("action");
//   if(t_action(0) == t_class(0)) return(true);
//   return(false);
// }
//
// // [[Rcpp::export]]
// Rcpp::NumericVector get_correct_set_cpp2(const Rcpp::StringVector& match_pop_actions, const Rcpp::String& t_class) {
//   NumericVector matches_indices;
//
//   for(int i = 0; i < match_pop_actions.length(); i++) {
//     if(match_pop_actions[i] == t_class) {
//       matches_indices.push_back(i+1); // R index is + 1
//     }
//   }
//   return(matches_indices);
// }
//
// // [[Rcpp::export]]
// int update_accuracy_cpp2(List& pop) {
//   int i;
//   for(i = 0; i < pop.length(); i++) {
//     Rcpp::List elem = pop[i];
//     elem["accuracy"] = float(elem["correct_count"]) / float(elem["match_count"]);
//     pop[i] = elem;
//   }
//   return(0);
// }
//
// // // [[Rcpp::export]]
// // Rcpp::List update_matched_accuracy_cpp(const List& pop) {
// //   int i;
// //   Rcpp::List L = pop;
// //   float accuracy = 0.0;
// //
// //   for(i = 0; i < L.length(); i++) {
// //     Rcpp::List elem = L[i];
// //
// //     //x$accuracy <- x$correct_count / x$match_count
// //     accuracy = float(elem["correct_count"]) / float(elem["match_count"]);
// //     elem["accuracy"] = accuracy;
// //     L[i] = elem;
// //   }
// //   return(L);
// // }
//
// // [[Rcpp::export]]
// int inc_param_count_cpp2(List& pop, const String& param_name) {
//   // for(int i = 0; i < pop.length(); i++) {
//   for(auto sub_list : pop) {
//     // Rcpp::List elem = pop[i];
//     Rcpp::List elem = sub_list;
//     elem[param_name] = int(elem[param_name])+1;
//     sub_list = elem;
//   }
//
//   return(0);
// }
//
// // [[Rcpp::export]]
// int inc_match_and_correct_count_cpp2(List& match_pop, const NumericVector correct_set) {
//   for(int i = 0; i < match_pop.length(); i++) {
//     Rcpp::List elem = match_pop[i];
//     elem["match_count"] = int(elem["match_count"])+1;
//
//     if (std::count(correct_set.begin(), correct_set.end(), i+1) > 0) {
//       elem["correct_count"] = int(elem["correct_count"])+1;
//     }
//       match_pop[i] = elem;
//   }
//
//   return(0);
// }
//
//
// // [[Rcpp::export]]
// float min_param_count_cpp(const List& pop, const String& param_name) {
//   float new_elem = 0.0;
//   float best_val = FLT_MAX;
//
//   // for(int i = 0; i < pop.length(); i++) {
//   for(auto sub_list : pop) {
//     // Rcpp::List elem = pop[i];
//     Rcpp::List elem = sub_list;
//     new_elem = float(elem[param_name]);
//     if(new_elem < best_val) { best_val = new_elem; }
//   }
//   // print(L);
//   return(best_val);
// }
//
// // [[Rcpp::export]]
// float mean_correct_count_cpp(const List& pop) {
//   int i;
//   int t_sum = 0;
//   int t_length = pop.length();
//
//   float mean_val = 0.0;
//
//   for(i = 0; i < t_length; i++) {
//     Rcpp::List elem = pop[i];
//     t_sum += int(elem["correct_count"]);
//   }
//   mean_val = t_sum / t_length;
//   return(mean_val);
// }

// [[Rcpp::export]]
IntegerVector which_cpp(const LogicalVector& x) {
  std::vector<int> out;
  for(int i = 0; i < x.size(); ++i) {
    if(x[i]) {
      out.push_back(i + 1); // 1-based indexing like R
    }
  }
  return wrap(out);
}

// // [[Rcpp::export]]
// Rcpp::NumericVector which_valid_rules_cpp(const Rcpp::NumericVector& numerosities, const Rcpp::NumericVector& lengths_fixed_bits) {
//   NumericVector matches_indices;
//
//   for(int i = 0; i < numerosities.length(); i++) {
//     if((numerosities(i) > 0) && (lengths_fixed_bits(i) > 0)) {
//       matches_indices.push_back(i+1); // R index is + 1
//     }
//   }
//   return(matches_indices);
// }
