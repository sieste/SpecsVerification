#include <RcppArmadillo.h>
using namespace Rcpp;

// [[Rcpp::depends(RcppArmadillo)]]

//' Calculate AUC difference `AUC(fcst,obs) - AUC(fcst_ref, obs)` of two forecasts for the same observations, and the sampling standard deviation of the AUC difference (Internal C++ implementation)
//' 
//' @param fcst numeric vector of forecasts (NAs are not allowed)
//' @param fcst_ref numeric vector of reference forecasts (NAs are not allowed)
//' @param obs vector of binary observations (obs[t] evaluates to TRUE if event happens at instance t, to FALSE otherwise) 
//' @return AUC values, their sampling standard deviations, the AUC difference, and their sampling standard deviations
//' @seealso Auc AucDiff
//' @export
// [[Rcpp::export]]
NumericVector aucdiff_cpp(NumericVector fcst, NumericVector fcst_ref, NumericVector obs) {

  int L = obs.size();

  // calculate order vectors of fcst and fcst_ref
  arma::uvec i_ord = arma::sort_index(Rcpp::as<arma::vec>(fcst), "ascend");
  arma::uvec i_ord_ref = arma::sort_index(Rcpp::as<arma::vec>(fcst_ref), "ascend");

  int n,m,nn,mm,i,j,jp1,k;
  double sumV, sumV2, sumW, sumW2, auc, v, w, sd_auc,
         sumV_ref, sumV2_ref, sumW_ref, sumW2_ref, auc_ref, v_ref, w_ref, sd_auc_ref,
         sumV_V_ref, sumW_W_ref, v12, w12, auc_diff, sd_auc_diff;

  // vectors or length L to save V_i and W_j values 
  std::vector<double> VW(L, 0);
  std::vector<double> VW_ref(L, 0);

  // temporary vectors to save indices while looping over duplicates
  std::vector<double> V_inds(0);
  std::vector<double> W_inds(0);

  // calculate V and W for AUC(fcst, obs)
  n = m = i = 0;
  while (1) {
    nn = mm = 0;
    V_inds.clear();
    W_inds.clear();
    while (1) {
      j = i_ord[i];
      if (obs[j]) {
        mm++;
        V_inds.push_back(j);
      } else {
        nn++;
        W_inds.push_back(j);
      }
      if (i == L-1) {
        break;
      } 
      jp1 = i_ord[i+1];
      if (fcst[j] != fcst[jp1]) {
        break;
      } 
      i++;
    }
    for (k = 0; k < mm; k++) {
      VW[V_inds[k]] = n + nn/2.0;
    }
    for (k = 0; k < nn; k++) {
      VW[W_inds[k]] = m + mm/2.0;
    }
    n += nn;
    m += mm;
    i++;
    if (i >= L) {
      break;
    }
  }

  // calculate V_ref and W_ref for AUC(fcst_ref, obs)
  n = m = i = 0;
  while (1) {
    nn = mm = 0;
    V_inds.clear();
    W_inds.clear();
    while (1) {
      j = i_ord_ref[i];
      if (obs[j]) {
        mm++;
        V_inds.push_back(j);
      } else {
        nn++;
        W_inds.push_back(j);
      }
      if (i == L-1) {
        break;
      } 
      jp1 = i_ord_ref[i+1];
      if (fcst_ref[j] != fcst_ref[jp1]) {
        break;
      } 
      i++;
    }
    for (k = 0; k < mm; k++) {
      VW_ref[V_inds[k]] = n + nn/2.0;
    }
    for (k = 0; k < nn; k++) {
      VW_ref[W_inds[k]] = m + mm/2.0;
    }
    n += nn;
    m += mm;
    i++;
    if (i >= L) {
      break;
    }
  }

  // calculate v = var(V/n), v_ref = var(V_ref/n), v12 = cov(V/n, V_ref/n)
  //           w = var(W/m), w_ref = var(W_ref/m), w12 = cov(W/m, W_ref/m)
  // using Welfords method for numerical stability
  double MV, MV_ref, MW, MW_ref = 0.0;
  double SV, SV_ref, SW, SW_ref = 0.0;
  double CV, CW = 0.0;
  double delta, delta2, delta_ref, delta2_ref, V_, Vref_, W_, Wref_;
  int nV, nW = 0;
  for (i = 0; i < L; i++) {
    if (obs[i]) {

      nV++;

      V_ = VW[i] / n;
      delta = V_ - MV;
      MV += delta / nV;
      delta2 = V_ - MV;
      SV += delta * delta2;

      Vref_ = VW_ref[i] / n;
      delta_ref = Vref_ - MV_ref;
      MV_ref += delta_ref / nV;
      delta2_ref = Vref_ - MV_ref;
      SV_ref += delta_ref * delta2_ref;

      CV += (nV-1) * delta / nV * delta_ref / nV - CV / nV;

    } else {

      nW++;

      W_ = VW[i] / m;
      delta = W_ - MW;
      MW += delta / nW;
      delta2 = W_ - MW;
      SW += delta * delta2;

      Wref_ = VW_ref[i] / m;
      delta_ref = Wref_ - MW_ref;
      MW_ref += delta_ref / nW;
      delta2_ref = Wref_ - MW_ref;
      SW_ref += delta_ref * delta2_ref;

      CW += (nW-1) * delta / nW * delta_ref / nW - CW / nW;

    }
  }

  auc = MV;
  auc_ref = MV_ref;

  v = SV / (m-1);
  v_ref = SV_ref / (m-1);
  v12 = CV * m / (m-1);

  w = SW / (n-1);
  w_ref = SW_ref / (n-1);
  w12 = CW * n / (n-1);

  sd_auc = sqrt(v / m + w / n);
  sd_auc_ref = sqrt(v_ref / m + w_ref / n);
  
  auc_diff = auc - auc_ref;
  sd_auc_diff = sqrt((v + v_ref - 2 * v12) / m + (w + w_ref - 2 * w12) / n);

  return NumericVector::create(auc, sd_auc, auc_ref, sd_auc_ref, auc_diff, sd_auc_diff);

}


