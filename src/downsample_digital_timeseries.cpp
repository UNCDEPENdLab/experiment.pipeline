#include <math.h>
#include "RcppArmadillo.h"
using namespace Rcpp;
using namespace arma;

//' This function downsamples a vector of integers by keeping the mode within each resampled chunk.
//'
//' @description When downsampling integers, it is difficult to decide on what value should be retained
//' since simpler subsampling approaches may retain a digital value that is not representative of the local
//' neighborhood of the vector. This is less of a problem for downsampling of analog channels where we assume
//' relative continuity across values.
//'
//' Here, we have adopted the approach of downsampling integers by retaining the most common intger value within
//' each subset of the vector. For example, if the vector is of length 100 and we are downsampling 10x, then
//' the function will look within each set of 10 integers and retain the most common value in each.
//'
//' @name downsample_digital_timeseries
//' @param x A vector of integer values to be downsampled by retaining the mode in each chunk
//' @param downsamp The factor by which data are downsampled (e.g., 3 will reduce the resulting vector 3x)
//' @param demote_zeros Logical indicating whether to prefer non-zero modes, even if they are no more likely in a chunk
//' @return A vector of length \code{ceiling(length(x)/downsamp)} with downsampled integers
//'
//' @details Note that this function does not allow for multiple modes. If there are multiple modes, it  always
//'   retains the mode with the lowest integer value in each chunk. The exception is if \code{demote_zeros} is \code{TRUE}.
//'   In this case, even if there is one non-zero value in a chunk, it will trump any number of zeros. This is helpful for
//'   TTL codes where a code may be on for part of the higher-frequency time series and we don't want it to become zero
//'   in the downsampled data.
//' @author Michael Hallquist
//' @export

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export]]

Rcpp::IntegerVector downsample_digital_timeseries(const arma::uvec& x, int downsamp, bool demote_zeros = true) {

  int n = x.size();
  int nchunks = ceil(static_cast<float>(n)/downsamp); //number of chunks

  //use an Rcpp result to avoid a 1xn return (i.e., matrix) with arma::urowvec
  Rcpp::IntegerVector result(nchunks);

  int mode;
  int number;
  int largest_count; //the number to beat to become the new mode
  int nrem; //the number of remaining elements in the vector
  int start; //starting position of the current candidate for mode within the vector

  for (int i = 0; i < nchunks; i++) {
    nrem = std::min(n - 1, (i+1)*downsamp - 1);

    // grab positions in x within this chunk and place in ascending order
    arma::uvec this_chunk = arma::sort( x.elem(regspace<uvec>(i*downsamp, nrem)) );

    mode = this_chunk[0]; //initialize mode of chunk to first element of sorted vector
    largest_count = 0; //number to beat within this chunk

    //Rcout << "This chunk size: " << this_chunk.size() << endl;
    //Rcout << "Operative chunk: " << endl << this_chunk << endl;

    int j = 0;
    while (j < this_chunk.size()) {
      number = this_chunk[j];
      start  = j;
      //Rcout << "j1: " << j << endl;

      //grouped loop over all values of this number
      while(this_chunk[j] == number && j < this_chunk.size()) { j++; }

      //Rcout << "j2: " << j << endl;

      int this_count = j - start;
      //Rcout << "this_count: " << this_count << endl;

      if (this_count > largest_count || (demote_zeros && mode == 0)) {
        //Rcout << "start: " << start << ", j: " << j << endl;
        //Rcout << "largest_count is: " << largest_count << ", this_count is: " << this_count << ", mode is now: " << number << endl << endl;
        mode = number;
        largest_count = this_count;
      }
    }

    result[i] = mode;
  }

  //return Rcpp::IntegerVector(result.begin(), result.end()); //if casting from arma uvec
  return result;
}

/*** R
#downsample_digital_timeseries(1:105, 10)
#downsample_digital_timeseries(c(rep(0, 5), rep(5, 22), rep(3, 26), rep(0, 15), 1:3), 10, FALSE)
vv <- c(rep(0, 5), rep(5, 22), rep(3, 26), rep(0, 15), 1:3)
downsample_digital_timeseries(vv, 10, TRUE)

#tt <- c(rep(10, 3), rep(5, 5), rep(2, 2), rep(0,9), sample(1:9, size=25, replace=TRUE))
#downsample_digital_timeseries(tt, 6, TRUE)

#tt <- c(sample(0:9, size=100, replace=TRUE))
#downsample_digital_timeseries(tt, 10, TRUE)

*/
