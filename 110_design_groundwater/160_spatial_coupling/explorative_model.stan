data {
  int<lower=1> N;               // sample size
  matrix[N, 2] X; // predictors: distance, depth
  real<lower=0, upper=1> prob[N]; // outcome variable
}

parameters {
  vector[2] slopes;
  real intercept;
  real<lower=0> kappa;
}


//transformed parameters {
//  vector<lower=0, upper=1>[N] estimator;
//  estimator = inv_logit(intercept + (X * slopes));  // ! brackets!
//}

model {
  // priors
  slopes ~ normal(0, 1);
  intercept ~ normal(0, 1);

  kappa ~ gamma(1, 1); // model residual

  // "posterior", "likelihood", give it a name...
  prob ~ beta_proportion(inv_logit(intercept + (X * slopes)), kappa);    //

}
