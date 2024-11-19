data {
  int<lower=1> N;               // sample size
  matrix[N, 3] X; // predictors: distance, depth
  real prob[N]; // outcome variable
}

parameters {
  vector[3] slopes;
  real intercept;
  real<lower=0> residual;      // residual variability / model error
  real<lower=0> dof;      // the "nu" parameter for Student's T (degrees of freedom)
}

model {
  // priors
  slopes ~ normal(0, 0.1);
  intercept ~ normal(0, 1);
  residual ~ cauchy(0, 1);
  dof ~ cauchy(0, 10);

  // "posterior", "likelihood", give it a name...
  prob ~ student_t(dof, intercept + (X * slopes), residual); // nu, mu, sigma

}
