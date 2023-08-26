data {
  int<lower=1> len_data;
  real logprem[len_data];
  real logloss[len_data];
  int<lower=1,upper=10> w[len_data];
  int<lower=1,upper=10> d[len_data];
}

parameters {
  real r_alpha[9];
  real r_beta[9];
  real logelr;
  real <lower=0,upper=100000> a_ig[10];
}

transformed parameters{
  real alpha[10];
  real beta[10];
  real sig2[10];
  real sig[10];
  real mu[len_data];
  alpha[1] = 0;
  for (i in 2:10) alpha[i] = r_alpha[i-1];
  for (i in 1:9) beta[i] = r_beta[i];
  beta[10] = 0;
  sig2[10] = gamma_cdf(1/a_ig[10],1,1);
  for (i in 1:9) sig2[10-i] = sig2[11-i]+gamma_cdf(1/a_ig[i],1,1);
  for (i in 1:10) sig[i] = sqrt(sig2[i]);
  for (i in 1:len_data){
    mu[i] = logprem[i]+logelr+alpha[w[i]]+beta[d[i]];
  }
}

model {
  r_alpha ~ normal(0,3.162);
  r_beta ~ normal(0,3.162);
  for (i in 1:10) a_ig[i] ~ inv_gamma(1,1);
  logelr ~ normal(-.4,3.162);
  for (i in 1:len_data) logloss[i] ~ normal(mu[i],sig[d[i]]);
}

generated quantities{
  vector[len_data] log_lik;
  for (i in 1:len_data) log_lik[i] = normal_lpdf(logloss[i]|mu[i],sig[d[i]]);
}

