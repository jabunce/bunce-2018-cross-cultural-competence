data {
  int<lower=1> J;                   // number of interviewees
  int<lower=1> K;                   // number of questions
  int<lower=1> T;                   // number of targets (ego, ingroup, outgroup)
  int<lower=1> N;                   // number of answers to questions (observations)
  int<lower=1,upper=J> jj[N];       // interviewee ID for observation n
  int<lower=1,upper=K> kk[N];       // question for observation n
  int<lower=1,upper=T> tt[N];       // target for observation n
  int<lower=0,upper=1> y[N];        // response (1 or 0) for obs n
}

parameters {
  real beta[T,K];                   //separate betas and gammas for each target
  real<lower=0> gamma[T,K];
  real a0[T];                       // mean interviewee location in latent dimension (mean ability intercept), one for each target
  real aIndiv[T,J];                 // location of people (differing from the mean), i.e., random effect for person. rows=targets, columns=individuals
  real<lower=0> sigma_beta[T];      // scale (stdev) of question difficulty, one for each target
  real<lower=0> sigma_gamma[T];     // scale of question discrimination
}

model {
  vector[N] params;
  real alpha;
  
  a0 ~ normal(0,1);
  aIndiv[1] ~ normal(0,1);          //identifying prior for location and scale
  aIndiv[2] ~ normal(0,1);
  aIndiv[3] ~ normal(0,1);

  beta[1] ~ normal(0,sigma_beta[1]);
  beta[2] ~ normal(0,sigma_beta[2]);
  beta[3] ~ normal(0,sigma_beta[3]);

  gamma[1] ~ normal(0,sigma_gamma[1]);
  gamma[2] ~ normal(0,sigma_gamma[2]);
  gamma[3] ~ normal(0,sigma_gamma[3]);

  sigma_beta ~ exponential(1);                        //exponential(beta), where here beta = lambda = 1/mean
  sigma_gamma ~ exponential(1);                       //or use uniform(0,5), both prevent ceiling effect

  for (n in 1:N) {
    alpha = ( a0[tt[n]] + aIndiv[tt[n],jj[n]] );      //random effect for person

    params[n] = gamma[tt[n],kk[n]]*(alpha - beta[tt[n],kk[n]]);

  } //for

  y ~ bernoulli_logit(params);
}

generated quantities {       //for computing waic
  vector[N] log_lik;
  real alpha;

  for (n in 1:N) {
    alpha = ( a0[tt[n]] + aIndiv[tt[n],jj[n]] );

    //needed for waic function, see Stan manual pg498
    log_lik[n] = bernoulli_logit_lpmf( y[n] | gamma[tt[n],kk[n]]*(alpha - beta[tt[n],kk[n]]) );
  } //for
}

