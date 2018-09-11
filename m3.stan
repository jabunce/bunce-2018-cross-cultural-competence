data {
  int<lower=1> J;         // number of interviewees
  int<lower=1> K;         // number of questions
  int<lower=1> T;       // number of targets (ego, ingroup, outgroup)
  int<lower=1> N;         // number of answers to questions (observations)
  int<lower=1,upper=J> jj[N];   // interviewee ID for observation n
  int<lower=1,upper=K> kk[N];   // question for observation n
  int<lower=1,upper=T> tt[N]; // target for observation n
  int<lower=0,upper=1> y[N];  // response (1 or 0) for obs n
}

parameters {
  matrix[T,J] zInt;         //matrix of intercept z-scores for each individual, rows = intercepts, cols = indivs
  vector[T] muInt;          //vector of intercept means for each target T
  vector<lower=0>[T] sigmaInt;    //vector of intercept stdevs for each target T
  cholesky_factor_corr[T] L_R_a;  //cholesky factor for correlation matrix for intercepts, a TxT matrix

  vector[T] muBeta;         //vector of beta means for each target T
  vector<lower=0>[T] sigmaBeta;   //vector of beta stdevs for each target T 

  vector[T] muGamma;        //vector of gamma means for each target T (will be exponentiated, so don't constrain positive)
  vector<lower=0>[T] sigmaGamma;  //vector of gamma stdevs for each target T

  matrix[T*2,K] zQuest;       //matrix of question beta and gamma z-scores for each question, rows = betas and gammas, cols = questions
  cholesky_factor_corr[T*2] L_R_q;  //cholesky factor for correlation matrix for betas and gammas, a T*2xT*2 matrix
}

transformed parameters {
  vector[T*2] muQuest;
  vector[T*2] sigmaQuest;

  matrix[J,T] off_Int;      //matrix of random offsets for each individual from mean of each intercept, rows = indivs, cols = intercepts. Note: dimensions are transposed from zInt
  matrix[K,T*2] off_quest;                              

  muQuest[1] = muBeta[1];
  muQuest[2] = muBeta[2];
  muQuest[3] = muBeta[3];
  muQuest[4] = muGamma[1];
  muQuest[5] = muGamma[2];
  muQuest[6] = muGamma[3];

  sigmaQuest[1] = sigmaBeta[1];
  sigmaQuest[2] = sigmaBeta[2];
  sigmaQuest[3] = sigmaBeta[3];
  sigmaQuest[4] = sigmaGamma[1];
  sigmaQuest[5] = sigmaGamma[2];
  sigmaQuest[6] = sigmaGamma[3];

                                  //Stan manual pg 150-151, Rethinking pg 409
  off_Int = (diag_pre_multiply(sigmaInt, L_R_a) * zInt)';     //create cholesky factor for cov matrix and multiply by matrix of intercept z-scores, then transpose so dimensions match off_Int.
  off_quest = (diag_pre_multiply(sigmaQuest, L_R_q) * zQuest)';
}

model {
  vector[N] params;         //temporary vector container of gamma(alpha-beta) for each N
  vector[T] alpha;          //temporary container for an individual's three reconstructed intercepts
  vector[T] beta;
  vector[T] gamma;

  to_vector(zInt) ~ normal(0,1);  //vectorize matrix of intercept z-scores for each indiv in order to assign sampling distribution
  muInt ~ normal(0,1);        //T-vector of mean intercepts
  sigmaInt ~ exponential(1);    //use cauchy(0,2)?, T-vector of stdevs from mean of each intercept
  L_R_a ~ lkj_corr_cholesky(4);   //lower the eta to allow more extreme correlations, Rethinking pg 394

  to_vector(zQuest) ~ normal(0,1);
  muBeta ~ normal(0,1);
  muGamma ~ normal(0,1);
  sigmaBeta ~ exponential(1);
  sigmaGamma ~ exponential(1);
  L_R_q ~ lkj_corr_cholesky(40);    //lower the eta to allow more extreme correlations, Rethinking pg 394


  for (n in 1:N) {

    alpha = muInt + off_Int[jj[n]]';            //linear model of intercepts: mean + indiv offset. Vector of T intercepts for each indiv. Note transpose of offsets to column vector

    beta[1] = muBeta[1] + off_quest[kk[n],1];
    beta[2] = muBeta[2] + off_quest[kk[n],2];
    beta[3] = muBeta[3] + off_quest[kk[n],3];

    gamma[1] = exp( muGamma[1] + off_quest[kk[n],4] );    //exponentiate to constrain discriminations positive
    gamma[2] = exp( muGamma[2] + off_quest[kk[n],5] );
    gamma[3] = exp( muGamma[3] + off_quest[kk[n],6] );

    params[n] = gamma[tt[n]]*( alpha[tt[n]] - beta[tt[n]] );

  } //for

  y ~ bernoulli_logit(params);
}

generated quantities {
  matrix[J,T] aInt;   //reconstructed intercepts for each person
  matrix[K,T] rBeta;  //reconstructed betas for each question
  matrix[K,T] rGamma; //reconstructed gammas for each question

  matrix[T,T] R_a;    //correlation matrix for intercepts
  matrix[T,T] Cov_a;  //variance-covariance matrix for intercepts
  matrix[T*2,T*2] R_q;  //correlation matrix for betas and gammas
  matrix[T*2,T*2] Cov_q;//variance-covariance matrix for betas and gammas

  vector[N] log_lik;  //for WAIC
  vector[T] alpha;
  vector[T] beta;
  vector[T] gamma;


  for (n in 1:N) {
    aInt[jj[n]] = muInt' + off_Int[jj[n]];                    //reconstruct intercepts three at a time for each indiv. Note transpose of means to row vector.
  
    rBeta[kk[n],1] = muBeta[1]' + off_quest[kk[n],1];
    rBeta[kk[n],2] = muBeta[2]' + off_quest[kk[n],2];
    rBeta[kk[n],3] = muBeta[3]' + off_quest[kk[n],3];

    rGamma[kk[n],1] = exp( muGamma[1]' + off_quest[kk[n],4] );
    rGamma[kk[n],2] = exp( muGamma[2]' + off_quest[kk[n],5] );
    rGamma[kk[n],3] = exp( muGamma[3]' + off_quest[kk[n],6] );

    //needed for waic function, see Stan manual pg498
    alpha = muInt + off_Int[jj[n]]';            
    beta[1] = muBeta[1] + off_quest[kk[n],1];
    beta[2] = muBeta[2] + off_quest[kk[n],2];
    beta[3] = muBeta[3] + off_quest[kk[n],3];
    gamma[1] = exp( muGamma[1] + off_quest[kk[n],4] );
    gamma[2] = exp( muGamma[2] + off_quest[kk[n],5] );
    gamma[3] = exp( muGamma[3] + off_quest[kk[n],6] );

    log_lik[n] = bernoulli_logit_lpmf( y[n] | gamma[tt[n]]*(alpha[tt[n]] - beta[tt[n]]) );
  } //for n

  R_a = L_R_a * L_R_a';                                 //reconstruct the correlation matrix to look at in output
  R_q = L_R_q * L_R_q';

  Cov_a = diag_pre_multiply(sigmaInt, L_R_a) * diag_pre_multiply(sigmaInt, L_R_a)';   //construct cov matrix from colesky factors of cov matrix to look at in output
                                            //sigmaInt contains stdevs, the final cov matrix has variances down the diagonal, but uncertainty in both parameters is carried through in this calculation, so sqrt(diag) is not exactly sigmaInt
  Cov_q = diag_pre_multiply(sigmaQuest, L_R_q) * diag_pre_multiply(sigmaQuest, L_R_q)';
}

