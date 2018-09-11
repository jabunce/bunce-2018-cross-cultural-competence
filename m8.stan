data {
  int<lower=1> J;         // number of interviewees
  int<lower=1> K;         // number of questions
  int<lower=1> T;       // number of targets (ego, ingroup, outgroup)
  int<lower=1> N;         // number of answers to questions (observations)
  int<lower=1,upper=J> jj[N];   // interviewee ID for observation n
  int<lower=1,upper=K> kk[N];   // question for observation n
  int<lower=1,upper=T> tt[N]; // target for observation n
  int<lower=0,upper=1> y[N];  // response (1 or 0) for obs n

  int<lower=1,upper=2> Machi[N];  //Matsigenka ethnicity: 1=yes, 2=no, for each obs n

  int<lower=0,upper=1> EdMes[N];  //education experience with mestizos
  int<lower=0,upper=1> LabMes[N]; //labor experience with mestizos
}

transformed data {
  vector[T] zeros = [0,0,0]';   //column vectors of 0s, note transpose at the end to turn into row vectors
  vector[T] ones = [1,1,1]';  //column vectors of 1s
}

parameters {
  vector[T] zInt[J];        //J-array of column T-vectors of intercept z-scores for each individual
  vector[T] muInt[2];       //intercept means: 2-array of T-vectors, one vector for each ethnicity
  vector<lower=0>[T] sigmaInt[2]; //intercept stdevs: 2-array of T-vectors, one vector for each ethnicity
  cholesky_factor_corr[T] L_R_a[2]; //2-array of cholesky factor TxT matrices, for correlation matrix for intercepts

  vector[T] aEdMes[2];        //coef for education experience with mestizos
  vector[T] aLabMes[2];       //coef for labor experience with mestizos

  vector[T] muBeta;         //vector of beta means for each target T
  vector<lower=0>[T] sigmaBeta;   //vector of beta stdevs for each target T 

  vector[T] muGamma;        //vector of gamma means for each target T
  vector<lower=0>[T] sigmaGamma;  //vector of gamma stdevs for each target T

  matrix[T*2,K] zQuest;       //matrix of question beta and gamma z-scores for each question, rows = betas and gammas, cols = questions
  cholesky_factor_corr[T*2] L_R_q;  //cholesky factor for correlation matrix for betas and gammas, a T*2xT*2 matrix
}

transformed parameters {
  vector[T*2] muQuest;
  vector[T*2] sigmaQuest;

  matrix[J,T] off_Int;                    //matrix of random offsets for each individual from mean of each intercept, rows = indivs, cols = intercepts
                                //Stan manual pg 150-151, Rethinking pg 409
                              //create cholesky factor for cov matrix and multiply by matrix of intercept z-scores, then transpose.
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

  for (n in 1:N) {
    off_Int[jj[n]] = (diag_pre_multiply(sigmaInt[Machi[n]], L_R_a[Machi[n]]) * zInt[jj[n]])';
  }

  off_quest = (diag_pre_multiply(sigmaQuest, L_R_q) * zQuest)';
}

model {
  vector[N] params;         //temporary vector container of gamma(alpha-beta) for each N
  vector[T] alpha;          //temporary container for an individual's three reconstructed intercepts
  vector[T] beta;
  vector[T] gamma;

  zInt ~ multi_normal(zeros, diag_matrix(ones));  //array of intercept z-scores for each indiv, sample two at a time from a normal
  muInt ~ multi_normal(zeros, diag_matrix(ones));   //array of T-vectors of mean intercepts
  aEdMes ~ multi_normal(zeros, diag_matrix(ones));  //array of T-vectors of experience offsets to the mean for each target
  aLabMes ~ multi_normal(zeros, diag_matrix(ones));
  for (Mach in 1:2) {
    sigmaInt[Mach] ~ exponential(2);        //use cauchy(0,2)?, array of T-vectors of stdevs from mean of each intercept
    L_R_a[Mach] ~ lkj_corr_cholesky(4);       //array of cholesky factor TxT matrices, lower the eta to allow more extreme correlations, Rethinking pg 394
  }

  to_vector(zQuest) ~ normal(0,1);
  muBeta ~ normal(0,1);
  muGamma ~ normal(0,1);
  sigmaBeta ~ exponential(2);
  sigmaGamma ~ exponential(2);
  L_R_q ~ lkj_corr_cholesky(40);    //lower the eta to allow more extreme correlations, Rethinking pg 394


  for (n in 1:N) {

    alpha = muInt[Machi[n]] + off_Int[jj[n]]' +         //linear model of intercepts: mean + indiv offset. Vector of T intercepts for each indiv. Note transpose of offsets to column vector
            aEdMes[Machi[n]] * EdMes[n] +
            aLabMes[Machi[n]] * LabMes[n];    

    beta[1] = muBeta[1] + off_quest[kk[n],1];
    beta[2] = muBeta[2] + off_quest[kk[n],2];
    beta[3] = muBeta[3] + off_quest[kk[n],3];

    gamma[1] = exp( muGamma[1] + off_quest[kk[n],4] );
    gamma[2] = exp( muGamma[2] + off_quest[kk[n],5] );
    gamma[3] = exp( muGamma[3] + off_quest[kk[n],6] );

    params[n] = gamma[tt[n]]*( alpha[tt[n]] - beta[tt[n]] );

  } //for

  y ~ bernoulli_logit(params);
}

generated quantities {
  matrix[J,T] aInt;   //reconstructed intercept for each individual
  matrix[K,T] rBeta;  //reconstructed betas for each question
  matrix[K,T] rGamma; //reconstructed gammas for each question

  matrix[T,T] R_a[2];   //2-array of TxT correlation matrices
  matrix[T,T] Cov_a[2];   //2-array of TxT variance-covariance matrices
  matrix[T*2,T*2] R_q;    //correlation matrix for betas and gammas
  matrix[T*2,T*2] Cov_q;  //variance-covariance matrix for betas and gammas

  vector[N] log_lik;  //for WAIC
  vector[T] alpha;
  vector[T] beta;
  vector[T] gamma;


  for (n in 1:N) {
    aInt[jj[n]] = muInt[Machi[n]]' + off_Int[jj[n]];        //reconstruct intercepts three at a time for each indiv. Note transpose of means to row vector.

    rBeta[kk[n],1] = muBeta[1]' + off_quest[kk[n],1];
    rBeta[kk[n],2] = muBeta[2]' + off_quest[kk[n],2];
    rBeta[kk[n],3] = muBeta[3]' + off_quest[kk[n],3];

    rGamma[kk[n],1] = exp( muGamma[1]' + off_quest[kk[n],4] );
    rGamma[kk[n],2] = exp( muGamma[2]' + off_quest[kk[n],5] );
    rGamma[kk[n],3] = exp( muGamma[3]' + off_quest[kk[n],6] );

    //needed for waic function, see Stan manual pg498
    alpha = muInt[Machi[n]] + off_Int[jj[n]]' +
            aEdMes[Machi[n]] * EdMes[n] +
            aLabMes[Machi[n]] * LabMes[n]; 
    beta[1] = muBeta[1] + off_quest[kk[n],1];
    beta[2] = muBeta[2] + off_quest[kk[n],2];
    beta[3] = muBeta[3] + off_quest[kk[n],3];
    gamma[1] = exp( muGamma[1] + off_quest[kk[n],4] );
    gamma[2] = exp( muGamma[2] + off_quest[kk[n],5] );
    gamma[3] = exp( muGamma[3] + off_quest[kk[n],6] );

    log_lik[n] = bernoulli_logit_lpmf( y[n] | gamma[tt[n]]*(alpha[tt[n]] - beta[tt[n]]) );
  } //for

  for (Mach in 1:2) {

    R_a[Mach] = L_R_a[Mach] * L_R_a[Mach]';               //reconstruct the correlation matrix to look at in output

    Cov_a[Mach] = diag_pre_multiply(sigmaInt[Mach], L_R_a[Mach]) * 
            diag_pre_multiply(sigmaInt[Mach], L_R_a[Mach])';  //construct cov matrix from colesky factors of cov matrix to look at in output
  } //for

  R_q = L_R_q * L_R_q';
  Cov_q = diag_pre_multiply(sigmaQuest, L_R_q) * diag_pre_multiply(sigmaQuest, L_R_q)';
}

