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
  
  vector[T] aInt[J];        //array of target intercepts for each individual, array of size J containing vectors of T elements. See Stan manual pg 45-46 and Beta on pg 149
  vector[T] muInt;          //vector of intercept means for each target T
  vector<lower=0>[T] sigmaInt;    //vector of intercept stdevs for each target T
  corr_matrix[T] R_a;       //correlation matrix for T intercepts

  vector[T] muBeta;         //vector of beta means for each target T
  vector<lower=0>[T] sigmaBeta;   //vector of beta stdevs for each target T 

  vector<lower=0>[T] muGamma;   //vector of gamma means for each target T
  vector<lower=0>[T] sigmaGamma;  //vector of gamma stdevs for each target T

  vector[T*2] quest[K];       //vector of question betas and gammas for each target
  corr_matrix[T*2] R_q;       //correlation matrix for question betas and gammas
}

transformed parameters {
  vector[T*2] muQuest;
  vector[T*2] sigmaQuest;

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
}

model {
  vector[N] params;     //temporary vector container of gamma(alpha-beta) for each N
  real alpha;       //temporary container for linear model
  real beta;
  real gamma;
  

  muInt ~ normal(0,1);      // T-vector of mean intercepts for each target
  sigmaInt ~ exponential(1);  //use cauchy(0,2)?, T-vector of stdevs for each target
  R_a ~ lkj_corr(2);

  muBeta ~ normal(0,1);     // T-vector of mean betas for each target
  sigmaBeta ~ exponential(1); // T-vector of beta stdevs

  muGamma ~ normal(0,1);
  sigmaGamma ~ exponential(1);

  R_q ~ lkj_corr(4);


  for (n in 1:N) {

    alpha = aInt[ jj[n], tt[n] ];     //first index is element of J-array, second index is element of T-vector

    beta = quest[ kk[n], tt[n] ];
    gamma = quest[ kk[n], tt[n]+T ];


    params[n] = gamma*(alpha - beta);

  } //for

  quest ~ multi_normal( muQuest, quad_form_diag(R_q, sigmaQuest) );
  aInt ~ multi_normal( muInt, quad_form_diag(R_a, sigmaInt) );      //sample a T-vector for each cell in the array of T-vectors, stan manual pg 149
  y ~ bernoulli_logit(params);
}

