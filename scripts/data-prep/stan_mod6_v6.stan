// bayesian latent trait model 6 for estimating latent opinion
// item intercepts and slopes identified by fixing expectations
// non-centered parameterisation for all parameters

data{
  int<lower=1> N;               	// number of national survey opinions
  int<lower=1> J;               	// number of countries
  int<lower=1> K;  	        		// number of items
  int<lower=1> P;  	        		// number of items-country combinations  
  int<lower=1> T;  	        		// number of years
  int<lower=1,upper=J> jj[N];   	// country j for opinion n
  int<lower=1,upper=K> kk[N];   	// item k for opinion n
  int<lower=1,upper=P> pp[N];   	// item-country p for opinion n
  int<lower=1,upper=T> tt[N];   	// year t for opinion n
  int<lower=1> x[N];   			    // vector of survey responses, count
  int<lower=1> samp[N];				// vector of sample sizes
  int it_len[K];					// number of countries for each item
  real mn_resp_log;			        // observed response mean proportion on logit scale
}

parameters{
  real<lower=0> sigma_theta;	    // opinion evolution error variance
  vector[P] delta_raw;			    // P raw item-country effects
  matrix[T,J] theta_raw; 	        // raw matrix of T by J latent traits
  row_vector[J] theta_init;			// initial latent traits for first year
  real<lower=0> phi;				// dispersion parameter
  corr_matrix[2] Omega;           	// correlation matrix for item pars
  vector<lower=0>[2] tau;         	// cor -> cov conversion
  real<lower=0> sigma_delta;	    // item-country intercept error variance
  real mu_lambda;         		    // item intercept expectation
  matrix[K,2] Gamma_raw;			// non-centered parameters for item parameters
}

transformed parameters{
  matrix[T,J] theta; 	            // matrix of T by J latent traits	
  matrix[T-1,J] theta_hat; 	        // matrix of T-1 by J fitted latent traits	
  vector[N] theta_tt_jj;			// N-vector for expanded theta vales
  vector<lower=0,upper=1>[N] eta; 	// fitted values, on logit scale
  vector[K] lambda;                 // estimated item intercepts
  vector[K] gamm;                   // estimated item slopes
  real mu_gamm;          		    // item slope expectation
  matrix[2,2] Sigma;				// variance-covariance matrix for item ints and slopes
  matrix[K,2] Gamma;				// matrix of item intercepts and slopes 
  vector<lower=0>[N] alpha;			// beta shape par 1
  vector<lower=0>[N] beta;			// beta shape par 2  
  vector[P] delta;			        // P item-country effects
  theta[1] = theta_init; 
  mu_gamm = 1.0;					// fix expectation of item slopes
  delta = sigma_delta * delta_raw;
  theta[1] = theta_init; 
  for (t in 2:T) 	                // dynamic model for theta
  theta[t] = theta[t-1] + sigma_theta * theta_raw[t-1];
  Sigma = quad_form_diag(Omega, tau); 
  for (k in 1:K)   					// bivariate normal for item ints and slopes
  Gamma[k] = [ mu_lambda , mu_gamm ] + Gamma_raw[k] * Sigma;
  lambda = Gamma[,1];
  gamm = Gamma[,2];  
  for (i in 1:N) 					
    theta_tt_jj[i] = theta[tt[i], jj[i]];  // expand theta to N-vector	
  eta = inv_logit(lambda[kk] + gamm[kk] .* theta_tt_jj + delta[pp]);  // fitted values model
  alpha = phi * eta; 					   // reparamaterise beta-binom alpha par
  beta = phi * (1 - eta); 				   // reparamaterise beta-binom beta par
}

model{
  int pos;                           // local variable indicating which item to evaluate	
  x ~ beta_binomial(samp, alpha, beta);  // response model
  phi ~ gamma(3, 0.04); 				
  sigma_theta ~ normal(0, 2); 
  sigma_delta ~ normal(0, 2); 			 
  tau ~ normal(0, 2);
  Omega ~ lkj_corr(2);
  theta_init ~ normal(0, 1);
  mu_lambda ~ normal(mn_resp_log, 0.5);
  to_vector(Gamma_raw) ~ normal(0, 1);
  to_vector(theta_raw) ~ normal(0, 1);
  pos = 1;
  for (k in 1:K) {                  // standard normal prior for item-country effects within items
    segment(delta_raw, pos, it_len[k]) ~ normal(0, 1);
    pos = pos + it_len[k];
  }
}

generated quantities {
  vector[N] x_pred;                // fitted data to check model
  vector[N] log_lik;               // log lik for WAIC calc
  for (i in 1:N) {
    x_pred[i] = beta_binomial_rng(samp[i], alpha[i], beta[i]);
    log_lik[i] = beta_binomial_lpmf(x[i] | samp[i], alpha[i], beta[i]); 
  }
}
