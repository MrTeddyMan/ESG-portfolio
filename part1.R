rm(list = ls())

setwd("/Users/teddy/Documents/Tasks/SF/Assignment/SF Ass")

##########################################################
## Section 3
# - Setting up excess market return
# - Import market returns and risk free rate
mkt_ret <- read.csv("./Data/Market return.csv")
rf      <- read.csv("./Data/Risk-free rate.csv")

# Combines mkt_ret and rf into the same dataset
mkt_prem            <- merge(mkt_ret, rf, by = "Year", all = TRUE)
colnames(mkt_prem)  <- c("Year", "mkt_ret", "rf")

# Creates a column with the market premium
mkt_prem$excess_mkt_ret <- mkt_prem$mkt_ret - mkt_prem$rf

# - Import stock betas and ESG ratings
ESG_A <- read.csv("./Data/ESG rating agency A.csv")
ESG_B <- read.csv("./Data/ESG rating agency B.csv")
ESG_C <- read.csv("./Data/ESG rating agency C.csv")
ESG_D <- read.csv("./Data/ESG rating agency D.csv")
ESG_E <- read.csv("./Data/ESG rating agency E.csv")
ESG_F <- read.csv("./Data/ESG rating agency F.csv")

# Number of years of ESG scores
T_esg <- length(ESG_A[, 1])

# Number of stocks
N <- length(ESG_A[1, ]) - 1

# - Import stock returns
stock_ret <- read.csv("./Data/Stock return.csv")

# Number of years of returns
T_ret <- length(stock_ret[, 1])

# - Aligning years of ESG scores and returns
T_beginning <- T_ret - T_esg + 1

# - Import stock beta
stock_beta <- read.csv("./Data/Stock beta.csv")
stock_beta <- as.matrix(stock_beta[, 2:(N + 1)])

stock_mkt_ret <- merge(mkt_prem, stock_ret, by = "Year")
#What do we need this one for?

# - Convert to matrix for easier handling
excess_stock_ret <- as.matrix(stock_ret[, 2:(N + 1)], nrow = T_R, ncol = N) - rf[, 2]

#Maybe we should plot a graph with the returns against time? Something to look
#with more time.

## Prepping for ESG rating
# Transform ESG ratings to standardized ratings using apply
# - Convert to matrices for easier handling
Z_score_A <- -(as.matrix(ESG_A[, 2:(N + 1)], nrow = T_esg, ncol = N) -
                 apply(as.matrix(ESG_A[, 2:(N + 1)], nrow = T_esg, ncol = N), 1, mean)) /
  apply(as.matrix(ESG_A[, 2:(N + 1)], nrow = T_esg, ncol = N), 1, sd)

# - Control for 0 mean and 1 sd
apply(Z_score_A, 1, mean)
apply(Z_score_A, 1, sd)

Z_score_B <- (as.matrix(ESG_B[, 2:(N + 1)], nrow = T_esg, ncol = N) -
                apply(as.matrix(ESG_B[, 2:(N + 1)], nrow = T_esg, ncol = N), 1, mean)) /
  apply(as.matrix(ESG_B[, 2:(N + 1)], nrow = T_esg, ncol = N), 1, sd)

Z_score_C <- (as.matrix(ESG_C[, 2:(N + 1)], nrow = T_esg, ncol = N) -
                apply(as.matrix(ESG_C[, 2:(N + 1)], nrow = T_esg, ncol = N), 1, mean)) /
  apply(as.matrix(ESG_C[, 2:(N + 1)], nrow = T_esg, ncol = N), 1, sd)

Z_score_D <- (as.matrix(ESG_D[, 2:(N + 1)], nrow = T_esg, ncol = N) -
                apply(as.matrix(ESG_D[, 2:(N + 1)], nrow = T_esg, ncol = N), 1, mean)) /
  apply(as.matrix(ESG_D[, 2:(N + 1)], nrow = T_esg, ncol = N), 1, sd)

Z_score_E <- (as.matrix(ESG_E[, 2:(N + 1)], nrow = T_esg, ncol = N) -
                apply(as.matrix(ESG_E[, 2:(N + 1)], nrow = T_esg, ncol = N), 1, mean)) /
  apply(as.matrix(ESG_E[, 2:(N + 1)], nrow = T_esg, ncol = N), 1, sd)

Z_score_F <- (as.matrix(ESG_F[, 2:(N + 1)], nrow = T_esg, ncol = N) -
                apply(as.matrix(ESG_F[, 2:(N + 1)], nrow = T_esg, ncol = N), 1, mean)) /
  apply(as.matrix(ESG_F[, 2:(N + 1)], nrow = T_esg, ncol = N), 1, sd)


##########################################################
## End of section 3
##########################################################
## Section 4.1 Beta portfolios
# - Empty return beta matrix for for loop
ret_beta_sorted <- matrix(0, nrow = T_esg - 1, ncol = 5)

# - Sort stocks by beta at each time step and group them into 5 portfolios
# from lowest to highest beta. Then, compute the average excess return
# for each portfolio and store it in the return-beta matrix

for (t in 1:(T_esg - 1)) {
  beta_index_order <- order(stock_beta[t, ])
  re_beta_ordered <- excess_stock_ret[(T_beginning + t), beta_index_order]
  ret_beta_sorted[t, 1] <- mean(re_beta_ordered[1:100])   #Lowest beta
  ret_beta_sorted[t, 2] <- mean(re_beta_ordered[101:200])
  ret_beta_sorted[t, 3] <- mean(re_beta_ordered[201:300])
  ret_beta_sorted[t, 4] <- mean(re_beta_ordered[301:400])
  ret_beta_sorted[t, 5] <- mean(re_beta_ordered[401:500]) # Highest beta
}
mean_ret_beta_ptf <- apply(ret_beta_sorted, 2, mean)
print(mean_ret_beta_ptf)

#################
## End of section 4.1
#################
## Section 4.2 ESG portfolios
# - Compute mean ESG score per stock per year
s_matrix <- (Z_score_A + Z_score_B + Z_score_C + Z_score_D + Z_score_E + Z_score_F) / 6

ESG_scores <- matrix(0, nrow = T_esg, ncol = N)

for (t in 1:T_esg) {
  sd_s_t <- sqrt(sum(s_matrix[t, ]^2) / (N - 1))
  ESG_scores[t, ] <- s_matrix[t, ] / sd_s_t
}
ret_ESG_sorted <- matrix(0, nrow = T_esg - 1, ncol = 5)

for (t in 1:(T_esg - 1)) {
  ESG_order <- order(ESG_scores[t, ])
  re_ESG_ordered <- excess_stock_ret[(T_beginning + t), ESG_order]
  ret_ESG_sorted[t, 1] <- mean(re_ESG_ordered[1:100])      # Lowest ESG
  ret_ESG_sorted[t, 2] <- mean(re_ESG_ordered[101:200])
  ret_ESG_sorted[t, 3] <- mean(re_ESG_ordered[201:300])
  ret_ESG_sorted[t, 4] <- mean(re_ESG_ordered[301:400])
  ret_ESG_sorted[t, 5] <- mean(re_ESG_ordered[401:500])    # Highest ESG
}
print(ret_ESG_sorted)

# - Compute average returns for each ESG portfolio
mean_ret_ESG_ptf <- apply(ret_ESG_sorted, 2, mean)
print(mean_ret_ESG_ptf)

##########################################################
## End of section 4.2
##########################################################
## Section 4.3 ESG uncertainty portfolios
ESGU <- matrix(0, nrow = T_esg, ncol = N)

for (t in 1:T_esg) {
  ESGU[t, ] <- apply(
    cbind(
      Z_score_A[t, ],
      Z_score_B[t, ],
      Z_score_C[t, ],
      Z_score_D[t, ],
      Z_score_E[t, ],
      Z_score_F[t, ]
    ),1,sd)
}

ret_ESGU_sorted <- matrix(0, nrow = T_esg - 1, ncol = 5)

for (t in 1:(T_esg - 1)) {
  ESGU_order <- order(ESGU[t, ])
  re_ESGU_ordered <- excess_stock_ret[(T_beginning + t), ESGU_order]
  ret_ESGU_sorted[t, 1] <- mean(re_ESGU_ordered[1:100])      # Lowest ESG
  ret_ESGU_sorted[t, 2] <- mean(re_ESGU_ordered[101:200])
  ret_ESGU_sorted[t, 3] <- mean(re_ESGU_ordered[201:300])
  ret_ESGU_sorted[t, 4] <- mean(re_ESGU_ordered[301:400])
  ret_ESGU_sorted[t, 5] <- mean(re_ESGU_ordered[401:500])    # Highest ESG
}
print(ret_ESG_sorted)

# - Compute average returns
mean_ret_ESGU_ptf <- apply(ret_ESGU_sorted, 2, mean)
print(mean_ret_ESGU_ptf)

##########################################################
## End of section 4.3
##########################################################
## Section 5 Construct asset pricing models
# Subsection 5.1 CAPM
# - Long high beta, short low beta portfolio
ls_beta <- ret_beta_sorted[, 5] - ret_beta_sorted[, 1]
ls_ESG <- ret_ESG_sorted[, 5] - ret_ESG_sorted[, 1]
ls_ESGU <- ret_ESGU_sorted[, 5] - ret_ESGU_sorted[, 1]
mean_ls_beta <- mean(ls_beta)
mean_ls_ESG <- mean(ls_ESG)
mean_ls_ESGU <- mean(ls_ESGU)
print(ls_beta)
print(ls_ESG)
print(ls_ESGU)
print(mean_ls_beta)
print(mean_ls_ESG)
print(mean_ls_ESGU)

# - Test CAPM
matrix_capm <- matrix(1, nrow = 24, ncol = 2)
matrix_capm[, 2] <- mkt_prem$excess_mkt_ret[(T_beginning + 1):T_ret]
coefs_capm <- solve(t(matrix_capm) %*% matrix_capm) %*% t(matrix_capm) %*%
  ls_beta
res_capm <- ls_beta - matrix_capm %*% coefs_capm
cov_coefs_capm <- solve(t(matrix_capm) %*% matrix_capm) * sum(res_capm^2) /
  (24 - 2)
t_stat_coefs_capm <- coefs_capm / sqrt(diag(cov_coefs_capm))
print(t_stat_coefs_capm)

matrix_ESG <- matrix(1, nrow = 24, ncol = 2)
matrix_ESG[, 2] <- mkt_prem$excess_mkt_ret[(T_beginning + 1):T_ret]
coefs_ESG <- solve(t(matrix_ESG) %*% matrix_ESG) %*% t(matrix_ESG) %*% ls_ESG
res_ESG <- ls_ESG - matrix_ESG %*% coefs_ESG
cov_coefs_ESG <- solve(t(matrix_ESG) %*% matrix_ESG) * sum(res_ESG^2) /
  (24 - 2)
t_stat_coefs_ESG <- coefs_ESG / sqrt(diag(cov_coefs_ESG))
print(t_stat_coefs_ESG)

matrix_ESGU <- matrix(1, nrow = 24, ncol = 2)
matrix_ESGU[, 2] <- mkt_prem$excess_mkt_ret[(T_beginning + 1):T_ret]
coefs_ESGU <- solve(t(matrix_ESGU) %*% matrix_ESGU) %*% t(matrix_ESGU) %*%
  ls_ESGU
res_ESGU <- ls_ESGU - matrix_ESGU %*% coefs_ESGU
cov_coefs_ESGU <- solve(t(matrix_ESGU) %*% matrix_ESGU) * sum(res_ESGU^2) /
  (24 - 2)
t_stat_coefs_ESGU <- coefs_ESGU / sqrt(diag(cov_coefs_ESGU))
print(t_stat_coefs_ESGU)

# Control for coefficients using lm()
reg_check_capm <- lm(ls_beta ~ mkt_prem$excess_mkt_ret[(T_beginning + 1):T_ret])
reg_check_ESG <- lm(ls_ESG ~ mkt_prem$excess_mkt_ret[(T_beginning + 1):T_ret])
reg_check_ESGU <- lm(ls_ESGU ~ mkt_prem$excess_mkt_ret[(T_beginning + 1):T_ret])
summary(reg_check_capm)
summary(reg_check_ESG)
summary(reg_check_ESGU)

##########################################################
## End of section 5.1
##########################################################
## Subsection 2&3 ESG and ESGU model
# ESG & ESGU model
coefs_ESG <- matrix(0, nrow = 24, ncol = 2)
coefs_3Factor <- matrix(0, nrow = 24, ncol = 4)

for (i in 1:24) {
  #ESG factor
  matrix <- matrix(1, nrow = 2, ncol = 500)
  matrix[2, ] <- ESG_scores[i, ]
  coefs_ESG[i, ] <- excess_stock_ret[(T_beginning + i), ] %*% t(matrix) %*%
    solve(matrix %*% t(matrix))
  
  #3Factor
  matrix <- matrix(1, nrow = 4, ncol = 500)
  matrix[2, ] <- stock_beta[i, ]
  matrix[3, ] <- ESG_scores[i, ]
  matrix[4, ] <- ESGU[i, ]
  coefs_3Factor[i, ] <- excess_stock_ret[(T_beginning + i), ] %*% t(matrix) %*%
    solve(matrix %*% t(matrix))
}
print(coefs_ESG)
print(coefs_3Factor)
colMeans(coefs_ESG)
colMeans(coefs_3Factor)

# - T-stat for estimated lambda ESG for ESG model
est_ESG_factor <- sum(coefs_ESG[, 2]) / 24
print(est_ESG_factor)

t_stat_ESG_factor <- sqrt(24 - 1) * (est_ESG_factor / sqrt(var(coefs_ESG[, 2])))
print(t_stat_ESG_factor)

reg_check <- lm(excess_stock_ret[(T_beginning + 1):T_ret] ~ coefs_ESG[, 2])
summary(reg_check)

# - Plot graph for market factor and excess return on the market
plot(
  coefs_3Factor[, 2],
  mkt_prem$excess_mkt_ret[(T_beginning + 1):T_ret],
  pch = 20,
  col = "blue",
  xlab = "Market factor",
  ylab = "Excess market return",
  xlim = c(-0.5, 0.5),
  ylim = c(-0.5, 0.5)
)

# Correlation between 3 factor model market factor and excess market returns
cor_3fbeta_mkt_ret <- cor(coefs_3Factor[, 2], mkt_prem$excess_mkt_ret[(T_beginning+1):T_ret])
print(cor_3fbeta_mkt_ret)

# - Estimates of factor risk premia, test of significance and Sharpe ratio
est_risk_factor_m <- mean(coefs_3Factor[(2:24), 2])
est_risk_factor_ESG <- mean(coefs_3Factor[(2:24), 3])
est_risk_factor_ESGU <- mean(coefs_3Factor[(2:24), 4])
print(est_risk_factor_m)
print(est_risk_factor_ESG)
print(est_risk_factor_ESGU)

t_stat_est_risk_factor_m <- sqrt(24 - 1) * (est_risk_factor_m / sqrt(var(coefs_3Factor[, 2])))
t_stat_est_risk_factor_ESG <- sqrt(24 - 1) * (est_risk_factor_ESG / sqrt(var(coefs_3Factor[, 3])))
t_stat_est_risk_factor_ESGU <- sqrt(24 - 1) * (est_risk_factor_ESGU / sqrt(var(coefs_3Factor[, 4])))
print(t_stat_est_risk_factor_m)
print(t_stat_est_risk_factor_ESG)
print(t_stat_est_risk_factor_ESGU)

sr_risk_factor_m <- est_risk_factor_m / sd(coefs_3Factor[, 2])
print(sr_risk_factor_m)
sr_risk_factor_ESG <- est_risk_factor_ESG / sd(coefs_3Factor[, 3])
print(sr_risk_factor_ESG)
sr_risk_factor_ESGU <- est_risk_factor_ESGU / sd(coefs_3Factor[, 4])
print(sr_risk_factor_ESGU)

##########################################################
## End of section 5.2 & 5.3
##########################################################
## Subsection 4 Cost of capital
# Run multivariate regression on each long-short portfolio from section 5.1
reg_ls_beta_3Factor <- lm(ls_beta ~ coefs_3Factor[, 2] + coefs_3Factor[, 3] + coefs_3Factor[, 4])
reg_ls_ESG_3Factor <- lm(ls_ESG ~ coefs_3Factor[, 2] + coefs_3Factor[, 3] + coefs_3Factor[, 4])
reg_ls_ESGU_3Factor <- lm(ls_ESGU ~ coefs_3Factor[, 2] + coefs_3Factor[, 3] + coefs_3Factor[, 4])
summary(reg_ls_beta_3Factor)
summary(reg_ls_ESG_3Factor)
summary(reg_ls_ESGU_3Factor)
summary(reg_check_capm)
summary(reg_check_ESG)
summary(reg_check_ESGU)

##########################################################
## End of section 5.4
##########################################################
## Section 6 Impact of ESG news on returns

# - Compute changes in ESG scores over time
ESGNews <- ESG_scores[2:T_esg, ] - ESG_scores[1:(T_esg - 1), ]
ESGUNews <- ESGU[2:T_esg, ] - ESGU[1:(T_esg - 1), ]

# Store regression coefficients for each year
b_coefs <- matrix(0, nrow = T_esg - 1, ncol = 2)
colnames(b_coefs) <- c("b_ESG", "b_ESGU")

for (i in 1:24) {
  matrix <- matrix(1, nrow = 2, ncol = 500)
  matrix[1, ] <- ESGNews[i, ]
  matrix[2, ] <- ESGUNews[i, ]
  b_coefs[i, ] <- excess_stock_ret[(T_beginning + i), ] %*% t(matrix) %*%
    solve(matrix %*% t(matrix))
}
print(b_coefs)

# - Compute T-stat for each coefficient
b_coefs_avg <- colMeans(b_coefs)
print(b_coefs_avg)

b_coefs_var <- apply(b_coefs, 2, var)
print(b_coefs_var)

t_stat_b_coefs <- sqrt(24 - 1) * (b_coefs_avg / sqrt(b_coefs_var))
print(t_stat_b_coefs)

##########################################################
## End of section 6
##########################################################
## Section 7 Double sorts

ds_beta_ESG <- matrix(0, nrow = 24, ncol = 25)

for (t in 1:24) {
  order_beta <- order(stock_beta[t, ])
  re_beta_ordered <- excess_stock_ret[T_beginning + t, order_beta]
  esg_beta_ordered <- ESG_scores[t, order_beta]
  
  for (m in 1:5) {
    M_L <- 1 + (m - 1) * 100
    M_H <- m * 100
    
    re_beta_group_m <- re_beta_ordered[M_L:M_H]
    order_esg <- order(esg_beta_ordered[M_L:M_H])
    
    re_beta_group_m_esg_ordered <- re_beta_group_m[order_esg]
    
    for (n in 1:5) {
      N_L <- 1 + (n - 1) * 20
      N_H <- n * 20
      colnumber <- 1 + (m - 1) * 5 + (n - 1)
      ds_beta_ESG[t, colnumber] <- mean(re_beta_group_m_esg_ordered[N_L:N_H])
    }
    
  }
}
means <- colMeans(ds_beta_ESG)
means[1:5]

# - Long-short portfolio beta ESG
long_ds_beta_ESG <- rowSums(ds_beta_ESG[, seq(5, 25, 5)]) / 5
short_ds_beta_ESG <- rowSums(ds_beta_ESG[, seq(1, 20, 5)]) / 5
ls_ds_beta_ESG <- long_ds_beta_ESG - short_ds_beta_ESG
sd_ls_ds_beta_ESG <- sqrt(var(ls_ds_beta_ESG))
mean_ls_ds_beta_ESG <- mean(ls_ds_beta_ESG)
print(mean_ls_ds_beta_ESG)
sr_ls_ds_beta_ESG <- mean_ls_ds_beta_ESG / sd_ls_ds_beta_ESG
print(sr_ls_ds_beta_ESG)

# - Correlation between long-short and estimated ESG factor
cor_ls_ds_beta_ESG <- cor(ls_ds_beta_ESG, coefs_3Factor[, 3])
print(cor_ls_ds_beta_ESG)

plot(
  ls_ds_beta_ESG,
  coefs_3Factor[, 3],
  pch = 20,
  col = "blue",
  xlab = "Long-short beta ESG portfolio",
  ylab = "Estimated ESG factor",
  xlim = c(-0.22, 0.22),
  ylim = c(-0.05, 0.04),
)

# - Long-short portfolio beta ESGU
ds_beta_ESGU <- matrix(0, nrow = 24, ncol = 25)

for (t in 1:24) {
  order_beta <- order(stock_beta[t, ])
  re_beta_ordered <- excess_stock_ret[T_beginning + t, order_beta]
  esgu_beta_ordered <- ESGU[t, order_beta]
  
  for (m in 1:5) {
    M_L <- 1 + (m - 1) * 100
    M_H <- m * 100
    
    re_beta_group_m <- re_beta_ordered[M_L:M_H]
    order_esgu <- order(esgu_beta_ordered[M_L:M_H])
    
    re_beta_group_m_esgu_ordered <- re_beta_group_m[order_esgu]
    
    for (n in 1:5) {
      N_L <- 1 + (n - 1) * 20
      N_H <- n * 20
      colnumber <- 1 + (m - 1) * 5 + (n - 1)
      ds_beta_ESGU[t, colnumber] <- mean(re_beta_group_m_esgu_ordered[N_L:N_H])
    }
    
  }
}
means <- colMeans(ds_beta_ESGU)
means[1:5]

# - Long-short portfolio beta ESGU
long_ds_beta_ESGU <- rowSums(ds_beta_ESGU[, seq(5, 25, 5)]) / 5
short_ds_beta_ESGU <- rowSums(ds_beta_ESGU[, seq(1, 20, 5)]) / 5
ls_ds_beta_ESGU <- long_ds_beta_ESGU - short_ds_beta_ESGU
sd_ls_ds_beta_ESGU <- sqrt(var(ls_ds_beta_ESGU))
mean_ls_ds_beta_ESGU <- mean(ls_ds_beta_ESGU)
print(mean_ls_ds_beta_ESGU)
sr_ls_ds_beta_ESGU <- mean_ls_ds_beta_ESGU / sd_ls_ds_beta_ESGU
print(sr_ls_ds_beta_ESGU)

# - Correlation between long-short and estimated ESGU factor
cor_ls_ds_beta_ESGU <- cor(ls_ds_beta_ESGU, coefs_3Factor[, 4])
print(cor_ls_ds_beta_ESGU)

plot(
  ls_ds_beta_ESGU,
  coefs_3Factor[, 4],
  pch = 20,
  col = "blue",
  xlab = "Long-short beta ESG portfolio",
  ylab = "Estimated ESGU factor",
  xlim = c(-0.1, 0.15),
  ylim = c(-0.05, 0.1),
)
