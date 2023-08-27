rstan::extract(object = clrs_2023, 'logelr')$logelr |> mean() |> round(3)
rstan::extract(object = clrs_2023, 'alpha')$alpha |> apply(MARGIN = 2, 
  FUN = mean) |> round(3)
rstan::extract(object = clrs_2023, 'beta')$beta |> apply(MARGIN = 2, 
  FUN = mean) |> round(3)
rstan::extract(object = clrs_2023, 'sig')$sig |> apply(MARGIN = 2, 
  FUN = mean) |> round(3)

# w = 9, d = 10
logprem <- train_df$EarnedPremNet_C[train_df$AccidentYear == 1996] |> 
  mean() |> log()
ults <- rlnorm(n = 12e3, meanlog = logprem + 
    rstan::extract(object = clrs_2023, 'logelr')$logelr + 
    rstan::extract(object = clrs_2023, 'alpha')$alpha[, 9] +
    rstan::extract(object = clrs_2023, 'beta')$beta[, 10],
  sdlog = rstan::extract(object = clrs_2023, 'sig')$sig[, 10])
hist(ults, main = 'Ultimates for 1996')
mean(ults)
