# Set the correct working directory (note that this needs to be the folder with
# the Stan script)

setwd('C:/Users/rajesh.sahasrabuddhe/Desktop/CLRS 2023/Concurrent Session/CLRS_2023_ST1')

dat <- readr::read_csv(file = 
    'https://www.casact.org/sites/default/files/2021-04/comauto_pos.csv')

incd353 <- dat |> 
  dplyr::filter(GRCODE == 353, DevelopmentYear <= 1997) |> 
  dplyr::mutate(incd = IncurLoss_C - BulkLoss_C) |> 
  ChainLadder::as.triangle(origin = 'AccidentYear', dev = 'DevelopmentLag', 
    value = 'incd')

mack <- ChainLadder::MackChainLadder(Triangle = incd353, est.sigma = 'Mack')
summary(mack)$ByOrigin[,c('Ultimate', 'Mack.S.E')]

paid353 <- dat |> 
  dplyr::filter(GRCODE == 353, DevelopmentYear <= 1997) |> 
  ChainLadder::as.triangle(origin = 'AccidentYear', dev = 'DevelopmentLag', 
    value = 'CumPaidLoss_C')

set.seed(12345)

ev <- ChainLadder::BootChainLadder(Triangle = paid353, R = 10e3, 
  process.distr = "od.pois")
summary(ev)$ByOrigin[,c('Mean Ultimate', 'SD IBNR')]

train_df <- dat |> 
  dplyr::filter(GRCODE == 353, DevelopmentYear <= 1997) |> 
  dplyr::mutate(incd = IncurLoss_C - BulkLoss_C)

clrs_2023 <- rstan::stan(file = 'CRC.stan', model_name = 'CLRS_2023', 
  data = list(len_data = nrow(train_df), logprem = log(train_df$EarnedPremNet_C), 
    logloss = log(train_df$incd), d = train_df$DevelopmentLag, 
    w = train_df$AccidentYear - min(train_df$AccidentYear) + 1), 
  pars = c("logelr","alpha","beta","sig","log_lik"), chains = 3, warmup = 1000, 
  iter = 5000
  )

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

#rmarkdown::render(input = 'For Rmd.R', output_format = 'word_document')
