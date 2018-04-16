library("rjags")

dat = read.csv(file="manacosts.csv", header=TRUE)
data_jags = list(numlands=dat$numlands, cmc_avg=dat$cmc_avg)

#data exploration
plot(y=dat$numlands, x=dat$cmc_avg)
plot(density(dat$numlands))
                                  #_                       _      _ 
 #_ __   ___  _ __ _ __ ___   __ _| |  _ __ ___   ___   __| | ___| |
#| '_ \ / _ \| '__| '_ ` _ \ / _` | | | '_ ` _ \ / _ \ / _` |/ _ \ |
#| | | | (_) | |  | | | | | | (_| | | | | | | | | (_) | (_| |  __/ |
#|_| |_|\___/|_|  |_| |_| |_|\__,_|_| |_| |_| |_|\___/ \__,_|\___|_|
var(dat$numlands)
# 5.1 -- use for dgamma prior

mod1_string ="
model {
    for (i in 1:length(numlands)) {
        numlands[i] ~ dnorm(mu[i], prec)
        mu[i] = b0 + b[1]*cmc_avg[i]
    }
    
    b0 ~ dnorm(0.0, 1.0/10.0)
    for (i in 1:1) {
        b[i] ~ dnorm(0.0, 1.0/10.0)
    }
    
    prec ~ dgamma(1.0/2.0, 1.0*5.0/2.0)
    sig2 = 1.0 / prec
    sig = sqrt(sig2)
}"
                                                                   
set.seed(42)
mod1 = jags.model(textConnection(mod1_string), data=data_jags, n.chains=3)
update(mod, 1e4)
params = c('b0', 'b', 'sig')
mod1_sim = coda.samples(model=mod1, variable.names=params, n.iter=5e4)
mod1_csim = as.mcmc(do.call(rbind, mod1_sim))
plot(mod1_sim, ask=TRUE)
colMeans(mod1_csim)
        #b        b0       sig 
 #2.160555 17.376887  1.845944 

model1_map = function(x){return(17.37+2.16*x)}
17.37 + 2.16 * 3.0
x = c(1:5)
y = 17.37 + 2.16 * x
#linear cmc_avg answer
#1     2     3     4     5
#19.53 21.69 23.85 26.01 28.17
#looks right!

#residual analysis
numlands_pred = 17.37 + 2.16*dat$cmc_avg
plot(numlands_pred)
plot(dat$numlands, numlands_pred)
#this plot says it all
plot(dat$cmc_avg, numlands_pred)

raftery.diag(mod1_csim)
gelman.diag(mod1_sim)
effectiveSize(mod1_csim)
dic.samples(mod1, 1e3)
"model diagnostics
> raftery.diag(mod_csim)

Quantile (q) = 0.025
Accuracy (r) = +/- 0.005
Probability (s) = 0.95 
                                           
     Burn-in  Total Lower bound  Dependence
     (M)      (N)   (Nmin)       factor (I)
 b   40       48696 3746         13.000    
 b0  27       26781 3746          7.150    
 sig 2        3700  3746          0.988    

> gelman.diag(mod_sim)
Potential scale reduction factors:

    Point est. Upper C.I.
b            1       1.01
b0           1       1.01
sig          1       1.00

Multivariate psrf

1
> effectiveSize(mod_sim)
        b        b0       sig 
 403.9961  394.1801 8751.4302 
> effectiveSize(mod_csim)
        b        b0       sig 
 396.5787  360.9120 6835.8859 
> dic.samples(mod, 1e3)
  |**************************************************| 100%
Mean deviance:  1373 
penalty 2.988 
Penalized deviance: 1376 
"

                                  #_ ____                        _      _ 
 #_ __   ___  _ __ _ __ ___   __ _| |___ \   _ __ ___   ___   __| | ___| |
#| '_ \ / _ \| '__| '_ ` _ \ / _` | | __) | | '_ ` _ \ / _ \ / _` |/ _ \ |
#| | | | (_) | |  | | | | | | (_| | |/ __/  | | | | | | (_) | (_| |  __/ |
#|_| |_|\___/|_|  |_| |_| |_|\__,_|_|_____| |_| |_| |_|\___/ \__,_|\___|_|
# normal model using manacost histogram instead of average mana cost
                                                                         
data_jags = as.list(dat)

mod_string ="
model {
    for (i in 1:length(numlands)) {
        numlands[i] ~ dnorm(mu[i], prec)
        mu[i] = b0 + b[1]*X0[i] + b[2]*X1[i] + b[3]*X2[i] + b[4]*X3[i] + b[5]*X4[i] + b[6]*X5[i]
            + b[7]*X6[i] + b[8]*X7[i] + b[9]*X8[i] + b[10]*X9[i] + b[11]*X10[i] + b[12]*X11[i]
            + b[13]*X12[i] + b[14]*X13[i]
    }
    
    b0 ~ dnorm(0.0, 1.0/10.0)
    for (i in 1:14) {
        b[i] ~ dnorm(0.0, 1.0/10.0)
    }
    
    prec ~ dgamma(1.0/2.0, 1.0*5.0/2.0)
    sig2 = 1.0 / prec
    sig = sqrt(sig2)
}"
set.seed(42)
mod = jags.model(textConnection(mod_string), data=data_jags, n.chains=3)
update(mod, 1e5) # 1e4
params = c('b0', 'b', 'sig')
mod_sim = coda.samples(model=mod, variable.names=params, n.iter=5e4) # 20e3
mod_csim = as.mcmc(do.call(rbind, mod_sim))
plot(mod_sim, ask=TRUE)
b = colMeans(mod_csim)
       #b[1]        b[2]        b[3]        b[4]        b[5]        b[6]        b[7] 
 #0.58853090  0.13273492  0.21502989  0.25418242  0.29926226  0.28938694  0.32567189 
       #b[8]        b[9]       b[10]       b[11]       b[12]       b[13]       b[14] 
 #0.06294833  0.44735465 -0.03325976  0.31197697  0.03740561  0.35615818  0.67312448 
         #b0         sig 
 #1.00393242  1.69275289 

#b from ramped up iterations
      #b[1]       b[2]       b[3]       b[4]       b[5]       b[6]       b[7]       b[8] 
#-0.8979771 -0.9186984 -0.9077249 -0.9090628 -0.9006002 -0.8857310 -0.8939167 -0.9218680 
      #b[9]      b[10]      b[11]      b[12]      b[13]      b[14]         b0        sig 
#-0.9010854 -0.9056919 -0.8328108 -0.8356099 -0.9450035 -0.8544660 56.6530761  0.4534020 


model2_map = function(b, X0, X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11, X12, X13){
    return (
           b[15] + b[1]*X0 + b[2]*X1 + b[3]*X2 + b[4]*X3 + b[5]*X4 + b[6]*X5
           + b[7]*X6 + b[8]*X7 + b[9]*X8 + b[10]*X9 + b[11]*X10 + b[12]*X11 + b[13]*X12 + b[14]*X13
       )
}
model2_map(b,0,2,10,11,5,4,3,0,0,0,0,0,0,0)
#25.01106 
model2_map(b,0,8,5,10,4,5,3,0,0,0,0,0,0,0)
#24.96143 
model2_map(b,0,8,7,13,2,2,3,1,0,0,0,0,0,0)
#23.95532 
model2_map(b,1,6,9,7,4,5,3,0,0,0,0,0,0,0)
#24.99714 
model2_map(b,0,2,20,5,4,6,0,0,0,0,0,0,0,0)
#23.19908
model2_map(b,0,16,8,12,4,0,0,0,0,0,0,0,0,0)
#red deck ties 20.18095

"
look at three similar lists:
 Young Pyromancer 5       24 2.333333  0  8 12 12  4  0  0  0  0  0   0   0   0   0
model2_map(b,0,8,12,12,4,0,0,0,0,0,0,0,0,0) => 23.89964 
   Young Pyromancer       24 2.555556  0  4 12 16  4  0  0  0  0  0   0   0   0   0
model2_map(b,0,4,12,16,4,0,0,0,0,0,0,0,0,0) => 23.93818
Younger Pyromancer2       24 2.555556  0  4 16  8  8  0  0  0  0  0   0   0   0   0
model2_map(b,0,4,16,8,8,0,0,0,0,0,0,0,0,0) => 23.97738 
possibly not responsive enough
what does model 1 predict for these?
model1_map(2.333333) => 22.41
model1_map(2.555556) => 22.89
model1s predictions look more accurate than model2
model2 most likely suffers from high variance
need to refit with holdout to perform cross validation and/or use laplace (ddexp) priors 
for model2s beta coeficients
"

raftery.diag(mod_csim)
gelman.diag(mod_sim)
effectiveSize(mod_csim)
dic.samples(mod, 1e3)
"
Mean deviance:  1309 
penalty 14.15 
Penalized deviance: 1323 
> raftery.diag(mod_csim)

Quantile (q) = 0.025
Accuracy (r) = +/- 0.005
Probability (s) = 0.95 
                                              
       Burn-in  Total  Lower bound  Dependence
       (M)      (N)    (Nmin)       factor (I)
 b[1]  180      188244 3746         50.300    
 b[2]  195      199335 3746         53.200    
 b[3]  260      340548 3746         90.900    
 b[4]  168      194320 3746         51.900    
 b[5]  126      144494 3746         38.600    
 b[6]  45       64881  3746         17.300    
 b[7]  30       31020  3746          8.280    
 b[8]  3        4290   3746          1.150    
 b[9]  4        4845   3746          1.290    
 b[10] 2        3886   3746          1.040    
 b[11] 2        3929   3746          1.050    
 b[12] 2        3886   3746          1.040    
 b[13] 2        3814   3746          1.020    
 b[14] 2        3865   3746          1.030    
 b0    144      159558 3746         42.600    
 sig   2        3740   3746          0.998    

> gelman.diag(mod_sim)
Potential scale reduction factors:

      Point est. Upper C.I.
b[1]        1.23       1.63
b[2]        1.30       1.82
b[3]        1.32       1.88
b[4]        1.28       1.77
b[5]        1.21       1.59
b[6]        1.16       1.47
b[7]        1.12       1.35
b[8]        1.03       1.10
b[9]        1.04       1.14
b[10]       1.00       1.00
b[11]       1.00       1.01
b[12]       1.00       1.01
b[13]       1.00       1.00
b[14]       1.00       1.00
b0          1.38       2.04
sig         1.00       1.00

Multivariate psrf

1.22
> effectiveSize(mod_csim)
        b[1]         b[2]         b[3]         b[4]         b[5]         b[6] 
   34.894522    16.640736    18.843360    21.759320    27.779157    42.003600 
        b[7]         b[8]         b[9]        b[10]        b[11]        b[12] 
   57.426994   186.869121   201.556042 13281.408688  6633.531890  8326.978059 
       b[13]        b[14]           b0          sig 
13979.205052 12626.770749     6.876772 12068.496801 
looks like we have super high autocorrelation until we hit b[10]

increasing the burn-in iterations from 1e3 to 1e4
and increase the number of coda.samples from 5e3 to 20e3 didn't seem to change much
... actually it did. not sure which step we had to sample more with tho. see next comment
> dic.samples(mod, 1e3)
  |**************************************************| 100%
Mean deviance:  1308 
penalty 14.18 
Penalized deviance: 1322 
> effectiveSize(mod_csim)
       b[1]        b[2]        b[3]        b[4]        b[5]        b[6]        b[7] 
   87.61618    59.97401    66.21691    83.49630   103.58503   141.15592   199.82257 
       b[8]        b[9]       b[10]       b[11]       b[12]       b[13]       b[14] 
  618.72547   742.31004 51991.46184 15876.45897 18636.76866 57205.71164 54445.09600 
         b0         sig 
   28.24582 44336.49363 
"
"scaled up params:
       Burn-in  Total  Lower bound  Dependence
       (M)      (N)    (Nmin)       factor (I)
 b[1]  68       73423  3746          19.60    
 b[2]  300      293300 3746          78.30    
 b[3]  420      469770 3746         125.00    
 b[4]  360      397000 3746         106.00    
 b[5]  308      320452 3746          85.50    
 b[6]  276      287592 3746          76.80    
 b[7]  155      177940 3746          47.50    
 b[8]  18       25110  3746           6.70    
 b[9]  36       50208  3746          13.40    
 b[10] 2        3871   3746           1.03    
 b[11] 3        4092   3746           1.09    
 b[12] 2        3985   3746           1.06    
 b[13] 2        3763   3746           1.00    
 b[14] 2        3792   3746           1.01    
 b0    341      344224 3746          91.90    
 sig   2        3831   3746           1.02    

> gelman.diag(mod_sim)
Potential scale reduction factors:

      Point est. Upper C.I.
b[1]        1.00       1.01
b[2]        1.01       1.02
b[3]        1.01       1.02
b[4]        1.01       1.02
b[5]        1.01       1.02
b[6]        1.01       1.02
b[7]        1.00       1.01
b[8]        1.00       1.00
b[9]        1.00       1.01
b[10]       1.00       1.00
b[11]       1.00       1.00
b[12]       1.00       1.00
b[13]       1.00       1.00
b[14]       1.00       1.00
b0          1.01       1.02
sig         1.00       1.00

Multivariate psrf

1
> effectiveSize(mod_csim)
       b[1]        b[2]        b[3]        b[4]        b[5]        b[6]        b[7] 
   689.4790    291.0163    273.7653    299.0344    327.7803    372.5835    448.8042 
       b[8]        b[9]       b[10]       b[11]       b[12]       b[13]       b[14] 
  1997.2758   1328.4714 121741.9950  32688.2608  54367.9601 140962.0343 126039.6454 
         b0         sig 
   255.9788  11782.8802 
dic
Mean deviance:  399.4 
penalty 16.51 
Penalized deviance: 415.9 
"

             #_                                           _      _ 
 #_ __   ___ (_)___ ___  ___  _ __    _ __ ___   ___   __| | ___| |
#| '_ \ / _ \| / __/ __|/ _ \| '_ \  | '_ ` _ \ / _ \ / _` |/ _ \ |
#| |_) | (_) | \__ \__ \ (_) | | | | | | | | | | (_) | (_| |  __/ |
#| .__/ \___/|_|___/___/\___/|_| |_| |_| |_| |_|\___/ \__,_|\___|_|
#|_|                                                               
# attempt to use the poisson model as the number of lands is technically a discrete count
mod_string = "
model {
  for (i in 1:length(numlands)) {
    numlands[i] ~ dpois(exp(lam))
  }
  lam ~ dnorm(0.0, 10.0e2)
}"

set.seed(42)
mod = jags.model(textConnection(mod_string), data=data_jags, n.chains=3)
update(mod, 1e3)
params = c('lam')
mod_sim = coda.samples(model=mod, variable.names=params, n.iter=5e3)
mod_csim = as.mcmc(do.call(rbind, mod_sim))
plot(mod_sim)

lambda = colMeans(mod_csim)
print(exp(lambda))
     #lam 
#15.25416 
#why so low? should recover the dataset average:
mean(dat$numlands)
#[1] 23.34125


             #_                     
 #_ __   ___ (_)___ ___  ___  _ __  
#| '_ \ / _ \| / __/ __|/ _ \| '_ \ 
#| |_) | (_) | \__ \__ \ (_) | | | |
#| .__/ \___/|_|___/___/\___/|_| |_|
#|_|                                
                                  #_             
 #_ __ ___  __ _ _ __ ___  ___ ___(_) ___  _ __  
#| '__/ _ \/ _` | '__/ _ \/ __/ __| |/ _ \| '_ \ 
#| | |  __/ (_| | | |  __/\__ \__ \ | (_) | | | |
#|_|  \___|\__, |_|  \___||___/___/_|\___/|_| |_|
          #|___/                                 
# attempt to use the poisson model as the number of lands is technically a discrete count
mod_string = "
model {
  for (i in 1:length(numlands)) {
    numlands[i] ~ dpois(lam[i])
    log(lam[i]) = b0 + b1*cmc_avg[i]
  }
  
  b0 ~ dnorm(0.0, 10.0e2)
  b1 ~ dnorm(0.0, 10.0e2)
}"

set.seed(42)
mod = jags.model(textConnection(mod_string), data=data_jags, n.chains=3)
update(mod, 1e3)
params = c('b0', 'b1')
mod_sim = coda.samples(model=mod, variable.names=params, n.iter=5e3)
mod_csim = as.mcmc(do.call(rbind, mod_sim))
plot(mod_sim)

colMeans(mod_csim)
       #b0        b1 
#0.9355340 0.7188221

lambda = exp(mod_csim[,'b0'] + mod_csim[,'b1']*3.0)
#land distribution for a deck with average cmc 2.0
land_dist = rpois(length(lambda), lambda)
plot(density(land_dist))
mean(land_dist) # maximum posteriori
# 22.06933
mean(20 < land_dist & land_dist < 28) #confidence bounds
# 0.4874

x = c(1:5)
y = exp(.93 + .71*x)

numlands_pred_poisson = exp(.93 + .71*dat$cmc_avg)
plot(numlands_pred_poisson)
plot(dat$numlands - numlands_pred_poisson) # residuals look normal
plot(numlands_pred_poisson, dat$numlands - numlands_pred_poisson)


#1       2        3        4        5 
#5.15517 10.48557 21.32756 43.38006 88.23467
#clearly something's wrong here

raftery.diag(mod_csim)
gelman.diag(mod_sim)
effectiveSize(mod_csim)
dic.samples(mod, 1e3)


"model diagnostics
Quantile (q) = 0.025
Accuracy (r) = +/- 0.005
Probability (s) = 0.95 
                                          
    Burn-in  Total Lower bound  Dependence
    (M)      (N)   (Nmin)       factor (I)
 b0 21       22323 3746         5.96      
 b1 18       23262 3746         6.21      

Potential scale reduction factors:

   Point est. Upper C.I.
b0       1.01       1.04
b1       1.01       1.04

Multivariate psrf

1.01

effectiveSize(mod_csim)
      b0       b1 
959.2669 994.3073 
  |**************************************************| 100%
Mean deviance:  3204 
penalty 1.278 
Penalized deviance: 3206 
"
