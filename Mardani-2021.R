#' ---
#' title: "Trustworthiness checks of Mardani 2021"
#' author: "R-Code by Martin Plöderl" 
#' date: "`r format(Sys.time(), '%d %B, %Y')`"
#' output:
#' html_document:
#' code_folding: hide
#' highlight: haddock
#' theme: flatly
#' toc: yes
#' toc_float: yes
#' ---


# paper here: https://link.springer.com/article/10.1186/s12888-021-03592-6   

library("rsprite2")
library("esc")

options(scipen = 999)

# Calculate the p-value from means
pfromt = function(mean1,sd1,n1,mean2,sd2,n2){
  t_stat <- (mean1 - mean2) / sqrt((sd1^2 / n1) + (sd2^2 / n2))
  df <- n1 + n2 - 2
  p_value <- 2 * pt(-abs(t_stat), df)
  p = paste0("p = ", p_value)
  return(p)
}

# Calculate d with CI
cohendci = function(mean1,sd1,n1,mean2,sd2,n2){
  cd = esc_mean_sd(grp1m = mean1, grp1sd = sd1, grp1n = n1,
                   grp2m = mean2, grp2sd = sd2, grp2n = n2, 
                   es.type="d")
  cdci=paste0(round(cd$es,2), " [", 
              round(cd$ci.lo,2)," to ",
              round(cd$ci.hi,2),"]")
  return(cdci)
}


# Age, Table 1
GRIMMER_test(mean = 30.33, sd = 8.63, n_obs = 15) # consistent
GRIMMER_test(mean = 32.06, sd = 9.43, n_obs = 15) # inconsistent !!!

# Depression, Table 3
# Baseline
GRIMMER_test(mean = 15.60, sd = 11.28, n_obs = 15) # consistent
GRIMMER_test(mean = 21.40, sd = 8.10, n_obs = 15) # consistent

# Post-Test
mean1 = 13.53; sd1=22.02; mean2 = 1.80; sd2=2.21; n1=n2=15
GRIMMER_test(mean = 13.53, sd = 22.02, n_obs = 15) # consistent
GRIMMER_test(mean = 1.80, sd = 2.21, n_obs = 15) # consistent
pfromt(mean1=mean1, sd1=sd1, n1=n1, 
        mean2=mean2, sd2=sd2, n2=n2) # p = 0.0495
cohendci(mean1=mean1, sd1=sd1, n1=n1, 
         mean2=mean2, sd2=sd2, n2=n2) # 0.75 [0.01 to 1.49]"

# FU
mean1 = 11.93; sd1 = 8.38;  n1=n2=15; mean2 = 1.46; sd2 = 2.32
GRIMMER_test(mean = 11.93, sd = 8.38, n_obs = 15) # consistent
GRIMMER_test(mean = 1.46, sd = 2.32, n_obs = 15) # inconsistent !!!
pfromt(mean1=mean1, sd1=sd1, n1=n1, 
       mean2=mean2, sd2=sd2, n2=n2) # p = 6.96-05
cohendci(mean1=mean1, sd1=sd1, n1=n1, 
       mean2=mean2, sd2=sd2, n2=n2) # "1.7 [0.87 to 2.54]"




