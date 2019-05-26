library(Lock5withR)

race_names = as.data.frame(table(SpeedDating$RaceF))
decisionF_names = as.data.frame(table(SpeedDating$DecisionFemale))
decisionM_names = as.data.frame(table(SpeedDating$DecisionMale))

race <- SpeedDating$RaceF[!is.na(SpeedDating$RaceF)]
decisionF <- SpeedDating$DecisionFemale[!is.na(SpeedDating$DecisionFemale)]
decisionM <- SpeedDating$DecisionMale[!is.na(SpeedDating$DecisionMale)]
age <- SpeedDating$AgeF[!is.na(SpeedDating$AgeF)]
like <- SpeedDating$LikeM[!is.na(SpeedDating$LikeM)]
decF <- SpeedDating$DecisionFemale[!is.na(SpeedDating$DecisionF)]
decM <- SpeedDating$DecisionMale[!is.na(SpeedDating$DecisionM)]

n <- length(decisionM)
anpha <- 0.05
conf_int <- 1 - anpha

# race
# for (name_var in race_names$Var1) {
#   print(name_var)
#   sum_calc <- sum(race == name_var, na.rm = TRUE)
#   print(prop.test(sum_calc, n, conf.level = conf_int, alternative = "two.sided"))
# }

calc_Proportion_Statistical_Test <- function(sample_size, total_size, p0, anpha, alt){
  p_hat <- sample_size/total_size
  se <- sqrt(p0*(1-p0)/total_size)
  z <- (p_hat - p0)/se
  if (alt == "two.sided"){
    p_value <- 2*(1 - pnorm(z))
    crit_val <- qnorm(1- anpha/2)  
  }else{
    p_value <- 1 - pnorm(z)
    crit_val <- qnorm(1- anpha)  
  }
  
  ret1 <- (p_value < anpha)
  ret2 <- (crit_val < z)

  ret_table <- matrix(c(p_hat, se, z, p_value, ret1, ret2), ncol=6, byrow=TRUE)
  colnames(ret_table) <- c("p_hat","se", "z", "pvalue", "[p_value < anpha]", "[crit_val < z]")
  rownames(ret_table) <- c("value")
  ret_table <- as.table(ret_table)
  return (ret_table)
}

cauc_size <- sum(race == "Caucasian")
n <- length(race) 
calc_Proportion_Statistical_Test(cauc_size, n, p0 = 0.5, anpha, alt = "two.sided")
prop.test(cauc_size, n, p = 0.5, conf.level = conf_int, alternative = "two.sided")

# decisionF
# for (name_var in decisionF_names$Var1) {
#   print(name_var)
#   sum_calc <- sum(decisionF == name_var, na.rm = TRUE)
#   print(prop.test(sum_calc, n, conf.level = conf_int, alternative = "two.sided"))
# }

(decisionF_no_size <- sum(decisionF == "No"))
calc_Proportion_Statistical_Test(decisionF_no_size, n, p0 = 0.5, anpha, alt="greater")
prop.test(decisionF_no_size, n, p = 0.5, conf.level = conf_int, alternative = "greater")

# decisionM,
# for (name_var in decisionM_names$Var1) {
#   print(name_var)
#   sum_calc <- sum(decisionM == name_var, na.rm = TRUE)
#   print(prop.test(sum_calc, n, conf.level = conf_int, alternative = "two.sided"))
# }
decisionM_yes_size <- sum(decisionM == "Yes")
calc_Proportion_Statistical_Test(decisionM_yes_size, n, p0 = 0.5, anpha, alt= "greater")
prop.test(decisionM_yes_size, n, p = 0.5, conf.level = conf_int, alternative = "greater")

# age
calc_Mean_Statistical_Test <- function(sample_size, mu0, anpha, alt){
  n <- length(sample_size)
  x_bar <- mean(sample_size, na.rm = TRUE)
  s <- sd(sample_size, na.rm = TRUE)
  se <- s/sqrt(n)
  t <- (x_bar - mu0)/se
  if (alt == "two.sided"){
    p_value <- 2*(1 - pt(t, df = n - 1))
    crit_val <- qt(1 - anpha/2, df = n - 1)
  }else{
    p_value <- (1 - pt(t, df = n-1))
    crit_val <- qt(1 - anpha, df = n - 1)
  }
  
  ret1 <- (p_value < anpha)
  ret2 <- (crit_val < t)
  
  ret_table <- matrix(c(x_bar, s, se, t, p_value, ret1, ret2), ncol=7, byrow=TRUE)
  colnames(ret_table) <- c("x_bar", "s", "se", "t", "p_value", "[p_value < anpha]", "[crit_val < t]")
  rownames(ret_table) <- c("Value")
  ret_table <- as.table(ret_table)
  return (ret_table)
}

anpha = 0.05
mu0 = 26
calc_Mean_Statistical_Test(age, mu0, anpha, alt="greater")
t.test(age, mu = mu0, conf.level = 1-anpha, alternative = "greater")
  

#like
anpha = 0.05
mu0 = 6.5
calc_Mean_Statistical_Test(like, mu0, anpha, alt="two.sided")
t.test(like, mu = mu0, conf.level = conf_int, alternative = "two.sided")
###################################################################################
# DecisionM và DecisionF
###################################################################################
anpha = 0.05
alt="greater"

p0 = 0
n1 <- length(decisionM)
n2 <- length(decisionF)

p_hat1 <- sum(decisionM == "Yes")/n1
p_hat2 <- sum(decisionF == "Yes")/n2
denta <- p_hat1-p_hat2
p_hat <- (p_hat1*n1 + p_hat2*n2)/(n1+n2)

se <- sqrt(p_hat*(1-p_hat)/n1 + p_hat*(1-p_hat)/n2)
z <- (denta - p0)/se
p_value <- 1 - pnorm(z)
crit_val <- qnorm(1- anpha)

ret1 <- (p_value < anpha)
ret2 <- (crit_val < z)

ret_table <- matrix(c(p_hat1, p_hat2, p_hat, denta, se, z, p_value, ret1, ret2), ncol=9, byrow=TRUE)
colnames(ret_table) <- c("p_hat1", "p_hat2", "p_hat","p1-p2","se", "z", "pvalue", "[p_value < anpha]", "[crit_val < z]")
rownames(ret_table) <- c("value")
(ret_table <- as.table(ret_table))

(denta + c(-z*se, z*se))

###################################################################################
#LikeM và RaceF
###################################################################################
anpha <- 0.05

alt = "greater"
caucLikeM <- subset(SpeedDating, RaceF=="Caucasian", select=c(LikeM))[[1]]
not_caucLikeM <- subset(SpeedDating, !(RaceF=="Caucasian"), select=c(LikeM))[[1]]

mu0 <- 0

x_bar1 <- mean(caucLikeM, na.rm=TRUE)
x_bar2 <- mean(not_caucLikeM, na.rm=TRUE)
mu <- x_bar1 - x_bar2
n1 <- length(caucLikeM)
n2 <- length(not_caucLikeM)
s1 <- sd(caucLikeM, na.rm = TRUE)
s2 <- sd(not_caucLikeM, na.rm = TRUE)
se <- sqrt((s1*s1)/n1 + (s2*s2)/n2)

t <- (mu - mu0)/se
n <- min(n1, n2)

p_value <- (1 - pt(t, df = n-1))
crit_val <- qt(1 - anpha, df = n - 1)

ret1 <- (p_value < anpha)
ret2 <- (crit_val < t)

ret_table <- matrix(c(x_bar1, x_bar2, mu, s1, s2, se, t, p_value, ret1, ret2), ncol=10, byrow=TRUE)
colnames(ret_table) <- c("x_bar1", "x_bar2", "mu", "s1", "s2", "se", "t", "p_value", "[p_value < anpha]", "[crit_val < t]")
rownames(ret_table) <- c("Value")
(ret_table <- as.table(ret_table))

(mu + c(-t*se, t*se))
###################################################################################
#LikeM và AgeF
###################################################################################
anpha <- 0.05

alt = "greater"
less28_LikeM <- subset(SpeedDating, AgeF<=28, select=c(LikeM))[[1]]
greater28_LikeM <- subset(SpeedDating, AgeF>28, select=c(LikeM))[[1]]

mu0 <- 0

x_bar1 <- mean(less28_LikeM, na.rm=TRUE)
x_bar2 <- mean(greater28_LikeM, na.rm=TRUE)
mu <- x_bar1 - x_bar2
n1 <- length(less28_LikeM)
n2 <- length(greater28_LikeM)
s1 <- sd(less28_LikeM, na.rm = TRUE)
s2 <- sd(greater28_LikeM, na.rm = TRUE)
se <- sqrt((s1*s1)/n1 + (s2*s2)/n2)

t <- (mu - mu0)/se
n <- min(n1, n2)

p_value <- (1 - pt(t, df = n-1))
crit_val <- qt(1 - anpha, df = n - 1)

ret1 <- (p_value < anpha)
ret2 <- (crit_val < t)

ret_table <- matrix(c(x_bar1, x_bar2, mu, s1, s2, se, t, p_value, ret1, ret2), ncol=10, byrow=TRUE)
colnames(ret_table) <- c("x_bar1", "x_bar2", "mu", "s1", "s2", "se", "t", "p_value", "[p_value < anpha]", "[crit_val < t]")
rownames(ret_table) <- c("Value")
(ret_table <- as.table(ret_table))

(mu + c(-t*se, t*se))