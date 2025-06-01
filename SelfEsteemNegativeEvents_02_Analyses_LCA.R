##################################################
######### 02 Latent Class Analyses (LCA) #########
##################################################

#### 1. Preparations

# Set working directory

input <- "/Users/Author/Desktop/Documents/NLE&SE/Analyses"

setwd(input)

# Load data 

load("NLESE_long.RData")

# Load relevant packages

library(tidyverse)
library(lcmm)

# Create numeric "ID" column (needed for HLME)

data_long$idnum <- as.numeric(factor(data_long$id))

# Remove subjects that have both NA's in self_esteem and NA's in the time variables.

data_long_clean <- data_long %>%
  filter(!is.na(self_esteem) & !is.na(time.lin))

# Check number of subjects

print(data_long_clean %>% summarise(num_sujects = n_distinct(idnum)))

### 2. Linear LCGA (no random effects)

set.seed(2024)
                          
lcga1 <- hlme(self_esteem ~ time.lin , subject = "idnum", ng=1, data = data_long_clean)

gridlcga2 <- gridsearch(hlme(self_esteem ~ time.lin , subject = "idnum", ng=2, mixture = ~ time.lin, nwg = FALSE,
                             data = data_long_clean, B = random(lcga1)), rep = 50, maxiter = 100, minit = lcga1)

gridlcga3 <- gridsearch(hlme(self_esteem ~ time.lin , subject = "idnum", ng=3, mixture = ~ time.lin, nwg = FALSE,
                             data = data_long_clean, B = random(lcga1)), rep = 50, maxiter = 100, minit = lcga1)

gridlcga4 <- gridsearch(hlme(self_esteem ~ time.lin , subject = "idnum", ng=4, mixture = ~ time.lin, nwg = FALSE,
                             data = data_long_clean, B = random(lcga1)), rep = 50, maxiter = 100, minit = lcga1)

gridlcga5 <- gridsearch(hlme(self_esteem ~ time.lin , subject = "idnum", ng=5, mixture = ~ time.lin, nwg = FALSE,
                             data = data_long_clean, B = random(lcga1)), rep = 50, maxiter = 100, minit = lcga1)

gridlcga6 <- gridsearch(hlme(self_esteem ~ time.lin , subject = "idnum", ng=6, mixture = ~ time.lin, nwg = FALSE,
                             data = data_long_clean, B = random(lcga1)), rep = 50, maxiter = 100, minit = lcga1)

gridlcga7 <- gridsearch(hlme(self_esteem ~ time.lin , subject = "idnum", ng=7, mixture = ~ time.lin, nwg = FALSE,
                             data = data_long_clean, B = random(lcga1)), rep = 50, maxiter = 100, minit = lcga1)

#Summary statistics of the fitted lcga models

summary(lcga1)
summary(gridlcga2)
summary(gridlcga3)
summary(gridlcga4)
summary(gridlcga5)
summary(gridlcga6)
summary(gridlcga7)

summarytable(gridlcga2, gridlcga3, gridlcga4, gridlcga5, gridlcga6, gridlcga7,
             which = c("G", "loglik", "npm", "BIC", "%class", "entropy"))

postprob(lcga1)
postprob(gridlcga2)
postprob(gridlcga3)
postprob(gridlcga4)
postprob(gridlcga5)
postprob(gridlcga6)
postprob(gridlcga7)

# Visual representation of the class specific mean predicted trajectories

plot((predictY(lcga11, data_long_clean, var.time = "time.lin")), lty = 1, lwd = 1, type = "l", col = rainbow(2), ylim = c(-2, 2), xlab = "Weeks after event", ylab = "Self-esteem", main = "Class-specific mean predicted trajectories in linear LCGA1")
plot((predictY(gridlcga2, data_long_clean, var.time = "time.lin")), lty = 1, lwd = 1, type = "l", col = rainbow(2), ylim = c(-2, 2), xlab = "Weeks after event", ylab = "Self-esteem", main = "Class-specific mean predicted trajectories in linear LCGA2")
plot((predictY(gridlcga3, data_long_clean, var.time = "time.lin")), lty = 1, lwd = 1, type = "l", col = rainbow(3), ylim = c(-2, 2), xlab = "Weeks after event", ylab = "Self-esteem", main = "Class-specific mean predicted trajectories in linear LCGA3")     
plot((predictY(gridlcga4, data_long_clean, var.time = "time.lin")), lty = 1, lwd = 1, type = "l", col = rainbow(4), ylim = c(-2, 2), xlab = "Weeks after event", ylab = "Self-esteem", main = "Class-specific mean predicted trajectories in linear LCGA4")          
plot((predictY(gridlcga5, data_long_clean, var.time = "time.lin")), lty = 1, lwd = 1, type = "l", col = rainbow(5), ylim = c(-2, 2), xlab = "Weeks after event", ylab = "Self-esteem", main = "Class-specific mean predicted trajectories in linear LCGA5")     
plot((predictY(gridlcga6, data_long_clean, var.time = "time.lin")), lty = 1, lwd = 1, type = "l", col = rainbow(6), ylim = c(-2, 2), xlab = "Weeks after event", ylab = "Self-esteem", main = "Class-specific mean predicted trajectories in linear LCGA6")    
plot((predictY(gridlcga7, data_long_clean, var.time = "time.lin")), lty = 1, lwd = 1, type = "l", col = rainbow(7), ylim = c(-2, 2), xlab = "Weeks after event", ylab = "Self-esteem", main = "Class-specific mean predicted trajectories in linear LCGA7")     

#### 3. Linear LGMM (class-specific random intercepts and fixed slopes)

# Var/Cov matrix expected to vary across groups, thus NWG = TRUE

set.seed(2024)

lgmm11 <- hlme(self_esteem ~ time.lin , subject = "idnum", ng = 1, random = ~ 1, data = data_long_clean)

gridlgmm12 <- gridsearch(hlme(self_esteem ~ time.lin , subject = "idnum", ng = 2, mixture = ~ time.lin, random = ~ 1, nwg = TRUE,
                             data = data_long_clean, B = random(lgmm11)), rep = 50, maxiter = 100, minit = lgmm11)

gridlgmm13 <- gridsearch(hlme(self_esteem ~ time.lin , subject = "idnum", ng = 3, mixture = ~ time.lin, random = ~ 1, nwg = TRUE,
                             data = data_long_clean, B = random(lgmm11)), rep = 50, maxiter = 100, minit = lgmm11)

gridlgmm14 <- gridsearch(hlme(self_esteem ~ time.lin , subject = "idnum", ng = 4, mixture = ~ time.lin, random = ~ 1, nwg = TRUE,
                             data = data_long_clean, B = random(lgmm11)), rep = 50, maxiter = 100, minit = lgmm11)

gridlgmm15 <- gridsearch(hlme(self_esteem ~ time.lin , subject = "idnum", ng = 5, mixture = ~ time.lin, random = ~ 1, nwg = TRUE,
                             data = data_long_clean, B = random(lgmm11)), rep = 50, maxiter = 100, minit = lgmm11)

gridlgmm16 <- gridsearch(hlme(self_esteem ~ time.lin , subject = "idnum", ng = 6, mixture = ~ time.lin, random = ~ 1, nwg = TRUE,
                             data = data_long_clean, B = random(lgmm11)), rep = 50, maxiter = 100, minit = lgmm11)

gridlgmm17 <- gridsearch(hlme(self_esteem ~ time.lin , subject = "idnum", ng = 7, mixture = ~ time.lin, random = ~ 1, nwg = TRUE,
                             data = data_long_clean, B = random(lgmm11)), rep = 50, maxiter = 100, minit = lgmm11)


#Summary statistics of the fitted lgmm models

summary(lgmm11)
summary(gridlgmm12)
summary(gridlgmm13)
summary(gridlgmm14)
summary(gridlgmm15)
summary(gridlgmm16)
summary(gridlgmm17)

summarytable(gridlgmm12, gridlgmm13, gridlgmm14, gridlgmm15, gridlgmm16, gridlgmm17,
             which = c("G", "loglik", "npm", "BIC", "%class", "entropy"))

?hlme


postprob(lgmm11)
postprob(gridlgmm12)
postprob(gridlgmm13)
postprob(gridlgmm14)
postprob(gridlgmm15)
postprob(gridlgmm16)
postprob(gridlgmm17)

# Visual representation of the class specific mean predicted trajectories

plot((predictY(lgmm11, data_long_clean, var.time = "time.lin")), lty = 1, lwd = 1, type = "l", col = rainbow(2), ylim = c(-2, 2), xlab = "Weeks after event", ylab = "Self-esteem", main = "Class-specific mean predicted trajectories in linear LGMM11")
plot((predictY(gridlgmm12, data_long_clean, var.time = "time.lin")), lty = 1, lwd = 1, type = "l", col = rainbow(2), ylim = c(-2, 2), xlab = "Weeks after event", ylab = "Self-esteem", main = "Class-specific mean predicted trajectories in linear LGMM12")
plot((predictY(gridlgmm13, data_long_clean, var.time = "time.lin")), lty = 1, lwd = 1, type = "l", col = rainbow(3), ylim = c(-2, 2), xlab = "Weeks after event", ylab = "Self-esteem", main = "Class-specific mean predicted trajectories in linear LGMM13")     
plot((predictY(gridlgmm14, data_long_clean, var.time = "time.lin")), lty = 1, lwd = 1, type = "l", col = rainbow(4), ylim = c(-2, 2), xlab = "Weeks after event", ylab = "Self-esteem", main = "Class-specific mean predicted trajectories in linear LGMM14")          
plot((predictY(gridlgmm15, data_long_clean, var.time = "time.lin")), lty = 1, lwd = 1, type = "l", col = rainbow(5), ylim = c(-2, 2), xlab = "Weeks after event", ylab = "Self-esteem", main = "Class-specific mean predicted trajectories in linear LGMM15")     
plot((predictY(gridlgmm16, data_long_clean, var.time = "time.lin")), lty = 1, lwd = 1, type = "l", col = rainbow(6), ylim = c(-2, 2), xlab = "Weeks after event", ylab = "Self-esteem", main = "Class-specific mean predicted trajectories in linear LGMM16")    
plot((predictY(gridlgmm17, data_long_clean, var.time = "time.lin")), lty = 1, lwd = 1, type = "l", col = rainbow(7), ylim = c(-2, 2), xlab = "Weeks after event", ylab = "Self-esteem", main = "Class-specific mean predicted trajectories in linear LGMM17")     

#### 4. Linear LGMM (class-specific random intercepts and random slopes)

# Var/Cov matrix expected to vary across groups, thus NWG = TRUE

set.seed(2024)

lgmm21 <- hlme(self_esteem ~ time.lin , subject = "idnum", ng = 1, random = ~ 1 + time.lin, data = data_long_clean)

gridlgmm22 <- gridsearch(hlme(self_esteem ~ time.lin , subject = "idnum", ng = 2, mixture = ~ time.lin, random = ~ 1 + time.lin,
                              nwg = TRUE, data = data_long_clean, B = random(lgmm21)), rep = 50, maxiter = 100, minit = lgmm21)

gridlgmm23 <- gridsearch(hlme(self_esteem ~ time.lin , subject = "idnum", ng = 3, mixture = ~ time.lin, random = ~ 1 + time.lin,
                              nwg = TRUE, data = data_long_clean, B = random(lgmm21)), rep = 50, maxiter = 100, minit = lgmm21)

gridlgmm24 <- gridsearch(hlme(self_esteem ~ time.lin , subject = "idnum", ng = 4, mixture = ~ time.lin, random = ~ 1 + time.lin,
                              nwg = TRUE, data = data_long_clean, B = random(lgmm21)), rep = 50, maxiter = 100, minit = lgmm21)

gridlgmm25 <- gridsearch(hlme(self_esteem ~ time.lin , subject = "idnum", ng = 5, mixture = ~ time.lin, random = ~ 1 + time.lin,
                              nwg = TRUE, data = data_long_clean, B = random(lgmm21)), rep = 50, maxiter = 100, minit = lgmm21)

gridlgmm26 <- gridsearch(hlme(self_esteem ~ time.lin , subject = "idnum", ng = 6, mixture = ~ time.lin, random = ~ 1 + time.lin,
                              nwg = TRUE, data = data_long_clean, B = random(lgmm21)), rep = 50, maxiter = 100, minit = lgmm21)

gridlgmm27 <- gridsearch(hlme(self_esteem ~ time.lin , subject = "idnum", ng = 7, mixture = ~ time.lin, random = ~ 1 + time.lin,
                              nwg = TRUE, data = data_long_clean, B = random(lgmm21)), rep = 50, maxiter = 100, minit = lgmm21)

#Summary statistics of the fitted lgmm models

summary(lgmm21)
summary(gridlgmm22)
summary(gridlgmm23)
summary(gridlgmm24)
summary(gridlgmm25)
summary(gridlgmm26)
summary(gridlgmm27)

summarytable(gridlgmm22, gridlgmm23, gridlgmm24, gridlgmm25, gridlgmm26, gridlgmm27,
             which = c("G", "loglik", "npm", "BIC", "%class", "entropy"))

postprob(lgmm21)
postprob(gridlgmm22)
postprob(gridlgmm23)
postprob(gridlgmm24)
postprob(gridlgmm25)
postprob(gridlgmm26)
postprob(gridlgmm27)

# Visual representation of the class specific mean predicted trajectories

plot((predictY(lgmm21, data_long_clean, var.time = "time.lin")), lty = 1, lwd = 1, type = "l", col = rainbow(2), ylim = c(-2, 2), xlab = "Weeks after event", ylab = "Self-esteem", main = "Class-specific mean predicted trajectories in linear LGMM21")
plot((predictY(gridlgmm22, data_long_clean, var.time = "time.lin")), lty = 1, lwd = 1, type = "l", col = rainbow(2), ylim = c(-2, 2), xlab = "Weeks after event", ylab = "Self-esteem", main = "Class-specific mean predicted trajectories in linear LGMM22")
plot((predictY(gridlgmm23, data_long_clean, var.time = "time.lin")), lty = 1, lwd = 1, type = "l", col = rainbow(3), ylim = c(-2, 2), xlab = "Weeks after event", ylab = "Self-esteem", main = "Class-specific mean predicted trajectories in linear LGMM23")     
plot((predictY(gridlgmm24, data_long_clean, var.time = "time.lin")), lty = 1, lwd = 1, type = "l", col = rainbow(4), ylim = c(-2, 2), xlab = "Weeks after event", ylab = "Self-esteem", main = "Class-specific mean predicted trajectories in linear LGMM24")          
plot((predictY(gridlgmm25, data_long_clean, var.time = "time.lin")), lty = 1, lwd = 1, type = "l", col = rainbow(5), ylim = c(-2, 2), xlab = "Weeks after event", ylab = "Self-esteem", main = "Class-specific mean predicted trajectories in linear LGMM25")     
plot((predictY(gridlgmm26, data_long_clean, var.time = "time.lin")), lty = 1, lwd = 1, type = "l", col = rainbow(6), ylim = c(-2, 2), xlab = "Weeks after event", ylab = "Self-esteem", main = "Class-specific mean predicted trajectories in linear LGMM26")    
plot((predictY(gridlgmm27, data_long_clean, var.time = "time.lin")), lty = 1, lwd = 1, type = "l", col = rainbow(7), ylim = c(-2, 2), xlab = "Weeks after event", ylab = "Self-esteem", main = "Class-specific mean predicted trajectories in linear LGMM27")     

### 5. Quadratic LCGA (no random effects)

set.seed(2024)

lcga1_quad <- hlme(self_esteem ~ time.lin+time.quad , subject = "idnum", ng=1,
              data = data_long_clean)

gridlcga2_quad <- gridsearch(hlme(self_esteem ~ time.lin+time.quad , subject = "idnum", ng=2, mixture = ~ time.lin+time.quad, nwg = FALSE,
                             data = data_long_clean, B = random(lcga1_quad)), rep = 50, maxiter = 100, minit = lcga1_quad)

gridlcga3_quad <- gridsearch(hlme(self_esteem ~ time.lin+time.quad , subject = "idnum", ng=3, mixture = ~ time.lin+time.quad, nwg = FALSE,
                             data = data_long_clean, B = random(lcga1_quad)), rep = 50, maxiter = 100, minit = lcga1_quad)

gridlcga4_quad <- gridsearch(hlme(self_esteem ~ time.lin+time.quad , subject = "idnum", ng=4, mixture = ~ time.lin+time.quad, nwg = FALSE,
                             data = data_long_clean, B = random(lcga1_quad)), rep = 50, maxiter = 100, minit = lcga1_quad)

gridlcga5_quad <- gridsearch(hlme(self_esteem ~ time.lin+time.quad , subject = "idnum", ng=5, mixture = ~ time.lin+time.quad, nwg = FALSE,
                             data = data_long_clean, B = random(lcga1_quad)), rep = 50, maxiter = 100, minit = lcga1_quad)

gridlcga6_quad <- gridsearch(hlme(self_esteem ~ time.lin+time.quad , subject = "idnum", ng=6, mixture = ~ time.lin+time.quad, nwg = FALSE,
                             data = data_long_clean, B = random(lcga1_quad)), rep = 50, maxiter = 100, minit = lcga1_quad)

gridlcga7_quad <- gridsearch(hlme(self_esteem ~ time.lin+time.quad , subject = "idnum", ng=7, mixture = ~ time.lin+time.quad, nwg = FALSE,
                             data = data_long_clean, B = random(lcga1_quad)), rep = 50, maxiter = 100, minit = lcga1_quad)

#Summary statistics of the fitted lcga models

summary(lcga1_quad)
summary(gridlcga2_quad)
summary(gridlcga3_quad)
summary(gridlcga4_quad)
summary(gridlcga5_quad)
summary(gridlcga6_quad)
summary(gridlcga7_quad)

summarytable(gridlcga2_quad, gridlcga3_quad, gridlcga4_quad, gridlcga5_quad, gridlcga6_quad, gridlcga7_quad,
             which = c("G", "loglik", "npm", "BIC", "%class", "entropy"))

postprob(lcga1_quad)
postprob(gridlcga2_quad)
postprob(gridlcga3_quad)
postprob(gridlcga4_quad)
postprob(gridlcga5_quad)
postprob(gridlcga6_quad)
postprob(gridlcga7_quad)

# Visual representation of the class specific mean predicted trajectories

plot((predictY(lcga1_quad, data_long_clean, var.time = "time.lin")), lty = 1, lwd = 1, type = "l", col = rainbow(2), ylim = c(-2, 2), xlab = "Weeks after event", ylab = "Self-esteem", main = "Class-specific mean predicted trajectories in quadratic LCGA1")
plot((predictY(gridlcga2_quad, data_long_clean, var.time = "time.lin")), lty = 1, lwd = 1, type = "l", col = rainbow(2), ylim = c(-2, 2), xlab = "Weeks after event", ylab = "Self-esteem", main = "Class-specific mean predicted trajectories in quadratic LCGA2")
plot((predictY(gridlcga3_quad, data_long_clean, var.time = "time.lin")), lty = 1, lwd = 1, type = "l", col = rainbow(3), ylim = c(-2, 2), xlab = "Weeks after event", ylab = "Self-esteem", main = "Class-specific mean predicted trajectories in quadratic LCGA3")     
plot((predictY(gridlcga4_quad, data_long_clean, var.time = "time.lin")), lty = 1, lwd = 1, type = "l", col = rainbow(4), ylim = c(-2, 2), xlab = "Weeks after event", ylab = "Self-esteem", main = "Class-specific mean predicted trajectories in quadratic LCGA4")          
plot((predictY(gridlcga5_quad, data_long_clean, var.time = "time.lin")), lty = 1, lwd = 1, type = "l", col = rainbow(5), ylim = c(-2, 2), xlab = "Weeks after event", ylab = "Self-esteem", main = "Class-specific mean predicted trajectories in quadratic LCGA5")     
plot((predictY(gridlcga6_quad, data_long_clean, var.time = "time.lin")), lty = 1, lwd = 1, type = "l", col = rainbow(6), ylim = c(-2, 2), xlab = "Weeks after event", ylab = "Self-esteem", main = "Class-specific mean predicted trajectories in quadratic LCGA6")     
plot((predictY(gridlcga7_quad, data_long_clean, var.time = "time.lin")), lty = 1, lwd = 1, type = "l", col = rainbow(7), ylim = c(-2, 2), xlab = "Weeks after event", ylab = "Self-esteem", main = "Class-specific mean predicted trajectories in quadratic LCGA7")     

#### 6. Quadratic LGMM (class-specific random intercepts and fixed slopes)

# Var/Cov matrix expected to vary across groups, thus NWG = TRUE

set.seed(2024)

lgmm11_quad <- hlme(self_esteem ~ time.lin+time.quad , subject = "idnum", ng = 1, random = ~ 1, data = data_long_clean)

gridlgmm12_quad <- gridsearch(hlme(self_esteem ~ time.lin+time.quad , subject = "idnum", ng = 2, mixture = ~ time.lin+time.quad, random = ~ 1, nwg = TRUE,
                              data = data_long_clean, B = random(lgmm11)), rep = 50, maxiter = 100, minit = lgmm11_quad)

gridlgmm13_quad <- gridsearch(hlme(self_esteem ~ time.lin+time.quad , subject = "idnum", ng = 3, mixture = ~ time.lin+time.quad, random = ~ 1, nwg = TRUE,
                              data = data_long_clean, B = random(lgmm11)), rep = 50, maxiter = 100, minit = lgmm11_quad)

gridlgmm14_quad <- gridsearch(hlme(self_esteem ~ time.lin+time.quad , subject = "idnum", ng = 4, mixture = ~ time.lin+time.quad, random = ~ 1, nwg = TRUE,
                              data = data_long_clean, B = random(lgmm11)), rep = 50, maxiter = 100, minit = lgmm11_quad)

gridlgmm15_quad <- gridsearch(hlme(self_esteem ~ time.lin+time.quad , subject = "idnum", ng = 5, mixture = ~ time.lin+time.quad, random = ~ 1, nwg = TRUE,
                              data = data_long_clean, B = random(lgmm11)), rep = 50, maxiter = 100, minit = lgmm11_quad)

gridlgmm16_quad <- gridsearch(hlme(self_esteem ~ time.lin+time.quad , subject = "idnum", ng = 6, mixture = ~ time.lin+time.quad, random = ~ 1, nwg = TRUE,
                              data = data_long_clean, B = random(lgmm11)), rep = 50, maxiter = 100, minit = lgmm11_quad)

gridlgmm17_quad <- gridsearch(hlme(self_esteem ~ time.lin+time.quad , subject = "idnum", ng = 7, mixture = ~ time.lin+time.quad, random = ~ 1, nwg = TRUE,
                              data = data_long_clean, B = random(lgmm11)), rep = 50, maxiter = 100, minit = lgmm11_quad)


#Summary statistics of the fitted lgmm models

summary(lgmm11_quad)
summary(gridlgmm12_quad)
summary(gridlgmm13_quad)
summary(gridlgmm14_quad)
summary(gridlgmm15_quad)
summary(gridlgmm16_quad)
summary(gridlgmm17_quad)

summarytable(gridlgmm12_quad, gridlgmm13_quad, gridlgmm14_quad, gridlgmm15_quad, gridlgmm16_quad, gridlgmm17_quad,
             which = c("G", "loglik", "npm", "BIC", "%class", "entropy"))

postprob(lgmm11_quad)
postprob(gridlgmm12_quad)
postprob(gridlgmm13_quad)
postprob(gridlgmm14_quad)
postprob(gridlgmm15_quad)
postprob(gridlgmm16_quad)
postprob(gridlgmm17_quad)

# Visual representation of the class specific mean predicted trajectories

plot((predictY(lgmm11_quad, data_long_clean, var.time = "time.lin")), lty = 1, lwd = 1, type = "l", col = rainbow(2), ylim = c(-2, 2), xlab = "Weeks after event", ylab = "Self-esteem", main = "Class-specific mean predicted trajectories in quadratic LGMM11")
plot((predictY(gridlgmm12_quad, data_long_clean, var.time = "time.lin")), lty = 1, lwd = 1, type = "l", col = rainbow(2), ylim = c(-2, 2), xlab = "Weeks after event", ylab = "Self-esteem", main = "Class-specific mean predicted trajectories in quadratic LGMM12")
plot((predictY(gridlgmm13_quad, data_long_clean, var.time = "time.lin")), lty = 1, lwd = 1, type = "l", col = rainbow(3), ylim = c(-2, 2), xlab = "Weeks after event", ylab = "Self-esteem", main = "Class-specific mean predicted trajectories in quadratic LGMM13")     
plot((predictY(gridlgmm14_quad, data_long_clean, var.time = "time.lin")), lty = 1, lwd = 1, type = "l", col = rainbow(4), ylim = c(-2, 2), xlab = "Weeks after event", ylab = "Self-esteem", main = "Class-specific mean predicted trajectories in quadratic LGMM14")          
plot((predictY(gridlgmm15_quad, data_long_clean, var.time = "time.lin")), lty = 1, lwd = 1, type = "l", col = rainbow(5), ylim = c(-2, 2), xlab = "Weeks after event", ylab = "Self-esteem", main = "Class-specific mean predicted trajectories in quadratic LGMM15")     
plot((predictY(gridlgmm16_quad, data_long_clean, var.time = "time.lin")), lty = 1, lwd = 1, type = "l", col = rainbow(6), ylim = c(-2, 2), xlab = "Weeks after event", ylab = "Self-esteem", main = "Class-specific mean predicted trajectories in quadratic LGMM16")     
plot((predictY(gridlgmm17_quad, data_long_clean, var.time = "time.lin")), lty = 1, lwd = 1, type = "l", col = rainbow(7), ylim = c(-2, 2), xlab = "Weeks after event", ylab = "Self-esteem", main = "Class-specific mean predicted trajectories in quadratic LGMM17") 

#### 7. Quadratic LGMM (class-specific random intercepts and random slopes)

# Var/Cov matrix expected to vary across groups, thus NWG = TRUE

set.seed(2024)

lgmm21_quad <- hlme(self_esteem ~ time.lin+time.quad , subject = "idnum", ng = 1, random = ~ 1 + time.lin+time.quad, data = data_long_clean)

gridlgmm22_quad <- gridsearch(hlme(self_esteem ~ time.lin+time.quad , subject = "idnum", ng = 2, mixture = ~ time.lin+time.quad, random = ~ 1 + time.lin+time.quad,
                              nwg = TRUE, data = data_long_clean, B = random(lgmm21)), rep = 50, maxiter = 100, minit = lgmm21_quad)

gridlgmm23_quad <- gridsearch(hlme(self_esteem ~ time.lin+time.quad , subject = "idnum", ng = 3, mixture = ~ time.lin+time.quad, random = ~ 1 + time.lin+time.quad,
                              nwg = TRUE, data = data_long_clean, B = random(lgmm21)), rep = 50, maxiter = 100, minit = lgmm21_quad)

gridlgmm24_quad <- gridsearch(hlme(self_esteem ~ time.lin+time.quad , subject = "idnum", ng = 4, mixture = ~ time.lin+time.quad, random = ~ 1 + time.lin+time.quad,
                              nwg = TRUE, data = data_long_clean, B = random(lgmm21)), rep = 50, maxiter = 100, minit = lgmm21_quad)

gridlgmm25_quad <- gridsearch(hlme(self_esteem ~ time.lin+time.quad, subject = "idnum", ng = 5, mixture = ~ time.lin+time.quad, random = ~ 1 + time.lin+time.quad,
                              nwg = TRUE, data = data_long_clean, B = random(lgmm21)), rep = 50, maxiter = 100, minit = lgmm21_quad)

gridlgmm26_quad <- gridsearch(hlme(self_esteem ~ time.lin+time.quad , subject = "idnum", ng = 6, mixture = ~ time.lin+time.quad, random = ~ 1 + time.lin+time.quad,
                              nwg = TRUE, data = data_long_clean, B = random(lgmm21)), rep = 50, maxiter = 100, minit = lgmm21_quad)

gridlgmm27_quad <- gridsearch(hlme(self_esteem ~ time.lin+time.quad , subject = "idnum", ng = 7, mixture = ~ time.lin+time.quad, random = ~ 1 + time.lin+time.quad,
                              nwg = TRUE, data = data_long_clean, B = random(lgmm21)), rep = 50, maxiter = 100, minit = lgmm21_quad)

#Summary statistics of the fitted lgmm models

summary(lgmm21_quad)
summary(gridlgmm22_quad)
summary(gridlgmm23_quad)
summary(gridlgmm24_quad)
summary(gridlgmm25_quad)
summary(gridlgmm26_quad)
summary(gridlgmm27_quad)

summarytable(gridlgmm22_quad, gridlgmm23_quad, gridlgmm24_quad, gridlgmm25_quad, gridlgmm26_quad, gridlgmm27_quad,
             which = c("G", "loglik", "npm", "BIC", "%class", "entropy"))

postprob(lgmm21_quad)
postprob(gridlgmm22_quad)
postprob(gridlgmm23_quad)
postprob(gridlgmm24_quad)
postprob(gridlgmm25_quad)
postprob(gridlgmm26_quad)
postprob(gridlgmm27_quad)

# Visual representation of the class specific mean predicted trajectories

plot((predictY(lgmm21_quad, data_long_clean, var.time = "time.lin")), lty = 1, lwd = 1, type = "l", col = rainbow(2), ylim = c(-2, 2), xlab = "Weeks after event", ylab = "Self-esteem", main = "Class-specific mean predicted trajectories in quadratic LGMM21")
plot((predictY(gridlgmm22_quad, data_long_clean, var.time = "time.lin")), lty = 1, lwd = 1, type = "l", col = rainbow(2), ylim = c(-2, 2), xlab = "Weeks after event", ylab = "Self-esteem", main = "Class-specific mean predicted trajectories in quadratic LGMM22")
plot((predictY(gridlgmm23_quad, data_long_clean, var.time = "time.lin")), lty = 1, lwd = 1, type = "l", col = rainbow(3), ylim = c(-2, 2), xlab = "Weeks after event", ylab = "Self-esteem", main = "Class-specific mean predicted trajectories in quadratic LGMM23")     
plot((predictY(gridlgmm24_quad, data_long_clean, var.time = "time.lin")), lty = 1, lwd = 1, type = "l", col = rainbow(4), ylim = c(-2, 2), xlab = "Weeks after event", ylab = "Self-esteem", main = "Class-specific mean predicted trajectories in quadratic LGMM24")          
plot((predictY(gridlgmm25_quad, data_long_clean, var.time = "time.lin")), lty = 1, lwd = 1, type = "l", col = rainbow(5), ylim = c(-2, 2), xlab = "Weeks after event", ylab = "Self-esteem", main = "Class-specific mean predicted trajectories in quadratic LGMM25")     
plot((predictY(gridlgmm26_quad, data_long_clean, var.time = "time.lin")), lty = 1, lwd = 1, type = "l", col = rainbow(6), ylim = c(-2, 2), xlab = "Weeks after event", ylab = "Self-esteem", main = "Class-specific mean predicted trajectories in quadratic LGMM26")    
plot((predictY(gridlgmm27_quad, data_long_clean, var.time = "time.lin")), lty = 1, lwd = 1, type = "l", col = rainbow(7), ylim = c(-2, 2), xlab = "Weeks after event", ylab = "Self-esteem", main = "Class-specific mean predicted trajectories in quadratic LGMM27")     


#### 8. Fully fitting best fitting / preferred model with 500 random start values and a maximum of 1000 iterations

# A linear LCGA model with four classes proved to be the best fit

set.seed(2024)

# Deviation from preregistration:
# Using months as the time unit for fully fitting the preferred latent class model instead of weeks
# This switch allows for a more meaningful interpretation of the resulting coefficients in the preferred latent class model

lcga1.m <- hlme(self_esteem ~ time.lin.m , subject = "idnum", ng=1,
              data = data_long_clean)

# Fully fit preferred model (Linear LCGA model with four classes; referred to as topmod for clarity reasons)

topmod <- gridsearch(hlme(self_esteem ~ time.lin.m , subject = "idnum", ng=4, mixture = ~ time.lin.m, nwg = FALSE,
                data = data_long_clean, B = random(lcga1.m)), rep = 500, maxiter = 1000, minit = lcga1.m)

summary(topmod)

# Plot class-specific mean predicted self-esteem trajectories in topmod

plot((predictY(topmod, data_long_clean, var.time = "time.lin.m")), lty = 1, lwd = 1, type = "l", col = rainbow(4), ylim = c(-2, 2), xlab = "Months after event", ylab = "Self-esteem", main = "Class-specific mean predicted trajectories in linear LCGA4")          

# Extract posterior probabilities for each participant in the model

pprob <- topmod$pprob

### 9. Export data

lgmm_data <- merge(data_long_clean, pprob, by = "idnum", all = T)

save(data_long_clean, file = "NLESE_long_clean.RData")
save(lgmm_data, file = "NLESE_lgmm.RData")
save(topmod, file = "NLESE_topmod.RData")

