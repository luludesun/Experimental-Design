
library(tidyverse)
load("ICPSR_37070\\DS0001\\37070-0001-Data.rda")
da37070.0001$ST1 = as.integer(da37070.0001$ST1)
da37070.0001$ID = as.integer(da37070.0001$ID)

# Transformed TREAT and SCHID to be integers

library(prettyR)
da99999.0001 = da37070.0001
lbls <- sort(levels(da99999.0001$TREAT))
lbls <- (sub("^\\([0-9]+\\) +(.+$)", "\\1", lbls))
da99999.0001$TREAT <- as.numeric(sub("^\\(0*([0-9]+)\\).+$", "\\1", da99999.0001$TREAT))
da99999.0001$TREAT <- add.value.labels(da99999.0001$TREAT, lbls)
da99999.0001 <- da99999.0001 %>% filter(TREAT != 0)

lbls <- sort(levels(da99999.0001$SCHTREAT))
lbls <- (sub("^\\([0-9]+\\) +(.+$)", "\\1", lbls))
da99999.0001$SCHTREAT <- as.numeric(sub("^\\(0*([0-9]+)\\).+$", "\\1", da99999.0001$SCHTREAT))
da99999.0001$SCHTREAT <- add.value.labels(da99999.0001$SCHTREAT, lbls)

wrist = sort(levels(da99999.0001$WRISTOW2))
wrist = (sub("^\\([0-9]+\\) +(.+$)", "\\1", wrist))
da99999.0001$WRISTOW2 <- as.numeric(sub("^\\(0*([0-9]+)\\).+$", "\\1", da99999.0001$WRISTOW2))
da99999.0001$WRISTOW2 <- add.value.labels(da99999.0001$WRISTOW2, wrist)

da99999.0001$TREAT[da99999.0001$TREAT==2]=0
da99999.0001$TREAT[da99999.0001$SCHTREAT==0]=0

# Self merge by STX


da99999.0001 <- da99999.0001 %>% drop_na(c("WRISTOW2", "SCHTREAT", "TREAT", "WRISTW2"))

wrist = da99999.0001$WRISTOW2
wrist[is.na(wrist)]=0
da99999.0001$WRISTOW2 = wrist

join <- function(da99999.0001) {
  for (i in 1:10) {
    if (i == 1) {
      merged_df <- da99999.0001 %>% drop_na(ID, TREAT, SCHID, SCHTREAT) %>% select(ID, ST1, ST2, ST3, ST4, ST5, ST6, ST7, ST8, ST9, ST10, TREAT, SCHID, SCHTREAT, WRISTOW2, STRB) 
      right = merged_df %>% select(ID, TREAT, SCHID)
      merged_df = merge(x = merged_df, y = right, by.x = c(paste0("ST", i), "SCHID"), by.y = c("ID", "SCHID"), all.x = TRUE)
      colnames(merged_df)[colnames(merged_df) == "TREAT.x"] = "i.treat"
      colnames(merged_df)[colnames(merged_df) == "TREAT.y"] = "st1.treat"
      }
    else {
      mg.before <- da99999.0001 %>% drop_na(ID, TREAT, SCHID, SCHTREAT, WRISTOW2) %>% select(ID, paste0('ST', i), TREAT, SCHID, SCHTREAT, WRISTOW2, STRB) 
      right = mg.before %>% select(ID, TREAT, SCHID)
      merged_df = merge(x = merged_df, y = right, by.x = c(paste0("ST", i), "SCHID"), by.y = c("ID", "SCHID"), all.x = TRUE)
      colnames(merged_df)[colnames(merged_df) == "TREAT"] = paste0("st",i,"treat")
    }
  }
  merged_df[is.na(merged_df)] = 0
  
  merged_df <- merged_df %>% mutate(indirect = (st1.treat + st2treat + st3treat + st4treat +
                                                  st5treat + st6treat + st7treat + st8treat +
                                                  st9treat + st10treat) >0)
  
  for (i in 1:dim(merged_df)[1]) {
    row = merged_df[i,]
    if (row$indirect != 1 & row$SCHTREAT == 1) {
      temp <- merged_df %>% filter(SCHID == row$SCHID, i.treat == 1) %>% select(ST1, ST2, ST3, ST4, ST5, ST6, ST7, ST8, ST9, ST10, i.treat)
      for (j in 1:dim(temp)[1]) {
        temp_row = temp[j,]
        if (temp_row$i.treat == 1 & (temp_row$ST1 == row$ID| 
                                     temp_row$ST2 == row$ID|
                                     temp_row$ST3 == row$ID|
                                     temp_row$ST4 == row$ID|
                                     temp_row$ST5 == row$ID|
                                     temp_row$ST6 == row$ID|
                                     temp_row$ST7 == row$ID|
                                     temp_row$ST8 == row$ID|
                                     temp_row$ST9 == row$ID|
                                     temp_row$ST10 == row$ID)) {
          merged_df[i,which(colnames(merged_df) == "indirect")] = TRUE
          break
        }
      }
    }
  }
  return(merged_df)
}

merged_df = join(da99999.0001)
exposure_map <- function(SCHTREAT, TREAT.x, TREAT.y) {
  if (SCHTREAT == 1 & TREAT.x == 1 & TREAT.y) {
    return("Direct+Indirect")
  }
  else if (SCHTREAT == 1 & TREAT.x == 0 & TREAT.y) {
    return("Indirect")
  }
  else if (SCHTREAT == 1 & TREAT.x == 1 & ! TREAT.y) {
    return("Direct")
  }
  else if (SCHTREAT == 1 & TREAT.x == 0 & ! TREAT.y) {
    return("School")
  } 
  else if (SCHTREAT == 0) {
    return("No School")
  }
}


exposure = rep(0, length(merged_df$ST1))
for (i in 1:dim(merged_df)[1]) {
  row = merged_df[i,]
  exposure[i] = exposure_map(row$SCHTREAT, row$i.treat, row$indirect)
}

merged_df = merged_df %>% mutate(exposure = exposure) #%>% group_by(exposure) %>% summarize(p = mean(WRISTOW2), n = n())

lm(WRISTOW2 ~ indirect + i.treat + SCHTREAT + indirect*i.treat + SCHTREAT*i.treat + SCHTREAT *indirect + SCHTREAT*i.treat*indirect, data = merged_df)

# Exposure probability

da99999.0002 = da99999.0001
da99999.0002$TREAT[da99999.0002$TREAT==2]=0
da99999.0002$TREAT[da99999.0002$SCHTREAT==0]=0


block_ran_school <- function(SCHRB, SCHID) {
  ran = rep(0, length(SCHRB)*4)
  for (i in 1:length(SCHRB)) {
    ran[(i*4-3):(i*4)] = sample(c(0,0,1,1),4)
  }
  df = data.frame(SCHID = SCHID, SCHTREAT = ran)
  return(df)
}

random_treat <- function(n) {
  sample(c(rep(0,n/2), rep(1, round(n/2+0.1))), n)
}

generate_treat <- function(da99999.0002) {
  schoolrand<- block_ran_school(unique(da99999.0002$SCHRB), 
                   unique(da99999.0002 %>% select(SCHID, SCHRB)) %>% arrange(SCHRB) %>% select(SCHID))
  
  students <- da99999.0002 %>% select(SCHID, ID, TREAT, STRB, ST1, ST2, ST3, ST4, ST5, ST6, ST7, ST8, ST9, ST10, WRISTOW2)
  students$index = 1:length(students$SCHID)
  # If STRB == 0, then none of the students will be assigned to treatment. So STRB == 0 is like TREAT == 0.
  program_df <- merge(schoolrand %>% filter(SCHTREAT == 1), students %>% filter(STRB != 0), by.x = c("SCHID"), by.y = c("SCHID"), all.x=FALSE, all.y = FALSE)
  program_df = program_df %>% arrange(SCHID, STRB)
  students <- merge(students, schoolrand, by=c("SCHID"))
  # 0 is control, 1 is treated. But in actual data, (0) Not treatment or control (2) Control (1) Treatment 
  random_cnt = program_df %>% group_by(SCHID, STRB) %>% summarize(n = n())
  

  
  random <- rep(0, length(program_df$SCHID))
  index = 1
  for (i in 1:length(random_cnt$SCHID)) {
    n = random_cnt[[i,3]]
    temp = random_treat(n)
    random[index:(index+n-1)] = temp
    index = index + n
  }
  
  program_df$TREAT = random
  students$TREAT = rep(0,length(students$SCHID))
  students$TREAT[as.integer(as.list(program_df %>% filter(TREAT == 1) %>% select(index))$index)] = 1
  return(students)
}

library(doFuture)
library(future)
library(foreach)
library(doRNG)

plan(multisession, workers = 16)
registerDoFuture()
R = 1600
output <- foreach(i = 1:R) %dorng% {
  students <- generate_treat(da99999.0002)
  df <- join(students)
  df$index = 1:length(df$SCHID)
  direct_indirect = rep(0, length(df$SCHID))
  direct = rep(0, length(df$SCHID))
  indirect = rep(0, length(df$SCHID))
  school = rep(0, length(df$SCHID))
  q = df %>% filter(indirect == TRUE & i.treat == 1) %>% select(index)
  q2 = df %>% filter(indirect == FALSE & i.treat == 1) %>% select(index)
  q3 = df %>% filter(indirect == TRUE & i.treat == 0) %>% select(index)
  q4 = df %>% filter(SCHTREAT == 1 & indirect == FALSE & i.treat == 0)
  direct_indirect[q$index] = 1
  direct[q2$index] = 1
  indirect[q3$index] = 1
  school[q4$index] = 1
  result <- tibble(direct_indirect, direct, indirect, school)
  result
}

saveRDS(output, "simulation.rds")

di = rep(0, length(merged_df$SCHID))
d = rep(0, length(merged_df$SCHID))
ind = rep(0, length(merged_df$SCHID))
sch = rep(0, length(merged_df$SCHID))
for (i in output){
  di = di + i$direct_indirect
  d = d + i$direct
  ind = ind + i$indirect
  sch = sch + i$school
}

merged_df$pd = (d)/(R)
merged_df$psch = (sch)/(R)
merged_df$pdi = (di)/(R)
merged_df$pind = (ind)/(R)
merged_df$noschool = rep(1/2, length(merged_df$SCHID))

merged_df$i.treat[merged_df$SCHTREAT==0] = 0

merged_df$indirect[merged_df$SCHTREAT==0] = 0

merged_df %>% summarize(mean(pdi), mean(pind), mean(psch), mean(noschool))
merged_df %>% group_by(i.treat, indirect, SCHTREAT) %>% summarize(mean(pdi), mean(pind), mean(psch), mean(noschool))
merged_df %>% group_by(i.treat, indirect, SCHTREAT) %>% summarize(min(pdi), min(pd), min(pind), min(psch), min(noschool)) # All d(k) is greater than zero
# Note, there exists someone who received indirect but not direct effect that will never received only school effect. 
# This unit is probably linked to all of the guys in the program in that school so that he or she will either receives an indirect effect or no school effect.



sum_di = 0
sum_d = 0
sum_ind = 0
sum_sch = 0
sum_noschool = 0
for (i in 1:nrow(merged_df)) {
  r <- merged_df[i,]
  if (r$i.treat == 1 & r$indirect == 1 & r$WRISTOW2 == 1) {
    sum_di = sum_di + 1/r$pdi
  } else if (r$i.treat == 1 & r$indirect == 0 & r$WRISTOW2 == 1) {
    sum_d = sum_d + 1/r$pd
  } else if (r$i.treat == 0 & r$indirect == 1 & r$WRISTOW2 ==1) {
    sum_ind = sum_ind + 1/r$pind
  } else if (r$i.treat == 0 & r$indirect ==0 & r$SCHTREAT == 1 & r$WRISTOW2 == 1) {
    sum_sch = sum_sch + 1/r$psch
  } else if (r$SCHTREAT == 0 & r$WRISTOW2 == 1) {
    sum_noschool = sum_noschool + 1/r$noschool
  }
}

sum_di/length(merged_df$SCHID) - sum_noschool/length(merged_df$SCHID)
sum_ind/length(merged_df$SCHID) - sum_noschool/length(merged_df$SCHID)
sum_d/length(merged_df$SCHID) - sum_noschool/length(merged_df$SCHID)
sum_sch/length(merged_df$SCHID) - sum_noschool/length(merged_df$SCHID)

merged_df %>% group_by(i.treat, indirect, SCHTREAT) %>% summarize(mean(WRISTOW2), n())

# Variance estimator
var_di = 0
var_d = 0
var_ind = 0
var_sch = 0
var_noschool = 0
for (i in 1:nrow(merged_df)) {
  r <- merged_df[i,]
  if (r$i.treat == 1 & r$indirect == 1 & r$WRISTOW2 == 1) {
    var_di = var_di + (1-r$pdi)*(1/r$pdi**2)
  } else if (r$i.treat == 1 & r$indirect == 0 & r$WRISTOW2 == 1) {
    var_d = var_d + (1-r$pd)*(1/r$pd**2)
  } else if (r$i.treat == 0 & r$indirect == 1 & r$WRISTOW2 ==1) {
    var_ind = var_ind + (1-r$pind)*(1/r$pind**2)
  } else if (r$i.treat == 0 & r$indirect ==0 & r$SCHTREAT == 1 & r$WRISTOW2 == 1) {
    var_sch = var_sch + (1-r$psch)*(1/r$psch**2)
  } else if (r$SCHTREAT == 0 & r$WRISTOW2 == 1) {
    var_noschool = var_noschool + (1-r$noschool)*(1/r$noschool**2)
  }
}


exposure = rep(0, length(merged_df$ST1))
for (i in 1:dim(merged_df)[1]) {
  row = merged_df[i,]
  exposure[i] = exposure_map(row$SCHTREAT, row$i.treat, row$indirect)
}

merged_df = merged_df %>% mutate(exposure = exposure) #%>% group_by(exposure) %>% summarize(p = mean(WRISTOW2), n = n())

# piik
for (i in 1:R) {
  mat = output[[i]]
  if (i == 1) {
    pi_di = mat$direct_indirect
    pi_d = mat$direct
    pi_ind = mat$indirect
    pi_sch = mat$school
  } else {
    pi_di = cbind(pi_di, mat$direct_indirect)
    pi_d = cbind(pi_d, mat$direct)
    pi_ind = cbind(pi_ind, mat$indirect)
    pi_sch = cbind(pi_sch, mat$school)
  }
}

ipi_di = pi_di %*% t(pi_di) / R
ipi_d = pi_d %*% t(pi_d) / R
ipi_ind = pi_ind %*% t(pi_ind) / R
ipi_sch = pi_sch %*% t(pi_sch) / R

merged_df$index = 1:dim(merged_df)[1]

write.csv(merged_df, "merged_df.csv")
save(ipi_di, file="ipi_di.RData")
save(ipi_d, file="ipi_d.RData")
save(ipi_ind, file="ipi_ind.RData")
save(ipi_sch, file="ipi_sch.RData")



di_df = merged_df %>% filter(exposure == "Direct+Indirect", WRISTOW2 == 1)
d_df = merged_df %>% filter(exposure == "Direct", WRISTOW2 == 1)
ind_df = merged_df %>% filter(exposure == "Indirect", WRISTOW2 == 1)
sch_df = merged_df %>% filter(exposure == "School", WRISTOW2 == 1)

cov_plus_estimate <- function(var_, df_, ipi_) {
  for (i in 1:nrow(df_)) {
    r <- df_[i,]
    
    for (j in 1:nrow(df_)) {
      if (j != i) {
        r2 <- df_[j,]
        index_i = r$index
        index_j = r2$index
        var_ = var_ + (ipi_[index_i, index_j] - ipi_[index_i, index_i]*ipi_[index_j, index_j])/ipi_[index_i, index_j]/(ipi_[index_i, index_i]*ipi_[index_j, index_j])
      
      }
    }
    return(var_)
  }
}

var_di = cov_plus_estimate(var_di, di_df, ipi_di)
var_d = cov_plus_estimate(var_d, d_df, ipi_d)
var_ind = cov_plus_estimate(var_ind, ind_df, ipi_ind)
var_sch = cov_plus_estimate(var_sch, sch_df, ipi_sch)


1/dim(merged_df)[1] * sqrt(var_di)
1/dim(merged_df)[1] * sqrt(var_d)
1/dim(merged_df)[1] * sqrt(var_ind)
1/dim(merged_df)[1] * sqrt(var_sch)

HT <- data.frame(Estimand = c("Direct+Indirect", "Direct", "Indirect", "School"),
                 Estimate = c(sum_di/length(merged_df$SCHID) - sum_noschool/length(merged_df$SCHID),
                              sum_d/length(merged_df$SCHID) - sum_noschool/length(merged_df$SCHID),
                              sum_ind/length(merged_df$SCHID) - sum_noschool/length(merged_df$SCHID),
                              sum_sch/length(merged_df$SCHID) - sum_noschool/length(merged_df$SCHID)
                 ),
                 S.E. = c(1/dim(merged_df)[1] * sqrt(var_di),
                          1/dim(merged_df)[1] * sqrt(var_d),
                          1/dim(merged_df)[1] * sqrt(var_ind),
                          1/dim(merged_df)[1] * sqrt(var_sch)))

HT


# Simulation about Misspecification
# 1. Assume y_school = 0.175, y_indirect = 0.124, y_direct+indirect = 0.265, y_direct = 0.270
# 2. Define Indirect to be only of the best friend's impact 
# 3. Define Direct to be only of 
# Run from here
#
merged_df<-read.csv("merged_df.csv")
load(file="ipi_di.RData")
load(file="ipi_d.RData")
load(file="ipi_ind.RData")
load(file="ipi_sch.RData")

weights = rep(0, dim(merged_df)[1])
weights[merged_df$exposure=="Direct+Indirect"] = merged_df$pdi[merged_df$exposure=="Direct+Indirect"]
weights[merged_df$exposure=="Direct"] = merged_df$pd[merged_df$exposure=="Direct"]
weights[merged_df$exposure=="Indirect"] = merged_df$pind[merged_df$exposure=="Indirect"]
weights[merged_df$exposure=="School"] = merged_df$psch[merged_df$exposure=="School"]
weights[merged_df$exposure=="No School"] = 0.5

merged_df$weights = weights

library(car)
# No Interaction WLS
merged_df$exposure[merged_df$exposure=="No School"] = "ANo School"
model_no_wls = lm(WRISTOW2 ~ exposure, merged_df)
model_wls = lm(WRISTOW2 ~ exposure, weights = 1/weights, data = merged_df)
summary(model_no_wls)
summary(model_wls)
sqrt(diag(hccm(model_wls , type = "hc0")))

# Interaction Factorial WLS
facmodel_no_wls = lm(WRISTOW2 ~ indirect+i.treat+indirect*i.treat, merged_df)
facmodel_wls = lm(WRISTOW2 ~ indirect+i.treat+indirect*i.treat, weights = 1/weights, data = merged_df)
sqrt(diag(hccm(facmodel_wls , type = "hc1")))
summary(facmodel_wls)

di_df = merged_df %>% filter(exposure == "Direct+Indirect")
d_df = merged_df %>% filter(exposure == "Direct")
ind_df = merged_df %>% filter(exposure == "Indirect")
sch_df = merged_df %>% filter(exposure == "School")
sim_df = merged_df


##
merged_df %>% filter(WRISTOW2==1, exposure != "0") %>% group_by(exposure) %>% summarize(sum(1/weights)/length(merged_df$SCHID))
##

simulate_estimate <- function(sim_df) {
  
  weights = rep(0, dim(sim_df)[1])
  weights[sim_df$exposure=="Direct+Indirect"] = sim_df$pdi[sim_df$exposure=="Direct+Indirect"]
  weights[sim_df$exposure=="Direct"] = sim_df$pd[sim_df$exposure=="Direct"]
  weights[sim_df$exposure=="Indirect"] = sim_df$pind[sim_df$exposure=="Indirect"]
  weights[sim_df$exposure=="School"] = sim_df$psch[sim_df$exposure=="School"]
  weights[sim_df$exposure=="0"] = 0.5
  weights[weights==0]=0.01
  sim_df$weights = weights
  a = sim_df %>% filter(WRISTOW2==1, exposure != "0") %>% group_by(exposure) %>% summarize(sum(1/weights)/length(sim_df$SCHID))
  
  ms = sim_df %>% filter(exposure != "0") %>% group_by(exposure) %>% summarize(m = mean(WRISTOW2))
  m = lm(WRISTOW2 ~ exposure, weights = 1/weights, data=sim_df)
  
  HT <- data.frame(Estimand = a$exposure,
                   HT_Estimate = a$`sum(1/weights)/length(sim_df$SCHID)`,
                   Avg_Estimate = ms$m,
                   WLS = c(m$coefficients[[2]], m$coefficients[[3]],m$coefficients[[4]],
                           m$coefficients[[5]]))
  return(HT)
}


#Y_di = 0.265
#Y_d = 0.27
#Y_ind = 0.124
#Y_sch = 0.175

Y_di = 0.299
Y_d = 0.305
Y_ind = 0.154
Y_sch = 0.057


plan(multisession, workers = 16)
registerDoFuture()
R = 160
simulate <- foreach(i = 1:R) %dorng% {
  
  students <- generate_treat(da99999.0002)
  df <- join(students)
  df$index = 1:length(df$SCHID)
  exposure = rep(0, length(df$SCHID))
  q = df %>% filter(indirect == TRUE & i.treat == 1) %>% select(index)
  q2 = df %>% filter(indirect == FALSE & i.treat == 1) %>% select(index)
  q3 = df %>% filter(indirect == TRUE & i.treat == 0) %>% select(index)
  q4 = df %>% filter(SCHTREAT == 1 & indirect == FALSE & i.treat == 0)%>% select(index)
  exposure[q$index] = "Direct+Indirect"
  exposure[q2$index] = "Direct"
  exposure[q3$index] = "Indirect"
  exposure[q4$index] = "School"
  df$exposure = exposure
  di_df = df %>% filter(exposure == "Direct+Indirect")
  d_df = df %>% filter(exposure == "Direct")
  ind_df = df %>% filter(exposure == "Indirect")
  sch_df = df %>% filter(exposure == "School")
  sim_df = df
  sim_df$pd = merged_df$pd
  sim_df$psch = merged_df$psch
  sim_df$pdi = merged_df$pdi
  sim_df$pind = merged_df$pind
  sim_df$noschool = rep(1/2, length(sim_df$SCHID))
  sim_df$WRISTOW2 = rep(0, dim(sim_df)[1])
  sim_df$WRISTOW2[di_df$index] = rbernoulli(dim(di_df)[1], Y_di)
  sim_df$WRISTOW2[d_df$index] = rbernoulli(dim(d_df)[1], Y_d)
  sim_df$WRISTOW2[ind_df$index] = rbernoulli(dim(ind_df)[1], Y_ind)
  sim_df$WRISTOW2[sch_df$index] = rbernoulli(dim(sch_df)[1], Y_sch)
  sim_df$index = 1:length(sim_df$SCHID)
  sim_df$exposure = exposure
  output <- simulate_estimate(sim_df)
  output
}

for (i in simulate) {
  if (is.nan(i$HT_Estimate[1])) {
    print(i)
  }
}

sim_true <- data.frame(Estimand =c("Direct+Indirect", "Direct", "Indirect", "School"),
                        Mean_HT = c(mean(sapply(simulate, function(x){x$HT_Estimate[1]})),
                                    mean(sapply(simulate, function(x){x$HT_Estimate[2]})),
                                    mean(sapply(simulate, function(x){x$HT_Estimate[3]})),
                                    mean(sapply(simulate, function(x){x$HT_Estimate[4]}))),
                        Sd_HT = c(sd(sapply(simulate, function(x){x$HT_Estimate[1]})),
                                  sd(sapply(simulate, function(x){x$HT_Estimate[2]})),
                                  sd(sapply(simulate, function(x){x$HT_Estimate[3]})),
                                  sd(sapply(simulate, function(x){x$HT_Estimate[4]}))),
                        Mean_avg = c(mean(sapply(simulate, function(x){x$Avg_Estimate[1]})),
                                     mean(sapply(simulate, function(x){x$Avg_Estimate[2]})),
                                     mean(sapply(simulate, function(x){x$Avg_Estimate[3]})),
                                     mean(sapply(simulate, function(x){x$Avg_Estimate[4]}))),
                        Sd_avg = c(sd(sapply(simulate, function(x){x$Avg_Estimate[1]})),
                                   sd(sapply(simulate, function(x){x$Avg_Estimate[2]})),
                                   sd(sapply(simulate, function(x){x$Avg_Estimate[3]})),
                                   sd(sapply(simulate, function(x){x$Avg_Estimate[4]}))),
                       Mean_WLS = c(mean(sapply(simulate, function(x){x$WLS[1]})),
                                   mean(sapply(simulate, function(x){x$WLS[2]})),
                                   mean(sapply(simulate, function(x){x$WLS[3]})),
                                   mean(sapply(simulate, function(x){x$WLS[4]}))),
                       Sd_WLS = c(sd(sapply(simulate, function(x){x$WLS[1]})),
                                 sd(sapply(simulate, function(x){x$WLS[2]})),
                                 sd(sapply(simulate, function(x){x$WLS[3]})),
                                 sd(sapply(simulate, function(x){x$WLS[4]}))),
                        RMSE_HT = c(sqrt(mean(sapply(simulate, function(x){(x$HT_Estimate[1] - Y_di)**2}))),
                                    sqrt(mean(sapply(simulate, function(x){(x$HT_Estimate[2] - Y_d)**2}))),
                                    sqrt(mean(sapply(simulate, function(x){(x$HT_Estimate[3] - Y_ind)**2}))),
                                    sqrt(mean(sapply(simulate, function(x){(x$HT_Estimate[4] - Y_sch)**2})))),
                        RMSE_avg = c(sqrt(mean(sapply(simulate, function(x){(x$Avg_Estimate[1] - Y_di)**2}))),
                                     sqrt(mean(sapply(simulate, function(x){(x$Avg_Estimate[2] - Y_d)**2}))),
                                     sqrt(mean(sapply(simulate, function(x){(x$Avg_Estimate[3] - Y_ind)**2}))),
                                     sqrt(mean(sapply(simulate, function(x){(x$Avg_Estimate[4] - Y_sch)**2})))),
                       RMSE_WLS = c(sqrt(mean(sapply(simulate, function(x){(x$WLS[1] - Y_di)**2}))),
                                    sqrt(mean(sapply(simulate, function(x){(x$WLS[2] - Y_d)**2}))),
                                    sqrt(mean(sapply(simulate, function(x){(x$WLS[3] - Y_ind)**2}))),
                                    sqrt(mean(sapply(simulate, function(x){(x$WLS[4] - Y_sch)**2})))))



# False exposure mapping: same mapping numbers but only ST1 counts

mis_join <- function(da99999.0001) {
  for (i in 1:1) {
    if (i == 1) {
      merged_df <- da99999.0001 %>% drop_na(ID, TREAT, SCHID, SCHTREAT) %>% select(ID, ST1, TREAT, SCHID, SCHTREAT, WRISTOW2, STRB) 
      right = merged_df %>% select(ID, TREAT, SCHID)
      merged_df = merge(x = merged_df, y = right, by.x = c(paste0("ST", i), "SCHID"), by.y = c("ID", "SCHID"), all.x = TRUE)
      colnames(merged_df)[colnames(merged_df) == "TREAT.x"] = "i.treat"
      colnames(merged_df)[colnames(merged_df) == "TREAT.y"] = "st1.treat"
    }
    else {
      mg.before <- da99999.0001 %>% drop_na(ID, TREAT, SCHID, SCHTREAT, WRISTOW2) %>% select(ID, paste0('ST', i), TREAT, SCHID, SCHTREAT, WRISTOW2, STRB) 
      right = mg.before %>% select(ID, TREAT, SCHID)
      merged_df = merge(x = merged_df, y = right, by.x = c(paste0("ST", i), "SCHID"), by.y = c("ID", "SCHID"), all.x = TRUE)
      colnames(merged_df)[colnames(merged_df) == "TREAT"] = paste0("st",i,"treat")
    }
  }
  merged_df[is.na(merged_df)] = 0
  
  merged_df <- merged_df %>% mutate(indirect = st1.treat >0)
  
  for (i in 1:dim(merged_df)[1]) {
    row = merged_df[i,]
    if (row$indirect != 1 & row$SCHTREAT == 1) {
      temp <- merged_df %>% filter(SCHID == row$SCHID, i.treat == 1) %>% select(ST1, i.treat)
      for (j in 1:dim(temp)[1]) {
        temp_row = temp[j,]
        if (temp_row$i.treat == 1 & temp_row$ST1 == row$ID) {
          merged_df[i,which(colnames(merged_df) == "indirect")] = TRUE
          break
        }
      }
    }
  }
  return(merged_df)
}

plan(multisession, workers = 16)
registerDoFuture()
R = 160
simulate2 <- foreach(i = 1:R) %dorng% {
  students <- generate_treat(da99999.0002)
  df <- mis_join(students)
  df$index = 1:length(df$SCHID)
  direct_indirect = rep(0, length(df$SCHID))
  direct = rep(0, length(df$SCHID))
  indirect = rep(0, length(df$SCHID))
  school = rep(0, length(df$SCHID))
  exposure = rep(0, length(df$SCHID))
  q = df %>% filter(indirect == TRUE & i.treat == 1) %>% select(index)
  q2 = df %>% filter(indirect == FALSE & i.treat == 1) %>% select(index)
  q3 = df %>% filter(indirect == TRUE & i.treat == 0) %>% select(index)
  q4 = df %>% filter(SCHTREAT == 1 & indirect == FALSE & i.treat == 0)
  exposure[q$index] = "Direct+Indirect"
  exposure[q2$index] = "Direct"
  exposure[q3$index] = "Indirect"
  exposure[q4$index] = "School"
  df$exposure = exposure
  di_df = df %>% filter(exposure == "Direct+Indirect")
  d_df = df %>% filter(exposure == "Direct")
  ind_df = df %>% filter(exposure == "Indirect")
  sch_df = df %>% filter(exposure == "School")
  df$WRISTOW2 = rep(0, dim(df)[1])
  df$WRISTOW2[di_df$index] = rbernoulli(dim(di_df)[1], Y_di)
  df$WRISTOW2[d_df$index] = rbernoulli(dim(d_df)[1], Y_d)
  df$WRISTOW2[ind_df$index] = rbernoulli(dim(ind_df)[1], Y_ind)
  df$WRISTOW2[sch_df$index] = rbernoulli(dim(sch_df)[1], Y_sch)
  sim_df = join(students)
  sim_df = sim_df %>% select(-WRISTOW2)
  sim_df = merge(x = sim_df, y = df%>% select(ID, SCHID, WRISTOW2), by.x = c("ID", "SCHID"), by.y = c("ID", "SCHID"))
  sim_df = merge(x = sim_df, y = merged_df %>% select(ID, SCHID, pd, psch, pdi, pind), by.x = c("ID", "SCHID"), by.y = c("ID", "SCHID"))
  sim_df$noschool = rep(1/2, length(sim_df$SCHID))
  exposure = rep(0, length(sim_df$SCHID))
  sim_df$index = 1:length(sim_df$SCHID)
  q = sim_df %>% filter(indirect == TRUE & i.treat == 1) %>% select(index)
  q2 = sim_df %>% filter(indirect == FALSE & i.treat == 1) %>% select(index)
  q3 = sim_df %>% filter(indirect == TRUE & i.treat == 0) %>% select(index)
  q4 = sim_df %>% filter(SCHTREAT == 1 & indirect == FALSE & i.treat == 0)%>% select(index)
  exposure[q$index] = "Direct+Indirect"
  exposure[q2$index] = "Direct"
  exposure[q3$index] = "Indirect"
  exposure[q4$index] = "School"
  sim_df$exposure = exposure
  output <- simulate_estimate(sim_df)
  output
}

sim_mis_1 <- data.frame(Estimand =c("Direct+Indirect", "Direct", "Indirect", "School"),
                        Mean_HT = c(mean(sapply(simulate2, function(x){x$HT_Estimate[1]})),
                                    mean(sapply(simulate2, function(x){x$HT_Estimate[2]})),
                                    mean(sapply(simulate2, function(x){x$HT_Estimate[3]})),
                                    mean(sapply(simulate2, function(x){x$HT_Estimate[4]}))),
                        Sd_HT = c(sd(sapply(simulate2, function(x){x$HT_Estimate[1]})),
                                  sd(sapply(simulate2, function(x){x$HT_Estimate[2]})),
                                  sd(sapply(simulate2, function(x){x$HT_Estimate[3]})),
                                  sd(sapply(simulate2, function(x){x$HT_Estimate[4]}))),
                        Mean_avg = c(mean(sapply(simulate2, function(x){x$Avg_Estimate[1]})),
                                     mean(sapply(simulate2, function(x){x$Avg_Estimate[2]})),
                                     mean(sapply(simulate2, function(x){x$Avg_Estimate[3]})),
                                     mean(sapply(simulate2, function(x){x$Avg_Estimate[4]}))),
                        Sd_avg = c(sd(sapply(simulate2, function(x){x$Avg_Estimate[1]})),
                                   sd(sapply(simulate2, function(x){x$Avg_Estimate[2]})),
                                   sd(sapply(simulate2, function(x){x$Avg_Estimate[3]})),
                                   sd(sapply(simulate2, function(x){x$Avg_Estimate[4]}))),
                        Mean_WLS = c(mean(sapply(simulate2, function(x){x$WLS[1]})),
                                     mean(sapply(simulate2, function(x){x$WLS[2]})),
                                     mean(sapply(simulate2, function(x){x$WLS[3]})),
                                     mean(sapply(simulate2, function(x){x$WLS[4]}))),
                        Sd_WLS = c(sd(sapply(simulate2, function(x){x$WLS[1]})),
                                   sd(sapply(simulate2, function(x){x$WLS[2]})),
                                   sd(sapply(simulate2, function(x){x$WLS[3]})),
                                   sd(sapply(simulate2, function(x){x$WLS[4]}))),
                        RMSE_HT = c(sqrt(mean(sapply(simulate2, function(x){(x$HT_Estimate[1] - Y_di)**2}))),
                                    sqrt(mean(sapply(simulate2, function(x){(x$HT_Estimate[2] - Y_d)**2}))),
                                    sqrt(mean(sapply(simulate2, function(x){(x$HT_Estimate[3] - Y_ind)**2}))),
                                    sqrt(mean(sapply(simulate2, function(x){(x$HT_Estimate[4] - Y_sch)**2})))),
                        RMSE_avg = c(sqrt(mean(sapply(simulate2, function(x){(x$Avg_Estimate[1] - Y_di)**2}))),
                                     sqrt(mean(sapply(simulate2, function(x){(x$Avg_Estimate[2] - Y_d)**2}))),
                                     sqrt(mean(sapply(simulate2, function(x){(x$Avg_Estimate[3] - Y_ind)**2}))),
                                     sqrt(mean(sapply(simulate2, function(x){(x$Avg_Estimate[4] - Y_sch)**2})))),
                        RMSE_WLS = c(sqrt(mean(sapply(simulate2, function(x){(x$WLS[1] - Y_di)**2}))),
                                     sqrt(mean(sapply(simulate2, function(x){(x$WLS[2] - Y_d)**2}))),
                                     sqrt(mean(sapply(simulate2, function(x){(x$WLS[3] - Y_ind)**2}))),
                                     sqrt(mean(sapply(simulate2, function(x){(x$WLS[4] - Y_sch)**2})))))


# False exposure mapping: less mapping numbers

plan(multisession, workers = 16)
registerDoFuture()
R = 160
simulate3 <- foreach(i = 1:R) %dorng% {
  students <- generate_treat(da99999.0002)
  df <- join(students)
  df$index = 1:length(df$SCHID)
  direct_indirect = rep(0, length(df$SCHID))
  direct = rep(0, length(df$SCHID))
  indirect = rep(0, length(df$SCHID))
  school = rep(0, length(df$SCHID))
  exposure = rep(0, length(df$SCHID))
  q = df %>% filter(indirect == TRUE & i.treat == 1) %>% select(index)
  q2 = df %>% filter(indirect == FALSE & i.treat == 1) %>% select(index)
  q3 = df %>% filter(indirect == TRUE & i.treat == 0) %>% select(index)
  q4 = df %>% filter(SCHTREAT == 1 & indirect == FALSE & i.treat == 0)
  exposure[q$index] = "Direct+Indirect"
  exposure[q2$index] = "Direct"
  exposure[q3$index] = "Indirect"
  exposure[q4$index] = "School"
  df$exposure = exposure
  di_df = df %>% filter(exposure == "Direct+Indirect")
  d_df = df %>% filter(exposure == "Direct")
  ind_df = df %>% filter(exposure == "Indirect")
  sch_df = df %>% filter(exposure == "School")
  df$WRISTOW2 = rep(0, dim(df)[1])
  df$WRISTOW2[d_df$index] = rbernoulli(dim(d_df)[1], Y_d)
  df$WRISTOW2[di_df$index] = rbernoulli(dim(di_df)[1], Y_d)
  df$WRISTOW2[sch_df$index] = rbernoulli(dim(sch_df)[1], Y_sch)
  df$WRISTOW2[ind_df$index] = rbernoulli(dim(ind_df)[1], Y_sch)
  df = merge(x = df, y = merged_df %>% select(ID, SCHID, pd, psch, pdi, pind), by.x = c("ID", "SCHID"), by.y = c("ID", "SCHID"))
  
  output <- simulate_estimate(df)
  output
}



sim_mis_2 <- data.frame(Estimand =c("Direct+Indirect", "Direct", "Indirect", "School"),
                        Mean_HT = c(mean(sapply(simulate3, function(x){x$HT_Estimate[1]})),
                                    mean(sapply(simulate3, function(x){x$HT_Estimate[2]})),
                                    mean(sapply(simulate3, function(x){x$HT_Estimate[3]})),
                                    mean(sapply(simulate3, function(x){x$HT_Estimate[4]}))),
                        Sd_HT = c(sd(sapply(simulate3, function(x){x$HT_Estimate[1]})),
                                  sd(sapply(simulate3, function(x){x$HT_Estimate[2]})),
                                  sd(sapply(simulate3, function(x){x$HT_Estimate[3]})),
                                  sd(sapply(simulate3, function(x){x$HT_Estimate[4]}))),
                        Mean_avg = c(mean(sapply(simulate3, function(x){x$Avg_Estimate[1]})),
                                     mean(sapply(simulate3, function(x){x$Avg_Estimate[2]})),
                                     mean(sapply(simulate3, function(x){x$Avg_Estimate[3]})),
                                     mean(sapply(simulate3, function(x){x$Avg_Estimate[4]}))),
                        Sd_avg = c(sd(sapply(simulate3, function(x){x$Avg_Estimate[1]})),
                                   sd(sapply(simulate3, function(x){x$Avg_Estimate[2]})),
                                   sd(sapply(simulate3, function(x){x$Avg_Estimate[3]})),
                                   sd(sapply(simulate3, function(x){x$Avg_Estimate[4]}))),
                        Mean_WLS = c(mean(sapply(simulate3, function(x){x$WLS[1]})),
                                     mean(sapply(simulate3, function(x){x$WLS[2]})),
                                     mean(sapply(simulate3, function(x){x$WLS[3]})),
                                     mean(sapply(simulate3, function(x){x$WLS[4]}))),
                        Sd_WLS = c(sd(sapply(simulate3, function(x){x$WLS[1]})),
                                   sd(sapply(simulate3, function(x){x$WLS[2]})),
                                   sd(sapply(simulate3, function(x){x$WLS[3]})),
                                   sd(sapply(simulate3, function(x){x$WLS[4]}))),
                        RMSE_HT = c(sqrt(mean(sapply(simulate3, function(x){(x$HT_Estimate[1] - Y_d)**2}))),
                                    sqrt(mean(sapply(simulate3, function(x){(x$HT_Estimate[2] - Y_d)**2}))),
                                    sqrt(mean(sapply(simulate3, function(x){(x$HT_Estimate[3] - Y_sch)**2}))),
                                    sqrt(mean(sapply(simulate3, function(x){(x$HT_Estimate[4] - Y_sch)**2})))),
                        RMSE_avg = c(sqrt(mean(sapply(simulate3, function(x){(x$Avg_Estimate[1] - Y_d)**2}))),
                                     sqrt(mean(sapply(simulate3, function(x){(x$Avg_Estimate[2] - Y_d)**2}))),
                                     sqrt(mean(sapply(simulate3, function(x){(x$Avg_Estimate[3] - Y_sch)**2}))),
                                     sqrt(mean(sapply(simulate3, function(x){(x$Avg_Estimate[4] - Y_sch)**2})))),
                        RMSE_WLS = c(sqrt(mean(sapply(simulate3, function(x){(x$WLS[1] - Y_d)**2}))),
                                     sqrt(mean(sapply(simulate3, function(x){(x$WLS[2] - Y_d)**2}))),
                                     sqrt(mean(sapply(simulate3, function(x){(x$WLS[3] - Y_sch)**2}))),
                                     sqrt(mean(sapply(simulate3, function(x){(x$WLS[4] - Y_sch)**2})))))

# False exposure mapping: more mapping numbers

mis_join2 <- function(da99999.0001) {
  for (i in 1:10) {
    if (i == 1) {
      merged_df <- da99999.0001 %>% drop_na(ID, TREAT, SCHID, SCHTREAT) %>% select(ID, ST1, ST2, ST3, ST4, ST5, ST6, ST7, ST8, ST9, ST10, TREAT, SCHID, SCHTREAT, WRISTOW2, STRB) 
      right = merged_df %>% select(ID, TREAT, SCHID)
      merged_df = merge(x = merged_df, y = right, by.x = c(paste0("ST", i), "SCHID"), by.y = c("ID", "SCHID"), all.x = TRUE)
      colnames(merged_df)[colnames(merged_df) == "TREAT.x"] = "i.treat"
      colnames(merged_df)[colnames(merged_df) == "TREAT.y"] = "st1.treat"
    }
    else {
      mg.before <- da99999.0001 %>% drop_na(ID, TREAT, SCHID, SCHTREAT, WRISTOW2) %>% select(ID, paste0('ST', i), TREAT, SCHID, SCHTREAT, WRISTOW2, STRB) 
      right = mg.before %>% select(ID, TREAT, SCHID)
      merged_df = merge(x = merged_df, y = right, by.x = c(paste0("ST", i), "SCHID"), by.y = c("ID", "SCHID"), all.x = TRUE)
      colnames(merged_df)[colnames(merged_df) == "TREAT"] = paste0("st",i,"treat")
    }
  }
  merged_df[is.na(merged_df)] = 0
  
  merged_df <- merged_df %>% mutate(indirect = (st1.treat + st2treat + st3treat + st4treat +
                                                  st5treat + st6treat + st7treat + st8treat +
                                                  st9treat + st10treat) >0)
  merged_df$indirect_fake = rep(FALSE, dim(merged_df)[1])
  for (i in 1:dim(merged_df)[1]) {
    row = merged_df[i,]
    if (row$indirect != 1 & row$SCHTREAT == 1) {
      temp <- merged_df %>% filter(SCHID == row$SCHID, i.treat == 1) %>% select(ST1, ST2, ST3, ST4, ST5, ST6, ST7, ST8, ST9, ST10, i.treat)
      for (j in 1:dim(temp)[1]) {
        temp_row = temp[j,]
        if (temp_row$i.treat == 1 & (temp_row$ST1 == row$ID| 
                                     temp_row$ST2 == row$ID|
                                     temp_row$ST3 == row$ID|
                                     temp_row$ST4 == row$ID|
                                     temp_row$ST5 == row$ID|
                                     temp_row$ST6 == row$ID|
                                     temp_row$ST7 == row$ID|
                                     temp_row$ST8 == row$ID|
                                     temp_row$ST9 == row$ID|
                                     temp_row$ST10 == row$ID)) {
          merged_df[i,which(colnames(merged_df) == "indirect_fake")] = TRUE
          break
        }
      }
    }
  }
  return(merged_df)
}


plan(multisession, workers = 16)
registerDoFuture()
R = 160
simulate4 <- foreach(i = 1:R) %dorng% {
  students <- generate_treat(da99999.0002)
  df <- mis_join2(students)
  df$index = 1:length(df$SCHID)
  exposure = rep(0, length(df$SCHID))
  q = df %>% filter(indirect == TRUE & i.treat == 1) %>% select(index)
  q2 = df %>% filter(indirect == FALSE & i.treat == 1 & indirect_fake == FALSE) %>% select(index)
  q3 = df %>% filter(indirect == TRUE & i.treat == 0) %>% select(index)
  q4 = df %>% filter(SCHTREAT == 1 & indirect == FALSE & i.treat == 0 & indirect_fake == FALSE) %>% select(index)
  q5 = df %>% filter(indirect_fake == TRUE & i.treat == 0) %>% select(index)
  q6 = df %>% filter(indirect_fake == TRUE & i.treat == 1) %>% select(index)
  exposure[q$index] = "Direct+Indirect"
  exposure[q2$index] = "Direct"
  exposure[q3$index] = "Indirect"
  exposure[q4$index] = "School"
  exposure[q5$index] = "2ndIndirect"
  exposure[q6$index] = "Direct+2ndIndirect"
  df$exposure = exposure
  WRISTOW2 = rep(0, length(exposure))
  WRISTOW2[exposure == "Direct+Indirect"] = rbernoulli(length(q$index), Y_di)
  WRISTOW2[exposure == "Direct+2ndIndirect"] = rbernoulli(length(q6$index), Y_di-0.1)
  WRISTOW2[exposure == "Direct"] = rbernoulli(length(q2$index), Y_d)
  WRISTOW2[exposure == "Indirect"] = rbernoulli(length(q3$index), Y_ind)
  WRISTOW2[exposure == "2ndIndirect"] = rbernoulli(length(q5$index), Y_ind - 0.1)
  WRISTOW2[exposure == "School"] = rbernoulli(length(q4$index), Y_sch)
  sim_df = join(students)
  sim_df$index = 1:length(sim_df$SCHID)
  q = sim_df %>% filter(indirect == TRUE & i.treat == 1) %>% select(index)
  q2 = sim_df %>% filter(indirect == FALSE & i.treat == 1) %>% select(index)
  q3 = sim_df %>% filter(indirect == TRUE & i.treat == 0) %>% select(index)
  q4 = sim_df %>% filter(SCHTREAT == 1 & indirect == FALSE & i.treat == 0)
  exposure = rep(0, length(df$SCHID))
  exposure[q$index] = "Direct+Indirect"
  exposure[q2$index] = "Direct"
  exposure[q3$index] = "Indirect"
  exposure[q4$index] = "School"
  sim_df$exposure = exposure
  sim_df$pd = merged_df$pd
  sim_df$psch = merged_df$psch
  sim_df$pdi = merged_df$pdi
  sim_df$pind = merged_df$pind
  sim_df$noschool = rep(1/2, length(sim_df$SCHID))
  sim_df$WRISTOW2 = WRISTOW2
  output <- simulate_estimate(sim_df)
  output
}



sim_mis_3 <- data.frame(Estimand =c("Direct+Indirect", "Direct", "Indirect", "School"),
                        Mean_HT = c(mean(sapply(simulate4, function(x){x$HT_Estimate[1]})),
                                    mean(sapply(simulate4, function(x){x$HT_Estimate[2]})),
                                    mean(sapply(simulate4, function(x){x$HT_Estimate[3]})),
                                    mean(sapply(simulate4, function(x){x$HT_Estimate[4]}))),
                        Sd_HT = c(sd(sapply(simulate4, function(x){x$HT_Estimate[1]})),
                                  sd(sapply(simulate4, function(x){x$HT_Estimate[2]})),
                                  sd(sapply(simulate4, function(x){x$HT_Estimate[3]})),
                                  sd(sapply(simulate4, function(x){x$HT_Estimate[4]}))),
                        Mean_avg = c(mean(sapply(simulate4, function(x){x$Avg_Estimate[1]})),
                                     mean(sapply(simulate4, function(x){x$Avg_Estimate[2]})),
                                     mean(sapply(simulate4, function(x){x$Avg_Estimate[3]})),
                                     mean(sapply(simulate4, function(x){x$Avg_Estimate[4]}))),
                        Sd_avg = c(sd(sapply(simulate4, function(x){x$Avg_Estimate[1]})),
                                   sd(sapply(simulate4, function(x){x$Avg_Estimate[2]})),
                                   sd(sapply(simulate4, function(x){x$Avg_Estimate[3]})),
                                   sd(sapply(simulate4, function(x){x$Avg_Estimate[4]}))),
                        Mean_WLS = c(mean(sapply(simulate4, function(x){x$WLS[1]})),
                                     mean(sapply(simulate4, function(x){x$WLS[2]})),
                                     mean(sapply(simulate4, function(x){x$WLS[3]})),
                                     mean(sapply(simulate4, function(x){x$WLS[4]}))),
                        Sd_WLS = c(sd(sapply(simulate4, function(x){x$WLS[1]})),
                                   sd(sapply(simulate4, function(x){x$WLS[2]})),
                                   sd(sapply(simulate4, function(x){x$WLS[3]})),
                                   sd(sapply(simulate4, function(x){x$WLS[4]}))),
                        RMSE_HT = c(sqrt(mean(sapply(simulate4, function(x){(x$HT_Estimate[1] - Y_di)**2}))),
                                    sqrt(mean(sapply(simulate4, function(x){(x$HT_Estimate[2] - Y_d)**2}))),
                                    sqrt(mean(sapply(simulate4, function(x){(x$HT_Estimate[3] - Y_ind)**2}))),
                                    sqrt(mean(sapply(simulate4, function(x){(x$HT_Estimate[4] - Y_sch)**2})))),
                        RMSE_avg = c(sqrt(mean(sapply(simulate4, function(x){(x$Avg_Estimate[1] - Y_di)**2}))),
                                     sqrt(mean(sapply(simulate4, function(x){(x$Avg_Estimate[2] - Y_d)**2}))),
                                     sqrt(mean(sapply(simulate4, function(x){(x$Avg_Estimate[3] - Y_ind)**2}))),
                                     sqrt(mean(sapply(simulate4, function(x){(x$Avg_Estimate[4] - Y_sch)**2})))),
                        RMSE_WLS = c(sqrt(mean(sapply(simulate4, function(x){(x$WLS[1] - Y_di)**2}))),
                                     sqrt(mean(sapply(simulate4, function(x){(x$WLS[2] - Y_d)**2}))),
                                     sqrt(mean(sapply(simulate4, function(x){(x$WLS[3] - Y_ind)**2}))),
                                     sqrt(mean(sapply(simulate4, function(x){(x$WLS[4] - Y_sch)**2})))))

# Results to report


sim_true = sim_true %>% mutate(True = c(Y_di, Y_d, Y_ind, Y_sch)) %>% mutate(Bias_HT = True - Mean_HT, Bias_Avg = True - Mean_avg)
sim_true
sim_mis_1 = sim_mis_1 %>% mutate(True = c(Y_di, Y_d, Y_ind, Y_sch)) %>% mutate(Bias_HT = True - Mean_HT, Bias_Avg = True - Mean_avg)
sim_mis_1
sim_mis_2 = sim_mis_2 %>% mutate(True = c(Y_d, Y_d, Y_sch, Y_sch)) %>% mutate(Bias_HT = True - Mean_HT, Bias_Avg = True - Mean_avg)
sim_mis_2
sim_mis_3
