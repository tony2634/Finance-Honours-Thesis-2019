setwd("G:/KP output")

library(tidyverse)
library(readr)
library(dplyr)
library(ggplot2)
library(lme4)
library(brms)
library(stargazer)
library(mediation)
library(plotly)
library(MASS)
library(censReg)
library(plm)
library(AER)
library(scales)
library(tikzDevice)
library(plotrix)
library(margins)
library(loo)
library(ggpubr)


all_trialinfo_files <- list.files(pattern="TrialInfo",recursive=TRUE)
all_param_files <- list.files(pattern="param2",recursive=TRUE)
all_clicks_files <- list.files(pattern="Clicks",recursive=TRUE)

trialinfo <- vector(mode="list")
param <- vector(mode="list")
clicks <- vector(mode="list")

no_participant <- 21


for (i in 1:no_participant) {
  # Import trialinfo
  trialinfo[[i]] <- read_delim(file=all_trialinfo_files[i],col_names = TRUE,skip=3,delim=";")
  
  # Import reward amount
  param[[i]] <- read_delim(file=all_param_files[i],delim=":")
  reward_amount <- strsplit(str_remove(param[[i]]$`1`[9],pattern="\\[") %>% str_remove(pattern="\\]"),split=",")[[1]]
  trialinfo[[i]] <- cbind(trialinfo[[i]],reward_amount)
  
  # Import number of clicks
  clicks[[i]] <- read_delim(file=all_clicks_files[i],col_names=TRUE,skip=3,delim=";")
  clicks_per_trial <- clicks[[i]] %>% group_by(block,trial) %>% count() %>% rename(num_clicks=n)
  trialinfo[[i]] <- merge(trialinfo[[i]],clicks_per_trial,by=c("block","trial"),all.x=TRUE)
  
  # Import the timestamp of the last click
  last_click_time <- aggregate(clicks[[i]]$time,by=list(clicks[[i]]$block,clicks[[i]]$trial),FUN=tail,n=1) %>% 
    rename(block=Group.1,trial=Group.2,last_click=x)
  trialinfo[[i]] <- merge(trialinfo[[i]],last_click_time,by=c("block","trial"),all.x=TRUE)
}

clicks_bound <- bind_rows(clicks,.id="Participant")
clicks_bound$trial_number <- (as.numeric(clicks_bound$Participant)-1) * 72 + (clicks_bound$block-1) * 12 + clicks_bound$trial
trialinfo_bound <- bind_rows(trialinfo,.id="Participant")
instancesinfo <- read_csv2(file="KP_kpk08_8_02 October, 2019, 16-15_InstancesInfo.txt",col_names = TRUE,skip=3)
dt_merged <- merge(trialinfo_bound,instancesinfo,by="instanceNumber")
propagations <- read_delim(file="instances_bucket.csv",delim=",")
propagations <- propagations[c("id","propagations")]
dt_merged <- merge(dt_merged,propagations,by="id",all.x=TRUE)

dt_merged$cost_digits <- nchar(dt_merged$RandomNumber)
dt_merged$reward_amount = as.numeric(as.character(dt_merged$reward_amount))
dt_merged$num_clicks[is.na(dt_merged$num_clicks)] <- 0
dt_merged$last_click[is.na(dt_merged$last_click)] <- 0
dt_merged$mem_correct <- dt_merged$RandomNumber == dt_merged$SubmittedNumber
dt_merged$phaseT <- dt_merged$region == "phaseT"
dt_merged$cost_digits_normalised <- (dt_merged$cost_digits - 1)/4
dt_merged$propagations <- dt_merged$propagations/1000
dt_merged$KP_correct <- dt_merged$answer == dt_merged$sol
dt_merged$full_time_spent <- dt_merged$timeSpent==20
#dt_merged$Participant <- as.numeric(dt_merged$Participant)
dt_merged$cost_digits_factor <- as.factor(dt_merged$cost_digits)


totaltrials <- nrow(dt_merged)


for (i in 1:totaltrials){
  
  if (dt_merged$block[i]==1 | dt_merged$block[i]==2){dt_merged$times_repeat[i]=0}
  else if (dt_merged$block[i]==3 | dt_merged$block[i]==4){dt_merged$times_repeat[i]=1}
  else {dt_merged$times_repeat[i]=2}
  
}

dt_merged$times_repeat_factor <- as.factor(dt_merged$times_repeat)

dt_merged_no_full_time_participant <- subset(dt_merged,Participant!=12 & Participant!=18)
dt_merged_mem_correct <- subset(dt_merged,mem_correct==TRUE)

dt_merged <- as_tibble(dt_merged)
dt_merged <- dt_merged[order(as.numeric(dt_merged$Participant),dt_merged$block,dt_merged$trial),]



## Sets of solutions visited ##


dt_merged$solution_sets <- rep(list(list()),totaltrials)


trial_number_progress <- 1


for (i in 1:totaltrials){
  
  
  if (dt_merged$num_clicks[i]!=0) {
  
    for (j in 1:(dt_merged$num_clicks[i])){
      
      if (j==1){
      
        dt_merged$solution_sets[[i]][j] <- clicks_bound[trial_number_progress:(trial_number_progress+j-1),4]
          
        
      }
      
      else {
        
        dt_merged$solution_sets[[i]][j][[1]] <- rbind(c(dt_merged$solution_sets[[i]][j-1][[1]],clicks_bound[(trial_number_progress+j-1),4]))
        
        
        if (100 %in% dt_merged$solution_sets[[i]][j][[1]]){
          
          temp <- which(dt_merged$solution_sets[[i]][j][[1]] %in% 100)
          
          dt_merged$solution_sets[[i]][j][[1]] <- dt_merged$solution_sets[[i]][j][[1]][0]
          
        }

        if (length(dt_merged$solution_sets[[i]][j][[1]])!=0 & length(dt_merged$solution_sets[[i]][j][[1]])!=1) {
        
          if (dt_merged$solution_sets[[i]][j][[1]][length(dt_merged$solution_sets[[i]][j][[1]])] %in% dt_merged$solution_sets[[i]][j][[1]][1:(length(dt_merged$solution_sets[[i]][j][[1]])-1)]) {
            
            dt_merged$solution_sets[[i]][j][[1]] <- setdiff(dt_merged$solution_sets[[i]][j][[1]],dt_merged$solution_sets[[i]][j][[1]][length(dt_merged$solution_sets[[i]][j][[1]])]) 
            
          }
          
        
        }

      
        
      }
      
      dt_merged$solution_sets[[i]][j][[1]] <- sort(as.numeric(dt_merged$solution_sets[[i]][j][[1]]))
      
      
    }
  
  }
  
  trial_number_progress <- trial_number_progress + dt_merged$num_clicks[i]
  
}



for (k in 1:totaltrials){
  
  
  dt_merged$no_solution_sets[k] <- length(dt_merged$solution_sets[[k]])
  dt_merged$no_unique_solution_sets[k] <- length(unique(dt_merged$solution_sets[[k]]))
  
}

sum(dt_merged$no_solution_sets==dt_merged$no_unique_solution_sets)/totaltrials


dog <- subset(clicks_bound,clicks_bound[4]==100)
dog_2 <- subset(dt_merged,dt_merged$num_clicks==0)


dog_3 = 0

for (m in 1:totaltrials){
  
  if (dt_merged$num_clicks[m]!=length(dt_merged$solution_sets[[m]])){
    dog_3 = dog_3+1
  }
  
}



###################################################################################

# ANALYSIS #

# Summaries of accuracy

average_accuracy <- dt_merged %>% summarise(overall_accuracy=mean(correct),KP_accuracy=mean(KP_correct),mem_accuracy=mean(mem_correct))

accuracy_by_block <- dt_merged %>% group_by(block) %>% summarise(overall_accuracy=mean(correct),overall_SEM=std.error(correct),KP_accuracy=mean(KP_correct),KP_SEM=std.error(KP_correct),mem_accuracy=mean(mem_correct),mem_SEM=std.error(mem_correct))
accuracy_by_diff <- dt_merged %>% group_by(region,sol) %>% summarise(overall_accuracy=mean(correct),overall_SEM=std.error(correct),KP_accuracy=mean(KP_correct),KP_SEM=std.error(KP_correct),mem_accuracy=mean(mem_correct),mem_SEM=std.error(mem_correct))
accuracy_by_cost <- dt_merged %>% group_by(cost_digits) %>% summarise(overall_accuracy=mean(correct),overall_SEM=std.error(correct),KP_accuracy=mean(KP_correct),KP_SEM=std.error(KP_correct),mem_accuracy=mean(mem_correct),mem_SEM=std.error(mem_correct))
accuracy_by_cost_p <- dt_merged %>% group_by(cost_digits,Participant) %>% summarise(accuracy=mean(correct)) %>% arrange(as.numeric(Participant))
accuracy_mem_by_participant <- dt_merged %>% group_by(Participant) %>% summarise(mem_accuracy=mean(mem_correct),KP_accuracy=mean(KP_correct))


ggplot(accuracy_by_cost_p,aes(x=cost_digits,y=accuracy,col=as.factor(Participant))) + geom_line() + geom_point() + theme_bw() 
ggplot(accuracy_mem_by_participant,aes(x=mem_accuracy,y=KP_accuracy,col=Participant)) + geom_point() + geom_smooth(method="lm")

cor(dt_merged$mem_correct,dt_merged$KP_correct)

propagations_by_region <- dt_merged %>% group_by(region) %>% summarise(propagations_no=mean(propagations))
ggplot(dt_merged,aes(x=region,y=propagations,col=region)) + geom_point() + theme_bw()
ggplot(dt_merged,aes(x=cost_digits_factor,y=no_unique_solution_sets,col=cost_digits_factor)) + geom_count() + theme_bw()


instance_accuracy_by_cost <- dt_merged %>% group_by(cost_digits,instanceNumber) %>% summarise(accuracy=mean(correct))
ggplot(instance_accuracy_by_cost,aes(x=cost_digits,y=accuracy,col=as.factor(instanceNumber))) + geom_line() + geom_point() + theme_bw()

contour_z <- matrix(nrow=24,ncol=3)

for (i in 1:3){
  contour_z[,i] <- subset(instance_accuracy_by_cost,cost_digits==((i-1)*2+1))$accuracy
}

plot_ly(x=c(1,3,5),y=instance_accuracy_by_cost$instanceNumber,z=contour_z) %>% add_surface()



# Analysis of full 20 seconds



full_time <- dt_merged %>% summarise(full_time_used=sum(timeSpent==20)/totaltrials)

full_time_by_region <- dt_merged %>% group_by(region,sol) %>% 
  summarise(if(region[1]=="phaseT"){full_time_used=sum(timeSpent==20)/(totaltrials/4)} 
            else{full_time_used=sum(timeSpent==20)/(totaltrials/3)})

full_time_by_cost <- dt_merged %>% group_by(cost_digits) %>% 
  summarise(full_time_used=sum(timeSpent==20)/(totaltrials/3))

full_time_by_block <- dt_merged %>% group_by(block) %>% 
  summarise(full_time_used=sum(timeSpent==20)/(totaltrials/6))

full_time_by_participant <- dt_merged %>% group_by(Participant) %>% 
  summarise(full_time_used=sum(timeSpent==20)/(totaltrials/no_participant))
  

timespent_by_participant <- dt_merged %>% group_by(Participant) %>% summarise(timespent=mean(timeSpent)) %>% arrange(as.numeric(Participant))
lastclick_by_participant <- dt_merged %>% group_by(Participant) %>% summarise(timespent_lastclick=mean(last_click)) %>% arrange(as.numeric(Participant))
timespentbyregion <- dt_merged %>% group_by(region,sol) %>% summarise(time=mean(timeSpent))
timespentbyblock <- dt_merged %>% group_by(block) %>% summarise(time=mean(timeSpent))
timespentbycost <- dt_merged %>% group_by(cost_digits) %>% summarise(time=mean(timeSpent))




# Logit mixed effects regressions

# Performance

logistic_model_base = glmer(correct ~ cost_digits + (1|Participant), dt_merged, family=binomial(link="logit"))
logistic_model_all = glmer(correct ~ cost_digits + phaseT + phaseT*cost_digits + as.factor(times_repeat) + (1|Participant), dt_merged, family=binomial(link="logit"))
logistic_model_all_mem = glmer(correct ~ cost_digits_normalised + phaseT + phaseT*cost_digits_normalised + (1|Participant) + mem_correct, dt_merged, family=binomial(link="logit"))
logistic_model_base_asfactor = glmer(correct ~ as.factor(cost_digits) + (1|Participant), dt_merged, family=binomial(link="logit"))
logistic_model_all_asfactor = glmer(correct ~ cost_digits_factor + phaseT + phaseT*cost_digits_factor  + (1|Participant), dt_merged, family=binomial(link="logit"))

summary(logistic_model_base)
summary(logistic_model_all)
summary(logistic_model_base_asfactor)
summary(logistic_model_all_asfactor)

stargazer(logistic_model_base,type="text")
stargazer(logistic_model_all,type="text")
stargazer(logistic_model_all_mem,type="text")
stargazer(logistic_model_base_asfactor,type="text")
stargazer(logistic_model_all_asfactor,type="text")




# Effort (time spent)

logistic_model_time_base = lmer(timeSpent ~ cost_digits + (1|Participant), dt_merged)
logistic_model_time_all = lmer(timeSpent ~ cost_digits + region + region*cost_digits + times_repeat +(1|Participant), dt_merged_no_full_time_participant)
logistic_model_time_base_asfactor = lmer(timeSpent ~ as.factor(cost_digits) + (1|Participant), dt_merged)
logistic_model_time_all_asfactor = lmer(timeSpent ~ as.factor(cost_digits) + propagations + propagations*as.factor(cost_digits) + (1|Participant), dt_merged)

summary(logistic_model_time_base)
summary(logistic_model_time_all)
summary(logistic_model_time_base_asfactor)
summary(logistic_model_time_all_asfactor)

stargazer(logistic_model_time_base,type="text")
stargazer(logistic_model_time_all,type="text")
stargazer(logistic_model_time_base_asfactor,type="text")
stargazer(logistic_model_time_all_asfactor,type="text")


# Effort (time spent) using multilevel tobit

dt_merged$trial_number_per_participant <- (dt_merged$block-1) * 12 + dt_merged$trial
dt_merged_panel <- pdata.frame(dt_merged,c("Participant","trial_number_per_participant"))
dt_merged_panel <- dt_merged_panel[c("Participant","trial_number_per_participant","timeSpent","phaseT","cost_digits")]
reg1 <- censReg(timeSpent ~ as.factor(cost_digits) + phaseT + phaseT*as.factor(cost_digits),right=20,data = dt_merged_panel)
stargazer(reg1,type="text")
summary(reg1)

# reg2 <- vglm(timeSpent ~ cost_digits + phaseT + phaseT*cost_digits + as.factor(Participant),data=dt_merged,family = tobit(Lower=0,Upper = 20,type.fitted = "censored"))
# summary(reg2)

reg3 <- tobit(timeSpent ~ as.factor(cost_digits) + phaseT + phaseT*as.factor(cost_digits) + as.factor(Participant),right=20,data=dt_merged)
stargazer(reg3,type="text")


# Effort (time spent) multilevel tobit with brms


dt_merged$censored = ifelse(dt_merged$timeSpent==20,"right","none")
reg1 <- brm(timeSpent|cens(censored) ~ cost_digits + phaseT + sol + times_repeat + (1|Participant),
            data=dt_merged, family=gaussian(link="identity"))
reg2 <- brm(timeSpent|cens(censored) ~ cost_digits_factor + region + (1|Participant),
            data=dt_merged, family=gaussian(link="identity"))
reg3 <- brm(timeSpent|cens(censored) ~ cost_digits + region + (1|Participant),
            data=dt_merged, family=gaussian(link="identity"))

loo(reg1); loo(reg2); loo(reg3);

stargazer(summary(reg1)$fixed,summary(reg2)$fixed, summary(reg3)$fixed, type="text");
stargazer(summary(reg1)$fixed,type="text")




# Effort (time spent by last click)

logistic_model_last_click_base = lmer(last_click ~ cost_digits + (1|Participant), dt_merged)
logistic_model_last_click_all = lmer(last_click ~ cost_digits + region + region*cost_digits + (1|Participant), dt_merged)
logistic_model_last_click_base_asfactor = lmer(last_click ~ as.factor(cost_digits) + (1|Participant), dt_merged)
logistic_model_last_click_all_asfactor = lmer(last_click ~ as.factor(cost_digits) + region + region*as.factor(cost_digits) + (1|Participant), dt_merged)

summary(logistic_model_last_click_base)
summary(logistic_model_last_click_all)
summary(logistic_model_last_click_base_asfactor)
summary(logistic_model_last_click_all_asfactor)

stargazer(logistic_model_last_click_base,type="text")
stargazer(logistic_model_last_click_all,type="text")
stargazer(logistic_model_last_click_base_asfactor,type="text")
stargazer(logistic_model_last_click_all_asfactor,type="text")



# Effort (num_clicks)

logistic_model_clicks_base = lmer(num_clicks ~ cost_digits + (1|Participant), dt_merged)
logistic_model_clicks_all = lmer(num_clicks ~ cost_digits + region + region*cost_digits + (1|Participant), dt_merged)
logistic_model_clicks_base_asfactor = lmer(num_clicks ~ as.factor(cost_digits) + (1|Participant), dt_merged)
logistic_model_clicks_all_asfactor = lmer(num_clicks ~ as.factor(cost_digits) + region + region*as.factor(cost_digits) + (1|Participant), dt_merged)

summary(logistic_model_clicks_base)
summary(logistic_model_clicks_all)
summary(logistic_model_clicks_base_asfactor)
summary(logistic_model_clicks_all_asfactor)

stargazer(logistic_model_clicks_base,type="text")
stargazer(logistic_model_clicks_all,type="text")
stargazer(logistic_model_clicks_base_asfactor,type="text")
stargazer(logistic_model_clicks_all_asfactor,type="text")


# Trade?

logistic_model_trade = glmer(KP_correct ~ mem_correct + cost_digits + phaseT + cost_digits*phaseT + (1|Participant),dt_merged,family=binomial(link="logit"))
stargazer(logistic_model_trade,type="text")

# lm1 <- mediate(y=KP_correct,x=cost_digits,m=mem_correct,data=dt_merged)


save(dt_merged, file="dt_merged.RData")


# Effort (solution sets)

logistic_model_solutionsets_all_asfactor = lmer(no_solution_sets ~ as.factor(cost_digits) + region + region*as.factor(cost_digits) + (1|Participant), dt_merged)
stargazer(logistic_model_solutionsets_all_asfactor,type="text")


logistic_model_unique_solutionsets_all_asfactor = lmer(no_unique_solution_sets ~ as.factor(cost_digits) + region + region*as.factor(cost_digits) + (1|Participant), dt_merged)
stargazer(logistic_model_unique_solutionsets_all_asfactor,type="text")






stargazer(logistic_model_all_asfactor,logistic_model_time_all_asfactor,logistic_model_unique_solutionsets_all_asfactor,type="latex")

