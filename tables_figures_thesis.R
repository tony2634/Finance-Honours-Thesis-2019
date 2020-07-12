
# Summary stats

stargazer(cbind(accuracy_by_block[,1],sapply(accuracy_by_block[,2:7],percent,accuracy=0.01)),type="latex",summary=FALSE,rownames = FALSE)
stargazer(cbind(as.tibble(accuracy_by_diff[,1:2]),sapply(accuracy_by_diff[,3:8],percent,accuracy=0.01)),type="latex",summary=FALSE,rownames = FALSE)
stargazer(cbind(accuracy_by_cost[,1],sapply(accuracy_by_cost[,2:7],percent,accuracy=0.01)),type="latex",summary=FALSE,rownames = FALSE)
stargazer(cbind(as.tibble(full_time_by_region[,1:2]),sapply(full_time_by_region[,3],percent,accuracy=0.01)),type="latex",summary=FALSE,rownames = FALSE)
stargazer(cbind(full_time_by_cost[,1],sapply(full_time_by_cost[,2],percent,accuracy=0.01)),type="latex",summary=FALSE,rownames = FALSE)
stargazer(cbind(full_time_by_block[,1],sapply(full_time_by_block[,2],percent,accuracy=0.01)),type="latex",summary=FALSE,rownames = FALSE)
stargazer(timespentbyregion,type="latex",summary=FALSE,rownames = FALSE)
stargazer(timespentbyblock,type="latex",summary=FALSE,rownames = FALSE)
stargazer(timespentbycost,type="latex",summary=FALSE,rownames = FALSE)


# Regressions

# Reg table 1
logistic_model_base = glmer(KP_correct ~ cost_digits + (1|Participant), dt_merged, family=binomial(link="logit"))
logistic_model_all = glmer(KP_correct ~ cost_digits + phaseT + phaseT*cost_digits + times_repeat + sol*phaseT + (1|Participant), dt_merged, family=binomial(link="logit"))
logistic_model_all_asfactor = glmer(KP_correct ~ cost_digits_factor + phaseT + phaseT*cost_digits_factor + sol*phaseT + times_repeat + (1|Participant), dt_merged, family=binomial(link="logit"))
logistic_model_all_asfactor_prop = glmer(KP_correct ~ cost_digits + propagations + propagations*cost_digits + times_repeat + sol*propagations + (1|Participant), dt_merged, family=binomial(link="logit"))
stargazer(logistic_model_base,logistic_model_all,logistic_model_all_asfactor,logistic_model_all_asfactor_prop,type="text")
summary(margins(logistic_model_base))
summary(margins(logistic_model_all))
summary(margins(logistic_model_all_asfactor))
summary(margins(logistic_model_all_asfactor_prop))

logistic_model_all = glmer(KP_correct ~ cost_digits + phaseT + phaseT*cost_digits + times_repeat + sol*phaseT + (1|Participant), dt_merged, family=binomial(link="logit"))
stargazer(logistic_model_all)
margins(logistic_model_all)


# Reg table 2
dt_merged$censored = ifelse(dt_merged$timeSpent==20,"right","none")
reg1 <- brm(timeSpent|cens(censored) ~ cost_digits + phaseT + sol + times_repeat + (1|Participant),
            data=dt_merged, family=gaussian(link="identity"))
stargazer(summary(reg1)$fixed,type="text")
summary(margins(reg1))


# Reg table 3
# logistic_model_time_all_asfactor = lmer(timeSpent ~ cost_digits_factor + region + region*cost_digits_factor + (1|Participant), dt_merged)
logistic_model_solutionsets_all = lmer(no_solution_sets ~ cost_digits + phaseT + sol + times_repeat + (1|Participant), dt_merged)
logistic_model_unique_solutionsets_all = lmer(no_unique_solution_sets ~ cost_digits + phaseT + sol + times_repeat + (1|Participant), dt_merged)
logistic_model_last_click_all = lmer(last_click ~ cost_digits + phaseT + sol + times_repeat + (1|Participant), dt_merged)
stargazer(logistic_model_solutionsets_all_asfactor,logistic_model_unique_solutionsets_all_asfactor,logistic_model_last_click_all_asfactor,type="latex")
summary(margins(logistic_model_solutionsets_all))
summary(margins(logistic_model_unique_solutionsets_all))
summary(margins(logistic_model_last_click_all))

# Reg table 4
logistic_model_all_region = glmer(KP_correct ~ cost_digits + region + region*cost_digits + times_repeat + (1|Participant), dt_merged, family=binomial(link="logit"))
stargazer(logistic_model_all_region,type="text")
margins(logistic_model_all)





##############################################################################################################


tikz(file = "accuracy_by_cost_participant.tex", width = 5, height = 4)

plot <- ggplot(accuracy_by_cost_p,aes(x=cost_digits,y=accuracy,col=Participant)) + geom_line() + geom_point() + theme_bw() +

  labs(x = "CostDigits",
       y = "Overall accuracy") +

  theme(plot.title = element_text(size = rel(0.9)), 
        axis.title = element_text(size = rel(0.9)),
        axis.title.y = element_text(size = rel(0.9)),
        axis.title.x = element_text(size = rel(0.9)))

print(plot)
dev.off()

##############################################################################################################


tikz(file = "accuracy_by_cost_instance.tex", width = 5, height = 4)

plot <- ggplot(instance_accuracy_by_cost,aes(x=cost_digits,y=accuracy,col=as.factor(instanceNumber))) + geom_line() + geom_point() + theme_bw() +

  labs(x = "CostDigits",
       y = "Overall accuracy",
       color="Instance number") +

  theme(plot.title = element_text(size = rel(0.9)), 
        axis.title = element_text(size = rel(0.9)),
        axis.title.y = element_text(size = rel(0.9)),
        axis.title.x = element_text(size = rel(0.9)))

print(plot)
dev.off()

##############################################################################################################


tikz(file = "timespent_by_participant.tex", width = 5, height = 4)

plot <- ggplot(timespent_by_participant, aes(x=Participant, y=timespent)) + 
  geom_bar(position=position_dodge(), stat="identity", fill="lightblue") +
  geom_errorbar(aes(ymin=timespent-std.error(timespent), ymax=timespent+std.error(timespent)),
                width=.2) +
  scale_y_continuous(breaks=0:20*4) +
  theme_bw() + 
  
  labs(x = "Participant",
       y = "Time spent") +
  
  theme(plot.title = element_text(size = rel(0.9)), 
        axis.title = element_text(size = rel(0.9)),
        axis.title.y = element_text(size = rel(0.9)),
        axis.title.x = element_text(size = rel(0.9)))

print(plot)
dev.off()

##############################################################################################################


tikz(file = "lastclick_by_participant.tex", width = 5, height = 4)

plot <- ggplot(lastclick_by_participant, aes(x=Participant, y=timespent_lastclick)) + 
  geom_bar(position=position_dodge(), stat="identity", fill="tomato3") +
  geom_errorbar(aes(ymin=timespent_lastclick-std.error(timespent_lastclick), ymax=timespent_lastclick+std.error(timespent_lastclick)),
                width=.2) +
  scale_y_continuous(breaks=0:20*4,limit=c(0,20)) +
  theme_bw() + 
  
  labs(x = "Participant",
       y = "Time spent by the last click") +
  
  theme(plot.title = element_text(size = rel(0.9)), 
        axis.title = element_text(size = rel(0.9)),
        axis.title.y = element_text(size = rel(0.9)),
        axis.title.x = element_text(size = rel(0.9)))

print(plot)
dev.off()

##############################################################################################################


tikz(file = "non-unique_solution_sets.tex", width = 2.8, height = 2)

plot <- ggplot(dt_merged, aes(x=no_solution_sets)) +
  geom_histogram(show.legend = FALSE,bins=15,fill="royalblue1") +
  scale_x_continuous(breaks=c(0,5,10,15,20,25,30,35)) +
  labs(x = "Number of non-unique solution sets", y = "Number of observations") +
  theme_bw() + 
  theme(axis.title = element_text(size=9))

print(plot)
dev.off()

##############################################################################################################


tikz(file = "unique_solution_sets.tex", width = 2.8, height = 2)

plot <- ggplot(dt_merged, aes(x=no_unique_solution_sets)) +
  geom_histogram(show.legend = FALSE,bins=13,fill="darkgoldenrod2") +
  scale_x_continuous(breaks=c(0,5,10,15,20,25,30,35)) +
  labs(x = "Number of unique solution sets", y = "Number of observations") +
  theme_bw() + 
  theme(axis.title = element_text(size=9))

print(plot)
dev.off()

##############################################################################################################


tikz(file = "propagations_against_region.tex", width = 4, height = 4.5)

plot <- ggplot(dt_merged,aes(x=region,y=propagations,col=region)) + geom_point() + theme_bw() +
  labs(x = "Region", y = "Propagations") +
  scale_x_discrete(labels=c("Over-constrained","Phase transition","Under-constrained")) +
  theme_bw() + 
  theme(legend.position = "none")


print(plot)
dev.off()

##############################################################################################################




















