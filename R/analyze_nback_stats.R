analyze_nback_stats <- function(group_names)
{

  library(readxl)
  library(stringi)
  library(lattice)
  library(ggplot2)
  library(rprime)
  library(lme4)
  library(lmerTest)
  library(emmeans)

  all_results_data <- vector()
  group_comparison_name <- vector()

  for (this_group_path in group_names){
    this_group_path_string_split = strsplit(this_group_path,"/")[1][1]
    this_group_id = vapply(this_group_path_string_split, tail, "", 1)

    this_group_results_data = read.csv(file.path("Nback_files", paste0(toString(this_group_id), "_results.csv")))
    this_group_results_data$group = this_group_id
    all_results_data = rbind(all_results_data, this_group_results_data)  #This data is already fed in as averaged

    group_comparison_name = paste0(toString(group_comparison_name), toString(this_group_id), "vs")
  }

all_accuracy_data_averaged <- aggregate(all_results_data["percent_correct"], by=list(all_results_data$group, all_results_data$nback_level),FUN=mean)
colnames(all_accuracy_data_averaged) <- c("group", "nback_level", "averaged_accuracy")

accuracy_std <- aggregate(all_results_data["percent_correct"], by=list(all_results_data$group, all_results_data$nback_level),FUN=sd)
colnames(accuracy_std) <- c("group", "nback_level", "std")


all_accuracy_data_averaged$std <- accuracy_std$std
all_accuracy_data_averaged$se <- accuracy_std$std
all_accuracy_data_averaged$lower_std <- all_accuracy_data_averaged$averaged_accuracy - all_accuracy_data_averaged$std
all_accuracy_data_averaged$upper_std <- all_accuracy_data_averaged$averaged_accuracy + all_accuracy_data_averaged$std


accuracy_lmer <- lmer(percent_correct ~ as.character(nback_level)*as.character(group) + (1|subject_id), data=all_results_data)  ##obviously not emotion condition

#summary(response_nback)
#anova(response_nback)

confidence_intervals = confint(accuracy_lmer)
str(confidence_intervals)

# old adult nback 0
confidence_interval_dataframe = data.frame(all_accuracy_data_averaged$group, all_accuracy_data_averaged$nback_level)
colnames(confidence_interval_dataframe) <- c("group", "nback_level")

lower_ci <- data.frame(confidence_intervals[3,1], all_accuracy_data_averaged$averaged_accuracy[2] + confidence_intervals[7,1], all_accuracy_data_averaged$averaged_accuracy[1] + confidence_intervals[4,1],
                       all_accuracy_data_averaged$averaged_accuracy[4] + confidence_intervals[7,1] + confidence_intervals[8,1], all_accuracy_data_averaged$averaged_accuracy[1] + confidence_intervals[5,1],
                       all_accuracy_data_averaged$averaged_accuracy[6] + confidence_intervals[7,1] + confidence_intervals[9,1], all_accuracy_data_averaged$averaged_accuracy[1] + confidence_intervals[6,1],
                       all_accuracy_data_averaged$averaged_accuracy[8] + confidence_intervals[7,1] + confidence_intervals[10,1]
          )
lower_ci = t(lower_ci)
colnames(lower_ci) <- c("lower_ci")

upper_ci <- data.frame(confidence_intervals[3,2], all_accuracy_data_averaged$averaged_accuracy[2] + confidence_intervals[7,2], all_accuracy_data_averaged$averaged_accuracy[1] + confidence_intervals[4,2],
                       all_accuracy_data_averaged$averaged_accuracy[4] + confidence_intervals[7,2] + confidence_intervals[8,2], all_accuracy_data_averaged$averaged_accuracy[1] + confidence_intervals[5,2],
                       all_accuracy_data_averaged$averaged_accuracy[6] + confidence_intervals[7,2] + confidence_intervals[9,2], all_accuracy_data_averaged$averaged_accuracy[1] + confidence_intervals[6,2],
                       all_accuracy_data_averaged$averaged_accuracy[8] + confidence_intervals[7,2] + confidence_intervals[10,2]
)
upper_ci = t(upper_ci)
colnames(upper_ci) <- c("upper_ci")


confidence_interval_dataframe$lower_ci <- lower_ci
all_accuracy_data_averaged$lower_ci <- lower_ci
confidence_interval_dataframe$upper_ci <- upper_ci
all_accuracy_data_averaged$upper_ci <- upper_ci


# young adult nback 0
#confidence_interval_data_frame = rbind(all_accuracy_data_averaged$averaged_accuracy -



all_responseTime_data_averaged <- aggregate(all_results_data["median_response_time"], by=list(all_results_data$group, all_results_data$nback_level),FUN=mean)
colnames(all_responseTime_data_averaged) <- c("group", "nback_level", "averaged_response_time")

responseTime_std <- aggregate(all_results_data["median_response_time"], by=list(all_results_data$group, all_results_data$nback_level),FUN=sd)
colnames(responseTime_std) <- c("group", "nback_level", "std")

all_responseTime_data_averaged$std <- responseTime_std$std
all_responseTime_data_averaged$lower <- all_responseTime_data_averaged$averaged_response_time - all_responseTime_data_averaged$std
all_responseTime_data_averaged$upper <- all_responseTime_data_averaged$averaged_response_time + all_responseTime_data_averaged$std


all_falsefires_data_averaged <- aggregate(all_results_data["number_of_false_fires"], by=list(all_results_data$group, all_results_data$nback_level),FUN=mean)
colnames(all_falsefires_data_averaged) <- c("group", "nback_level", "averaged_number_of_false_fires")

falsefires_std <- aggregate(all_results_data["number_of_false_fires"], by=list(all_results_data$group, all_results_data$nback_level),FUN=sd)
colnames(falsefires_std) <- c("group", "nback_level", "std")

all_falsefires_data_averaged$std <- falsefires_std$std
all_falsefires_data_averaged$lower <- all_falsefires_data_averaged$averaged_number_of_false_fires - all_falsefires_data_averaged$std
all_falsefires_data_averaged$upper <- all_falsefires_data_averaged$averaged_number_of_false_fires + all_falsefires_data_averaged$std


all_dprime_data_averaged <- aggregate(all_results_data["dprime"], by=list(all_results_data$group, all_results_data$nback_level),FUN=mean)
colnames(all_dprime_data_averaged) <- c("group", "nback_level", "averaged_dprime")

dprime_std <- aggregate(all_results_data["dprime"], by=list(all_results_data$group, all_results_data$nback_level),FUN=sd)
colnames(dprime_std) <- c("group", "nback_level", "std")

all_dprime_data_averaged$std <- dprime_std$std
all_dprime_data_averaged$lower <- all_dprime_data_averaged$averaged_dprime - all_dprime_data_averaged$std
all_dprime_data_averaged$upper <- all_dprime_data_averaged$averaged_dprime + all_dprime_data_averaged$std


accuracy_file_name_tiff = paste0("GroupComparison_Accuracy",".tiff")
file = file.path("Figures", accuracy_file_name_tiff)
ggplot(data=all_accuracy_data_averaged, aes(fill = factor(group), x = factor(nback_level), y=averaged_accuracy)) + geom_bar(position = "dodge", stat = "identity") +
  geom_errorbar(aes(ymin=lower_std, ymax=upper_std), width=.2, position = position_dodge(width = 0.9), color = "white")  +
  scale_y_continuous(name = "Accuracy (%)",
                     breaks = seq(0, 100, 10),
                     limits=c(0, 120)) +
  scale_x_discrete(name = "Nback Level") +
  ggtitle("Group Comparison Accuracy") +
  theme(plot.title = element_text(hjust = 0.5, size = 14, family = "Tahoma", face = "bold", color = "white"),
        #text = element_text(size = 12, family = "Tahoma"),
        axis.title = element_text(face="bold", color = "white"),
        axis.text.x=element_text(size = 11, color = "white"),
        axis.text.y=element_text(size = 11, color = "white"),
        axis.ticks = element_line(colour = 'white', size = .5),
        legend.position = "bottom",
        legend.text = element_text(color = "white"),
        legend.background = element_rect(fill = "#1e1e1e"),
        legend.title =  element_text(color = "white"),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "#1e1e1e"),
        plot.background = element_rect(fill = "#1e1e1e", color = "#1e1e1e"),
        axis.line = element_line(colour = "white",size=0.1, linetype = "dotted"))  +
  scale_fill_manual(values=c("purple","green")) +
  labs(fill = "ISI")
ggsave(file)

accuracy_file_name_tiff = paste0("GroupComparison_Accuracy_NoLabels",".tiff")
file = file.path("Figures", accuracy_file_name_tiff)
ggplot(data=all_accuracy_data_averaged, aes(fill = factor(group), x = factor(nback_level), y=averaged_accuracy)) + geom_bar(position = "dodge", stat = "identity") +
  geom_errorbar(aes(ymin=lower_std, ymax=upper_std), width=.2, position = position_dodge(width = 0.9), color = "white")  +
  scale_y_continuous(name = "Accuracy (%)",
                     breaks = seq(0, 100, 10),
                     limits=c(0, 120)) +
  scale_x_discrete(name = "Nback Level") +
  ggtitle("Group Comparison Accuracy") +
  theme(plot.title = element_blank(),
        #text = element_text(size = 12, family = "Tahoma"),
        axis.title = element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks = element_line(colour = 'white', size = .5),
        legend.position = "none",
        legend.text = element_text(color = "white"),
        legend.background = element_rect(fill = "#1e1e1e"),
        legend.title =  element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "#1e1e1e"),
        plot.background = element_rect(fill = "#1e1e1e", color = "#1e1e1e"),
        axis.line = element_line(colour = "white",size=0.1, linetype = "dotted"))  +
  scale_fill_manual(values=c("purple","green")) +
  labs(fill = "ISI")
ggsave(file)


responsetime_file_name_tiff = paste0("GroupComparison_ResponseTime",".tiff")
file = file.path("Figures", responsetime_file_name_tiff)
ggplot(data=all_responseTime_data_averaged, aes(fill = factor(group), x = factor(nback_level), y=averaged_response_time)) + geom_bar(position = "dodge", stat = "identity") +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.2, position = position_dodge(width = 0.9), color = "white") +
  scale_y_continuous(name = "Response Time (ms)",
                     breaks = seq(0, 1000, 50),
                     limits=c(0, 1000)) +
  scale_x_discrete(name = "Nback Level") +
  ggtitle("Group Comparison Response Time") +
  theme(plot.title = element_text(hjust = 0.5, size = 14, family = "Tahoma", face = "bold", color = "white"),
        #text = element_text(size = 12, family = "Tahoma"),
        axis.title = element_text(face="bold", color = "white"),
        axis.text.x=element_text(size = 11, color = "white"),
        axis.text.y=element_text(size = 11, color = "white"),
        axis.ticks = element_line(colour = 'white', size = .5),
        legend.position = "bottom",
        legend.text = element_text(color = "white"),
        legend.background = element_rect(fill = "#1e1e1e"),
        legend.title =  element_text(color = "white"),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "#1e1e1e"),
        plot.background = element_rect(fill = "#1e1e1e", color = "#1e1e1e"),
        axis.line = element_line(colour = "white",size=0.1, linetype = "dotted"))  +
  scale_fill_manual(values=c("purple","green")) +
  labs(fill = "ISI")
ggsave(file)


responsetime_file_name_tiff = paste0("GroupComparison_ResponseTime_NoLabels",".tiff")
file = file.path("Figures", responsetime_file_name_tiff)
ggplot(data=all_responseTime_data_averaged, aes(fill = factor(group), x = factor(nback_level), y=averaged_response_time)) + geom_bar(position = "dodge", stat = "identity") +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.2, position = position_dodge(width = 0.9), color = "white") +
  scale_y_continuous(name = "Response Time (ms)",
                     breaks = seq(0, 1000, 50),
                     limits=c(0, 1000)) +
  scale_x_discrete(name = "Nback Level") +
  ggtitle("Group Comparison Response Time") +
  theme(plot.title = element_blank(),
        #text = element_text(size = 12, family = "Tahoma"),
        axis.title = element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks = element_line(colour = 'white', size = .5),
        legend.position = "none",
        legend.text = element_text(color = "white"),
        legend.background = element_rect(fill = "#1e1e1e"),
        legend.title =  element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "#1e1e1e"),
        plot.background = element_rect(fill = "#1e1e1e", color = "#1e1e1e"),
        axis.line = element_line(colour = "white",size=0.1, linetype = "dotted"))  +
  scale_fill_manual(values=c("purple","green")) +
  labs(fill = "ISI")
ggsave(file)



falsefire_file_name_tiff = paste0("GroupComparison_FalseFires",".tiff")
file = file.path("Figures", falsefire_file_name_tiff)
ggplot(all_falsefires_data_averaged, aes(fill = factor(group), x = factor(nback_level), y=averaged_number_of_false_fires)) + geom_bar(position = "dodge", stat = "identity") + # + stat_count(width = 0.5, fill="blue") + #geom_bar(position = "dodge", stat="bin") +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.2, position = position_dodge(width = 0.9), color = "white") +
  ggtitle("Group Comparison False Fire Rate") +
  scale_y_continuous(name = "Number of False Fires") +
  scale_x_discrete(name = "Nback Level") +
  theme(plot.title = element_text(hjust = 0.5, size = 14, family = "Tahoma", face = "bold", color = "white"),
        #text = element_text(size = 12, family = "Tahoma"),
        axis.title = element_text(face="bold", color = "white"),
        axis.text.x=element_text(size = 11, color = "white"),
        axis.text.y=element_text(size = 11, color = "white"),
        axis.ticks = element_line(colour = 'white', size = .5),
        legend.position = "bottom",
        legend.text = element_text(color = "white"),
        legend.background = element_rect(fill = "#1e1e1e"),
        legend.title =  element_text(color = "white"),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "#1e1e1e"),
        plot.background = element_rect(fill = "#1e1e1e", color = "#1e1e1e"),
        axis.line = element_line(colour = "white",size=0.1, linetype = "dotted"))  +
  scale_fill_manual(values=c("purple","green")) +
  labs(fill = "ISI")
ggsave(file)

falsefire_file_name_tiff = paste0("GroupComparison_FalseFires_NoLabels",".tiff")
file = file.path("Figures", falsefire_file_name_tiff)
ggplot(all_falsefires_data_averaged, aes(fill = factor(group), x = factor(nback_level), y=averaged_number_of_false_fires)) + geom_bar(position = "dodge", stat = "identity") + # + stat_count(width = 0.5, fill="blue") + #geom_bar(position = "dodge", stat="bin") +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.2, position = position_dodge(width = 0.9), color = "white") +
  ggtitle("Group Comparison False Fire Rate") +
  scale_y_continuous(name = "Number of False Fires") +
  scale_x_discrete(name = "Nback Level") +
  theme(plot.title = element_blank(),
        #text = element_text(size = 12, family = "Tahoma"),
        axis.title = element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks = element_line(colour = 'white', size = .5),
        legend.position = "none",
        legend.text = element_text(color = "white"),
        legend.background = element_rect(fill = "#1e1e1e"),
        legend.title =  element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "#1e1e1e"),
        plot.background = element_rect(fill = "#1e1e1e", color = "#1e1e1e"),
        axis.line = element_line(colour = "white",size=0.1, linetype = "dotted"))  +
  scale_fill_manual(values=c("purple","green")) +
  labs(fill = "ISI")
ggsave(file)

dprime_file_name_tiff = paste0("GroupComparison_dprime",".tiff")
file = file.path("Figures", dprime_file_name_tiff)
ggplot(all_dprime_data_averaged, aes(fill = factor(group), x = factor(nback_level), y=averaged_dprime)) + geom_bar(position = "dodge", stat = "identity") + # + stat_count(width = 0.5, fill="blue") + #geom_bar(position = "dodge", stat="bin") +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.2, position = position_dodge(width = 0.9), color = "white") +
  ggtitle( "Group Comparison Sensitivity Analysis") +
  scale_y_continuous(name = "Z value (hit rate - false alarm)") +
  scale_x_discrete(name = "Nback Level") +
  theme(plot.title = element_text(hjust = 0.5, size = 14, family = "Tahoma", face = "bold", color = "white"),
        #text = element_text(size = 12, family = "Tahoma"),
        axis.title = element_text(face="bold", color = "white"),
        axis.text.x=element_text(size = 11, color = "white"),
        axis.text.y=element_text(size = 11, color = "white"),
        axis.ticks = element_line(colour = 'white', size = .5),
        legend.position = "bottom",
        legend.text = element_text(color = "white"),
        legend.background = element_rect(fill = "#1e1e1e"),
        legend.title =  element_text(color = "white"),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "#1e1e1e"),
        plot.background = element_rect(fill = "#1e1e1e", color = "#1e1e1e"),
        axis.line = element_line(colour = "white",size=0.1, linetype = "dotted")) +
  scale_fill_manual(values=c("purple","green")) +
  labs(fill = "ISI")
ggsave(file)


dprime_file_name_tiff = paste0("GroupComparison_dprime_NoLabels",".tiff")
file = file.path("Figures", dprime_file_name_tiff)
ggplot(all_dprime_data_averaged, aes(fill = factor(group), x = factor(nback_level), y=averaged_dprime)) + geom_bar(position = "dodge", stat = "identity") + # + stat_count(width = 0.5, fill="blue") + #geom_bar(position = "dodge", stat="bin") +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.2, position = position_dodge(width = 0.9), color = "white") +
  ggtitle( "Group Comparison Sensitivity Analysis") +
  scale_y_continuous(name = "Z value (hit rate - false alarm)") +
  scale_x_discrete(name = "Nback Level") +
  theme(plot.title = element_blank(),
        #text = element_text(size = 12, family = "Tahoma"),
        axis.title = element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks = element_line(colour = 'white', size = .5),
        legend.position = "none",
        legend.text = element_text(color = "white"),
        legend.background = element_rect(fill = "#1e1e1e"),
        legend.title = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "#1e1e1e"),
        plot.background = element_rect(fill = "#1e1e1e", color = "#1e1e1e"),
        axis.line = element_line(colour = "white",size=0.1, linetype = "dotted")) +
  scale_fill_manual(values=c("purple","green")) +
  labs(fill = "ISI")
ggsave(file)

#response_nback <- lmer(subject_response_onset_correct ~ as.character(nback_level_correct)*as.character(interstimulus_interval_correct ) + (1|subject_id), data=this_group_response_data)  ##obviously not emotion condition

#summary(response_nback)
#anova(response_nback)

#rg_response_nback <- ref_grid(response_nback)
#results <- emmeans(rg_response_nback, "nback")

#confint(response_nback)


#library(nlme)
#TF_GUESS_MODEL <- lme(subject_response_onset_correct ~ nback_level_correct + (1|subject_id), data=this_group_response_data)  ##obviously not emotion condition
#summary(TF_GUESS_MODEL)
#anova(TF_GUESS_MODEL)

##  IDK, maybe its predcting accuracy by ISI?
#Accuracy_guess_model<- lmer(subject_accuracy ~ ISI + (1|subject_id), data=this_group_accuracy_data)  ##obviously not emotion condition
#summary(Accuracy_guess_model)
#anova(Accuracy_guess_model)

##could it be predicting accuracy by nback level?


#Accuracy_guess_model2<- lmer(subject_accuracy ~ as.character(nback) + (1|subject_id), data=this_group_accuracy_data)  ##obviously not emotion condition
#summary(Accuracy_guess_model2)

#anova(Accuracy_guess_model2)
#rg_Accuracy_guess_model2 <- ref_grid(Accuracy_guess_model2)
#results <- emmeans(rg_Accuracy_guess_model2, "nback")

#confint(Accuracy_guess_model2)



#NonRM_Acc_model2 <- aov(subject_accuracy ~ as.character(nback) + (1|subject_id), data=this_group_accuracy_data)
#summary(NonRM_Acc_model2)
#TukeyHSD(NonRM_Acc_model2, "nback")




#doesnt seem to work, similar to 2
#library(nlme)
#anova.lme(Accuracy_guess_model3)
#Accuracy_guess_model3 = lme(subject_accuracy ~ nback + (1|subject_id),
#             random = ~1,
#             data=this_group_accuracy_data,
#             method="REML")




#This is supposed to be an attempt at some post-hoc, additional comments are provided from the stats class, as was the code
#
#(leastsquare1b = lsmeans::lsmeans(Accuracy_guess_model2,
#                                  pairwise ~ task*Condition,
#                                  adjust="none") )      ###  Tukey-adjusted comparisons
#leastsquare1b$contrasts
#lsmeans adjust options include
#"tukey", "scheffe", "sidak", "bonferroni", "dunnettx", "mvt", and "none"
#These are tricky to look at, but they have the information we need
#Some contrasts compare different treatments at pretest or at posttest
#These will give us the equivalent of SPSS simple effects tables
}

