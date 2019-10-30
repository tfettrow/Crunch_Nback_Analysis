analyze_nback_group <- function(group_name, subject_paths)
{
  library(readxl)
  library(stringi)
  library(lattice)
  library(ggplot2)
  library(rprime)
  library(lme4)
  library(emmeans)
  library(lmerTest)

  all_results_data <- vector()

  for (this_subject_path in subject_paths){
    this_subject_path_string_split = strsplit(this_subject_path,"/")[1][1]
    this_subject_id = vapply(this_subject_path_string_split, tail, "", 1)

    # Goes into Subject directory to load file
    # A not very elegant way of creating folders and then navigating to dir to read
    setwd(this_subject_path)
    setwd("..")
    dir.create("Group_Results")
    setwd("Group_Results")
    dir.create("Figures")
    dir.create("Nback_files")
    setwd("..")
    setwd(this_subject_path)

    results_data = read.csv(file.path(paste0("Processed/Nback_files/results_", toString(this_subject_id), ".csv")))
    results_data$subject_id = this_subject_id
    all_results_data = rbind(all_results_data, results_data)

     setwd("..")
  }

  write.csv(all_results_data, file = file.path("Group_Results/Nback_files", paste0(toString(group_name), "_results",".csv")))


  all_accuracy_data_averaged <- aggregate(all_results_data["percent_correct"], by=list(all_results_data$ISI, all_results_data$nback_level),FUN=mean)
  colnames(all_accuracy_data_averaged) <- c("ISI", "nback_level", "averaged_accuracy")

  accuracy_std <- aggregate(all_results_data["percent_correct"], by=list(all_results_data$ISI, all_results_data$nback_level),FUN=sd)
  colnames(accuracy_std) <- c("ISI", "nback_level", "std")

  all_accuracy_data_averaged$std <- accuracy_std$std
  all_accuracy_data_averaged$lower <- all_accuracy_data_averaged$averaged_accuracy - all_accuracy_data_averaged$std
  all_accuracy_data_averaged$upper <- all_accuracy_data_averaged$averaged_accuracy + all_accuracy_data_averaged$std


  all_responseTime_data_averaged <- aggregate(all_results_data["median_response_time"], by=list(all_results_data$ISI, all_results_data$nback_level),FUN=mean)
  colnames(all_responseTime_data_averaged) <- c("ISI", "nback_level", "averaged_response_time")

  responseTime_std <- aggregate(all_results_data["median_response_time"], by=list(all_results_data$ISI, all_results_data$nback_level),FUN=sd)
  colnames(responseTime_std) <- c("ISI", "nback_level", "std")

  all_responseTime_data_averaged$std <- responseTime_std$std
  all_responseTime_data_averaged$lower <- all_responseTime_data_averaged$averaged_response_time - all_responseTime_data_averaged$std
  all_responseTime_data_averaged$upper <- all_responseTime_data_averaged$averaged_response_time + all_responseTime_data_averaged$std


  all_falsefires_data_averaged <- aggregate(all_results_data["number_of_false_fires"], by=list(all_results_data$ISI, all_results_data$nback_level),FUN=mean)
  colnames(all_falsefires_data_averaged) <- c("ISI", "nback_level", "averaged_number_of_false_fires")

  falsefires_std <- aggregate(all_results_data["number_of_false_fires"], by=list(all_results_data$ISI, all_results_data$nback_level),FUN=sd)
  colnames(falsefires_std) <- c("ISI", "nback_level", "std")

  all_falsefires_data_averaged$std <- falsefires_std$std
  all_falsefires_data_averaged$lower <- all_falsefires_data_averaged$averaged_number_of_false_fires - all_falsefires_data_averaged$std
  all_falsefires_data_averaged$upper <- all_falsefires_data_averaged$averaged_number_of_false_fires + all_falsefires_data_averaged$std


  all_dprime_data_averaged <- aggregate(all_results_data["dprime"], by=list(all_results_data$ISI, all_results_data$nback_level),FUN=mean)
  colnames(all_dprime_data_averaged) <- c("ISI", "nback_level", "averaged_dprime")

  dprime_std <- aggregate(all_results_data["dprime"], by=list(all_results_data$ISI, all_results_data$nback_level),FUN=sd)
  colnames(dprime_std) <- c("ISI", "nback_level", "std")

  all_dprime_data_averaged$std <- dprime_std$std
  all_dprime_data_averaged$lower <- all_dprime_data_averaged$averaged_dprime - all_dprime_data_averaged$std
  all_dprime_data_averaged$upper <- all_dprime_data_averaged$averaged_dprime + all_dprime_data_averaged$std


  #  -----------------------------------------------------------------------------------------------
  # # create LMERs

  this_group_accuracy_model <- lmer(percent_correct ~ as.character(nback_level)*as.character(ISI) + (1|subject_id), data=all_results_data)  ##obviously not emotion condition
  summary(this_group_accuracy_model)
  anova(this_group_accuracy_model)
  accuracy_confint_output <- confint(this_group_accuracy_model)

  # accuracy_confint_diff_matrix <- matrix(accuracy_confint_output, ncol = ncol(accuracy_confint_output))
  # colnames(accuracy_confint_diff_matrix) <- c("lower", "upper")
  #
  # accuracy_confint_matrix <-

    # all_accuracy_data_averaged


  this_group_responseTime_model <- lmer(median_response_time ~ as.character(nback_level)*as.character(ISI) + (1|subject_id), data=all_results_data)  ##obviously not emotion condition
  summary(this_group_responseTime_model)
  anova(this_group_responseTime_model)
  confint(this_group_responseTime_model)

  this_group_falsefire_model <- lmer(number_of_false_fires ~ as.character(nback_level)*as.character(ISI) + (1|subject_id), data=all_results_data)  ##obviously not emotion condition
  summary(this_group_falsefire_model)
  anova(this_group_falsefire_model)
  confint(this_group_falsefire_model)

  this_group_dprime_model <- lmer(dprime ~ as.character(nback_level)*as.character(ISI) + (1|subject_id), data=all_results_data)  ##obviously not emotion condition
  summary(this_group_dprime_model)
  anova(this_group_dprime_model)
  confint(this_group_dprime_model)

  #  -----------------------------------------------------------------------------------------------

  # # PLOT # #
  accuracy_file_name_tiff = paste0(toString(group_name), "_Accuracy",".tiff")
  file = file.path("Group_Results/Figures",accuracy_file_name_tiff)
  ggplot(data=all_accuracy_data_averaged, aes(fill = factor(ISI), x = factor(nback_level), y=averaged_accuracy)) + geom_bar(position = "dodge", stat = "identity") +
    geom_errorbar(aes(ymin=lower, ymax=upper), width=.2, position = position_dodge(width = 0.9)) +
    scale_y_continuous(name = "Accuracy (%)",
                       breaks = seq(0, 100, 10),
                       limits=c(0, 100)) +
    scale_x_discrete(name = "Nback Level") +
    ggtitle(paste(toString(group_name),"Group Accuracy")) +
    theme(plot.title = element_text(hjust = 0.5, size = 14, family = "Tahoma", face = "bold"),
          text = element_text(size = 12, family = "Tahoma"),
          axis.title = element_text(face="bold"),
          axis.text.x=element_text(size = 11),
          legend.position = "bottom") +
    scale_fill_manual(values=c("orange","blue")) +
    labs(fill = "ISI")
  ggsave(file)


  responsetime_file_name_tiff = paste0(toString(group_name), "_ResponseTime",".tiff")
  file = file.path("Group_Results/Figures", responsetime_file_name_tiff)
  ggplot(data=all_responseTime_data_averaged, aes(fill = factor(ISI), x = factor(nback_level), y=averaged_response_time)) + geom_bar(position = "dodge", stat = "identity") +
    geom_errorbar(aes(ymin=lower, ymax=upper), width=.2, position = position_dodge(width = 0.9)) +
    scale_y_continuous(name = "Response Time (ms)",
                       breaks = seq(0, 1000, 50),
                       limits=c(0, 1000)) +
    scale_x_discrete(name = "Nback Level") +
    ggtitle(paste(toString(group_name),"Group Response Time")) +
    theme(plot.title = element_text(hjust = 0.5, size = 14, family = "Tahoma", face = "bold"),
          text = element_text(size = 12, family = "Tahoma"),
          axis.title = element_text(face="bold"),
          axis.text.x=element_text(size = 11),
          legend.position = "bottom") +
    scale_fill_manual(values=c("orange","blue")) +
    labs(fill = "ISI")
  ggsave(file)

  falsefire_file_name_tiff = paste0(toString(group_name), "_FalseFires",".tiff")
  file = file.path(subject_path,"Figures",falsefire_file_name_tiff)
  ggplot(all_falsefires_data_averaged, aes(fill = factor(ISI), x = factor(nback_level), y=averaged_number_of_false_fires)) + geom_bar(position = "dodge", stat = "identity") + # + stat_count(width = 0.5, fill="blue") + #geom_bar(position = "dodge", stat="bin") +
    geom_errorbar(aes(ymin=lower, ymax=upper), width=.2, position = position_dodge(width = 0.9)) +
    ggtitle(paste(toString(group_name), "Group False Fire Rate")) +
    scale_y_continuous(name = "Number of False Fires") +
    scale_x_discrete(name = "Nback Level") +
    theme(plot.title = element_text(hjust = 0.5, size = 14, family = "Tahoma", face = "bold"),
          text = element_text(size = 12, family = "Tahoma"),
          axis.title = element_text(face="bold"),
          axis.text.x=element_text(size = 11),
          legend.position = "bottom") +
    scale_fill_manual(values=c("orange","blue")) +
    labs(fill = "ISI")
  ggsave(file)

  dprime_file_name_tiff = paste0(toString(group_name), "_dprime",".tiff")
  file = file.path(subject_path,"Figures",dprime_file_name_tiff)
  ggplot(all_dprime_data_averaged, aes(fill = factor(ISI), x = factor(nback_level), y=averaged_dprime)) + geom_bar(position = "dodge", stat = "identity") + # + stat_count(width = 0.5, fill="blue") + #geom_bar(position = "dodge", stat="bin") +
    geom_errorbar(aes(ymin=lower, ymax=upper), width=.2, position = position_dodge(width = 0.9)) +
    ggtitle(paste(toString(group_name), "Group Sensitivity Analysis")) +
    scale_y_continuous(name = "Z value (hit rate - false alarm)") +
    scale_x_discrete(name = "Nback Level") +
    theme(plot.title = element_text(hjust = 0.5, size = 14, family = "Tahoma", face = "bold"),
          text = element_text(size = 12, family = "Tahoma"),
          axis.title = element_text(face="bold"),
          axis.text.x=element_text(size = 11),
          legend.position = "bottom") +
    scale_fill_manual(values=c("orange","blue")) +
    labs(fill = "ISI")
  ggsave(file)
}

