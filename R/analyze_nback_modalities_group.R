analyze_nback_modalities_group <- function(subject_ids)
{
  library(readxl)
  library(stringi)
  library(lattice)
  library(ggplot2)
  library(rprime)
  library(lme4)
  library(purrr)
  library(dplyr)
  library(emmeans)
  library(lmerTest)
  library(plotrix)                                    # Load plotrix package
  all_results_data <- vector()
  dates_completed <- vector()


  for (this_subject_path in subject_ids){
    this_subject_path_string_split = strsplit(this_subject_path,"/")[1][1]
    this_subject_id = vapply(this_subject_path_string_split, tail, "", 1)

    color_cell <- vector()
    color_cell = c("royalblue3","cyan3","darkgreen")

    # two sets of results...
    # one based on modality specific results, and the other based on order of acquisition
      # order of acquisition operations
        # go into modality folder, and read the file creation date, and store that info in a cell
        # after concatenating all files for this subj, order the data into all_results_data_ordered
    if (dir.exists(paste0("fnirs/",toString(this_subject_id))))
    {
      fNIRS_nback_data<-read.csv(paste0("fnirs/",toString(this_subject_id),"/Processed/Nback_files/results_",toString(this_subject_id),".csv"))
      fNIRS_nback_data$modality = "fnirs"
      fNIRS_nback_data$subject_id = this_subject_id
      all_results_data = rbind(all_results_data, fNIRS_nback_data)
      fnirs_date <- read.table(paste0("fnirs/",toString(this_subject_id),"/Raw/Nback_files/date_completed.txt"),header = TRUE)
      fnirs_date_completed <- fnirs_date
      fnirs_date_completed$modality <- "fnirs"
      fnirs_date_completed$subject <- this_subject_id
      dates_completed <- rbind(dates_completed, fnirs_date_completed)
    }

    if (dir.exists(paste0("eeg/",toString(this_subject_id))))
    {
      EEG_nback_data<-read.csv(paste0("eeg/",toString(this_subject_id),"/Processed/Nback_files/results_",toString(this_subject_id),".csv"))
      EEG_nback_data$modality = "eeg"
      EEG_nback_data$subject_id = this_subject_id
      all_results_data = rbind(all_results_data, EEG_nback_data)
      eeg_date <- read.table(paste0("eeg/",toString(this_subject_id),"/Raw/Nback_files/date_completed.txt"),header = TRUE)
      eeg_date_completed <- eeg_date
      eeg_date_completed$modality <- "eeg"
      eeg_date_completed$subject <- this_subject_id
      dates_completed <- rbind(dates_completed, eeg_date_completed)
    }

    if (dir.exists(paste0("fmri/",toString(this_subject_id))))
    {
      fMRI_nback_data<-read.csv(paste0("fmri/",toString(this_subject_id),"/Processed/Nback_files/results_",toString(this_subject_id),".csv"))
      fMRI_nback_data$modality = "fmri"
      fMRI_nback_data$subject_id = this_subject_id
      all_results_data = rbind(all_results_data, fMRI_nback_data)
      fmri_date <- read.table(paste0("fmri/",toString(this_subject_id),"/Raw/Nback_files/date_completed.txt"),header = TRUE)
      fmri_date_completed <- fmri_date
      fmri_date_completed$modality <- "fmri"
      fmri_date_completed$subject <- this_subject_id
      dates_completed <- rbind(dates_completed, fmri_date_completed)
    }
    }


  dates_completed <- dates_completed %>% arrange(dates_completed$subject,as.character(dates_completed$date))

  dates_completed$order <- matrix(NA,length(dates_completed$date), ncol=1)
  # assigning order to the dates_completed
  for (this_subject in unique(dates_completed$subject)){
    this_subject_indices_to_order <- which(dates_completed$subject == this_subject)
    dates_completed$order[this_subject_indices_to_order] <- order(dates_completed$date[this_subject_indices_to_order])
  }

  for (this_subject in unique(all_results_data$subject_id)){
    this_subject_indices_to_apply_order <- which(all_results_data$subject_id == this_subject)
    for (this_modality in unique(all_results_data$modality[this_subject_indices_to_apply_order])){
      this_subject_modality_all_results_indices = which(all_results_data$modality[this_subject_indices_to_apply_order] == this_modality)
      this_subject_modality_order_index = which(dates_completed$modality == this_modality & dates_completed$subject == this_subject)
      all_results_data$order[this_subject_indices_to_apply_order[this_subject_modality_all_results_indices]] = dates_completed$order[this_subject_modality_order_index]
    }
  }

  modality_percent_data_averaged <- aggregate(all_results_data["percent_correct"], by=list(all_results_data$modality, all_results_data$nback_level, all_results_data$ISI),FUN=mean)
  colnames(modality_percent_data_averaged) <- c("modality", "nback_level", "ISI", "percent_correct")

  modality_accuracy_std <- aggregate(all_results_data["percent_correct"], by=list(all_results_data$modality, all_results_data$nback_level, all_results_data$ISI),FUN=sd,na.rm=T) #,FUN = function(x) c(se = std.error(x)))#,FUN=sd)
  colnames(modality_accuracy_std) <- c("modality", "nback_level", "ISI", "std")

  modality_percent_data_averaged$std <- accuracy_std$std
  modality_percent_data_averaged$lower <- modality_percent_data_averaged$percent_correct - modality_percent_data_averaged$std
  modality__percent_data_averaged$upper <- modality__percent_data_averaged$percent_correct + modality__percent_data_averaged$std




  modality_responsetime_data_averaged <- aggregate(all_results_data["median_response_time"], by=list(all_results_data$modality, all_results_data$nback_level, all_results_data$ISI),FUN=mean,na.rm=T)
  colnames(modality_responsetime_data_averaged) <- c("modality", "nback_level", "ISI", "median_response_time")

  modality_responsetime_std <- aggregate(all_results_data["median_response_time"], by=list(all_results_data$modality, all_results_data$nback_level, all_results_data$ISI),FUN = function(x) c(se = std.error(x)))#,FUN=sd,na.rm=T)
  colnames(modality_responsetime_std) <- c("modality", "nback_level", "ISI", "std")

  modality_responsetime_data_averaged$std <- modality_responsetime_std$std
  modality_responsetime_data_averaged$lower <- modality_responsetime_data_averaged$median_response_time - modality_responsetime_data_averaged$std
  modality_responsetime_data_averaged$upper <- modality_responsetime_data_averaged$median_response_time + modality_responsetime_data_averaged$std




  modality_percentFalsefires_data_averaged <- aggregate(all_results_data["percent_of_false_fires"], by=list(all_results_data$modality, all_results_data$nback_level, all_results_data$ISI),FUN=mean,na.rm=T)
  colnames(modality_percentFalsefires_data_averaged) <- c("modality", "nback_level", "ISI", "percent_of_false_fires")

  modality_percentFalsefires_std <- aggregate(all_results_data["percent_of_false_fires"], by=list(all_results_data$modality, all_results_data$nback_level, all_results_data$ISI),FUN = function(x) c(se = std.error(x)))#,FUN=sd,na.rm=T)
  colnames(modality_percentFalsefires_std) <- c("modality", "nback_level", "ISI", "std")

  modality_percentFalsefires_data_averaged$std <- modality_percentFalsefires_std$std
  modality_percentFalsefires_data_averaged$lower <- modality_percentFalsefires_data_averaged$percent_of_false_fires - modality_percentFalsefires_data_averaged$std
  modality_percentFalsefires_data_averaged$upper <- modality_percentFalsefires_data_averaged$percent_of_false_fires + modality_percentFalsefires_data_averaged$std




  modality_dprime_data_averaged <- aggregate(all_results_data["dprime"], by=list(all_results_data$modality, all_results_data$nback_level, all_results_data$ISI),FUN=mean,na.rm=T)
  colnames(modality_dprime_data_averaged) <- c("modality", "nback_level", "ISI", "dprime")

  modality_dprime_std <- aggregate(all_results_data["dprime"], by=list(all_results_data$modality, all_results_data$nback_level, all_results_data$ISI),FUN = function(x) c(se = std.error(x)))
  colnames(modality_dprime_std) <- c("modality", "nback_level", "ISI", "std")

  modality_dprime_data_averaged$std <- modality_dprime_std$std
  modality_dprime_data_averaged$lower <- modality_dprime_data_averaged$dprime - modality_dprime_data_averaged$std
  modality_dprime_data_averaged$upper <- modality_dprime_data_averaged$dprime + modality_dprime_data_averaged$std


  accuracy_file_name_tiff = paste0("AvgModalityComparison_Accuracy",".tiff")
  file = file.path("Comparison_Figures",accuracy_file_name_tiff)
  ggplot(data=modality_percent_data_averaged, aes(fill = factor(modality), x = factor(nback_level), y=percent_correct)) + geom_bar(position = "dodge", stat = "identity") +
    geom_errorbar(aes(ymin=lower, ymax=upper), width=.2, position = position_dodge(width = 0.9), color = "white") +
    ggtitle("Group Average Nback vs Accuracy (Modalities)") +
    scale_y_continuous(name = "Accuracy (%)") +
    scale_x_discrete(name = "Nback Level") +
    theme(plot.title = element_text(hjust = 0.5, size = 14, family = "Tahoma", face = "bold", color = "white"),
          axis.title = element_text(face="bold", color = "white"),
          axis.text.x=element_text(size = 11, color = "white"),
          axis.text.y=element_text(size = 11, color = "white"),
          axis.ticks = element_line(colour = 'white', size = .5),
          legend.position = "bottom",
          legend.text = element_text(color = "white"),
          legend.background = element_rect(fill = "#1e1e1e"),
          panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_rect(fill = "#1e1e1e"),
          plot.background = element_rect(fill = "#1e1e1e", color = "#1e1e1e"),
          axis.line = element_line(colour = "white",size=0.1, linetype = "dotted"))  +
    scale_fill_manual(values=color_cell) +
    facet_grid(. ~ ISI) +
    labs(fill = "ISI", color="white")
  ggsave(file)

  responsetime_file_name_tiff = paste0("AvgModalityComparison_ResponseTime",".tiff")
  file = file.path("Comparison_Figures",responsetime_file_name_tiff)
  ggplot(data=modality_responsetime_data_averaged, aes(fill = factor(modality), x = factor(nback_level), y=median_response_time)) + geom_bar(position = "dodge", stat = "identity") +
    geom_errorbar(aes(ymin=lower, ymax=upper), width=.2, position = position_dodge(width = 0.9), color = "white") +
    ggtitle("Group Average Nback vs Response Time (Modalities)") +
    scale_y_continuous(name = "response time (ms)") +
    scale_x_discrete(name = "Nback Level") +
    theme(plot.title = element_text(hjust = 0.5, size = 14, family = "Tahoma", face = "bold", color = "white"),
          axis.title = element_text(face="bold", color = "white"),
          axis.text.x=element_text(size = 11, color = "white"),
          axis.text.y=element_text(size = 11, color = "white"),
          axis.ticks = element_line(colour = 'white', size = .5),
          legend.position = "bottom",
          legend.text = element_text(color = "white"),
          legend.background = element_rect(fill = "#1e1e1e"),
          panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_rect(fill = "#1e1e1e"),
          plot.background = element_rect(fill = "#1e1e1e", color = "#1e1e1e"),
          axis.line = element_line(colour = "white",size=0.1, linetype = "dotted"))  +
    scale_fill_manual(values=color_cell) +
    facet_grid(. ~ ISI) +
    labs(fill = "ISI", color="white")
  ggsave(file)

  percentFalsefires_file_name_tiff = paste0("AvgModalityComparison_FalseFire",".tiff")
  file = file.path("Comparison_Figures",percentFalsefires_file_name_tiff)
  ggplot(data=modality_percentFalsefires_data_averaged, aes(fill = factor(modality), x = factor(nback_level), y=percent_of_false_fires)) + geom_bar(position = "dodge", stat = "identity") +
    geom_errorbar(aes(ymin=lower, ymax=upper), width=.2, position = position_dodge(width = 0.9), color = "white") +
    ggtitle("Group Average Nback vs False Fires (Modalities)") +
    scale_y_continuous(name = "false fires (%)") +
    scale_x_discrete(name = "Nback Level") +
    theme(plot.title = element_text(hjust = 0.5, size = 14, family = "Tahoma", face = "bold", color = "white"),
          axis.title = element_text(face="bold", color = "white"),
          axis.text.x=element_text(size = 11, color = "white"),
          axis.text.y=element_text(size = 11, color = "white"),
          axis.ticks = element_line(colour = 'white', size = .5),
          legend.position = "bottom",
          legend.text = element_text(color = "white"),
          legend.background = element_rect(fill = "#1e1e1e"),
          panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_rect(fill = "#1e1e1e"),
          plot.background = element_rect(fill = "#1e1e1e", color = "#1e1e1e"),
          axis.line = element_line(colour = "white",size=0.1, linetype = "dotted"))  +
    scale_fill_manual(values=color_cell) +
    facet_grid(. ~ ISI) +
    labs(fill = "ISI", color="white")
  ggsave(file)

  dprime_file_name_tiff = paste0("AvgModalityComparison_dprime",".tiff")
  file = file.path("Comparison_Figures",dprime_file_name_tiff)
  ggplot(data=modality_dprime_data_averaged, aes(fill = factor(modality), x = factor(nback_level), y=dprime)) + geom_bar(position = "dodge", stat = "identity") +
    geom_errorbar(aes(ymin=lower, ymax=upper), width=.2, position = position_dodge(width = 0.9), color = "white") +
    ggtitle("Group Average Nback vs dprime (Modalities)") +
    scale_y_continuous(name = "dprime (z-score)") +
    scale_x_discrete(name = "Nback Level") +
    theme(plot.title = element_text(hjust = 0.5, size = 14, family = "Tahoma", face = "bold", color = "white"),
          axis.title = element_text(face="bold", color = "white"),
          axis.text.x=element_text(size = 11, color = "white"),
          axis.text.y=element_text(size = 11, color = "white"),
          axis.ticks = element_line(colour = 'white', size = .5),
          legend.position = "bottom",
          legend.text = element_text(color = "white"),
          legend.background = element_rect(fill = "#1e1e1e"),
          panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_rect(fill = "#1e1e1e"),
          plot.background = element_rect(fill = "#1e1e1e", color = "#1e1e1e"),
          axis.line = element_line(colour = "white",size=0.1, linetype = "dotted"))  +
    scale_fill_manual(values=color_cell) +
    facet_grid(. ~ ISI) +
    labs(fill = "ISI", color="white")
  ggsave(file)



  ########### Collection Order Figs ###################
  ordered_percent_data_averaged <- aggregate(all_results_data["percent_correct"], by=list(all_results_data$order, all_results_data$nback_level, all_results_data$ISI),FUN=mean)
  colnames(ordered_percent_data_averaged) <- c("order", "nback_level", "ISI", "percent_correct")

  ordered_accuracy_std <- aggregate(all_results_data["percent_correct"], by=list(all_results_data$order, all_results_data$nback_level, all_results_data$ISI),FUN=sd,na.rm=T) #,FUN = function(x) c(se = std.error(x)))#,FUN=sd)
  colnames(ordered_accuracy_std) <- c("order", "nback_level", "ISI", "std")

  ordered_percent_data_averaged$std <- ordered_accuracy_std$std
  ordered_percent_data_averaged$lower <- ordered_percent_data_averaged$percent_correct - ordered_percent_data_averaged$std
  ordered_percent_data_averaged$upper <- ordered_percent_data_averaged$percent_correct + ordered_percent_data_averaged$std


  ordered_responsetime_data_averaged <- aggregate(all_results_data["median_response_time"], by=list(all_results_data$order, all_results_data$nback_level, all_results_data$ISI),FUN=mean,na.rm=T)
  colnames(modality_responsetime_data_averaged) <- c("order", "nback_level", "ISI", "median_response_time")

  ordered_responsetime_std <- aggregate(all_results_data["median_response_time"], by=list(all_results_data$order, all_results_data$nback_level, all_results_data$ISI),FUN = function(x) c(se = std.error(x)))#,FUN=sd,na.rm=T)
  colnames(ordered_responsetime_std) <- c("order", "nback_level", "ISI", "std")

  ordered_responsetime_data_averaged$std <- ordered_responsetime_std$std
  ordered_responsetime_data_averaged$lower <- ordered_responsetime_data_averaged$median_response_time - ordered_responsetime_data_averaged$std
  ordered_responsetime_data_averaged$upper <- ordered_responsetime_data_averaged$median_response_time + ordered_responsetime_data_averaged$std




  ordered_percentFalsefires_data_averaged <- aggregate(all_results_data["percent_of_false_fires"], by=list(all_results_data$order, all_results_data$nback_level, all_results_data$ISI),FUN=mean,na.rm=T)
  colnames(ordered_percentFalsefires_data_averaged) <- c("order", "nback_level", "ISI", "percent_of_false_fires")

  ordered_percentFalsefires_std <- aggregate(all_results_data["percent_of_false_fires"], by=list(all_results_data$order, all_results_data$nback_level, all_results_data$ISI),FUN = function(x) c(se = std.error(x)))#,FUN=sd,na.rm=T)
  colnames(ordered_percentFalsefires_std) <- c("order", "nback_level", "ISI", "std")

  ordered_percentFalsefires_data_averaged$std <- ordered_percentFalsefires_std$std
  ordered_percentFalsefires_data_averaged$lower <- ordered_percentFalsefires_data_averaged$percent_of_false_fires - ordered_percentFalsefires_data_averaged$std
  ordered_percentFalsefires_data_averaged$upper <- ordered_percentFalsefires_data_averaged$percent_of_false_fires + ordered_percentFalsefires_data_averaged$std




  ordered_dprime_data_averaged <- aggregate(all_results_data["dprime"], by=list(all_results_data$order, all_results_data$nback_level, all_results_data$ISI),FUN=mean,na.rm=T)
  colnames(ordered_dprime_data_averaged) <- c("order", "nback_level", "ISI", "dprime")

  ordered_dprime_std <- aggregate(all_results_data["dprime"], by=list(all_results_data$order, all_results_data$nback_level, all_results_data$ISI),FUN = function(x) c(se = std.error(x)))
  colnames(ordered_dprime_std) <- c("order", "nback_level", "ISI", "std")

  ordered_dprime_data_averaged$std <- ordered_dprime_std$std
  ordered_dprime_data_averaged$lower <- ordered_dprime_data_averaged$dprime - ordered_dprime_data_averaged$std
  ordered_dprime_data_averaged$upper <- ordered_dprime_data_averaged$dprime + ordered_dprime_data_averaged$std


  accuracy_file_name_tiff = paste0("AvgOrderComparison_Accuracy",".tiff")
  file = file.path("Comparison_Figures",accuracy_file_name_tiff)
  ggplot(data=ordered_percent_data_averaged, aes(fill = factor(order), x = factor(nback_level), y=percent_correct)) + geom_bar(position = "dodge", stat = "identity") +
    geom_errorbar(aes(ymin=lower, ymax=upper), width=.2, position = position_dodge(width = 0.9), color = "white") +
    ggtitle("Group Average Nback vs Accuracy (order)") +
    scale_y_continuous(name = "Accuracy (%)") +
    scale_x_discrete(name = "Nback Level") +
    theme(plot.title = element_text(hjust = 0.5, size = 14, family = "Tahoma", face = "bold", color = "white"),
          axis.title = element_text(face="bold", color = "white"),
          axis.text.x=element_text(size = 11, color = "white"),
          axis.text.y=element_text(size = 11, color = "white"),
          axis.ticks = element_line(colour = 'white', size = .5),
          legend.position = "bottom",
          legend.text = element_text(color = "white"),
          legend.background = element_rect(fill = "#1e1e1e"),
          panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_rect(fill = "#1e1e1e"),
          plot.background = element_rect(fill = "#1e1e1e", color = "#1e1e1e"),
          axis.line = element_line(colour = "white",size=0.1, linetype = "dotted"))  +
    scale_fill_manual(values=color_cell) +
    facet_grid(. ~ ISI) +
    labs(fill = "ISI", color="white")
  ggsave(file)

  responsetime_file_name_tiff = paste0("AvgOrderComparison_ResponseTime",".tiff")
  file = file.path("Comparison_Figures",responsetime_file_name_tiff)
  ggplot(data=ordered_responsetime_data_averaged, aes(fill = factor(order), x = factor(nback_level), y=median_response_time)) + geom_bar(position = "dodge", stat = "identity") +
    geom_errorbar(aes(ymin=lower, ymax=upper), width=.2, position = position_dodge(width = 0.9), color = "white") +
    ggtitle("Group Average Nback vs Response Time (order)") +
    scale_y_continuous(name = "response time (ms)") +
    scale_x_discrete(name = "Nback Level") +
    theme(plot.title = element_text(hjust = 0.5, size = 14, family = "Tahoma", face = "bold", color = "white"),
          axis.title = element_text(face="bold", color = "white"),
          axis.text.x=element_text(size = 11, color = "white"),
          axis.text.y=element_text(size = 11, color = "white"),
          axis.ticks = element_line(colour = 'white', size = .5),
          legend.position = "bottom",
          legend.text = element_text(color = "white"),
          legend.background = element_rect(fill = "#1e1e1e"),
          panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_rect(fill = "#1e1e1e"),
          plot.background = element_rect(fill = "#1e1e1e", color = "#1e1e1e"),
          axis.line = element_line(colour = "white",size=0.1, linetype = "dotted"))  +
    scale_fill_manual(values=color_cell) +
    facet_grid(. ~ ISI) +
    labs(fill = "ISI", color="white")
  ggsave(file)

  percentFalsefires_file_name_tiff = paste0("AvgOrderComparison_FalseFire",".tiff")
  file = file.path("Comparison_Figures",percentFalsefires_file_name_tiff)
  ggplot(data=ordered_percentFalsefires_data_averaged, aes(fill = factor(order), x = factor(nback_level), y=percent_of_false_fires)) + geom_bar(position = "dodge", stat = "identity") +
    geom_errorbar(aes(ymin=lower, ymax=upper), width=.2, position = position_dodge(width = 0.9), color = "white") +
    ggtitle("Group Average Nback vs False Fires (order)") +
    scale_y_continuous(name = "false fires (%)") +
    scale_x_discrete(name = "Nback Level") +
    theme(plot.title = element_text(hjust = 0.5, size = 14, family = "Tahoma", face = "bold", color = "white"),
          axis.title = element_text(face="bold", color = "white"),
          axis.text.x=element_text(size = 11, color = "white"),
          axis.text.y=element_text(size = 11, color = "white"),
          axis.ticks = element_line(colour = 'white', size = .5),
          legend.position = "bottom",
          legend.text = element_text(color = "white"),
          legend.background = element_rect(fill = "#1e1e1e"),
          panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_rect(fill = "#1e1e1e"),
          plot.background = element_rect(fill = "#1e1e1e", color = "#1e1e1e"),
          axis.line = element_line(colour = "white",size=0.1, linetype = "dotted"))  +
    scale_fill_manual(values=color_cell) +
    facet_grid(. ~ ISI) +
    labs(fill = "ISI", color="white")
  ggsave(file)

  dprime_file_name_tiff = paste0("AvgOrderComparison_dprime",".tiff")
  file = file.path("Comparison_Figures",dprime_file_name_tiff)
  ggplot(data=ordered_dprime_data_averaged, aes(fill = factor(order), x = factor(nback_level), y=dprime)) + geom_bar(position = "dodge", stat = "identity") +
    geom_errorbar(aes(ymin=lower, ymax=upper), width=.2, position = position_dodge(width = 0.9), color = "white") +
    ggtitle("Group Average Nback vs dprime (order)") +
    scale_y_continuous(name = "dprime (z-score)") +
    scale_x_discrete(name = "Nback Level") +
    theme(plot.title = element_text(hjust = 0.5, size = 14, family = "Tahoma", face = "bold", color = "white"),
          axis.title = element_text(face="bold", color = "white"),
          axis.text.x=element_text(size = 11, color = "white"),
          axis.text.y=element_text(size = 11, color = "white"),
          axis.ticks = element_line(colour = 'white', size = .5),
          legend.position = "bottom",
          legend.text = element_text(color = "white"),
          legend.background = element_rect(fill = "#1e1e1e"),
          panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_rect(fill = "#1e1e1e"),
          plot.background = element_rect(fill = "#1e1e1e", color = "#1e1e1e"),
          axis.line = element_line(colour = "white",size=0.1, linetype = "dotted"))  +
    scale_fill_manual(values=color_cell) +
    facet_grid(. ~ ISI) +
    labs(fill = "ISI", color="white")
  ggsave(file)



}
