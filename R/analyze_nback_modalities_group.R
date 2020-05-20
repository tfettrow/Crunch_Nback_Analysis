analyze_nback_modalities_group <- function(subject_ids)
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

  for (this_subject_path in subject_ids){
    this_subject_path_string_split = strsplit(this_subject_path,"/")[1][1]
    this_subject_id = vapply(this_subject_path_string_split, tail, "", 1)

    color_cell <- vector()
    if (dir.exists(paste0("fnirs/",toString(this_subject_id))))
    {
      setwd(paste0("fnirs/",toString(this_subject_id)))
      setwd("Processed/Nback_files")
      fNIRS_nback_data<-read.csv(paste0("results_",toString(this_subject_id),".csv"))
      setwd("../../../..")
      fNIRS_nback_data$modality = "fnirs"
      fNIRS_nback_data$subject_id = this_subject_id
      all_results_data = rbind(all_results_data, fNIRS_nback_data)
      color_cell = c(color_cell, "royalblue3")
    }

    if (dir.exists(paste0("eeg/",toString(this_subject_id))))
    {
      setwd(paste0("eeg/",toString(this_subject_id)))
      setwd("Processed/Nback_files")
      EEG_nback_data<-read.csv(paste0("results_",toString(this_subject_id),".csv"))
      setwd("../../../..")
      EEG_nback_data$modality = "eeg"
      EEG_nback_data$subject_id = this_subject_id
      all_results_data = rbind(all_results_data, EEG_nback_data)
      color_cell = c(color_cell, "cyan3")
    }

    if (dir.exists(paste0("fmri/",toString(this_subject_id))))
    {
      setwd(paste0("fmri/",toString(this_subject_id)))
      setwd("Processed/Nback_files")
      fMRI_nback_data<-read.csv(paste0("results_",toString(this_subject_id),".csv"))
      setwd("../../../..")
      fMRI_nback_data$modality = "fmri"
      fMRI_nback_data$subject_id = this_subject_id
      all_results_data = rbind(all_results_data, fMRI_nback_data)
      color_cell = c(color_cell, "darkgreen")
    }
  }


  ## SYNTAX FOR NBACK LEVEL vs CORRECT (Across Modalities)   #######
  all_percent_data_averaged <- aggregate(all_results_data["percent_correct"], by=list(all_results_data$modality, all_results_data$nback_level, all_results_data$ISI),FUN=mean)
  colnames(all_percent_data_averaged) <- c("modality", "nback_level", "ISI", "percent_correct")

  accuracy_std <- aggregate(all_results_data["percent_correct"], by=list(all_results_data$modality, all_results_data$nback_level, all_results_data$ISI),FUN=sd)
  colnames(accuracy_std) <- c("modality", "nback_level", "ISI", "std")

  all_percent_data_averaged$std <- accuracy_std$std
  all_percent_data_averaged$lower <- all_percent_data_averaged$percent_correct - all_percent_data_averaged$std
  all_percent_data_averaged$upper <- all_percent_data_averaged$percent_correct + all_percent_data_averaged$std


  all_responsetime_data_averaged <- aggregate(all_results_data["median_response_time"], by=list(all_results_data$modality, all_results_data$nback_level, all_results_data$ISI),FUN=mean,na.rm=T)
  colnames(all_responsetime_data_averaged) <- c("modality", "nback_level", "ISI", "median_response_time")

  responsetime_std <- aggregate(all_results_data["median_response_time"], by=list(all_results_data$modality, all_results_data$nback_level, all_results_data$ISI),FUN=sd,na.rm=T)
  colnames(responsetime_std) <- c("ISI", "nback_level", "ISI", "std")

  all_responsetime_data_averaged$std <- responsetime_std$std
  all_responsetime_data_averaged$lower <- all_responsetime_data_averaged$median_response_time - all_responsetime_data_averaged$std
  all_responsetime_data_averaged$upper <- all_responsetime_data_averaged$median_response_time + all_responsetime_data_averaged$std

#
#   all_dprime_data_averaged <- aggregate(all_results_data["dprime"], by=list(all_results_data$modality, all_results_data$nback_level, all_results_data$ISI),FUN=mean)
#   colnames(all_dprime_data_averaged) <- c("modality", "nback_level", "ISI", "dprime")
#
#   dprime_std <- aggregate(all_results_data["dprime"], by=list(all_results_data$modality, all_results_data$nback_level, all_results_data$ISI),FUN=sd)
#   colnames(dprime_std) <- c("ISI", "nback_level", "ISI", "std")
#
#   all_dprime_data_averaged$std <- dprime_std$std
#   all_dprime_data_averaged$lower <- all_dprime_data_averaged$median_response_time - all_dprime_data_averaged$std
#   all_dprime_data_averaged$upper <- all_dprime_data_averaged$median_response_time + all_dprime_data_averaged$std


  accuracy_file_name_tiff = paste0("AvgModalityComparison_Accuracy",".tiff")
  file = file.path("Comparison_Figures",accuracy_file_name_tiff)
  ggplot(data=all_percent_data_averaged, aes(fill = factor(modality), x = factor(nback_level), y=percent_correct)) + geom_bar(position = "dodge", stat = "identity") +
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
  ggplot(data=all_responsetime_data_averaged, aes(fill = factor(modality), x = factor(nback_level), y=median_response_time)) + geom_bar(position = "dodge", stat = "identity") +
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

  # dprime_file_name_tiff = paste0("AvgModalityComparison_dprime",".tiff")
  # ggplot(data=all_dprime_data_averaged, aes(fill = factor(modality), x = factor(nback_level), y=dprime)) + geom_bar(position = "dodge", stat = "identity") +
  #   geom_errorbar(aes(ymin=lower, ymax=upper), width=.2, position = position_dodge(width = 0.9), color = "white") +
  #   ggtitle("Group Average Nback vs dprime (Modalities)") +
  #   scale_y_continuous(name = "dprime (z-score)") +
  #   scale_x_discrete(name = "Nback Level") +
  #   theme(plot.title = element_text(hjust = 0.5, size = 14, family = "Tahoma", face = "bold", color = "white"),
  #         axis.title = element_text(face="bold", color = "white"),
  #         axis.text.x=element_text(size = 11, color = "white"),
  #         axis.text.y=element_text(size = 11, color = "white"),
  #         axis.ticks = element_line(colour = 'white', size = .5),
  #         legend.position = "bottom",
  #         legend.text = element_text(color = "white"),
  #         legend.background = element_rect(fill = "#1e1e1e"),
  #         panel.border = element_blank(),
  #         panel.grid.major = element_blank(),
  #         panel.grid.minor = element_blank(),
  #         panel.background = element_rect(fill = "#1e1e1e"),
  #         plot.background = element_rect(fill = "#1e1e1e", color = "#1e1e1e"),
  #         axis.line = element_line(colour = "white",size=0.1, linetype = "dotted"))  +
  #   scale_fill_manual(values=color_cell) +
  #   facet_grid(. ~ ISI) +
  #   labs(fill = "ISI", color="white")
  # ggsave(file)

}
