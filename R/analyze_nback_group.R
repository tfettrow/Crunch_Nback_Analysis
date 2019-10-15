analyze_nback_group <- function(group_name, subject_paths)
{
  library(readxl)
  library(stringi)
  library(lattice)
  library(ggplot2)
  library(rprime)

  all_accuracy_data <- vector()
  all_responseTime_data <- vector()
  all_falsefires_data <- vector()

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

    accuracy_data = read.csv(file.path(paste0("Processed/Nback_files/accuracy_", toString(this_subject_id), ".csv")))
    all_accuracy_data = rbind(all_accuracy_data, accuracy_data)

    responseTime_data = read.csv(file.path(paste0("Processed/Nback_files/responseTime_", toString(this_subject_id), ".csv")))
    all_responseTime_data = rbind(all_responseTime_data, responseTime_data)

    falsefires_data = read.csv(file.path(paste0("Processed/Nback_files/falsefires_", toString(this_subject_id), ".csv")))
    all_falsefires_data = rbind(all_falsefires_data, falsefires_data)
    setwd("..")

  }

  write.csv(all_accuracy_data, file = file.path("Group_Results/Nback_files", paste0(toString(group_name), "_accuracy",".csv")))
  write.csv(all_responseTime_data, file = file.path("Group_Results/Nback_files", paste0(toString(group_name), "_responseTime",".csv")))
  write.csv(all_falsefires_data, file = file.path("Group_Results/Nback_files", paste0(toString(group_name), "_falsefires",".csv")))


  all_accuracy_data_averaged <- aggregate(all_accuracy_data["subject_accuracy"], by=list(all_accuracy_data$ISI, all_accuracy_data$nback),FUN=mean)
  colnames(all_accuracy_data_averaged) <- c("ISI", "nback", "averaged_accuracy")

  accuracy_file_name_tiff = paste0(toString(group_name), "_Accuracy",".tiff")
  file = file.path("Group_Results/Figures", accuracy_file_name_tiff)
  accuracy_fig = ggplot(data=all_accuracy_data_averaged, aes(fill = ISI, x = nback, y=averaged_accuracy)) + geom_bar(position = "dodge", stat = "identity")
  accuracy_fig + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                       panel.background = element_blank(), axis.line = element_line(colour = "black")) + ggtitle("Subject Accuracy for N-Back Levels and ISI") + xlab("N-Back Level") + ylab("Percent Correct (%)") +
    scale_fill_manual(values=c("orange","blue"))
  ggsave(file)

  responsetime_file_name_tiff = paste0(toString(group_name), "_ResponseTime",".tiff")
  file = file.path("Group_Results/Figures", responsetime_file_name_tiff)
  ggplot(all_responseTime_data, aes(x = factor(nback_level_correct), y = subject_response_onset_correct)) + geom_violin(aes(fill = factor(interstimulus_interval_correct))) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"), legend.title = element_blank()) +
    scale_fill_manual(values=c("orange","blue"))  + ggtitle("Reaction Time for N-Back Levels and ISI") + xlab("N-Back Level") + ylab("Onset Time (ms)")
  ggsave(file)

  falsefire_file_name_tiff = paste0("FalseFires",toString(group_name),".tiff")
  file = file.path("Group_Results/Figures",falsefire_file_name_tiff)
  ggplot(all_falsefires_data, aes(x = false_fires_nback_level)) + stat_count(width = 0.5, fill="blue") + #geom_bar(position = "dodge", stat="bin") +
    ggtitle("False Fire Rate") +
    theme(plot.title = element_text(hjust = 0.5, size = 14, family = "Tahoma", face = "bold"),
          text = element_text(size = 12, family = "Tahoma"),
          axis.title = element_text(face="bold"),
          axis.text.x=element_text(size = 11),
          legend.position = "bottom")
  ggsave(file)
}

