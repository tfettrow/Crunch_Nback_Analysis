analyze_nback_group <- function(subject_paths)
{
  library(readxl)
  library(stringi)
  library(lattice)
  library(ggplot2)
  library(rprime)

  # TO DO: need a way to find the Study folder regardless of current directory

<<<<<<< HEAD
  # TO DO: how to structure group ..  make another argument in function "group_name" and use this to name files and figs
=======
  # TO DO: how to structure group .. (i.e. OA vs YA vs whatever) ... make another argument in function "group_name" and use this to name files and figs
>>>>>>> merge_test


  all_accuracy_data <- vector()
  all_responseTime_data <- vector()

  for (this_subject_path in subject_paths){
    this_subject_path_string_split = strsplit(this_subject_path,"/")[1][1]
    this_subject_id = vapply(this_subject_path_string_split, tail, "", 1)

    accuracy_data = read.csv(file.path(this_subject_path, paste0("Processed/Nback_files/accuracy_", toString(this_subject_id), ".csv")))
    all_accuracy_data = rbind(all_accuracy_data, accuracy_data)


    responseTime_data = read.csv(file.path(this_subject_path, paste0("Processed/Nback_files/responseTime_", toString(this_subject_id), ".csv")))
    all_responseTime_data = rbind(all_responseTime_data, responseTime_data)

  }

  write.csv(all_accuracy_data, file = file.path("Group_Results/Nback_files", paste0("Nback_files", toString("_Group_accuracy"),".csv")))

  write.csv(all_responseTime_data, file = file.path("Group_Results/Nback_files", paste0("Nback_files", toString("_Group_responseTime"),".csv")))


  all_accuracy_data_averaged <- aggregate(all_accuracy_data["subject_accuracy"], by=list(all_accuracy_data$ISI, all_accuracy_data$nback),FUN=mean)
  colnames(all_accuracy_data_averaged) <- c("ISI", "nback", "averaged_accuracy")


  accuracy_file_name_pdf = paste0("Group_Accuracy",".pdf")
  file = file.path("Group_Results/Figures", accuracy_file_name_pdf)
  accuracy_fig = ggplot(data=all_accuracy_data_averaged, aes(fill = ISI, x = nback, y=averaged_accuracy)) + geom_bar(position = "dodge", stat = "identity")
  accuracy_fig + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                       panel.background = element_blank(), axis.line = element_line(colour = "black")) + ggtitle("Subject Accuracy for N-Back Levels and ISI") + xlab("N-Back Level") + ylab("Percent Correct (%)") +
    scale_fill_manual(values=c("orange","blue"))
  ggsave(file)

  accuracy_file_name_jpeg = paste0("Group_Accuracy",".jpeg")
  file = file.path("Group_Results/Figures", accuracy_file_name_jpeg)
  accuracy_fig = ggplot(data=all_accuracy_data_averaged, aes(fill = ISI, x = nback, y=averaged_accuracy)) + geom_bar(position = "dodge", stat = "identity")
  accuracy_fig + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                       panel.background = element_blank(), axis.line = element_line(colour = "black")) + ggtitle("Subject Accuracy for N-Back Levels and ISI") + xlab("N-Back Level") + ylab("Percent Correct (%)") +
    scale_fill_manual(values=c("orange","blue"))
  ggsave(file)

  responsetime_file_name_pdf = paste0("Group_ResponseTime",".pdf")
  file = file.path("Group_Results/Figures", responsetime_file_name_pdf)
  ggplot(all_responseTime_data, aes(x = factor(nback_level_correct), y = subject_response_onset_correct)) + geom_violin(aes(fill = factor(interstimulus_interval_correct))) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"), legend.title = element_blank()) +
    scale_fill_manual(values=c("orange","blue"))  + ggtitle("Reaction Time for N-Back Levels and ISI") + xlab("N-Back Level") + ylab("Onset Time (ms)")
  ggsave(file)

   responsetime_file_name_jpeg = paste0("Group_ResponseTime",".jpeg")
   file = file.path("Group_Results/Figures", responsetime_file_name_jpeg)
   ggplot(all_responseTime_data, aes(x = factor(nback_level_correct), y = subject_response_onset_correct)) + geom_violin(aes(fill = factor(interstimulus_interval_correct))) +
     theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"), legend.title = element_blank()) +
     scale_fill_manual(values=c("orange","blue"))  + ggtitle("Reaction Time for N-Back Levels and ISI") + xlab("N-Back Level") + ylab("Onset Time (ms)")
   ggsave(file)

}

