analyze_nback_group <- function(subject_paths)
{
  library(readxl)
  library(stringi)
  library(lattice)
  library(ggplot2)
  library(rprime)


  all_accuracy_data <- vector()
  all_responseTime_data <- vector()
  # some funny stuff to be able to grab the subject ID for folder finding and file naming
  for (this_subject_path in subject_paths){
    this_subject_path_string_split = strsplit(this_subject_path,"/")[1][1]
    this_subject_id = vapply(this_subject_path_string_split, tail, "", 1)

    #print(this_subject_id)


   accuracy_data = read.csv(file.path(this_subject_path, paste0("Processed/Nback_files/accuracy_", toString(this_subject_id), ".csv")))

   responseTime_data = read.csv(file.path(this_subject_path, paste0("Processed/Nback_files/responseTime_", toString(this_subject_id), ".csv")))


    #print(accuracy_data)

    #print(responseTime_data)

   # concatenate the data for each subject's responseTime.txt




    #condition_onset_times_corrected = append(condition_onset_times_corrected, this_condition_onset_time_corrected)


    # for each subject id read the accuracy.txt

    # concatenate the data for each subject's accuracy.txt


    all_accuracy_data = rbind(all_accuracy_data, accuracy_data)

    #print(all_accuracy_data)

    write.csv(all_accuracy_data, file = file.path("Results/Nback_files", paste0("Nback_files", toString("_Group_accuracy"),".csv")))



    all_responseTime_data = rbind.list(all_responseTime_data, responseTime_data)
    write.csv(all_responseTime_data, file = file.path("Results/Nback_files", paste0("Nback_files", toString("_Group_responseTime"),".csv")))






  }








  # plot the concatenated responseTime data

  # plot the concatenated accuracy data


 # accuracy_file_name_pdf = paste0("Accuracy_",toString(this_subject_id),".pdf")
  #file = file.path(subject_paths,"Results",accuracy_file_name_pdf)
  #accuracy_fig = ggplot(data=all_accuracy_data, aes(fill = ISI, x = nback, y=subject_accuracy)) + geom_bar(position = "dodge", stat = "identity")
  #accuracy_fig + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
   #                    panel.background = element_blank(), axis.line = element_line(colour = "black")) + ggtitle("Subject Accuracy for N-Back Levels and ISI") + xlab("N-Back Level") + ylab("Percent Correct (%)") +
    #scale_fill_manual(values=c("orange","blue"))
  #ggsave(file)

  #accuracy_file_name_jpeg = paste0("Accuracy_",toString(this_subject_id),".jpeg")
  #file = file.path(subject_paths,"Results",accuracy_file_name_jpeg)
  #accuracy_fig = ggplot(data=all_accuracy_data, aes(fill = ISI, x = nback, y=subject_accuracy)) + geom_bar(position = "dodge", stat = "identity")
  #accuracy_fig + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
   #                    panel.background = element_blank(), axis.line = element_line(colour = "black")) + ggtitle("Subject Accuracy for N-Back Levels and ISI") + xlab("N-Back Level") + ylab("Percent Correct (%)") +
    #scale_fill_manual(values=c("orange","blue"))
  #ggsave(file)

}

