analyze_nback_subject <- function(subject_path)
{
  library(readxl)
  library(stringi)
  library(lattice)
  library(ggplot2)
  library(rprime)

  # Grabbing bits and pieces of larger .xlsx file
  nback_data1 = read_excel(file.path(subject_path,"Raw/Nback_files/nback_results.xlsx"), range = "EB2:EB450", sheet = 1, col_types = "text")
  nback_data2 = read_excel(file.path(subject_path,"Raw/Nback_files/nback_results.xlsx"), range = "IS2:JC450", sheet = 1, col_types = "text")
  nback_data3 = read_excel(file.path(subject_path,"Raw/Nback_files/nback_results.xlsx"), range = "GJ2:GJ450", sheet = 1, col_types = "text")

  interstimulus_interval = nback_data1$ISI

  stimulus_onset_times = nback_data2$Stimulus.OnsetTime

  expected_correct_response = nback_data2$Stimulus.CRESP
  subject_response = nback_data2$Stimulus.RESP
  subject_response_onset = nback_data2$Stimulus.RT

  nback_block_labels = nback_data3$`Running[SubTrial]`
  nback_level = stri_sub(nback_block_labels, -1,-1)
  nback_level = as.numeric(nback_level)

  subject_accuracy_eprime = nback_data2$Stimulus.ACC

  #  -----------------------------------------------------------------------------------------------

  # # ORGANIZE # #

  # determine the onset time of each condition
  unique_subtrials = unique(nback_block_labels)
  condition_onset_times_corrected <- vector()
  first_condition_onset_time_eprime <- vector()
  for (this_condition in unique_subtrials){
    this_condition_first_stim_index = min(which(nback_block_labels == this_condition))
    this_condition_onset_time_eprime = stimulus_onset_times[this_condition_first_stim_index]

    # reset the time correction every 8 conditions (this makes up a run)
    if (length(condition_onset_times_corrected)%%8 == 0 & length(condition_onset_times_corrected) >= 8){
      first_condition_onset_time_eprime <- vector()
    }

    if (length(first_condition_onset_time_eprime) == 0){
      first_condition_onset_time_eprime = this_condition_onset_time_eprime
      this_condition_onset_time_corrected = 4.5
    } else {
      this_condition_onset_time_corrected = ((as.numeric(this_condition_onset_time_eprime) - as.numeric(first_condition_onset_time_eprime)) / 1000) + 4.5
    }

    condition_onset_times_corrected = append(condition_onset_times_corrected, this_condition_onset_time_corrected)
  }
  condition_onset_info_dataframe = data.frame(condition_onset_times_corrected, unique_subtrials)

  # remove indices of condition where subject forgot which nback they were performing
  noresponse_condition_indices <- vector()
  for (this_condition in unique_subtrials){
      this_condition_stim_indices = (which(nback_block_labels == this_condition))
      subject_response_this_condition <- subject_response[this_condition_stim_indices]
      if (is.na(any(subject_response_this_condition==1))){
        noresponse_condition_indices <- append(noresponse_condition_indices, this_condition_stim_indices)
    }
  }

  # TO DO: what to do if subject responded when a response was NOT expected ... for now ignoring erroneous repsonses.. only interested in respond to expected or not
  # TO DO: what happens if response triggers on next stimulus?/ reaction time is really low?

  # # subject response accuracy # #
  # produces a logical array indicating whether the subject responded when expected, or not
  subject_accuracy_r = subject_accuracy_eprime == expected_correct_response

  # find only the indices where an expected response was expected and remove the subtrials where subject forgot which nback they were on
  accuracy_na_indices_removed = which(!is.na(subject_accuracy_r))
  accuracy_na_indices_removed = accuracy_na_indices_removed [!accuracy_na_indices_removed %in% noresponse_condition_indices]

  # convert accuracy into percent ... find a simpler method!
  accuracy_dataframe_nona = data.frame(nback_level[accuracy_na_indices_removed], subject_accuracy_r[accuracy_na_indices_removed], interstimulus_interval[accuracy_na_indices_removed])

  zero_back_short_indices = which(accuracy_dataframe_nona$interstimulus_interval==500 & accuracy_dataframe_nona$nback_level == 0)
  one_back_short_indices = which(accuracy_dataframe_nona$interstimulus_interval==500 & accuracy_dataframe_nona$nback_level == 1)
  two_back_short_indices = which(accuracy_dataframe_nona$interstimulus_interval==500 & accuracy_dataframe_nona$nback_level == 2)
  three_back_short_indices = which(accuracy_dataframe_nona$interstimulus_interval==500 & accuracy_dataframe_nona$nback_level == 3)

  zero_back_long_indices = which(accuracy_dataframe_nona$interstimulus_interval==1500 & accuracy_dataframe_nona$nback_level == 0)
  one_back_long_indices = which(accuracy_dataframe_nona$interstimulus_interval==1500 & accuracy_dataframe_nona$nback_level == 1)
  two_back_long_indices = which(accuracy_dataframe_nona$interstimulus_interval==1500 & accuracy_dataframe_nona$nback_level == 2)
  three_back_long_indices = which(accuracy_dataframe_nona$interstimulus_interval==1500 & accuracy_dataframe_nona$nback_level == 3)

  zero_back_short_accuracy = accuracy_dataframe_nona$subject_accuracy_r[zero_back_short_indices]
  one_back_short_accuracy = accuracy_dataframe_nona$subject_accuracy_r[one_back_short_indices]
  two_back_short_accuracy = accuracy_dataframe_nona$subject_accuracy_r[two_back_short_indices]
  three_back_short_accuracy = accuracy_dataframe_nona$subject_accuracy_r[three_back_short_indices]

  zero_back_long_accuracy = accuracy_dataframe_nona$subject_accuracy_r[zero_back_long_indices]
  one_back_long_accuracy = accuracy_dataframe_nona$subject_accuracy_r[one_back_long_indices]
  two_back_long_accuracy = accuracy_dataframe_nona$subject_accuracy_r[two_back_long_indices]
  three_back_long_accuracy = accuracy_dataframe_nona$subject_accuracy_r[three_back_long_indices]

  zero_back_short_accuracy_percent = sum(zero_back_short_accuracy) / length (zero_back_short_accuracy) * 100
  one_back_short_accuracy_percent = sum(one_back_short_accuracy) / length (one_back_short_accuracy) * 100
  two_back_short_accuracy_percent = sum(two_back_short_accuracy) / length (two_back_short_accuracy) * 100
  three_back_short_accuracy_percent = sum(three_back_short_accuracy) / length (three_back_short_accuracy) * 100

  zero_back_long_accuracy_percent = sum(zero_back_long_accuracy) / length (zero_back_long_accuracy) * 100
  one_back_long_accuracy_percent = sum(one_back_long_accuracy) / length (one_back_long_accuracy) * 100
  two_back_long_accuracy_percent = sum(two_back_long_accuracy) / length (two_back_long_accuracy) * 100
  three_back_long_accuracy_percent = sum(three_back_long_accuracy) / length (three_back_long_accuracy) * 100

  subject_long_percents = data.frame(nback = c(as.character(0:3)), subject_accuracy = c(as.numeric(zero_back_long_accuracy_percent),
                                                                                        as.numeric(two_back_long_accuracy_percent), as.numeric(two_back_long_accuracy_percent), as.numeric(three_back_long_accuracy_percent)),
                                     isi = c("long", "long", "long", "long"))

  subject_short_percents = data.frame(nback = c(as.character(0:3)), subject_accuracy = c(as.numeric(zero_back_short_accuracy_percent),
                                                                                         as.numeric(two_back_short_accuracy_percent), as.numeric(two_back_short_accuracy_percent), as.numeric(three_back_short_accuracy_percent)),
                                      isi = c("short", "short", "short", "short"))

  accuracy_dataframe_complete = rbind(subject_long_percents,subject_short_percents)


  # # response time by nback # #
  #response_correct_indices = expected_correct_response == correct_response_eprime

  subject_accuracy_r_numeric = as.numeric(subject_accuracy_r)
  response_correct_indices = which(subject_accuracy_r_numeric == 1)

  nback_level_correct = nback_level[response_correct_indices]
  subject_response_onset_correct = subject_response_onset[response_correct_indices]
  interstimulus_interval_correct = interstimulus_interval[response_correct_indices]

  responsetime_dataframe = data.frame(nback_level_correct, subject_response_onset_correct, interstimulus_interval_correct)

  #  -----------------------------------------------------------------------------------------------

  # # WRITE DATA OF INTEREST TO FILE # #

  # some funny stuff to be able to grab the subject ID for file naming
  subject_path_string_split = strsplit(subject_path,"/")[1][1]
  subject_id = vapply(subject_path_string_split, tail, "", 1)

  # # Store Data in Processed folder # #
  write.csv(responsetime_dataframe, file = file.path(subject_path, paste0("Processed/Nback_files/responseTime_", toString(subject_id),".csv")))
  write.csv(accuracy_dataframe_complete, file = file.path(subject_path, paste0("Processed/Nback_files/accuracy_", toString(subject_id),".csv")))
  write.csv(condition_onset_info_dataframe, file = file.path(subject_path, paste0("Processed/Nback_files/conditionOnset_", toString(subject_id),".csv")))

  #  -----------------------------------------------------------------------------------------------

  # # PLOT # #
  # TO DO: what to do when only one response time (not engough to create violin plot)?

  accuracy_file_name_pdf = paste0("Accuracy_",toString(subject_id),".pdf")
  file = file.path(subject_path,"Figures",accuracy_file_name_pdf)
  accuracy_fig = ggplot(data=accuracy_dataframe_complete, aes(fill = ISI, x = nback, y=subject_accuracy)) + geom_bar(position = "dodge", stat = "identity")
  accuracy_fig + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                       panel.background = element_blank(), axis.line = element_line(colour = "black")) + ggtitle("Subject Accuracy for N-Back Levels and ISI") + xlab("N-Back Level") + ylab("Percent Correct (%)") +
    scale_fill_manual(values=c("orange","blue"))
  ggsave(file)

  accuracy_file_name_jpeg = paste0("Accuracy_",toString(subject_id),".jpeg")
  file = file.path(subject_path,"Figures",accuracy_file_name_jpeg)
  accuracy_fig = ggplot(data=accuracy_dataframe_complete, aes(fill = ISI, x = nback, y=subject_accuracy)) + geom_bar(position = "dodge", stat = "identity")
  accuracy_fig + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                       panel.background = element_blank(), axis.line = element_line(colour = "black")) + ggtitle("Subject Accuracy for N-Back Levels and ISI") + xlab("N-Back Level") + ylab("Percent Correct (%)") +
    scale_fill_manual(values=c("orange","blue"))
  ggsave(file)



  responsetime_file_name_pdf = paste0("ResponseTime_",toString(subject_id),".pdf")
  file = file.path(subject_path,"Figures",responsetime_file_name_pdf)
  ggplot(data = responsetime_dataframe, aes(fill = interstimulus_interval_correct, x = factor(nback_level_correct), y = subject_response_onset_correct)) + geom_violin(position = position_dodge(1)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black")) +
    scale_fill_manual(values=c("orange","blue"))  + ggtitle("Subject Reaction Time for N-Back Levels and ISI") + xlab("N-Back Level") + ylab("Onset Time (ms)")
  ggsave(file)

  responsetime_file_name_jpeg = paste0("ResponseTime_",toString(subject_id),".jpeg")
  file = file.path(subject_path,"Figures",responsetime_file_name_jpeg)
  ggplot(data = responsetime_dataframe, aes(fill = interstimulus_interval_correct, x = factor(nback_level_correct), y = subject_response_onset_correct)) + geom_violin(position = position_dodge(1)) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black")) +
    scale_fill_manual(values=c("orange","blue"))  + ggtitle("Subject Reaction Time for N-Back Levels and ISI") + xlab("N-Back Level") + ylab("Onset Time (ms)")
  ggsave(file)

  #  -----------------------------------------------------------------------------------------------

}

