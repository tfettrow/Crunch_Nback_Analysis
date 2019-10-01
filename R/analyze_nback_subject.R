analyze_nback_subject <- function(subject_path)
{
  library(readxl)
  library(stringi)
  library(lattice)
  library(ggplot2)
  library(rprime)

  # TO DO:
  # Change the read_excel to read .txt .. should be a function out there.
  # Figure out the responses on the MASK

  # some funny stuff to be able to grab the subject ID for file naming
  subject_path_string_split = strsplit(subject_path,"/")[1][1]
  subject_id = vapply(subject_path_string_split, tail, "", 1)

  # need way to ensure this is with respect to subject_dir
  setwd(subject_id) # paste0("/",toString(
  dir.create("Figures")
  dir.create("Processed")
  setwd("Processed")
  dir.create("Nback_files")
  setwd("..")
  setwd("..")
  # Grabbing bits and pieces of larger .xlsx file
  nback_data = read_excel(file.path(subject_path,"Raw/Nback_files/nback_results.xlsx"), cell_rows(2:450), sheet = 1, col_types = "text")

  nback_block_labels = nback_data$`Running[SubTrial]`
  nback_level = stri_sub(nback_block_labels, -1,-1)
  nback_level = as.numeric(nback_level)

  nback_interval = stri_sub(nback_block_labels, 1, 1)
  interstimulus_interval = nback_interval # just a place holder for interstimulus_interval
  interstimulus_interval[nback_interval == "S"] <- 500
  interstimulus_interval[nback_interval == "L"] <- 1500

  subject_accuracy_eprime = nback_data$Stimulus.ACC
  stimulus_onset_times = nback_data$Stimulus.OnsetTime
  expected_correct_response = nback_data$Stimulus.CRESP
  subject_response = nback_data$Stimulus.RESP
  subject_response_onset = nback_data$Stimulus.RT

  # check if data coming form Stimulus is same length as other data.. if not.. do below.. (bug in FNIRS program)
  total_number_of_stimuli_this_experiment <- nback_level[!is.na(nback_level)]
  number_of_stimuli_in_stimulus_var <- subject_accuracy_eprime[!is.na(subject_accuracy_eprime)]
  if (length(total_number_of_stimuli_this_experiment) > length(number_of_stimuli_in_stimulus_var))
  {

    subject_accuracy_eprime1 = nback_data$Stimulus1.ACC
    stimulus_onset_times1 = nback_data$Stimulus1.OnsetTime
    expected_correct_response1 = nback_data$Stimulus1.CRESP
    subject_response1 = nback_data$Stimulus1.RESP
    subject_response_onset1 = nback_data$Stimulus1.RT

    indices_to_replace1 = which(subject_accuracy_eprime1 != "NA")

    subject_accuracy_eprime[indices_to_replace1] = subject_accuracy_eprime1[indices_to_replace1]
    stimulus_onset_times[indices_to_replace1] = stimulus_onset_times1[indices_to_replace1]
    expected_correct_response[indices_to_replace1] = expected_correct_response1[indices_to_replace1]
    subject_response[indices_to_replace1] = subject_response1[indices_to_replace1]
    subject_response_onset[indices_to_replace1] = subject_response_onset1[indices_to_replace1]

    subject_accuracy_eprime2 = nback_data$Stimulus2.ACC
    stimulus_onset_times2 = nback_data$Stimulus2.OnsetTime
    expected_correct_response2 = nback_data$Stimulus2.CRESP
    subject_response2 = nback_data$Stimulus2.RESP
    subject_response_onset2 = nback_data$Stimulus2.RT

    indices_to_replace2 = which(subject_accuracy_eprime2 != "NA")

    subject_accuracy_eprime[indices_to_replace2] = subject_accuracy_eprime2[indices_to_replace2]
    stimulus_onset_times[indices_to_replace2] = stimulus_onset_times2[indices_to_replace2]
    expected_correct_response[indices_to_replace2] = expected_correct_response2[indices_to_replace2]
    subject_response[indices_to_replace2] = subject_response2[indices_to_replace2]
    subject_response_onset[indices_to_replace2] = subject_response_onset2[indices_to_replace2]


    subject_accuracy_eprime3 = nback_data$Stimulus3.ACC
    stimulus_onset_times3 = nback_data$Stimulus3.OnsetTime
    expected_correct_response3 = nback_data$Stimulus3.CRESP
    subject_response3 = nback_data$Stimulus3.RESP
    subject_response_onset3 = nback_data$Stimulus3.RT

    indices_to_replace3 = which(subject_accuracy_eprime3 != "NA")

    subject_accuracy_eprime[indices_to_replace3] = subject_accuracy_eprime3[indices_to_replace3]
    stimulus_onset_times[indices_to_replace3] = stimulus_onset_times3[indices_to_replace3]
    expected_correct_response[indices_to_replace3] = expected_correct_response2[indices_to_replace3]
    subject_response[indices_to_replace3] = subject_response3[indices_to_replace3]
    subject_response_onset[indices_to_replace3] = subject_response_onset3[indices_to_replace3]

  }

  # Just removing excess indices in the case of FNIRS data
  indices_to_keep = which(!is.na(nback_level))
  stimulus_onset_times <- stimulus_onset_times[indices_to_keep]
  expected_correct_response <- expected_correct_response[indices_to_keep]
  subject_response <- subject_response[indices_to_keep]
  subject_response_onset <- subject_response_onset[indices_to_keep]
  nback_block_labels <- nback_block_labels[indices_to_keep]
  nback_level <- nback_level[indices_to_keep]
  interstimulus_interval <- interstimulus_interval[indices_to_keep]
  subject_accuracy_eprime <- subject_accuracy_eprime[indices_to_keep]
  #  -----------------------------------------------------------------------------------------------

  # # ORGANIZE # #

  # determine the onset time of each condition
  unique_subtrials = unique(nback_block_labels)
  condition_onset_times_corrected <- vector()
  first_condition_onset_time_eprime <- vector()
  for (this_condition in unique_subtrials){
    this_condition_first_stim_index = min(which(nback_block_labels == this_condition))
    this_condition_onset_time_eprime = stimulus_onset_times[this_condition_first_stim_index]

    # reset the time correction every 8 conditions (this makes up a run (for fMRI))
    if (length(condition_onset_times_corrected)%%8 == 0 & length(condition_onset_times_corrected) >= 8){
      first_condition_onset_time_eprime <- vector()
    }

    if (length(first_condition_onset_time_eprime) == 0){
      first_condition_onset_time_eprime = this_condition_onset_time_eprime
      this_condition_onset_time_corrected = 4500
    } else {
      this_condition_onset_time_corrected = ((as.numeric(this_condition_onset_time_eprime) - as.numeric(first_condition_onset_time_eprime))) + 4500
    }

    condition_onset_times_corrected = append(condition_onset_times_corrected, this_condition_onset_time_corrected)
  }
  condition_onset_info_dataframe = data.frame(condition_onset_times_corrected, unique_subtrials)



  stimulus_onset_times_continuous_corrected = as.numeric(stimulus_onset_times) - as.numeric(stimulus_onset_times[1]) + 4500
  stimulus_onset_info_dataframe = data.frame(stimulus_onset_times_continuous_corrected, nback_block_labels)


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
  # TO DO: what happens if response triggers on next stimulus?/ reaction time is really low? Check response in the MASK!!

  # # subject response accuracy # #
  # produces a logical array indicating whether the subject responded when expected, or not
  subject_accuracy_r = subject_accuracy_eprime == expected_correct_response

  # find only the indices where an expected response was expected and remove the subtrials where subject forgot which nback they were on
  accuracy_na_indices_removed = which(!is.na(subject_accuracy_r))
  accuracy_na_indices_removed = accuracy_na_indices_removed [!accuracy_na_indices_removed %in% noresponse_condition_indices]

  # convert accuracy into percent ... find a simpler method!
  accuracy_dataframe_nona = data.frame(nback_level[accuracy_na_indices_removed], subject_accuracy_r[accuracy_na_indices_removed], interstimulus_interval[accuracy_na_indices_removed])

  zero_back_short_indices = which(accuracy_dataframe_nona$interstimulus_interval == 500 & accuracy_dataframe_nona$nback_level == 0)
  one_back_short_indices = which(accuracy_dataframe_nona$interstimulus_interval == 500 & accuracy_dataframe_nona$nback_level == 1)
  two_back_short_indices = which(accuracy_dataframe_nona$interstimulus_interval == 500 & accuracy_dataframe_nona$nback_level == 2)
  three_back_short_indices = which(accuracy_dataframe_nona$interstimulus_interval == 500 & accuracy_dataframe_nona$nback_level == 3)

  zero_back_long_indices = which(accuracy_dataframe_nona$interstimulus_interval == 1500 & accuracy_dataframe_nona$nback_level == 0)
  one_back_long_indices = which(accuracy_dataframe_nona$interstimulus_interval == 1500 & accuracy_dataframe_nona$nback_level == 1)
  two_back_long_indices = which(accuracy_dataframe_nona$interstimulus_interval == 1500 & accuracy_dataframe_nona$nback_level == 2)
  three_back_long_indices = which(accuracy_dataframe_nona$interstimulus_interval == 1500 & accuracy_dataframe_nona$nback_level == 3)

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

  subject_long_percents = data.frame(nback = c(as.character(0:3)), subject_accuracy = c(as.numeric(zero_back_long_accuracy_percent), as.numeric(one_back_short_accuracy_percent), as.numeric(two_back_long_accuracy_percent), as.numeric(three_back_long_accuracy_percent)), ISI = c("long", "long", "long", "long"), subject_id)

  subject_short_percents = data.frame(nback = c(as.character(0:3)), subject_accuracy = c(as.numeric(zero_back_short_accuracy_percent), as.numeric(one_back_long_accuracy_percent), as.numeric(two_back_short_accuracy_percent), as.numeric(three_back_short_accuracy_percent)), ISI = c("short", "short", "short", "short"), subject_id)

  accuracy_dataframe_complete = rbind(subject_long_percents,subject_short_percents)


  # # response time by nback # #
  #response_correct_indices = expected_correct_response == correct_response_eprime

  subject_accuracy_r_numeric = as.numeric(subject_accuracy_r)
  response_correct_indices = which(subject_accuracy_r_numeric == 1)

  nback_level_correct = nback_level[response_correct_indices]
  subject_response_onset_correct = subject_response_onset[response_correct_indices]
  interstimulus_interval_correct = interstimulus_interval[response_correct_indices]

  responsetime_dataframe = data.frame(nback_level_correct, subject_response_onset_correct, interstimulus_interval_correct, subject_id)

  #  -----------------------------------------------------------------------------------------------

  # # WRITE DATA OF INTEREST TO FILE # #

  # # Store Data in Processed folder # #
  write.csv(responsetime_dataframe, file = file.path(subject_path, paste0("Processed/Nback_files/responseTime_", toString(subject_id),".csv")))
  write.csv(accuracy_dataframe_complete, file = file.path(subject_path, paste0("Processed/Nback_files/accuracy_", toString(subject_id),".csv")))
  write.csv(condition_onset_info_dataframe, file = file.path(subject_path, paste0("Processed/Nback_files/conditionOnset_", toString(subject_id),".csv")))
  write.csv(stimulus_onset_info_dataframe, file = file.path(subject_path, paste0("Processed/Nback_files/stimulusnOnsetContinuous_", toString(subject_id),".csv")))
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


  #Currently not operational.. bug in geom_violin func when too few data
   # responsetime_file_name_pdf = paste0("ResponseTime_",toString(subject_id),".pdf")
   # file = file.path(subject_path,"Figures",responsetime_file_name_pdf)
   # ggplot(data = responsetime_dataframe, aes(fill = interstimulus_interval_correct, x = factor(nback_level_correct), y = subject_response_onset_correct)) + geom_violin(position = position_dodge(1)) +
   # theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black")) +
   #   scale_fill_manual(values=c("orange","blue"))  + ggtitle("Subject Reaction Time for N-Back Levels and ISI") + xlab("N-Back Level") + ylab("Onset Time (ms)")
   # ggsave(file)
   #
   # responsetime_file_name_jpeg = paste0("ResponseTime_",toString(subject_id),".jpeg")
   # file = file.path(subject_path,"Figures",responsetime_file_name_jpeg)
   # ggplot(data = responsetime_dataframe, aes(fill = interstimulus_interval_correct, x = factor(nback_level_correct), y = subject_response_onset_correct)) + geom_boxplot(position = position_dodge(1)) +
   #   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black")) +
   # scale_fill_manual(values=c("orange","blue"))  + ggtitle("Subject Reaction Time for N-Back Levels and ISI") + xlab("N-Back Level") + ylab("Onset Time (ms)")
   # ggsave(file)

  #  -----------------------------------------------------------------------------------------------

}

