analyze_nback_subject <- function(subject_path)
{
  library(readxl)
  library(stringi)
  library(lattice)
  library(ggplot2)
  library(rprime)
  library(dplyr)

  subject_path_string_split = strsplit(subject_path,"/")[1][1]
  subject_id = vapply(subject_path_string_split, tail, "", 1)

  # need way to ensure this is with respect to subject_dir
  setwd(subject_id)
  dir.create("Figures")
  dir.create("Processed")
  setwd("Processed")
  dir.create("Nback_files")
  setwd("..")
  setwd("..")
  current_path = getwd()
  study_path_split = strsplit(current_path,"/")[1][1]
  study_folder = vapply(study_path_split, tail, "", 1)

  if ((subject_id == "1012" & subject_id == "1004") | study_folder == "MiM_Data")
    {
      nback_data1 = read_excel(file.path(subject_path,"Raw/Nback_files/nback_results_Trial1.xlsx"), cell_rows(2:450), sheet = 1, col_types = "text")
      if (subject_id == "1004")
      {
        nback_data2 = read_excel(file.path(subject_path,"Raw/Nback_files/nback_results_Trial2.xlsx"), cell_rows(2:114), sheet = 1, col_types = "text")
      }
      if (subject_id == "1012" )
      {
        nback_data2 = read_excel(file.path(subject_path,"Raw/Nback_files/nback_results_Trial2.xlsx"), cell_rows(2:226), sheet = 1, col_types = "text")
      }
      nback_block_labels = nback_data1$`Running[SubTrial]`

      subject_accuracy_eprime = nback_data1$Stimulus.ACC
      stimulus_onset_times = nback_data1$Stimulus.OnsetTime
      expected_correct_response = nback_data1$Stimulus.CRESP
      subject_response = nback_data1$Stimulus.RESP
      subject_response_onset = nback_data1$Stimulus.RT

      subject_response_SMask = nback_data1$SMask.RESP
      subject_response_onset_SMask = nback_data1$SMask.RT
      subject_response_LMask = nback_data1$LMask.RESP
      subject_response_onset_LMask = nback_data1$LMask.RT

      nback_block_labels_2 = nback_data2$`Running[SubTrial]`

      subject_accuracy_eprime_2 = nback_data2$Stimulus.ACC
      stimulus_onset_times_2 = nback_data2$Stimulus.OnsetTime
      expected_correct_response_2 = nback_data2$Stimulus.CRESP
      subject_response_2 = nback_data2$Stimulus.RESP
      subject_response_onset_2 = nback_data2$Stimulus.RT

      subject_response_SMask_2 = nback_data2$SMask.RESP
      subject_response_onset_SMask_2 = nback_data2$SMask.RT
      subject_response_LMask_2 = nback_data2$LMask.RESP
      subject_response_onset_LMask_2 = nback_data2$LMask.RT

      # for 1012 remove blocks 3 and 4 from data_1 and replace with data_2
      block_number_1 = stri_sub(nback_block_labels, 2, 2)
      block_number_2 = stri_sub(nback_block_labels_2, 2, 2)


      if (subject_id == "1004")
      {
        block_number_2 = as.numeric(block_number_2) + 3 # setting the block # to 4
        block_number_2 = as.character(block_number_2)
        (stri_sub(nback_block_labels_2, 2, 2) <- as.character(block_number_2))
        indices_to_replace = which(is.na(block_number_1))
      }
      if (subject_id == "1012")
      {
        block_number_2 = as.numeric(block_number_2) + 2 # setting the block # to 3 and 4
        block_number_2 = as.character(block_number_2)
        (stri_sub(nback_block_labels_2, 2, 2) <- as.character(block_number_2))
        indices_to_replace = which(block_number_1 == "3" | block_number_1 == "4")
      }
      # find indices for blocks 3 and 4 in data_1

      nback_block_labels[indices_to_replace] = nback_block_labels_2
      subject_accuracy_eprime[indices_to_replace] = subject_accuracy_eprime_2
      stimulus_onset_times[indices_to_replace] = stimulus_onset_times_2
      expected_correct_response[indices_to_replace] = expected_correct_response_2
      subject_response[indices_to_replace] = subject_response_2
      subject_response_onset[indices_to_replace] = subject_response_onset_2
      subject_response_SMask[indices_to_replace] = subject_response_SMask_2
      subject_response_onset_SMask[indices_to_replace] = subject_response_onset_SMask_2
      subject_response_LMask[indices_to_replace] = subject_response_LMask_2
      subject_response_onset_LMask[indices_to_replace] = subject_response_onset_LMask_2

    }else{
      nback_data = read_excel(file.path(subject_path,"Raw/Nback_files/nback_results.xlsx"), cell_rows(2:450), sheet = 1, col_types = "text")

      nback_block_labels = nback_data$`Running[SubTrial]`

      subject_accuracy_eprime = nback_data$Stimulus.ACC
      stimulus_onset_times = nback_data$Stimulus.OnsetTime
      expected_correct_response = nback_data$Stimulus.CRESP
      subject_response = nback_data$Stimulus.RESP
      subject_response_onset = nback_data$Stimulus.RT

      subject_response_SMask = nback_data$SMask.RESP
      subject_response_onset_SMask = nback_data$SMask.RT
      subject_response_LMask = nback_data$LMask.RESP
      subject_response_onset_LMask = nback_data$LMask.RT
    }


  nback_level = stri_sub(nback_block_labels, 3,3)
  nback_level = as.numeric(nback_level)

  nback_interval = stri_sub(nback_block_labels, 1, 1)
  interstimulus_interval = array(data=NA,length(nback_interval))
  interstimulus_interval[nback_interval == "S"] <- 500
  interstimulus_interval[nback_interval == "L"] <- 1500

  indices_to_extract_from_SMask = which(subject_response_onset_SMask != "NA")
  indices_to_extract_from_LMask = which(subject_response_onset_LMask != "NA")

  subject_response_suspected_late = array(data=NA,length(subject_response))
  subject_response_onset_suspected_late = array(data=NA,length(subject_response_onset))

  subject_response_suspected_late[indices_to_extract_from_SMask] = subject_response_SMask[indices_to_extract_from_SMask]
  subject_response_onset_suspected_late[indices_to_extract_from_SMask] = subject_response_onset_SMask[indices_to_extract_from_SMask]
  subject_response_suspected_late[indices_to_extract_from_LMask] = subject_response_LMask[indices_to_extract_from_LMask]
  subject_response_onset_suspected_late[indices_to_extract_from_LMask] = subject_response_onset_LMask[indices_to_extract_from_LMask]

  total_number_of_stimuli_this_experiment <- nback_level[!is.na(nback_level)]
  number_of_stimuli_in_stimulus_var <- subject_accuracy_eprime[!is.na(subject_accuracy_eprime)]

  #  -----------------------------------------------------------------------------------------------------------------------

  # Fixing some FNIRS eprime bugs
  if (length(total_number_of_stimuli_this_experiment) > length(which(subject_response_onset_suspected_late!="NA")))
  {
    subject_response_SMask1 = nback_data$SMask1.RESP
    subject_response_onset_SMask1 = nback_data$SMask1.RT

    subject_response_LMask1 = nback_data$LMask1.RESP
    subject_response_onset_LMask1 = nback_data$LMask1.RT

    subject_response_LMask2 = nback_data$LMask2.RESP
    subject_response_onset_LMask2 = nback_data$LMask2.RT


    indices_to_extract_from_SMask1 = which(subject_response_onset_SMask1 != "NA")
    indices_to_extract_from_LMask1 = which(subject_response_onset_LMask1 != "NA")
    indices_to_extract_from_LMask2 = which(subject_response_onset_LMask2 != "NA")

    subject_response_suspected_late[indices_to_extract_from_SMask1] = subject_response_SMask1[indices_to_extract_from_SMask1]
    subject_response_onset_suspected_late[indices_to_extract_from_SMask1] = subject_response_onset_SMask1[indices_to_extract_from_SMask1]

    subject_response_suspected_late[indices_to_extract_from_LMask1] = subject_response_LMask1[indices_to_extract_from_LMask1]
    subject_response_onset_suspected_late[indices_to_extract_from_LMask1] = subject_response_onset_LMask1[indices_to_extract_from_LMask1]

    subject_response_suspected_late[indices_to_extract_from_LMask2] = subject_response_LMask2[indices_to_extract_from_LMask2]
    subject_response_onset_suspected_late[indices_to_extract_from_LMask2] = subject_response_onset_LMask2[indices_to_extract_from_LMask2]

  }


  #  -----------------------------------------------------------------------------------------------------------------------
  # Fixing some FNIRS eprime bugs

  # check if data coming form Stimulus is same length as other data.. if not.. do below.. (bug in FNIRS program)
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
  subject_response_onset_suspected_late <- subject_response_onset_suspected_late[indices_to_keep]
  subject_response_suspected_late <- subject_response_suspected_late[indices_to_keep]

  #  -----------------------------------------------------------------------------------------------------------------------
  # # check if subject responded in mask and populate accordingly # #
  # # if subject responded in Mask (late) and not directly after a stimulus then classify this as false-fire # #

  # for each expected response
  # check whether there is a subject response at that index,
  # if not, check subject_response_late for following index,
  # if exists, assign this to subject response and add 500 to the RT
  # keep track of indices that are actually late responses and grab late responses that were not responses.. classify these as false-fires

  indices_to_check = which(expected_correct_response == "1")
  actual_late_response = array(data=NA,length(expected_correct_response))
  for (this_index in indices_to_check)
    {
      if (is.na(subject_response[this_index]))
      {
        if (!is.na(subject_response_suspected_late[this_index]))
        {
          actual_late_response[this_index] = "1"
        }
      }
  }

  indices_actual_late = which(!is.na(actual_late_response))
  indices_suspected_late = which(!is.na(subject_response_suspected_late))

  false_fires_index = setdiff(indices_suspected_late,indices_actual_late)

  subject_response[indices_actual_late] = "1"

  subject_response_onset[indices_actual_late] = subject_response_onset_suspected_late[indices_actual_late]

  expected_correct_response_padded = expected_correct_response
  na_indices= which(is.na(expected_correct_response_padded))
  expected_correct_response_padded[na_indices] = 0

  subject_response_padded = subject_response
  na_indices= which(is.na(subject_response_padded))
  subject_response_padded[na_indices] = 0

  # determine the accurate responses
  subject_accuracy_r_logical = subject_response_padded == expected_correct_response_padded
  subject_accuracy_r = subject_accuracy_r_logical * 1

  # # find indices that are expected and accurate # #
  subject_accuracy_r_numeric = as.numeric(subject_accuracy_r)
  expected_correct_response_padded_numeric = as.numeric(expected_correct_response_padded)
  subject_accurate_and_expected = as.numeric(subject_accuracy_r_numeric == expected_correct_response_padded_numeric)
  response_correct_indices = which(subject_accurate_and_expected == 1)

  # # create array of onset times
  subject_response_onset_correct = as.numeric(subject_response_onset[response_correct_indices])

  # # find indices that exceed thresholds
  response_onset_outliers = which(subject_response_onset_correct > 1000 | subject_response_onset_correct < 70)
  indices_to_remove_from_correct_responses = response_correct_indices[response_onset_outliers]

  # # variables with new
  response_correct_indices = setdiff(response_correct_indices, indices_to_remove_from_correct_responses)

  subject_response_onset_correct = as.numeric(subject_response_onset[response_correct_indices])

  subject_response_padded[indices_to_remove_from_correct_responses] = 0

  # determine which indices are expecte (this seems redundant)
  indices_response = which(subject_response_padded == "1")
  indices_expected = which(expected_correct_response_padded == "1")

  false_fires_index <- unique(sort(append(false_fires_index, setdiff(indices_response,indices_expected))))
  false_fires_nback_level <- as.character(nback_level[false_fires_index])
  false_fires_isi <- interstimulus_interval[false_fires_index]

  # index correct responses for RT data frame
  nback_level_correct = nback_level[response_correct_indices]
  interstimulus_interval_correct =  as.character(interstimulus_interval[response_correct_indices])

  # create data frames
  responsetime_dataframe = data.frame(nback_level_correct, subject_response_onset_correct,interstimulus_interval_correct, subject_id)
  false_fires_dataframe = data.frame(false_fires_index, false_fires_nback_level, false_fires_isi, subject_id)

  # redo subject accuracy after removing the outliers
  subject_accuracy_r_logical = subject_response_padded == expected_correct_response_padded
  subject_accuracy_r = subject_accuracy_r_logical * 1

  # create all_response indices to remove conditions where subject did not respond at all
  all_response_indices = subject_response_padded
  all_response_indices[false_fires_index] = 1

  #  -----------------------------------------------------------------------------------------------------------------------
  # remove indices of condition where subject forgot which nback they were performing
  # make sure there are no false fires in these as well
  unique_subtrials = unique(nback_block_labels)
  noresponse_condition_indices <- vector()
  for (this_condition in unique_subtrials){
      this_condition_stim_indices = (which(nback_block_labels == this_condition))
      subject_response_this_condition <- all_response_indices[this_condition_stim_indices]
      if (is.na(any(subject_response_this_condition==1))){
        noresponse_condition_indices <- append(noresponse_condition_indices, this_condition_stim_indices)
      }
  }
  #  -----------------------------------------------------------------------------------------------------------------------

  # convert accuracy into percent ... find a simpler method!
  if (any(noresponse_condition_indices)){
    accuracy_dataframe = data.frame(nback_level[-noresponse_condition_indices], subject_accuracy_r[-noresponse_condition_indices], interstimulus_interval[-noresponse_condition_indices])
  } else{
    accuracy_dataframe = data.frame(nback_level, subject_accuracy_r, interstimulus_interval)
  }

  #  -----------------------------------------------------------------------------------------------------------------------

  zero_back_short_indices = which(accuracy_dataframe$interstimulus_interval == 500 & accuracy_dataframe$nback_level == 0 & as.numeric(expected_correct_response_padded) == 1)
  one_back_short_indices = which(accuracy_dataframe$interstimulus_interval == 500 & accuracy_dataframe$nback_level == 1 & as.numeric(expected_correct_response_padded) == 1)
  two_back_short_indices = which(accuracy_dataframe$interstimulus_interval == 500 & accuracy_dataframe$nback_level == 2 & as.numeric(expected_correct_response_padded) == 1)
  three_back_short_indices = which(accuracy_dataframe$interstimulus_interval == 500 & accuracy_dataframe$nback_level == 3 & as.numeric(expected_correct_response_padded) == 1)

  zero_back_long_indices = which(accuracy_dataframe$interstimulus_interval == 1500 & accuracy_dataframe$nback_level == 0 & as.numeric(expected_correct_response_padded) == 1)
  one_back_long_indices = which(accuracy_dataframe$interstimulus_interval == 1500 & accuracy_dataframe$nback_level == 1 & as.numeric(expected_correct_response_padded) == 1)
  two_back_long_indices = which(accuracy_dataframe$interstimulus_interval == 1500 & accuracy_dataframe$nback_level == 2 & as.numeric(expected_correct_response_padded) == 1)
  three_back_long_indices = which(accuracy_dataframe$interstimulus_interval == 1500 & accuracy_dataframe$nback_level == 3 & as.numeric(expected_correct_response_padded) == 1)

  zero_back_short_accuracy = accuracy_dataframe$subject_accuracy_r[zero_back_short_indices]
  one_back_short_accuracy = accuracy_dataframe$subject_accuracy_r[one_back_short_indices]
  two_back_short_accuracy = accuracy_dataframe$subject_accuracy_r[two_back_short_indices]
  three_back_short_accuracy = accuracy_dataframe$subject_accuracy_r[three_back_short_indices]

  zero_back_long_accuracy = accuracy_dataframe$subject_accuracy_r[zero_back_long_indices]
  one_back_long_accuracy = accuracy_dataframe$subject_accuracy_r[one_back_long_indices]
  two_back_long_accuracy = accuracy_dataframe$subject_accuracy_r[two_back_long_indices]
  three_back_long_accuracy = accuracy_dataframe$subject_accuracy_r[three_back_long_indices]

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

  #  -----------------------------------------------------------------------------------------------------------------------
  # # create condition onset arrays # #

  # determine the onset time of each condition
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

  #  -----------------------------------------------------------------------------------------------=
  # # WRITE DATA OF INTEREST TO FILE # #

  # # Store Data in Processed folder # #
  write.csv(responsetime_dataframe, file = file.path(subject_path, paste0("Processed/Nback_files/responseTime_", toString(subject_id),".csv")))
  write.csv(accuracy_dataframe_complete, file = file.path(subject_path, paste0("Processed/Nback_files/accuracy_", toString(subject_id),".csv")))
  write.csv(condition_onset_info_dataframe, file = file.path(subject_path, paste0("Processed/Nback_files/conditionOnset_", toString(subject_id),".csv")))
  write.csv(stimulus_onset_info_dataframe, file = file.path(subject_path, paste0("Processed/Nback_files/stimulusnOnsetContinuous_", toString(subject_id),".csv")))
  write.csv(false_fires_dataframe, file = file.path(subject_path, paste0("Processed/Nback_files/falsefires_", toString(subject_id),".csv")))
  #  -----------------------------------------------------------------------------------------------

  # # PLOT # #
  accuracy_file_name_tiff = paste0("Accuracy_",toString(subject_id),".tiff")
  file = file.path(subject_path,"Figures",accuracy_file_name_tiff)
  ggplot(data=accuracy_dataframe_complete, aes(fill = ISI, x = nback, y=subject_accuracy)) + geom_bar(position = "dodge", stat = "identity") +
  scale_y_continuous(name = "Accuracy (%)",
                     breaks = seq(0, 100, 10),
                     limits=c(0, 100)) +
    scale_x_discrete(name = "Nback Level") +
    ggtitle("Subject Accuracy") +
    theme(plot.title = element_text(hjust = 0.5, size = 14, family = "Tahoma", face = "bold"),
        text = element_text(size = 12, family = "Tahoma"),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(size = 11),
        legend.position = "bottom") +
    scale_fill_manual(values=c("orange","blue")) +
    labs(fill = "ISI")
  ggsave(file)

  responsetime_file_name_tiff = paste0("ResponseTime_",toString(subject_id),".tiff")
  file = file.path(subject_path,"Figures",responsetime_file_name_tiff)
  ggplot(responsetime_dataframe, aes(fill = interstimulus_interval_correct, x = factor(nback_level_correct), y = subject_response_onset_correct)) + geom_boxplot(alpha=0.7) + scale_x_discrete(name = "nback_level_correct") +
    scale_y_continuous(name = "Reaction Time (ms)",
                       breaks = seq(0, 1000, 50),
                       limits=c(0, 1000)) +
    scale_x_discrete(name = "Nback Level") +
    ggtitle("Subject Reaction Time") +
    theme(plot.title = element_text(hjust = 0.5, size = 14, family = "Tahoma", face = "bold"),
          text = element_text(size = 12, family = "Tahoma"),
          axis.title = element_text(face="bold"),
          axis.text.x=element_text(size = 11),
          legend.position = "bottom") +
    scale_fill_manual(values=c("orange","blue")) +
    labs(fill = "ISI")
  ggsave(file)

  falsefire_file_name_tiff = paste0("FalseFires",toString(subject_id),".tiff")
  file = file.path(subject_path,"Figures",falsefire_file_name_tiff)
  ggplot(false_fires_dataframe, aes(x = false_fires_nback_level)) + stat_count(width = 0.5, fill="blue") + #geom_bar(position = "dodge", stat="bin") +
    ggtitle("False Fire Rate") +
    theme(plot.title = element_text(hjust = 0.5, size = 14, family = "Tahoma", face = "bold"),
          text = element_text(size = 12, family = "Tahoma"),
          axis.title = element_text(face="bold"),
          axis.text.x=element_text(size = 11),
          legend.position = "bottom")
  ggsave(file)
}

