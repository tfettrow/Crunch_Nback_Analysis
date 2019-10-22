analyze_nback_subject <- function(subject_path)
{
  library(readxl)
  library(stringi)
  library(lattice)
  library(ggplot2)
  library(rprime)
  library(dplyr)
  library(psycho)
  library(readr)

  # TO DO
  # 1) add deprime variable and plot/save something
  # 2) convert outlier removal to 2std away from median


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


  if ((subject_id == "1012" || subject_id == "1004") && study_folder == "MiM_Data")
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

      if (!(nback_data$Subject[1] == subject_id))
      {
        warning('This results file does not match the subject folder')
      }

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

  subject_response_onset[indices_actual_late] = as.numeric(subject_response_onset_suspected_late[indices_actual_late]) + 500 # TO DO: make sure this adds 500!!

  expected_correct_response_padded = expected_correct_response
  expected_rejected_indices = which(is.na(expected_correct_response_padded))
  expected_correct_response_padded[expected_rejected_indices] = 0

  expected_rejected = array(data=NA,length(expected_correct_response))
  expected_rejected[expected_rejected_indices] = 1
  expected_rejected[-expected_rejected_indices] = 0

  subject_response_padded = subject_response
  na_indices= which(is.na(subject_response_padded))
  subject_response_padded[na_indices] = 0

  # determine the accurate responses
  subject_accuracy_r_logical = subject_response_padded == expected_correct_response_padded
  total_subject_accuracy_r = subject_accuracy_r_logical * 1

  expected_correct_response_padded_numeric = as.numeric(expected_correct_response_padded)

  subject_response_and_expected = array(data=0,length(expected_correct_response))
  subject_rejected_and_expected = array(data=0,length(expected_correct_response))
  for (this_index in 1:length(total_subject_accuracy_r))
  {
    if (total_subject_accuracy_r[this_index] == 1 & expected_correct_response_padded_numeric[this_index] == 1)
    {
      subject_response_and_expected[this_index] = 1
    }
    if (total_subject_accuracy_r[this_index] == 1 & expected_rejected[this_index] == 1)
    {
      subject_rejected_and_expected[this_index] = 1
    }
  }

  # determine which indices are expected (this seems redundant)
  indices_response = which(subject_response_padded == "1")
  indices_expected = which(expected_correct_response_padded == "1")

  response_correct_indices = which(subject_response_and_expected == 1)

  # # create array of onset times
  subject_response_onset_correct = as.numeric(subject_response_onset[response_correct_indices])

  false_fires_index <- unique(sort(append(false_fires_index, setdiff(indices_response,indices_expected))))
  false_fires_nback_level <- as.character(nback_level[false_fires_index])
  false_fires_isi <- interstimulus_interval[false_fires_index]

  # index correct responses for RT data frame
  nback_level_correct = nback_level[response_correct_indices]
  interstimulus_interval_correct =  as.character(interstimulus_interval[response_correct_indices])

  # create data frames
  responsetime_dataframe = data.frame(nback_level_correct, subject_response_onset_correct,interstimulus_interval_correct, subject_id)

  # mad_responsetime_dataframe <- aggregate(responsetime_dataframe$subject_response_onset_correct,
  #                                         by = list(nback_level = responsetime_dataframe$nback_level_correct, ISI = responsetime_dataframe$interstimulus_interval_correct, subject_id = responsetime_dataframe$subject_id),
  #                                         function(x) c(mad_value = 2.5 * median(abs(x - median(x)))))
  # colnames(mad_responsetime_dataframe) <- c("nback_level", "ISI", "subject_id", "mad_value")
  # mad_responsetime_dataframe <- mad_responsetime_dataframe[,c(1,2,4,3)]
  # mad_responsetime_dataframe <- mad_responsetime_dataframe[order(-as.numeric(mad_responsetime_dataframe$ISI)),]
  #
  # median_responsetime_dataframe <- aggregate(responsetime_dataframe$subject_response_onset_correct,
  #                                            by = list(nback_level = responsetime_dataframe$nback_level_correct, ISI = responsetime_dataframe$interstimulus_interval_correct, subject_id = responsetime_dataframe$subject_id),
  #                                            FUN=median)
  # colnames(median_responsetime_dataframe) <- c("nback_level", "ISI", "subject_id", "median_response_time")
  # median_responsetime_dataframe <- median_responsetime_dataframe[,c(1,2,4,3)]
  # median_responsetime_dataframe <- median_responsetime_dataframe[order(-as.numeric(median_responsetime_dataframe$ISI)),]
  #

  # then determine which indices of these correspond to values of response_correct_indices so we can remove the appropriate indices
  # from nback_level, interstimulus interval, subject_response, subject_response_onset




  # vv need to remove outliers before this step vv
  # calculate median again
  median_responsetime_dataframe <- aggregate(responsetime_dataframe$subject_response_onset_correct,
                                             by = list(nback_level = responsetime_dataframe$nback_level_correct, ISI = responsetime_dataframe$interstimulus_interval_correct, subject_id = responsetime_dataframe$subject_id),
                                             FUN=median)
  colnames(median_responsetime_dataframe) <- c("nback_level", "ISI", "subject_id", "median_response_time")
  median_responsetime_dataframe <- median_responsetime_dataframe[,c(1,2,4,3)]
  median_responsetime_dataframe <- median_responsetime_dataframe[order(-as.numeric(median_responsetime_dataframe$ISI)),]

  std_responsetime_dataframe <- aggregate(responsetime_dataframe$subject_response_onset_correct,
                                          by = list(nback_level = responsetime_dataframe$nback_level_correct, ISI = responsetime_dataframe$interstimulus_interval_correct, subject_id = responsetime_dataframe$subject_id),
                                          FUN=sd)

  false_fires_array = array(data=0,length(expected_correct_response))
  false_fires_array[false_fires_index] = 1

  false_fires_dataframe = data.frame(false_fires_array, nback_level, interstimulus_interval, subject_id)
  total_false_fires_dataframe <- aggregate(false_fires_dataframe$false_fires_array,
                                           by = list(false_fires_dataframe$nback_level, false_fires_dataframe$interstimulus_interval,
                                                     false_fires_dataframe$subject_id), FUN=sum)
  colnames(total_false_fires_dataframe) <- c("nback_level", "ISI", "subject_id", "number_of_false_fires")
  total_false_fires_dataframe <- total_false_fires_dataframe[,c(1,2,4,3)]

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

  # removing the indices where the subject did not respond at all
  if (any(noresponse_condition_indices)){
    nback_level_results = nback_level[-noresponse_condition_indices]
    interstimulus_interval_results = interstimulus_interval[-noresponse_condition_indices]
    expected_correct_response_padded_numeric_results = expected_correct_response_padded_numeric[-noresponse_condition_indices]
    subject_response_and_expected_results = subject_response_and_expected[-noresponse_condition_indices]
    expected_rejected_results = expected_rejected[-noresponse_condition_indices]
  } else{
    nback_level_results = nback_level
    interstimulus_interval_results = interstimulus_interval
    expected_correct_response_padded_numeric_results = expected_correct_response_padded_numeric
    subject_response_and_expected_results = subject_response_and_expected
    expected_rejected_results = expected_rejected
  }

  results_dataframe <- aggregate(expected_correct_response_padded_numeric_results, by = list(nback_level_results, interstimulus_interval_results), FUN = sum)
  colnames(results_dataframe) <- c("nback_level", "ISI", "number_of_expected_responses")

  correct_responses_dataframe <- aggregate(subject_response_and_expected_results, by = list(nback_level_results, interstimulus_interval_results), FUN = sum)
  colnames(correct_responses_dataframe) <-  c("nback_level", "ISI", "number_of_correct_responses")
  results_dataframe$number_of_correct_responses = correct_responses_dataframe$number_of_correct_responses

  results_dataframe$percent_correct = results_dataframe$number_of_correct_responses/results_dataframe$number_of_expected_responses * 100
  results_dataframe$median_response_time = median_responsetime_dataframe$median_response_time

  expected_rejected_dataframe <- aggregate(expected_rejected_results, by = list(nback_level_results, interstimulus_interval_results), FUN = sum)
  colnames(expected_rejected_dataframe) <-  c("nback_level", "ISI", "number_of_expected_rejected")
  results_dataframe$number_of_expected_rejected = expected_rejected_dataframe$number_of_expected_rejected

  results_dataframe$number_of_false_fires = total_false_fires_dataframe$number_of_false_fires

  # d'prime calculation

  #Arguments
  #n_hit: Number of hits.   (number_of_correct_responses)
  #n_fa: Number of false alarms.  (number_of_false_alarms)
  #n_miss: Number of misses.
  #n_cr: Number of correct rejections.
  #n_targets: Number of targets (n_hit + n_miss).
  #n_distractors: Number of distractors (n_fa + n_cr).
  #adjusted: Should it use the Hautus (1995) adjustments for extreme values.

  sensitivity_dataframe <- psycho::dprime(
    n_hit = results_dataframe$number_of_correct_responses,
    n_fa = results_dataframe$number_of_false_fires,
    n_targets = results_dataframe$number_of_expected_responses,
    n_distractors = results_dataframe$number_of_expected_rejected,
    adjusted = FALSE
    )

  results_dataframe$dprime <- round(sensitivity_dataframe$dprime, digits = 2)
  results_dataframe$beta <- round(sensitivity_dataframe$beta, digits = 2)
  results_dataframe$aprime <- round(sensitivity_dataframe$aprime, digits = 2)
  results_dataframe$bppd <- round(sensitivity_dataframe$bppd, digits = 2)
  results_dataframe$c <- round(sensitivity_dataframe$c, digits = 2)

  #  -----------------------------------------------------------------------------------------------------------------------
  # # create condition onset arrays # #

  # determine the onset time of each condition
  condition_onset_times_corrected <- vector()
  first_condition_onset_time_eprime <- vector()
  run_numbers = unique(stri_sub(unique_subtrials, 2,2))

  for (this_run in run_numbers){
    this_condition_onset_time_corrected <- vector()
    this_condition_rest_time_corrected <- vector()
    condition_onset <- vector()
    rest_onset <- vector()
    condition_names <- vector()

    this_run_indices = which(stri_sub(unique_subtrials, 2,2) == this_run)

    for (this_condition in unique_subtrials[this_run_indices]){

      this_nback_level = stri_sub(this_condition, 3,3)
      this_nback_interval = stri_sub(this_condition, 1, 1)

      this_condition_first_stim_index = min(which(nback_block_labels == this_condition))
      this_condition_last_stim_index = max(which(nback_block_labels == this_condition))
      this_condition_onset_time_eprime = stimulus_onset_times[this_condition_first_stim_index]

      if (this_nback_interval == "L")
      {
        this_condition_rest_time_eprime = as.numeric(stimulus_onset_times[this_condition_last_stim_index]) + 2000
      } else if (this_nback_interval == "S")
      {
        this_condition_rest_time_eprime = as.numeric(stimulus_onset_times[this_condition_last_stim_index]) + 1000
      }

      # reset the time correction every 8 conditions (this makes up a run)
      if (length(condition_onset_times_corrected)%%8 == 0 & length(condition_onset_times_corrected) >= 8){
        first_condition_onset_time_eprime <- vector()
      }

      if (length(first_condition_onset_time_eprime) == 0){
        first_condition_onset_time_eprime = this_condition_onset_time_eprime
        this_condition_onset_time_corrected = 4500
        this_condition_rest_time_corrected = this_condition_rest_time_eprime
      } else {
        this_condition_onset_time_corrected = ((as.numeric(this_condition_onset_time_eprime) - as.numeric(first_condition_onset_time_eprime))) + 4500
        this_condition_rest_time_corrected = ((as.numeric(this_condition_rest_time_eprime) - as.numeric(first_condition_onset_time_eprime))) + 4500
      }
      condition_onset = append(condition_onset, this_condition_onset_time_corrected)
      rest_onset = append(rest_onset, this_condition_rest_time_corrected)

      if (this_nback_level == "0" & this_nback_interval == "L") {
        this_condition_name = "long_zero"
      } else if (this_nback_level == "1" & this_nback_interval == "L") {
        this_condition_name = "long_one"
      } else if (this_nback_level == "2" & this_nback_interval == "L"){
        this_condition_name = "long_two"
      } else if (this_nback_level == "3" & this_nback_interval == "L"){
        this_condition_name = "long_three"
      } else if (this_nback_level == "0" & this_nback_interval == "S"){
        this_condition_name = "short_zero"
      } else if (this_nback_level == "1" & this_nback_interval == "S"){
        this_condition_name = "short_one"
      } else if (this_nback_level == "2" & this_nback_interval == "S"){
        this_condition_name = "short_two"
      } else if (this_nback_level == "3" & this_nback_interval == "S"){
        this_condition_name = "short_three"
      }
      condition_names = append(condition_names, this_condition_name)
    }
    condition_onset_info_dataframe = data.frame(condition_onset, rest_onset, condition_names)

    #condition_onset_info_dataframe = data.frame(condition_onset_times_corrected, new_condition_names)

    write_csv(condition_onset_info_dataframe, file.path(subject_path, paste0("Processed/Nback_files/Condition_Onsets_Run", toString(this_run),".csv")))
  }

  stimulus_onset_times_continuous_corrected = as.numeric(stimulus_onset_times) - as.numeric(stimulus_onset_times[1]) + 4500
  stimulus_onset_info_dataframe = data.frame(stimulus_onset_times_continuous_corrected, nback_block_labels)

  #  -----------------------------------------------------------------------------------------------=
  # # WRITE DATA OF INTEREST TO FILE # #

  # # Store Data in Processed folder # #
  #write_csv(median_responsetime_dataframe, file.path(subject_path, paste0("Processed/Nback_files/median_responsetime_", toString(subject_id),".csv")))
  #write.csv(median_responsetime_dataframe, file = file.path(subject_path, paste0("Processed/Nback_files/median_responsetime_", toString(subject_id),".csv")))
  #write_csv(responsetime_dataframe, file.path(subject_path, paste0("Processed/Nback_files/all_responsetime_", toString(subject_id),".csv")))
  #write_csv(accuracy_dataframe_complete, file.path(subject_path, paste0("Processed/Nback_files/accuracy_", toString(subject_id),".csv")))
  write_csv(stimulus_onset_info_dataframe, file.path(subject_path, paste0("Processed/Nback_files/Stimulus_Onsets_", toString(subject_id),".csv")))
  write_csv(results_dataframe, file.path(subject_path, paste0("Processed/Nback_files/results_", toString(subject_id),".csv")))

  #write_csv(total_false_fires_dataframe, file.path(subject_path, paste0("Processed/Nback_files/falsefires_", toString(subject_id),".csv")))
  #  -----------------------------------------------------------------------------------------------

  # # PLOT # #
  accuracy_file_name_tiff = paste0("Accuracy_",toString(subject_id),".tiff")
  file = file.path(subject_path,"Figures",accuracy_file_name_tiff)
  ggplot(data=results_dataframe, aes(fill = factor(ISI), x = factor(nback_level), y=percent_correct)) + geom_bar(position = "dodge", stat = "identity") +
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
  ggplot(responsetime_dataframe, aes(fill = interstimulus_interval_correct, x = factor(nback_level_correct), y = subject_response_onset_correct)) + stat_boxplot(geom ='errorbar') + geom_boxplot(alpha=0.7) + scale_x_discrete(name = "nback_level_correct") +
    geom_point(aes(fill = interstimulus_interval_correct), size = 3, shape = 21, position = position_jitterdodge()) +
    scale_x_discrete(name = "Nback Level") +
    ggtitle("Subject Reaction Time") +
    theme(plot.title = element_text(hjust = 0.5, size = 14, family = "Tahoma", face = "bold"),
          text = element_text(size = 12, family = "Tahoma"),
          axis.title = element_text(face="bold"),
          axis.text.x=element_text(size = 11),
          legend.position = "bottom") +
    scale_fill_manual(values=c("blue","orange")) +
    labs(fill = "ISI")
  ggsave(file)

  falsefire_file_name_tiff = paste0("FalseFires",toString(subject_id),".tiff")
  file = file.path(subject_path,"Figures",falsefire_file_name_tiff)
  ggplot(results_dataframe, aes(fill = factor(ISI), x = factor(nback_level), y=number_of_false_fires)) + geom_bar(position = "dodge", stat = "identity") + # + stat_count(width = 0.5, fill="blue") + #geom_bar(position = "dodge", stat="bin") +
    ggtitle("False Fire Rate") +
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
}
