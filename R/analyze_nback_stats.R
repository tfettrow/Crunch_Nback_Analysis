analyze_nback_stats <- function(group_paths)
{

  library(readxl)
  library(stringi)
  library(lattice)
  library(ggplot2)
  library(rprime)
  library(lmerTest)


  ## Difference between conditions, ISI, accuracy, RT

  for (this_group_path in group_paths){
    this_group_path_string_split = strsplit(this_group_path,"/")[1][1]
    this_group_id = vapply(this_group_path_string_split, tail, "", 1)
    #setwd("..")
    # Goes into Subject directory to load file
    setwd(this_group_path)

    this_variable = paste0(toString(this_group_id),"accuracy_data")
    this_variable = read.csv(file.path("Group_Results/Nback_files/", paste0(toString(this_group_id), "_accuracy", ".csv")))

    paste0(toString(this_group_id),"responseTime_data") = read.csv(file.path("Group_Results/Nback_files/", paste0(toString(this_group_id), "_responseTime", ".csv")))
    #all_responseTime_data = rbind(all_responseTime_data, responseTime_data)  This data is already fed in as averaged

    # This goes back to Study folder to confirm proper current directory for saving into Group_Results folder
    setwd("..")
  }


  #group_accuracy_data_averaged <- aggregate(accuracy_data["subject_accuracy"], by=list(accuracy_data$ISI, accuracy_data$nback),FUN=mean)

}

fit <- lmer(Subjective_Valence ~ Emotion_Condition + (1|Participant_ID), data=df)
anova(fit)
