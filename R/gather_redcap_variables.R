analyze_nback_group <- function(subject_path)
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

  subject_path_string_split = strsplit(subject_path,"/")[1][1]
  subject_id = vapply(subject_path_string_split, tail, "", 1)

  # need way to ensure this is with respect to subject_dir
  setwd(subject_id)

  #setwd("Processed")

  template_data = read.csv(file.path("Processed/Nback_files/MindInMotion_ImportTemplate.csv"))

  subject_redcap_data = read.csv(file.path(paste0("Processed/Nback_files/redcap_variables_", toString(subject_id), ".csv")),row.names=F)

  for (this_redcap_variable_index in 1:length(subject_redcap_data))
  {
    this_variable_name = names(subject_redcap_data[this_redcap_variable_index])
    this_variable_data = (subject_redcap_data[this_redcap_variable_index])

    this_variable_template_index = which(template_data$Variable...Field.Name == this_variable_name)
    template_data[this_variable_template_index,2] <- this_variable_data
  }
  write.csv(template_data, file = file.path("Processed/Nback_files/MindInMotion_ImportTemplate.csv"))
}

