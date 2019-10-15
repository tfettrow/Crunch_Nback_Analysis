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

    #this_variable = paste0(toString(this_group_id),"_accuracy")
   # (paste0("Processed/Nback_files/accuracy_"), toString(this_subject_id))


    this_group_accuracy_data = read.csv(file.path("Group_Results/Nback_files", paste0(toString(this_group_id), "_accuracy.csv")))
    this_group_response_data = read.csv(file.path("Group_Results/Nback_files/", paste0(toString(this_group_id), "_responseTime.csv")))




    #all_responseTime_data = rbind(all_responseTime_data, responseTime_data)  This data is already fed in as averaged

    # This goes back to Study folder to confirm proper current directory for saving into Group_Results folder
    setwd("..")
  }


  #group_accuracy_data_averaged <- aggregate(this_group_accuracy_data["this_group_accuracy_data"], by=list(accuracy_data$ISI, accuracy_data$nback),FUN=mean)

}

#TF GUESS
TF_GUESS_MODEL <- lmer(subject_response_onset_correct ~ nback_level_correct + (1|subject_id), data=this_group_response_data)  ##obviously not emotion condition
summary(TF_GUESS_MODEL)
anova(TF_GUESS_MODEL)
##  IDK, maybe its predcting accuracy by ISI?
Accuracy_guess_model<- lmer(subject_accuracy ~ ISI + (1|subject_id), data=this_group_accuracy_data)  ##obviously not emotion condition
summary(Accuracy_guess_model)
anova(Accuracy_guess_model)
##could it be predicting accuracy by nback level?
Accuracy_guess_model2<- lmer(subject_accuracy ~ nback + (1|subject_id), data=this_group_accuracy_data)  ##obviously not emotion condition
summary(Accuracy_guess_model2)
anova(Accuracy_guess_model2)




#doesnt seem to work, similar to 2
#library(nlme)
#anova.lme(Accuracy_guess_model3)
#Accuracy_guess_model3 = lme(subject_accuracy ~ nback + (1|subject_id),
#             random = ~1,
#             data=this_group_accuracy_data,
#             method="REML")


library(multcompView)
library(emmeans)

#This is supposed to be an attempt at some post-hoc, additional comments are provided from the stats class, as was the code
#
#(leastsquare1b = lsmeans::lsmeans(Accuracy_guess_model2,
#                                  pairwise ~ task*Condition,
#                                  adjust="none") )      ###  Tukey-adjusted comparisons
#leastsquare1b$contrasts
#lsmeans adjust options include
#"tukey", "scheffe", "sidak", "bonferroni", "dunnettx", "mvt", and "none"
#These are tricky to look at, but they have the information we need
#Some contrasts compare different treatments at pretest or at posttest
#These will give us the equivalent of SPSS simple effects tables
