analyze_nback_group <- function(subject_paths)
{
  library(readxl)
  library(stringi)
  library(lattice)
  library(ggplot2)
  library(rprime)

  # some funny stuff to be able to grab the subject ID for folder finding and file naming
  for (this_subject_path in subject_paths){
    this_subject_path_string_split = strsplit(this_subject_path,"/")[1][1]
    this_subject_id = vapply(this_subject_path_string_split, tail, "", 1)




    # for each subject id read the responseTime.txt
    # nback_data1 = read_excel(file.path(subject_path,"Raw/Nback_files/nback_results.xlsx"), range = "EA2:EC450", sheet = 1, col_types = "text")

    # concatenate the data for each subject's responseTime.txt






    # for each subject id read the accuracy.txt

    # concatenate the data for each subject's accuracy.txt





  }








  # plot the concatenated responseTime data

  # plot the concatenated accuracy data



}
