library(ggplot2)

subject_id = 1010

all_results_data <- vector()
color_cell <- vector()
if (dir.exists(paste0("fnirs/",toString(subject_id))))
{
  setwd(paste0("fnirs/",toString(subject_id)))
  setwd("Processed/Nback_files")
  fNRIS_nback_data<-read.csv(paste0("results_",toString(subject_id),".csv"))
  setwd("../../../..")
  fNRIS_nback_data$modality = "fnirs"
  all_results_data = rbind(all_results_data, fNRIS_nback_data)
  color_cell = c(color_cell, "royalblue3")
}

if (dir.exists(paste0("eeg/",toString(subject_id))))
{
  setwd(paste0("eeg/",toString(subject_id)))
  setwd("Processed/Nback_files")
  EEG_nback_data<-read.csv(paste0("results_",toString(subject_id),".csv"))
  setwd("../../../..")
  EEG_nback_data$modality = "eeg"
  all_results_data = rbind(all_results_data, EEG_nback_data)
  color_cell = c(color_cell, "cyan3")
}

if (dir.exists(paste0("fmri/",toString(subject_id))))
{
  setwd(paste0("fmri/",toString(subject_id)))
  setwd("Processed/Nback_files")
  fMRI_nback_data<-read.csv(paste0("results_",toString(subject_id),".csv"))
  setwd("../../../..")
  fMRI_nback_data$modality = "fmri"
  all_results_data = rbind(all_results_data, fMRI_nback_data)
  color_cell = c(color_cell, "darkgreen")
}

### SYNTAX FOR NBACK LEVEL vs CORRECT (Across Modalities)   #######


accuracy_modality_fig_tiff = paste0("Accuracy",toString(subject_id),".tiff")
file = file.path("Comparison_Figures",accuracy_modality_fig_tiff)
ggplot(all_results_data, aes(fill = factor(modality), x = factor(nback_level), y=percent_correct)) + geom_bar(position = "dodge", stat = "identity") +
  ggtitle("Nback vs Accuracy (Modalities)") +
  scale_y_continuous(name = "Accuracy (%)") +
  scale_x_discrete(name = "Nback Level") +
  theme(plot.title = element_text(hjust = 0.5, size = 14, family = "Tahoma", face = "bold", color = "white"),
        axis.title = element_text(face="bold", color = "white"),
        axis.text.x=element_text(size = 11, color = "white"),
        axis.text.y=element_text(size = 11, color = "white"),
        axis.ticks = element_line(colour = 'white', size = .5),
        legend.position = "bottom",
        legend.text = element_text(color = "white"),
        legend.background = element_rect(fill = "#1e1e1e"),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "#1e1e1e"),
        plot.background = element_rect(fill = "#1e1e1e", color = "#1e1e1e"),
        axis.line = element_line(colour = "white",size=0.1, linetype = "dotted"))  +
  scale_fill_manual(values=color_cell) +
  facet_grid(. ~ ISI) +
  labs(fill = "ISI", color="white")
ggsave(file)

####### SYNTAX FOR NBACK LEVEL vs Response Time (Across Modalities)   #######
responsetime_modality_fig_tiff = paste0("Response_Time",toString(subject_id),".tiff")
file = file.path("Comparison_Figures",responsetime_modality_fig_tiff)
ggplot(all_results_data, aes(fill = factor(modality), x = factor(nback_level), y=median_response_time)) + geom_bar(position = "dodge", stat = "identity") +
  ggtitle("Nback vs Response_Time (Modalities)") +
  scale_y_continuous(name = "Response Time (ms)") +
  scale_x_discrete(name = "Nback Level") +
  theme(plot.title = element_text(hjust = 0.5, size = 14, family = "Tahoma", face = "bold", color = "white"),
        axis.title = element_text(face="bold", color = "white"),
        axis.text.x=element_text(size = 11, color = "white"),
        axis.text.y=element_text(size = 11, color = "white"),
        axis.ticks = element_line(colour = 'white', size = .5),
        legend.position = "bottom",
        legend.text = element_text(color = "white"),
        legend.background = element_rect(fill = "#1e1e1e"),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "#1e1e1e"),
        plot.background = element_rect(fill = "#1e1e1e", color = "#1e1e1e"),
        axis.line = element_line(colour = "white",size=0.1, linetype = "dotted"))  +
  scale_fill_manual(values=color_cell) +
  facet_grid(. ~ ISI) +
  labs(fill = "ISI", color="white")
ggsave(file)

####### SYNTAX FOR NBACK LEVEL vs dprime (Across Modalities)   #######
dprime_modality_fig_tiff = paste0("Dprime",toString(subject_id),".tiff")
file = file.path("Comparison_Figures",dprime_modality_fig_tiff)
ggplot(all_results_data, aes(fill = factor(modality), x = factor(nback_level), y=dprime)) + geom_bar(position = "dodge", stat = "identity") +
  ggtitle("Nback vs Dprime (Modalities)") +
  scale_y_continuous(name = "Dprime (z-score)") +
  scale_x_discrete(name = "Nback Level") +
  theme(plot.title = element_text(hjust = 0.5, size = 14, family = "Tahoma", face = "bold", color = "white"),
        axis.title = element_text(face="bold", color = "white"),
        axis.text.x=element_text(size = 11, color = "white"),
        axis.text.y=element_text(size = 11, color = "white"),
        axis.ticks = element_line(colour = 'white', size = .5),
        legend.position = "bottom",
        legend.text = element_text(color = "white"),
        legend.background = element_rect(fill = "#1e1e1e"),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "#1e1e1e"),
        plot.background = element_rect(fill = "#1e1e1e", color = "#1e1e1e"),
        axis.line = element_line(colour = "white",size=0.1, linetype = "dotted"))  +
  scale_fill_manual(values=color_cell) +
  facet_grid(. ~ ISI) +
  labs(fill = "ISI", color="white")
ggsave(file)
