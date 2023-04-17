library(tidyverse)
library(glue)

DIR=basename(getwd())

# create a function to read & parse nfcore peakcount tsv output
read_peak_file <- function(tsvFile) {
  read_tsv(tsvFile) %>% 
    mutate(PeakCount = rowSums(select_if(., is.numeric), na.rm = TRUE)) %>% 
    select(Category, PeakCount) %>% 
    separate(col=Category, into=c("Species", "Assay", "Mark", "Maturity", "Tissue","Rep"), remove = F)
}


# load multiple tsv files and combine into one df
df_peaks <- list.files(path="./", full.names = TRUE, pattern = "tsv") %>% 
  lapply(read_peak_file) %>% 
  bind_rows


# summarise peaks for mean and stdev
df_summary <- df_peaks %>% 
  mutate(grouped = paste(Mark,Tissue, sep = "_")) %>% 
  group_by(grouped) %>% 
  summarise(disp = mean(PeakCount), sd = sd(PeakCount)) 


# quick plotting of peaks summary by histone marks
df_summary %>% 
  separate(col=grouped, into = c("Mark", "Tissue"), remove = F) %>% 
  filter (Mark %in% c("H3K27ac","H3K27me3", "H3K4me1", "H3K4me3")) %>% 
  ggplot()+
  geom_col(mapping=aes(x=Tissue, y= disp),
           position = "identity", na.rm = TRUE, fill="maroon", width = 0.6) +
  theme_classic()+
  labs(x = "\nSample\n",
       y = "\n Mean no. of peaks\n") +
  theme(axis.text.y = element_text(color="black", size = 14), axis.text.x = element_text(size = 14)) +
  theme(axis.title.x = element_text(color = "black", size = 16),
        axis.title.y = element_text(color = "black", size = 16),
        plot.title = element_text(size=20)) +
  ggtitle(glue("\n {DIR} : Total peaks MACS2 \n \n \n"))+
  theme(plot.margin=unit(c(0.5,2,.5,.5),"cm"),
        strip.text = element_text(size = 14))+
  theme(text=element_text(family="Arial"))+
  facet_grid(~ Mark)
