library(tidyverse)
chromHMM_output <- read_tsv("final.out.act_enh_devmap_Ssalar.txt")
colnames(chromHMM_output)[1] <- "Motif"

chromHMM_output  <- chromHMM_output %>% 
  separate(Motif, sep = "_", into = c("Motif_a", "Motif_b"), remove = F) %>% 
  separate(Motif_b, sep = "\\.", into = c("a", "b", "TF"), remove = F) %>% 
  select(Motif_a, TF, Motif_b)

list_all_motif <- chromHMM_output$Motif_a

write.table(list_all_motif, "list_all_motif.txt", col.names = F, row.names = F,
            quote = F)
