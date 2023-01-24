# Mock input files preparation (to be deleted)
samples <- list("M1", "M2", "M3", "M4")
for(sample in samples) {
  Sample_M <- readr::read_delim(paste("~/processed_samples/",sample,"/Sample_",sample,"_clustering.txt", sep=""),
                                "\t", escape_double = FALSE, trim_ws = TRUE)
  M_cluster <- readr::read_csv(paste("~/processed_samples/",sample,"/",sample,"_cluster.csv", sep=""))

  names(M_cluster)[names(M_cluster) == "Cluster.ID"] <- "ID"

  Mock_input <- merge(Sample_M, M_cluster, by = 'ID')
  Mock_input$ID <- Mock_input$Center
  Mock_input <- Mock_input[ -c(4:6) ]

  write.csv(Mock_input, paste("~/Documents/",sample,"_input.csv",sep=""), row.names = FALSE)

}

# Ref input file preparation
gavage_file <- readr::read_csv(paste("~/processed_samples/Sample_Gavage_cluster.csv", sep=""))
gavage_file$Cluster.ID = NULL
gavage_file$Cluster.Score = NULL
names(gavage_file)[names(gavage_file) == "Center"] <- "ID"
names(gavage_file)[names(gavage_file) == "time_point_1"] <- "Reads"
gavage_file$Time = 0
#colnames(Gavage.index)[4]="M5.ID" -> pour changer nom des colonnes
write.csv(gavage_file, paste("~/Documents/doblin/demo/ref_time0.csv",sep=""), row.names = FALSE)

# Create M1 as an input with t=0
library("dplyr")
library("plyr")
library("readr")
Mock_data <- list("~/Documents/doblin/demo/M1_input.csv", "~/Documents/doblin/demo/ref_time0.csv")%>%
  lapply(read_csv) %>%
  bind_rows
write.csv(Mock_data, "~/Documents/doblin/demo/M1_input.csv", row.names = FALSE)


# ALL_generation.txt --> sample_test.csv (ID, Time, Reads)
# from wide- to long-format
all_gen <- read.table("~/SodaPop_NatEco/out/model_denovo1/ALL_generations.txt", sep="")
colnames(all_gen) <- c("ID", 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17)
new_gen <- reshape2::melt(all_gen, id.vars = c("ID"),
            variable.name = "Time",
            value.name = "Reads")
write.csv(new_gen, "~/Documents/doblin/demo/model_denovo1.csv", row.names = FALSE)

