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
gavage_file <- gavage_file[,c("ID", "Time", "Reads")]
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
all_gen <- read.table("~/Documents/SodaPop_tools/SodaPop_NatEco/out/Z2_r1_popZ_0.01_gen800/ALL_generations.txt", sep="")
colnames(all_gen) <- c("ID", 1,25,50,75,100,125,150,175,200,225,250,275,300,325,350,375,400,425,450,475,500,525,550,575,600,625,650,675,700,725,750,775,800)
new_gen <- reshape2::melt(all_gen, id.vars = c("ID"),
            variable.name = "Time",
            value.name = "Reads")
write.csv(new_gen, "~/Documents/SodaPop_tools/SodaPop_NatEco/out/Z2_r1_popZ_0.01_gen800/Z2_r1_popZ_0.01_gen800.csv", row.names = FALSE)

## Nature Eco Evo: exp1_well-F9 (no drug)
Sample_nd <- readr::read_delim("~/Documents/exp1_well-F9_NEE/Sample_no_drug_clustering.txt", "\t", escape_double = FALSE, trim_ws = TRUE)
nd_cluster <- readr::read_csv("~/Documents/exp1_well-F9_NEE/pooled_barcodes/no_drug-pooled/no_drug_cluster.csv")

names(nd_cluster)[names(nd_cluster) == "Cluster.ID"] <- "ID"

nd_input <- merge(Sample_nd, nd_cluster, by = 'ID')
nd_input$ID <- nd_input$Center
nd_input <- nd_input[ -c(4:6) ]

write.csv(nd_input, "~/Documents/exp1_well-F9_NEE/nd_input.csv", row.names = FALSE)
