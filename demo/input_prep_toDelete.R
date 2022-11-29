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
#colnames(Gavage.index)[4]="M5.ID" -> pour changer nom des colonnes
write.csv(gavage_file, paste("~/Documents/ref_time0.csv",sep=""), row.names = FALSE)


