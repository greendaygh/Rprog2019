extract_values_from_own_matrix <- function(x){
  index <- which(!is.na(x))
  return(x[index])
}

extract_values_from_other_matrix <- function(x){
  conditions <- x[1:8]
  well_names <- x[9:16]
  index <- which(!is.na(conditions))
  return(well_names[index])
}


multiple_plate_excel_reader <- function(design_file_name, data_file_name){
  require(readxl)
  
  ## read excel files
  mydesign <- as.data.frame(read_excel(design_file_name, sheet=1, range="A1:L8", skip = 0, col_names=F))
  mydata <- as.data.frame(read_excel(data_file_name, sheet=1))
  
  ## build a position matrix
  pos1 <- rep(LETTERS[1:8], time=12)
  pos2 <- rep(sprintf("%02d", 1:12), each=8)
  well_position_labels <- paste(pos1, pos2, sep="")
  well_position_matrix <- matrix(well_position_labels, nrow=8, ncol=12)
  
  ## extract positions from position matrix by using design matrix indexes
  colnames(mydesign) <- as.character(1:12)
  colnames(well_position_matrix) <- as.character(1:12)
  bound_matrix <- rbind(mydesign, well_position_matrix)
  tmpv <- lapply(bound_matrix, extract_values_from_other_matrix)
  well_names <- unlist(tmpv)
  
  ## extract conditions from design matrix by design matrix indexes
  tmpv <- lapply(mydesign, extract_values_from_own_matrix)
  well_conditions <- unlist(tmpv)
  
  ## build well_info matrix 
  well_info <- data.frame(well_names, well_conditions, stringsAsFactors = F)
  
  ## subset of the data filtered by the wells that we are interested in
  tmpidx <- match(mydata$Well, well_info$well_names)
  tmp_mydata_subset <- subset(mydata, !is.na(tmpidx))
  
  ## extract OD, GFP, etc if there is any
  sel_column <- c(3, seq(6, ncol(tmp_mydata_subset), by=2))
  mydata_subset <- tmp_mydata_subset[,sel_column]
  
  ## to make the condition column more readable
  tmp_final_data <- merge(well_info, mydata_subset, by.x="well_names", by.y="Well")
  tmpcond <- sapply(tmp_final_data$well_conditions, function(x){
    tmp <- unlist(strsplit(x, ";"))
    names(tmp) <- c("sample_names", "replication", "concentration")
    return(tmp)
  }
  )
  
  ## make final data matrix
  t_tmpcond <- t(tmpcond)
  t_tmpcond2 <- cbind(t_tmpcond, well_conditions=rownames(t_tmpcond))
  rownames(t_tmpcond2) <- NULL
  t_tmpcond2 <- data.frame(t_tmpcond2)
  t_tmpcond2$concentration <- as.numeric(as.character(t_tmpcond2$concentration))
  final_data <- merge(tmp_final_data, t_tmpcond2, by="well_conditions")
  final_data <- final_data[,-1]
  
  return(final_data)
}

