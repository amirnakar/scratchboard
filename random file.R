library(filesstrings)
random_files <- function(path, percent_number, pattern = ".txt"){
  
  ####################################################################
  # path = path to folder with files to select                                 
  #                                                                            
  # percent_number = percentage or number of recordings to select. If value is 
  #   between 0 and 1 percentage of files is assumed, if value greater than 1, 
  #   number of files is assumed                                               
  #                                                                            
  # pattern = file extension to select. By default it selects wav files. For   
  #   other type of files replace wav and WAV by the desired extension         
  ####################################################################
  
  # Get file list with full path and file names
  files <- list.files(path, full.names = TRUE, pattern = pattern, recursive = T, include.dirs = T)
  file_names <- list.files(path, pattern = pattern, recursive = T, include.dirs = T)
  
  # Select the desired % or number of file by simple random sampling 
  randomize <- sample(seq(files))
  files2analyse <- files[randomize]
  names2analyse <- file_names[randomize]
  if(percent_number <= 1){
    size <- floor(percent_number * length(files))
  }else{
    size <- percent_number
  }
  files2analyse <- files2analyse[(1:size)]
  names2analyse <- names2analyse[(1:size)]
  
  # Create folder to output
  results_folder <- paste0(path, '/test')
  dir.create(results_folder, recursive=TRUE)
  
  # Write csv with file names
  write.table(files2analyse, file = paste0(results_folder, "/test set.csv"),
              col.names = "Files", row.names = FALSE)
  
  # Move files
  for(i in seq(files2analyse)){
    file.move(files2analyse[i], results_folder)
    
    
    
  }
}

### Script Start ###


path = "C:/Users/di58lag/Desktop/PhD/Projects/Enterobacteriacea database/Data/Experiment/By strain/Shigella 55"
percent_number = 0.1
pattern = "txt"

random_files(path, percent_number, pattern)





