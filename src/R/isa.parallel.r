library(future); library(furrr); library(tidyverse); library(stringr)

split_and_save_ini <- function(ini_file_path, split_num, ISP_folder_root, output_filepaths){

  # Summary:
  # This function takes a fully functional INI file, splits it for parallel processing,
  # evenly distributes iterations, and manages output file paths.
  # It returns various summaries and the paths of new INI files.
  
  # Check if the INI file exists:
  if (!file.exists(ini_file_path)) {
    stop("INI file does not exist")
  }
  
  # Read the INI file content:
  ini_content <- readLines(ini_file_path)
  
  # Extract iteration count from ini file after "IterationCount=" using str_extract and regex:
  iteration_lines <- stringr::str_extract(ini_content, "IterationCount=\\d+")
  
  # Filter out NA values:
  iteration_lines <- iteration_lines[!is.na(iteration_lines)]  
  
  if (length(iteration_lines) == 0) {
    stop("No iteration count found in INI file")
  } 
  
  else if (length(iteration_lines) > 1) {
    warning("Multiple iteration counts found in INI file, using the first one")
  }
  
  total_iterations <- as.numeric(stringr::str_replace(iteration_lines[1], "IterationCount=", ""))
  
  # Extract Set Seed from ini file after "Seed=" using str_extract and regex:
  seed_lines <- stringr::str_extract(ini_content, "Seed=\\d+")
  
  # Filter out NA values:
  seed_lines <- seed_lines[!is.na(seed_lines)]  
  
  if(length(seed_lines) == 0) {
    stop("No seed found in INI file")
  } 
  
  else if (length(seed_lines) > 1) {
    warning("Multiple seeds found in INI file, using the first one")
  }

  Initial_seed = as.numeric(str_replace(seed_lines[1], "Seed=", ""))

  # Extract the base scenario name from the file path:
  ScName <- tools::file_path_sans_ext(basename(ini_file_path))
  
  # Evenly distribute iterations across splits:
  base_iterations <- floor(total_iterations / split_num)
  extra_iterations <- total_iterations %% split_num
  iteration_counts <- rep(base_iterations, split_num)
  iteration_counts[1:extra_iterations] <- iteration_counts[1:extra_iterations] + 1
  
  # Initialize vector to store paths of new INI files and output file paths:
  ini_Spaths <- vector("character", split_num)
  output_filepaths_summary <- list()
  
  # Initialize tibble for split information
  split_info <- tidyr::tibble(Split_Number = integer(), Iteration_Count = integer(), 
   vac_result = character(), det_result = character(), inf_result = character())
  
  # Process each split:
  for (i in 1:split_num) {
    # Modify IterationCount:
    modified_content <- sub("IterationCount=\\d+", paste0("IterationCount=", iteration_counts[i]), ini_content)
    
    modified_content <- sub("Seed=\\d+", paste0("Seed=", Initial_seed + i*2), modified_content)
    
    # Modify output file paths based on the provided structure or specific paths:
    output_types <- c("_inf", "_det", "_vac")
    for (type in output_types) {
      output_filepath <- ifelse(output_filepaths["general"] == "general", 
       gsub("%%type%%", type, output_filepaths["general_structure"]),
       output_filepaths[type])
      
      output_filepath <-  gsub("%%type%%", type, output_filepaths["general_structure"])
      
      output_suffix <- paste0("_split", i, ".txt")
      pattern <- paste0("Filename=.*", type, ".*\\.txt")
      
      # Remove .txt from output_filepath:
      output_filepath2 <- gsub(".txt", "", output_filepath)

      replacement <- paste0("Filename=", ISP_folder_root, output_filepath2, output_suffix)
      
      # replacement_slash = strreplaceall \\ for /:
      replacement_slash <- gsub("\\\\", "/", replacement)
      
      modified_content <- gsub(pattern, replacement_slash, modified_content)
      
      output_filepaths_summary[[paste0(type, "_split", i)]] <- replacement_slash
      
      replacement_rpath <- gsub("/", "\\\\", replacement)
      
      # Remove Filename == from replacement_rpath
      replacement_rpath <- gsub("Filename=", "", replacement_rpath)
      
      
      # Assign the full path to the appropriate variable:
      if (type == "_vac") {
        vac_result_path <- replacement_rpath
      } 
      
      else if (type == "_det") {
        det_result_path <- replacement_rpath
      } 
      
      else if (type == "_inf") {
        inf_result_path <- replacement_rpath
      }
    }
    
    # Define new INI file path:
    new_ini_path <- paste0(ISP_folder_root, "ini/Temp/", ScName, "_split", i, ".ini")
    
    # Create directory if it doesn't exist:
    if (!dir.exists(dirname(new_ini_path))) {
      dir.create(dirname(new_ini_path), recursive = TRUE)
    }
    
    # Save the modified INI file:
    writeLines(modified_content, new_ini_path)
    
    # Store the new INI file path in the vector:
    ini_Spaths[i] <- new_ini_path
    
    # Add information to split_info tibble
    split_info <- dplyr::add_row(split_info, 
                          Split_Number = i, 
                          Iteration_Count = iteration_counts[i],
                          vac_result = vac_result_path, 
                          det_result = det_result_path, 
                          inf_result = inf_result_path)
  }
  
  # Create input summary:
  input_summary <- list(
    "INI File Path" = ini_file_path,
    "Split Number" = split_num,
    "ISP Folder Root" = ISP_folder_root,
    "Output File Paths" = output_filepaths,
    "Iteration Count" =   total_iterations
  )
  
  return(list(
    ini_paths = ini_Spaths,
    split_info = split_info,
    input_summary = input_summary,
    output_filepaths_summary = output_filepaths_summary
  ))
}

run_simulation <- function(ini_path) {
  
  # Check if ini_path exists:
  if (!file.exists(ini_path)) {
    stop("INI file does not exist: ", ini_path)
  }
  
  # Create log file path by replacing '/ini/' with '/logs/' and changing the extension to '.log':
  log_file <- gsub("/ini/", "/logs/", sub("\\.ini$", ".log", ini_path))
  
  # Ensure the directory for the log file exists:
  log_dir <- dirname(log_file)
  if (!dir.exists(log_dir)) {
    dir.create(log_dir, recursive = TRUE)
  }
  
  # Assuming InterspreadPlus is correctly set in PATH or provide full path to the executable:
  command <- paste("InterspreadPlus", shQuote(ini_path))
  
  # Update command to redirect output to log file.
  # Ensure correct handling of redirection in Windows command line:
  command_with_log <- sprintf("cmd /c %s > %s 2>&1", command, shQuote(log_file))
  
  tryCatch({
    # Run the simulation and capture output in log file:
    shell(command_with_log, wait = TRUE)
  }, error = function(e) {
    cat("Error in running simulation with INI file: ", ini_path, "\nError Message: ", e$message, "\n")
    
    # Optionally write the error message to the log file:
    writeLines(paste("Error Message:", e$message), log_file)
  })
}

adjust_and_combine_splits <- function(split_info, result_type, output_path) {
  combined_df <- tidyr::tibble()
  starting_iterations <- cumsum(c(1, head(split_info$Iteration_Count, -1)))
  
  # Create the split_results directory if it doesn't exist:
  split_results_dir <- file.path(dirname(split_info[[result_type]][1]), "split_results")
  if (!dir.exists(split_results_dir)) {
    dir.create(split_results_dir)
  }
  
  for (i in seq_along(split_info[[result_type]])) {
    split_file_path <- split_info[[result_type]][i]
    
    if (!file.exists(split_file_path)) {
      next
    } 
    
    else if (file.size(split_file_path) == 0) {
      message("File is empty: ", split_file_path)
      file.rename(split_file_path, file.path(split_results_dir, basename(split_file_path)))
      next
    }
    
    split_df <- read.table(split_file_path, header = FALSE, sep = "")
    file.rename(split_file_path, file.path(split_results_dir, basename(split_file_path)))
    colnames(split_df)[1] <- "itno"
    split_df$itno <- split_df$itno + starting_iterations[i] - 1
    combined_df <- dplyr::bind_rows(combined_df, split_df)
  }
  
  if (nrow(combined_df) == 0) {
    if (!file.exists(output_path)) {
      file.create(output_path)
      warning("No data found in the split files, created empty file at ", output_path)
    } 
    
    else {
      warning("No data found in the split files, existing file at ", output_path, " will be left as is")
    }
  } else if (nrow(combined_df) > 0) {
    
    # Write table but remove the headers:
    write.table(combined_df, 
                file = output_path,
                row.names = FALSE,
                col.names = FALSE,
                sep = " ", quote = FALSE)
  }
}


# Summary:
# This function splits an .ini file, runs simulations in parallel, and combines the results.
# It takes file paths, output types, and other configuration details as arguments.
isa.parallel <- function(ini_file_path, 
                               ISP_folder_root = "C:/ISP/",
                               total_iterations,
                               ncores = NA,
                               output_filepaths = c("general"= "general", 
                                                    # Change to match the file structure of y
                                                    "general_structure" =  "results/SC01/results%%type%%_SC01.txt")) {
  
  # Extract scenario_name from ini_file_path:
  scenario_name <- basename(ini_file_path)
  
  # Determine the number of cores to use:
  num_cores <- ifelse(is.na(ncores), as.numeric(future::availableCores() - 2), ncores)
  
  # Split the ini file:
  SC_split <- split_and_save_ini(ini_file_path, num_cores, ISP_folder_root, output_filepaths)
  
  # Generate log paths for each ini file:
  log_paths <- sapply(SC_split$ini_paths, function(ini) {
    gsub("/ini/", "/logs/", sub("\\.ini$", ".log", ini))
  })
  
  # Consolidate log paths into a single string:
  consolidated_log_paths <- paste(log_paths, collapse = ";")
  
  # Check and create logs directory if it doesn't exist:
  logs_dir <- file.path(ISP_folder_root, "logs")
  
  if (!dir.exists(logs_dir)) {
    dir.create(logs_dir)
  }
  
  # Initialize the master log file:
  master_log_file <- file.path(logs_dir, "Logs_Master.csv")
  master_log_data <- data.frame(Scenario = scenario_name, LogPaths = consolidated_log_paths)
  
  if (!file.exists(master_log_file)) {
    write.csv(master_log_data, master_log_file, row.names = FALSE)
  } 
  
  else {
    write.table(x = master_log_data, file = master_log_file, row.names = FALSE, append = TRUE)
  }
  
  # Run simulations in parallel:
  plan(future::multisession, workers = num_cores)
  
  furrr::future_map(SC_split$ini_paths, run_simulation)
  future::plan(future::sequential)
  
  # Combine results for different output types:
  output_types <- c("inf_result", 
                    "det_result", 
                    "vac_result")
  
  for (type in output_types) {
    type_short = strsplit(type, "_")[[1]][1]
    
    adjust_and_combine_splits(SC_split$split_info, 
                              type,
                              output_path = paste0(ISP_folder_root, 
                                                  stringr::str_replace_all(output_filepaths["general_structure"], 
                                                                  "%%type%%", 
                                                                  type_short)
                                                  )
                              )
  }
}


# Simulation Processing Repository

# Overview
# These functions to help with the parallel processing of epidemiological simulations using Interspread Plus.

# Functions
# - `split_and_save_ini1`: Splits an .ini file for parallel simulation runs.
# - `run_simulation`: Executes a simulation using a given .ini file.
# - `adjust_and_combine_splits`: Combines results from simulations across multiple splits.
# - `process_simulation`: Handles the entire process flow, from splitting the .ini file to combining the outputs.
# 
# Getting Started.
# Ensure Interspread Plus is installed and properly set up in your PATH before using these functions.

# Usage.
# To process a simulation, use the `process_simulation` function with the appropriate arguments:

# Example usage of the function:
# process_simulation(
#   # Change to match the path of your .ini file:
#   ini_file_path = "C:/ISP/ini/SC14.ini", 
#   
#   # Change to match the root folder of your ISP installation:
#   ISP_folder_root = "C:/ISP/", 
#   
#   output_filepaths = c("general"= "general", 
# 
#    # Change to match the file structure of your results files and folders
#    # so if the root folder is "C:/ISP/"
#    # and the infection result file is in "C:/ISP/results/SC13/" with the file name
#    # "ID_NTB_inf_SC13.txt" then the general structure should be 
#    # "results/SC13/ID_NTB%%type%%_SC13.txt".
#    
#    # Whereas if the infection result file is in "C:/ISP/results/ID_NTB_inf.txt"
#    # then the general structure should be "results/ID_NTB_%%type%%.txt"
#   "general_structure" =  "results/SC14/ID_NTB_%%type%%_SC14.txt")
# )

