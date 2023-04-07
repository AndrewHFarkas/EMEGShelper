
close_then_stop <- function(error_message, open_file_connection) {

  close(open_file_connection)

  stop(error_message)

}

find_files_paths <- function(path,
                             pattern,
                             open_file_connection) {

  if (missing(path) ||
      missing(pattern) ||
      missing(open_file_connection)) {
    close_then_stop("all three arguments are necessary",
                    open_file_connection)
  }

  lines <- character()

  files <- dir(path, pattern = pattern)

  number_of_files <- length(files)

  if (number_of_files == 0) return()

  for (file_index in 1:number_of_files) {

    path_to_file <- paste0(path,
                           "/",
                           files[file_index])

    elements_of_path <- stringr::str_split(path_to_file,
                                           "/")

    elements_of_path <- unlist(elements_of_path)

    elements_with_slash <- paste0(elements_of_path,
                                  "\\")

    collapsed_elements <- paste(elements_with_slash,
                                collapse = "")

    collapsed_elements <- substr(collapsed_elements,
                                 start = 1,
                                 stop = nchar(path_to_file))

    lines <- c(lines, collapsed_elements)



  }

  lines

}

mac_find_files_paths <- function(path,
                                 pattern,
                                 open_file_connection) {

  if (missing(path) ||
      missing(pattern) ||
      missing(open_file_connection)) {
    close_then_stop("all three arguments are necessary",
                    open_file_connection)
  }

  lines <- character()

  files <- dir(path, pattern = pattern)

  number_of_files <- length(files)

  if (number_of_files == 0) return()

  for (file_index in 1:number_of_files) {

    path_to_file <- paste0(path,
                           "/",
                           files[file_index])


    lines <- c(lines, path_to_file)

  }

  lines

}


#' Create an EMEGS batchfile for a windows directory system
#'
#' This function uses r regex patterns to find files and writes
#' batch text files which EMEGS can read. It is capable of searching
#' in subfolders. This is for the windows directory system. A mac
#' version is called mac_emegs_batchfile_maker.
#'
#' @param patterns 1 or more patterns to find files
#' @param batch_path Where the batch text file will be written, must
#' include filename with .txt ending if specifying path
#' @param parent_path Path to where the function will start searching
#' @param search_subfolders Argument to specify if the function should search
#' folders in the parent_path
#' @param search_only_subfolders Argument to specify if the files in the parent
#' folder should not be searched
#' @param patterns_exclude not currently in use
#' @return writes a batch text file in batch_path
#'
#' @author Andrew H Farkas, \email{andrewhfarkas at g mail dot com}
#'
#' @export
windows_emegs_batchfile_maker <- function(patterns,
                                          batch_path,
                                          parent_path,
                                          search_subfolders = F,
                                          search_only_subfolders = F) {

  if (missing('patterns')) stop("specify at least one string pattern to match")

  if (missing('batch_path')) {

    batch_path <- "default_batch.txt"

  } else if (!substr(batch_path,
                     start = nchar(batch_path) - 3,
                     stop = nchar(batch_path)) == '.txt') {

    stop("You possibly provided a path without a text filename, batch_path should end with .txt")

  }

  if (missing('parent_path')) parent_path <- getwd()

  #start writing batchfile

  lines <- character()

  file_connection <- file(batch_path)

  #for each pattern

  number_of_patterns <- length(patterns)


  for (pattern_index in 1:number_of_patterns) {

    current_pattern <- patterns[pattern_index]

    #search in parent directory

    if (!search_only_subfolders) {

      lines_to_add <- find_files_paths(parent_path,
                                       pattern = current_pattern,
                                       open_file_connection = file_connection)

      lines <- c(lines, lines_to_add)

    }

    #search the subfolders

    if (search_subfolders) {

      dirs <- list.dirs(path = parent_path,
                        full.names = F)

      dirs <- tail(dirs, -1)

      number_of_dirs <- length(dirs)

      for (dir_index in 1:number_of_dirs) {

        current_dir <- dirs[dir_index]

        current_path <- paste0(parent_path,
                               "/",
                               current_dir)

        lines_to_add <- find_files_paths(current_path,
                                         pattern = current_pattern,
                                         open_file_connection = file_connection)

        lines <- c(lines, lines_to_add)

      }
    }
  }


  writeLines(lines, file_connection)
  close(file_connection)
}


#' Create an EMEGS batchfile for a Mac directory system
#'
#' This function uses r regex patterns to find files and writes
#' batch text files which EMEGS can read. It is capable of searching
#' in subfolders. This is for the mac directory system. A windows
#' version is called mac_emegs_batchfile_maker.
#'
#' @param patterns 1 or more patterns to find files
#' @param batch_path Where the batch text file will be written, must
#' include filename with .txt ending if specifying path
#' @param parent_path Path to where the function will start searching
#' @param search_subfolders Argument to specify if the function should search
#' folders in the parent_path
#' @param search_only_subfolders Argument to specify if the files in the parent
#' folder should not be searched
#' @param patterns_exclude not currently in use
#' @return writes a batch text file in batch_path
#'
#' @author Andrew H Farkas, \email{andrewhfarkas at g mail dot com}
#'
#' @export
mac_emegs_batchfile_maker <- function(patterns,
                                      batch_path,
                                      parent_path,
                                      search_subfolders = F,
                                      search_only_subfolders = F) {

  if (missing('patterns')) stop("specify at least one string pattern to match")

  if (missing('batch_path')) {

    batch_path <- "default_batch.txt"

  } else if (!substr(batch_path,
                     start = nchar(batch_path) - 3,
                     stop = nchar(batch_path)) == '.txt') {

    stop("You possibly provided a path without a text filename, batch_path should end with .txt")

  }

  if (missing('parent_path')) parent_path <- getwd()

  #start writing batchfile

  lines <- character()

  file_connection <- file(batch_path)

  #for each pattern

  number_of_patterns <- length(patterns)


  for (pattern_index in 1:number_of_patterns) {

    current_pattern <- patterns[pattern_index]

    #search in parent directory

    if (!search_only_subfolders) {

      lines_to_add <- mac_find_files_paths(parent_path,
                                           pattern = current_pattern,
                                           open_file_connection = file_connection)

      lines <- c(lines, lines_to_add)

    }

    #search the subfolders

    if (search_subfolders) {

      dirs <- list.dirs(path = parent_path,
                        full.names = F)

      dirs <- tail(dirs, -1)

      number_of_dirs <- length(dirs)

      for (dir_index in 1:number_of_dirs) {

        current_dir <- dirs[dir_index]

        current_path <- paste0(parent_path,
                               "/",
                               current_dir)

        lines_to_add <- mac_find_files_paths(current_path,
                                             pattern = current_pattern,
                                             open_file_connection = file_connection)

        lines <- c(lines, lines_to_add)

      }
    }
  }


  writeLines(lines, file_connection)
  close(file_connection)
}

#' Read ERP data from one AR file
#'
#' This function reads data from one AR file that come from EMEGS.
#'
#' @param path_to_ar The path to the AR file
#' @param extract_channels Choose which channels to extract, must know numeric
#' index (can be found in Emegs2d)
#' @param baseline_pts Select baseline points to baseline the channels as they
#' read in. Ex: c(0:50) which is the first 51 data points
#' @param select_time_points Select time points to keep. Works after baselining. You select sample
#' points not in millisecond. You will need to know your samplerate and scene onset
#' @param average_timepoints Logical argument if selected time points should be averaged
#' @param average_channels Logical argument to average selected channels together
#' @param include_channel_name Logical argument for whether to put channel names in output dataframe
#' @param include_path_name Logical argument for whether to put path name in output dataframe
#' @param include_file_name Logical argument for whether to put file name in output dataframe
#' @return Returns a dataframe of the ERP data extracted from an AR file
#'
#' @author Andrew H Farkas, \email{andrewhfarkas at g mail dot com}
#'
#' @export
read_ar_file <- function(path_to_ar = NULL,
                         extract_channels = NULL,
                         baseline_pts = NULL,
                         select_time_points = NULL,
                         average_timepoints = F,
                         average_channels = F,
                         include_channel_name = F,
                         include_path_name = F,
                         include_file_name = F) {

  reticulate::py_run_string("import numpy as np")
  reticulate::py_run_string("import array")
  reticulate::py_run_string("import struct")

  AR_file <<- path_to_ar

  #grab version number
  reticulate::py_run_string("version = struct.unpack_from(\">7sh\", open(r.AR_file,\"rb\").read())")
  reticulate::py_run_string("version_num = version[1]")
  version_num <- reticulate::py$version_num

  if (version_num == 8) {

    # AR file starts with some information about the file, here we create keys for the information that will be distracted
    reticulate::py_run_string("keys = \"VStr vNum EegMeg nChan_extra trigPoint dType num_elec data_pts\".split()")
    # unpack string says unpack a string of 7 characters, followed by a short int followed by 6 float32
    reticulate::py_run_string("ERP_dict = dict(zip(keys,struct.unpack_from(\">7sh6f\",open(r.AR_file,\"rb\").read())))")
    # create new string to tell python how many data points there are to extract based on info from the AR file
    reticulate::py_run_string("avgmat_length_str ='>7sh6f' + str(int(ERP_dict[\"num_elec\"]*ERP_dict[\"data_pts\"])) + 'f'")
    # read the AR file and extract AR data
    reticulate::py_run_string("fid = open(r.AR_file,\"rb\").read()")
    reticulate::py_run_string("all_dat = struct.unpack_from(avgmat_length_str, fid)")
    reticulate::py_run_string("avg_mat = all_dat[8:]")

    #get number of electrodes
    reticulate::py_run_string("ar_info = struct.unpack_from(\">7sh6f\", open(r.AR_file,\"rb\").read())")
    reticulate::py_run_string("electrode_num = ar_info[6]")
    electrode_num <- reticulate::py$electrode_num
  }

  if (version_num == 9){

    # AR file starts with some information about the file, here we create keys for the information that will be distracted
    reticulate::py_run_string("keys = \"VStr vNum EegMeg nChan_extra trigPoint dType unknown_1 unknown_2 num_elec data_pts\".split()")
    # unpack string says unpack a string of 7 characters, followed by a short int followed by 8 float32
    reticulate::py_run_string("ERP_dict = dict(zip(keys,struct.unpack_from(\">7sh8f\",open(r.AR_file,\"rb\").read())))")
    # create new string to tell python how many data points there are to extract based on info from the AR file
    reticulate::py_run_string("avgmat_length_str ='>7sh8f' + str(int(ERP_dict[\"num_elec\"]*ERP_dict[\"data_pts\"])) + 'f'")
    # read the AR file and extract AR data
    reticulate::py_run_string("fid = open(r.AR_file,\"rb\").read()")
    reticulate::py_run_string("all_dat = struct.unpack_from(avgmat_length_str, fid)")
    reticulate::py_run_string("avg_mat = all_dat[10:]")

    #get number of electrodes
    reticulate::py_run_string("ar_info = struct.unpack_from(\">7sh8f\", open(r.AR_file,\"rb\").read())")
    reticulate::py_run_string("electrode_num = ar_info[8]")
    electrode_num <- reticulate::py$electrode_num

  }
  if (!(version_num %in% c(8,9))) {
    stop("Something went wrong, check that an appropriate AR file is used")
  }

  avg_mat <- reticulate::py$avg_mat

  avg_mat <- as.data.frame(matrix(unlist(avg_mat), nrow = electrode_num))

  # extract channels
  if (is.numeric(extract_channels)){
    avg_mat <- avg_mat[extract_channels,]
  }

  # baseline channels

  if (is.numeric(baseline_pts)) {
    baseline_vec <- rowMeans(avg_mat[, baseline_pts])
    avg_mat <- avg_mat - baseline_vec
  }

  # select timepoints

  if (!is.null(select_time_points)) {

    avg_mat <- avg_mat[, select_time_points]

  }

  #average time points

  if(average_timepoints){

    avg_mat <- data.frame("averaged_time_points" = rowMeans(avg_mat))

  }

  # average channels

  if (average_channels) {

    avg_mat <- t(data.frame("avg_of_channels" = colMeans(avg_mat)))

  }

  #include channel name

  if(include_channel_name){

    if (is.null(extract_channels)) {

      extract_channels <- 1:nrow(avg_mat)

    }

    avg_mat <- cbind.data.frame(extract_channels, avg_mat)

    colnames(avg_mat)[1] <- "channel_names"

  }

  # include path name

  if(include_path_name){

    rep_path_name  <- rep(path_to_ar, nrow(avg_mat))

    avg_mat <- cbind.data.frame(rep_path_name, avg_mat)

    colnames(avg_mat)[1] <- "path_name"

  }


  # include_file_name

  if(include_file_name) {


    file_name <- basename(path_to_ar)



    rep_file_name  <- rep(file_name, nrow(avg_mat))

    avg_mat <- cbind.data.frame(rep_file_name, avg_mat)

    colnames(avg_mat)[1] <- "file_name"



  }

  avg_mat

}

#' Read ERP data from multiple AR files
#'
#' This function reads data from AR files that come from EMEGS.
#'
#' @param data_folders select multiple data folders to search
#' @param patterns One or more patterns to find files
#' @param search_subfolder Logical argument to specify if subfolder should be
#' searched
#' @param search_only_subfolders Argument to specify if the files in the parent
#' folder should not be searched
#' @param extract_channels Choose which channels to extract, must know numeric
#' index (can be found in Emegs2d)
#' @param baseline_pts Select baseline points to baseline the channels as they
#' read in. Ex: c(0:50) which is the first 51 data points
#' @param select_time_points Select time points to keep. Works after baselining. You select sample
#' points not in millisecond. You will need to know your samplerate and scene onset
#' @param average_timepoints Logical argument if selected time points should be averaged
#' @param average_channels Logical argument to average selected channels together
#' @param include_channel_name Logical argument for whether to put channel names in output dataframe
#' @param include_path_name Logical argument for whether to put path name in output dataframe
#' @param include_file_name Logical argument for whether to put file name in output dataframe
#' @return Returns a dataframe of the ERP data extracted from AR files
#'
#' @author Andrew H Farkas, \email{andrewhfarkas at g mail dot com}
#'
#' @export
read_ar_files <- function(data_folders = NULL,
                          patterns = NULL,
                          search_subfolders = F,
                          search_only_subfolders = F,
                          extract_channels = NULL,
                          baseline_pts = NULL,
                          select_time_points = NULL,
                          average_timepoints = F,
                          average_channels = F,
                          include_channel_name = F,
                          include_path_name = F,
                          include_file_name = F) {


  if (is.null(data_folders)) {
    stop("must give at least one path to a data folder")
  }
  if (is.null(patterns)) {
    stop("must give at least one patterns argument to search for files (ex: 'ar')")
  }


  file_paths <- character()

  number_of_folders <- length(data_folders)

  for (folder_index in 1:number_of_folders) {

    current_folder <- data_folders[folder_index]

    files_in_current_folder <- character()

    if (!search_only_subfolders) {

      if (is.character(patterns)) {

        number_of_patterns <- length(patterns)

        for (pattern_index in 1:number_of_patterns) {

          current_pattern <- patterns[pattern_index]

          files_in_current_folder <- dir(current_folder,
                                         pattern = current_pattern)

          current_paths <- paste0(current_folder,
                                  "/",
                                  files_in_current_folder)

          file_paths <- c(file_paths,
                          current_paths)

        }


      } else {

        files_in_current_folder <- dir(current_folder)

        current_paths <- paste0(current_folder,
                                "/",
                                files_in_current_folder)

        file_paths <- c(file_paths,
                        current_paths)
      }

      if (search_subfolders) {

        dirs <- list.dirs(path = current_folder,
                          full.names = F)

        dirs <- tail(dirs, -1)

        number_of_dirs <- length(dirs)

        for (dir_index in 1:number_of_dirs) {

          files_in_current_subfolder <- character()

          current_dir <- dirs[dir_index]

          current_subfolder <- paste0(current_folder,
                                      "/",
                                      current_dir)

          if (is.character(patterns)) {

            number_of_patterns <- length(patterns)

            for (pattern_index in 1:number_of_patterns) {

              current_pattern <- patterns[pattern_index]

              files_in_current_subfolder <- dir(current_subfolder,
                                                pattern = current_pattern)

              current_paths <- paste0(current_subfolder,
                                      "/",
                                      files_in_current_subfolder)

              file_paths <- c(file_paths,
                              current_paths)

            }


          } else {

            files_in_current_subfolder <- dir(current_subfolder)

            current_paths <- paste0(current_subfolder,
                                    "/",
                                    files_in_current_subfolder)

            file_paths <- c(file_paths,
                            current_paths)
          }
        }
      }
    }
  }

  example_data_table <- data.table::data.table(
    read_ar_file(path_to_ar = file_paths[1]))

  number_of_channels <- nrow(example_data_table)

  number_of_data_points <- ncol(example_data_table)

  number_of_paths <- length(file_paths)

  number_of_total_rows <- number_of_paths*number_of_channels

  # Create a list of empty numeric vectors
  data_point_columns_empty <- lapply(1:number_of_data_points,
                          function(x) vector(mode = "numeric",
                                             length = number_of_total_rows))

  # Name the elements of the list V1 to end
  names(data_point_columns_empty) <- paste0("V", 1:number_of_data_points)

  next_example_data_table <- data.table::data.table(
    read_ar_file(path_to_ar = file_paths[1],
                 extract_channels = extract_channels,
                 baseline_pts = baseline_pts,
                 select_time_points = select_time_points,
                 average_timepoints = average_timepoints,
                 average_channels = average_channels,
                 include_channel_name = include_channel_name,
                 include_path_name = include_path_name,
                 include_file_name = include_file_name))

  information_columns_to_add <-
    next_example_data_table[,1:(ncol(next_example_data_table)-number_of_data_points)]

  info_col_names <- colnames(information_columns_to_add)
  info_col_types <- sapply(information_columns_to_add, class)

  info_vectors_list_empty <- list()

  for (i in seq_along(info_col_names)) {
    # Create an empty vector of the same data type
    empty_vector <- vector(mode = info_col_types[i], length = number_of_total_rows)

    # Assign the empty vector to the list and name it based on the column name
    info_vectors_list_empty[[info_col_names[i]]] <- empty_vector
  }


  multiple_ar_files <- data.table::setDT(c(info_vectors_list_empty,
                                         data_point_columns_empty))

  start_row_index <- 1
  end_row_index <- number_of_channels

  for (path_index in 1:number_of_paths) {

    current_path <- file_paths[path_index]

    current_df <- read_ar_file(path_to_ar = current_path,
                               extract_channels = extract_channels,
                               baseline_pts = baseline_pts,
                               select_time_points = select_time_points,
                               average_timepoints = average_timepoints,
                               average_channels = average_channels,
                               include_channel_name = include_channel_name,
                               include_path_name = include_path_name,
                               include_file_name = include_file_name)


    multiple_ar_files <- data.table::set(multiple_ar_files,
                             i = start_row_index:end_row_index,
                             j = c(1:length(current_df)),
                             current_df)

    start_row_index <- start_row_index + number_of_channels
    end_row_index <- end_row_index + number_of_channels

  }

  data.frame(multiple_ar_files)

}

#' Channel information for the 64 sensor Biosemi cap
#'
#' This data frame shows the channel names in order as well as each channels
#' Cartesian and Spherical coordinates. You may notice that the channels are
#' slightly lower than the 128 channels or the Oostenveld channels. This is
#' because this is the way the Biosemi the company and an older version of
#' EMEGS thought the channels were located. It is a small difference, but we
#' leaving the older channel positions for this cap such that it is consistent
#' with our already published studies. Cartesian coordinates assume a radius of
#' 1.
#'
#' @format A data frame with 64 rows and 9 variables:
#' \describe{
#'   \item{channel_number}{channel number used in EMEGS and read from AR file}
#'   \item{channel_name}{actual channel name}
#'   \item{channel_name_uppercase}{channel name but all letters are uppercase which can be useful for matching channels as researchers are not always consistent}
#'   \item{X}{channel X coordinate with negative values toward the right ear}
#'   \item{Y}{channel Y coordinate with positive values toward the nose}
#'   \item{Z}{channel Z coordinate with 1 at Cz}
#'   \item{EPosSphere_theta}{angle in radians from Cz north pole to south pole with Cz being 0}
#'   \item{EPosSphere_phi}{angle in radians from right ear (0) toward nose then left ear continuing until back at right ear}
#'   ...
#' }
"biosemi64_channel_info"

#' Channel information for the 128 sensor Biosemi cap
#'
#' This data frame details the channels and their positions for the 128 channel
#' Biosemi cap. These positions are inline with the Oostenveld positions taken
#' from sphere 1 of his blog (https://robertoostenveld.nl/electrode/#aes1994).
#' These positions are slightly different than the 64 Biosemi channel cap with
#' each position being slightly closer together.
#'
#' @format A data frame with 128 rows and 9 variables:
#' \describe{
#'   \item{actiview}{the order in which actiview see the channels}
#'   \item{wired}{the wire connected to each channel}
#'   \item{channel_name}{actual channel name}
#'   \item{channel_name_uppercase}{channel name but all letters are uppercase which can be useful for matching channels as researchers are not always consistent}
#'   \item{X}{channel X coordinate with negative values toward the right ear}
#'   \item{Y}{channel Y coordinate with positive values toward the nose}
#'   \item{Z}{channel Z coordinate with 1 at Cz}
#'   \item{EPosSphere_theta}{angle in radians from Cz north pole to south pole with Cz being 0}
#'   \item{EPosSphere_phi}{angle in radians from right ear (0) toward nose then left ear continuing until back at right ear}
#'   ...
#' }
"biosemi128_channel_info"

#' Channel information for all positions courtesy of Oostenveld's blog
#'
#' This data frame details the channels and their positions for the positions
#' outlined by Dr. Oostenveld. These positions where taken from Oostenveld's
#' sphere 1 off of his blog (https://robertoostenveld.nl/electrode/#aes1994).
#' These positions are slightly different than the 64 Biosemi channel cap with
#' each position being slightly closer together. This was used to find the
#' positions for the 128 biosemi cap
#'
#' @format A data frame with 128 rows and 7 variables:
#' \describe{
#'   \item{channel_name}{actual channel name}
#'   \item{channel_name_uppercase}{channel name but all letters are uppercase which can be useful for matching channels as researchers are not always consistent}
#'   \item{X}{channel X coordinate with negative values toward the right ear}
#'   \item{Y}{channel Y coordinate with positive values toward the nose}
#'   \item{Z}{channel Z coordinate with 1 at Cz}
#'   \item{EPosSphere_theta}{angle in radians from Cz north pole to south pole with Cz being 0}
#'   \item{EPosSphere_phi}{angle in radians from right ear (0) toward nose then left ear continuing until back at right ear}
#'   ...
#' }
"Oostenveld_channel_info"


#' Make changes to ctf MEG marker file and save the original
#'
#' This function saves original marker files used in ctf MEG files (.ds files)
#' new folder called original_marker_files in the parent folder. It then will
#' alter one present in the folder such that triggers can be removed or adjusted.
#'
#' @param folders select at least one or more data folders to search for marker files
#' @param target_triggers at least one or more target triggers
#' @param delete_trigger_numbers numeric vector of which triggers should be removed
#' from marker file
#'
#' @author Andrew H Farkas, \email{andrewhfarkas at g mail dot com}
#'
#' @export
marker_file_editor <- function(folders = NULL,
                               target_triggers = NULL,
                               delete_trigger_numbers = NULL,
                               skip_already_edited = T) {


  if (!is.character(folders) |
      !is.character(target_triggers) |
      !is.numeric(delete_trigger_numbers)) {

    stop()

  }
  if (!(delete_trigger_numbers > 0)) {

    stop()

  }

  # get paths to all marker files

  file_paths <- character()

  for (folder_index in 1:length(folders)) {

    current_folder_path <- folders[folder_index]

    meg_folder <- dir(current_folder_path, pattern = ".ds")

    meg_folder_path <- file.path(current_folder_path, meg_folder)

    current_file_path <- file.path(meg_folder_path,
                                   "MarkerFile.mrk")

    path_to_folder_of_original <- file.path(current_folder_path,
                                            "original_markerfile")

    if (file.exists(path_to_folder_of_original) && skip_already_edited) {
      next
    }

    if (!file.exists(path_to_folder_of_original)) {

      dir.create(path = path_to_folder_of_original)

      file.copy(current_file_path, path_to_folder_of_original)

    }

    file_paths <- c(file_paths, current_file_path)

  }

  # maybe search sub folders

  for (path_index in 1:length(file_paths)) {

    current_marker <- file_paths[path_index]

    marker_lines <- readLines(con = current_marker)

    new_marker_lines <- marker_lines

    for (trigger_index in 1:length(target_triggers)) {

      current_trigger <- target_triggers[trigger_index]

      number_of_samples_row <- which(marker_lines == current_trigger) +10

      first_trial_marker <- which(marker_lines == current_trigger) +13

      number_of_triggers_to_remove <- length(delete_trigger_numbers)

      new_number_of_triggers <- as.numeric(marker_lines[number_of_samples_row]) -
        number_of_triggers_to_remove

      new_marker_lines[number_of_samples_row] <- as.character(new_number_of_triggers)

      trigger_rows_to_remove <- first_trial_marker + delete_trigger_numbers -1

      new_marker_lines <- new_marker_lines[-trigger_rows_to_remove]

      writeLines(text = new_marker_lines, con = current_marker)

    }


  }

}

#' Replaces marker file with the original
#'
#' This function replaced the edited marker file with the original
#'
#' @param folders select at least one or more data folders to search for marker files
#'
#' @author Andrew H Farkas, \email{andrewhfarkas at g mail dot com}
#'
#' @export
get_original_markers <- function(folders = NULL) {

  for (folder_index in 1:length(folders)) {

    current_folder_path <- folders[folder_index]

    meg_folder <- dir(current_folder_path, pattern = ".ds")

    meg_folder_path <- file.path(current_folder_path, meg_folder)

    current_file_path <- file.path(meg_folder_path,
                                   "MarkerFile.mrk")

    path_to_folder_of_original <- file.path(current_folder_path,
                                            "original_markerfile")

    path_to_original_marker_file <- file.path(current_folder_path,
                                              "original_markerfile",
                                              "MarkerFile.mrk")

    if (!file.exists(path_to_original_marker_file)) {
      next
    }

    file.copy(path_to_original_marker_file,
              current_file_path,
              overwrite = T)
  }
}

#' Turns radians into degrees
#'
#' Simple function that turns radians to degrees.
#'
#' @param radians the radians number to be turned to degrees
#'
#' @author Andrew H Farkas, \email{andrewhfarkas at g mail dot com}
#'
#' @export
rad2deg <- function(radians) {(radians * 180) / (pi)}


#' Turns degrees into radians
#'
#' Simple function that turns degrees to radians.
#'
#' @param degrees the degrees number to be turned to degrees
#'
#' @author Andrew H Farkas, \email{andrewhfarkas at g mail dot com}
#'
#' @export
deg2rad <- function(degrees) {(degrees * pi) / (180)}

#' Finds theta in radians with a Z coordinate
#'
#' Takes Cartesian Z coordinate and returns theta angle in radians which is 0
#' at Cz which is a Z coordinate of 1
#'
#'
#' @param z_coor z coordinate between -1 and 1
#'
#' @author Andrew H Farkas, \email{andrewhfarkas at g mail dot com}
#'
#' @export
cart2theta_radians <- function(z_coor) {

  if ((-1 > z_coor || z_coor > 1)) {
    stop("z_coor needs to be between -1 to 1")
  }

  theta_radians <- acos(z_coor)

  names(theta_radians) <- "theta_radians"

  theta_radians
}

#' Finds theta in degrees with a Z coordinate
#'
#' Takes Cartesian Z coordinate and returns theta angle in degrees which is 0
#' at Cz which is a Z coordinate of 1
#'
#'
#' @param z_coor z coordinate between -1 and 1
#'
#' @author Andrew H Farkas, \email{andrewhfarkas at g mail dot com}
#'
#' @export
cart2theta_degrees <- function(z_coor){

  theta_radians <- cart2theta_radians(z_coor)

  theta_degrees <- rad2deg(theta_radians)

  names(theta_degrees) <- "theta_degrees"

  theta_degrees
}

#' Finds phi in radians with a X and Y coordinates
#'
#' Takes Cartesian X and Y coordinates and returns phi angle in radians which is 0
#' at the right ear or an X at -1 and Y at 0.
#'
#'
#' @param x_coor x coordinate between -1 and 1
#' @param y_coor y coordinate between -1 and 1
#'
#' @author Andrew H Farkas, \email{andrewhfarkas at g mail dot com}
#'
#' @export
cart2phi_radians <- function(x_coor, y_coor) {

  phi_radians <- atan2(y_coor, -x_coor)

  negative_phi <- phi_radians < 0

  phi_radians[negative_phi] <- abs((2*pi) + phi_radians[negative_phi])

  names(phi_radians) <- "phi_radians"

  phi_radians

}


#' Finds phi in degrees with a X and Y coordinates
#'
#' Takes Cartesian X and Y coordinates and returns phi angle in degrees which is 0
#' at the right ear or an X at -1 and Y at 0.
#'
#'
#' @param x_coor x coordinate between -1 and 1
#' @param y_coor y coordinate between -1 and 1
#'
#' @author Andrew H Farkas, \email{andrewhfarkas at g mail dot com}
#'
#' @export
cart2phi_degrees <- function(x_coor, y_coor) {

  phi_radians <- cart2phi_radians(x_coor, y_coor)

  phi_degrees <- rad2deg(phi_radians)

  names(phi_degrees) <- "phi_degrees"

  phi_degrees
}

cart_coor_check <- function(x_coor, y_coor, z_coor) {

  if (-1 > x_coor || -1 > y_coor || -1 >  z_coor) {
    stop("each coordinate has to be within -1 and 1")
  }

  if (1 < x_coor || 1 < y_coor || 1 <  z_coor) {
    stop("each coordinate has to be within -1 and 1")
  }

  if (any(((x_coor^2) + (y_coor^2) + (z_coor^2)) > 1.001)) {
    stop("cartesian coordinate is outside of a sphere with a radius of 1")
  }

  if (any(((x_coor^2) + (y_coor^2) + (z_coor^2)) < .95)) {
    warning("cartesian coordinate is likely not on the sphere")
  }

}

#' Turns Cartesian coordinates into polar coordinates
#'
#' This function takes Cartesian coordinates and turns them into polar/geographic
#' coordinates. Finding these positions are necessary for creating ecfg configuration files for EMEGS
#' such that the software knows its three dimensional location on a sphere. Assumes
#' radius of one, X is right ear (negative) to left ear (positive), Y positive toward nose
#' , and Z is 1 at Cz. Spherical coordinates are returned in radians and degrees. Theta is 0 at Cz
#' pi at other pole. Phi is zero at right ear, pi/2 (90°) at nose, pi (180°) at left ear, 3/2*pi (270°) at
#' Oz.
#'
#' @param x_coor x coordinate between -1 and 1
#' @param y_coor y coordinate between -1 and 1
#' @param z_coor z coordinate between -1 and 1
#' @return Returns a named vector with theta and phi in degrees and radians
#'
#' @author Andrew H Farkas, \email{andrewhfarkas at g mail dot com}
#'
#' @export
cart2polar <- function(x_coor, y_coor, z_coor) {

  cart_coor_check(x_coor, y_coor, z_coor)

  theta_radians  <- cart2theta_radians(z_coor)
  theta_degrees  <- cart2theta_degrees(z_coor)
  phi_radians    <- cart2phi_radians(x_coor, y_coor)
  phi_degrees    <- cart2phi_degrees(x_coor, y_coor)

  polar_coor_vec <- c(theta_radians, theta_degrees,
                      phi_radians,   phi_degrees)

  polar_coor_vec
}

#' Turns polar coordinates (EPosSphere) into Cartesian coordinates
#'
#' This function takes polar coordinates (EPosSphere in EMEGS) and turns them into
#' Cartesian 3d coordinates. Assumes radius of one, X is right ear (negative) to left ear (positive),
#' Y positive toward nose, and Z is 1 at Cz. Spherical coordinates are returned in radians
#' and degrees. Theta is 0 at Cz pi at other pole. Phi is zero at right ear, pi/2 (90°) at
#' nose, pi (180°) at left ear, 3/2*pi (270°) at Oz.
#'
#' @param theta_radians angle in radians from north pole Cz
#' @param phi_radians angle in radians from right ear to nose
#' @return Returns a named vector with X, Y, and Z coordinates
#'
#' @author Andrew H Farkas, \email{andrewhfarkas at g mail dot com}
#'
#' @export
polar2cart <- function(theta_radians, phi_radians) {

  if (theta_radians < 0 || theta_radians > pi) {
    stop("theta_radians has to be within 0 and pi")
  }

  if (phi_radians < 0 || phi_radians > (2*pi)) {
    stop("phi_radians has to be within 0 and 2*pi")
  }


  z_coor = cos(theta_radians)

  y_coor = sin(theta_radians) * sin(phi_radians)

  x_coor = -(sin(theta_radians) * cos(phi_radians))

  cart_coor_check(x_coor, y_coor, z_coor)

  cart_coors <- c(x_coor, y_coor, z_coor)

  names(cart_coors) <- c("X", "Y", "Z")

  cart_coors
}


#' Finds the standard error
#'
#' A simple function to find standard error. There is different defintions of
#' standard error so beware
#'
#' @param vec a vector of numerical values
#' @param na.rm a logical argument to remove NA values before calculating the
#' standard error
#' @return Returns a numeric standard error value
#'
#' @author Andrew H Farkas, \email{andrewhfarkas at g mail dot com}
#'
#' @export
se <- function(vec, na.rm = FALSE) {
  sd(vec, na.rm)/sqrt(n())
}
