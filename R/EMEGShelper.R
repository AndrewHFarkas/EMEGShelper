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


#' Create an EMEGS batchfile for a windows directory system
#'
#' This function uses r regex patterns to find files and writes
#' batch text files which EMEGS can read. It is capable of searching
#' in subfolders. This is for the windows directory system. A mac
#' version (if finished) is called mac_emegs_batchfile_maker.
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
#' @author Andrew H Farkas, \email{andrewhfarkas@gmail.com}
#'
#' @export
windows_emegs_batchfile_maker <- function(patterns,
                                     batch_path,
                                     parent_path,
                                     search_subfolders = F,
                                     search_only_subfolders = F,
                                     patterns_exclude) {

  if (missing('patterns')) stop("specify at least one string pattern to match")

  if (missing('file_name_andor_path')) {

    batch_path <- "default_batch.txt"

  } else if (!substr(batch_path,
                     start = nchar(file_name_andor_path) - 3,
                     stop = nchar(file_name_andor_path)) == '.txt') {

    stop("You possibly provided a path without a text filename, file_name_and_or_path should end with .txt")

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

      dirs <- list.dirs(full.names = F)

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
#' @author Andrew H Farkas, \email{andrewhfarkas@gmail.com}
#'
#' @export
mac_emegs_batchfile_maker <- function(patterns,
                                     batch_path,
                                     parent_path,
                                     search_subfolders = F,
                                     search_only_subfolders = F,
                                     patterns_exclude) {

  if (missing('patterns')) stop("specify at least one string pattern to match")

  if (missing('file_name_andor_path')) {

    batch_path <- "default_batch.txt"

  } else if (!substr(batch_path,
                     start = nchar(file_name_andor_path) - 3,
                     stop = nchar(file_name_andor_path)) == '.txt') {

    stop("You possibly provided a path without a text filename, file_name_and_or_path should end with .txt")

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

      dirs <- list.dirs(full.names = F)

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


