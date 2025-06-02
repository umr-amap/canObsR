#'Create the recommended files architecture
#'
#'@description Create the recommended files architecture.
#'
#'@param main_folder_path Character. The path where to create the main folder
#'@param main_folder_name  Character. The name of the main folder. Must be provided.
#'
#'
#'
#'@export


create_files_architecture <-

   function(
      main_folder_path = getwd(),
      main_folder_name = NULL
      ){

      if (is.null(main_folder_name) || !nzchar(main_folder_name)) {
         stop("Please provide a non-empty 'main_folder_name'.")
      }

      # Create main folder if it does not exist
      if (!dir.exists(main_folder_path)) {
         dir.create(main_folder_path, recursive = TRUE)
         message("Created main folder: ", main_folder_path)
      } else {
         message("Main folder already exists: ", main_folder_path)
      }

      # List of subfolders to create
      subfolders <- c(
         "1_my_drone_data",
         "2_orthomosaics",
         "3_orthomosaics_aligned",
         "4_crowns_img"
      )

      # Create each subfolder if it does not exist
      for (subfolder in subfolders) {
         folder_path <- file.path(main_folder_path, subfolder)
         if (!dir.exists(folder_path)) {
            dir.create(folder_path)
            message("Created folder: ", folder_path)
         } else {
            message("Folder already exists: ", folder_path)
         }
      }

      invisible(NULL)

   }
