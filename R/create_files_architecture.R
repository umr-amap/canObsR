#'Create the recommended files architecture
#'
#'@description Create the recommended files architecture.
#'
#'@param path chr. The path where to create the main folder
#'@param main_folder_name  chr. The name of the main folder
#'
#'
#'@export


create_files_architecture <-

   function(
      path = NULL,
      main_folder_name = NULL
      ){

      # Create the main folder -----------------------------------------------

      dir.create(paste(path,main_folder_name,sep='/'))

      # Create secondary folders ------------------------------------------

      dir.create(paste(path,main_folder_name,'1_drone_images',sep='/'))
      dir.create(paste(path,main_folder_name,'2_drone_images_ref',sep='/'))
      dir.create(paste(path,main_folder_name,'3_crowns',sep='/'))
      dir.create(paste(path,main_folder_name,'4_Time_SIFT_output',sep='/'))
      dir.create(paste(path,main_folder_name,'5_arosics_output',sep='/'))
      dir.create(paste(path,main_folder_name,'6_bbox',sep='/'))
      dir.create(paste(path,main_folder_name,'7_crownsImages',sep='/'))
      dir.create(paste(path,main_folder_name,'8_labeling_file',sep='/'))
      dir.create(paste(path,main_folder_name,'9_outputs',sep='/'))



   }
