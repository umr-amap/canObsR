#' Create xlsx labeling file
#'
#' @description A function to create the xlsx file where to encode the phenophase.
#'
#' @param path_crowns  chr. Path to the crown delinetion shapefile
#' @param site chr. site name (p.e "Bouamir").
#' @param dates chr. vector of dates (format should be 'YYYY_MM_DD', p.e c('2022_09_25','2022_10_10').
#' @param out_dir_path chr. The path to the directory use to stored the images. By defaut it is NULL,
#' the data will not be saved but will be return as tibble.
#' @param file_type chr. Needed only if `out_dir_path` is not NULL. By default it is '.RData' but can be '.csv' or '.xlsx'.

#' @return A tibble with the variable site, id, family, genus, species, n, obs, update, date, phenophase and comments.
#' where n, obs, update, phenophase and comments and comments will be NULL. This tibble can be used in the `shiny_labels()` applications to be filled.
#'
#'
#' @export
#'
#' @import dplyr
#' @importFrom openxlsx write.xlsx
#' @importFrom magrittr "%>%"

create_labelingFile <- function(
      path_crowns,
      site,
      dates,
      out_dir_path = NULL
   ){



# Extract id, species, genus, family from the crowns file ------------------------------------

      crownsFile <-   read_sf(path_crowns) %>%
         group_by(id) %>%
         summarise (species = unique(species), genus = unique(genus), family = unique(family)) %>%
         ungroup() %>%
         as_tibble() %>%
         select(id:family)


# Create dataframe --------------------------------------------------------

      labeling_file <- expand.grid(id = crownsFile$id,
                        date = dates,
                        stringsAsFactors = FALSE) %>%
         mutate(phenophase = '', comments = '', update = '', obs = '', Usable_crown = '') %>%
         left_join(crownsFile) %>%
         group_by(species, genus, family) %>%
         mutate (n = n()/length(dates)) %>%
         arrange(desc(n), species, genus, family, id) %>%
         mutate(site = site) %>%
         ungroup() %>%
         select(site, id, family, genus, species, n, obs, update, everything())



# Save xlsx ---------------------------------------------------------------

      if(!is.null(out_dir_path)){


         # Check path out ----------------------------------------------

         if (!dir.exists(out_dir_path)) {
            stop("The folder does not exist : ", out_dir_path)
         }

         if(!is.null(out_dir_path) & file_type == '.RData'){

            save(labeling_file, file = file.path(out_dir_path, paste(site,'_labelingFile',
                                                                     paste0(format(as.Date(Sys.Date(),format="%Y-%m-%d"), format = "%Y%m%d"), '.RData')
                                                                     , sep = '_' )
            ))

            print(paste('File has been written :',file.path(out_dir_path, paste(site,'_labelingFile',
                                                                                paste0(format(as.Date(Sys.Date(),format="%Y-%m-%d"), format = "%Y%m%d"), '.RData')
                                                                                , sep = '_' )
            )))

         }

         if(!is.null(out_dir_path) & file_type == '.csv'){

            write.csv(results.final, file = file.path(out_dir_path, paste(site,'_labelingFile',
                                                                          paste0(format(as.Date(Sys.Date(),format="%Y-%m-%d"), format = "%Y%m%d"), '.csv')
                                                                          , sep = '_' )
            ))

            print(paste('File has been written :',file.path(out_dir_path, paste(site,'_labelingFile',
                                                                                paste0(format(as.Date(Sys.Date(),format="%Y-%m-%d"), format = "%Y%m%d"), '.csv')
                                                                                , sep = '_' )
            )))

         }

         if(!is.null(out_dir_path) & file_type == '.xlsx'){

            write.xlsx(results.final, path = file.path(out_dir_path, paste(site,'_labelingFile',
                                                                           paste0(format(as.Date(Sys.Date(),format="%Y-%m-%d"), format = "%Y%m%d"), '.xlsx')
                                                                           , sep = '_' )
            ))

            print(paste('File has been written :',file.path(out_dir_path, paste(site,'_labelingFile',
                                                                                paste0(format(as.Date(Sys.Date(),format="%Y-%m-%d"), format = "%Y%m%d"), '.xlsx')
                                                                                , sep = '_' )
            )))

         }

      }

# return tibble -----------------------------------------------------------

      return(labeling_file)

   }

