#' Create xlsx labeling file
#'
#' @description A function to create the xlsx file where to encode the phenophase.
#'
#' @param path_crowns  chr. Path to the crown delinetion shapefile
#' @param site chr. site name (p.e "Bouamir").
#' @param dates chr. vector of dates (format should be '%Y-%m-%d' or '%Y%m%d' or '%Y_%m_%d').
#' @param out_dir_path chr. The path to the directory used to store the images. By defaut it is NULL,
#' the data will not be saved but will be return as tibble. If it is not NULL, an xlsx file will be saved.
#' @return A tibble with the variable site, id, family, genus, species, n, obs, update, date, phenophase and comments.
#' where n, obs, update, phenophase and comments and comments will be NULL. This tibble can be used in the `shiny_labels()` applications to be filled.
#'
#'
#' @export
#'
#' @import dplyr
#' @import stringr
#' @importFrom openxlsx write.xlsx
#' @importFrom magrittr "%>%"

create_labelingFile <- function(
      path_crowns,
      site,
      dates,
      out_dir_path = NULL
   ){



# Check dates format ------------------------------------------------------

   if (unique(!is.na(as.Date(unique(dates), "%Y-%m-%d")))){

      dates <- str_replace_all(dates,'-','_')
   }

   if (unique(!is.na(as.Date(unique(dates), "%Y%m%d")))){

      dates <- paste(str_sub(dates,1,4),str_sub(dates,5,6),str_sub(dates,7,8),sep='_')
   }

   if (unique(is.na(as.Date(unique(dates), "%Y_%m_%d")))){

      stop(paste("Format of dates should be '%Y-%m-%d' or '%Y%m%d' or '%Y_%m_%d'"))
   }

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
         mutate(phenophase = NA, comments = NA, update = NA, obs = NA, Usable_crown = NA) %>%
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


         if(!is.null(out_dir_path) ){


            write.xlsx(labeling_file, file = file.path(path,paste(site,'labelingFile',paste0(format(as.Date(Sys.Date(),format="%Y-%m-%d"), format = "%Y%m%d"), '.xlsx'), sep = '_' )))

            print(paste('File has been written :',file.path(path,paste(site,'labelingFile',paste0(format(as.Date(Sys.Date(),format="%Y-%m-%d"), format = "%Y%m%d"), '.xlsx'), sep = '_' ))))

         }

      }

# return tibble -----------------------------------------------------------

      return(labeling_file)

   }

