#' Plot the heatmap of Labels
#'
#' @description A fct function
#'
#'
#' @param longLabels \code{tibble} or \code{dataframe} of long data format for labels
#' @param Species chr. Specifying the species you want to filter
#' @param Genus chr. Specifying the genus you want to filter
#' @param Family chr. Specifying the family(ies) you want to filter
#' @param title chr. The title of the plot
#' @param simplify logical. When TRUE, the plot will use simplified labels instead of raw labels.
#' @param repro logical. When TRUE, the flowers and fruits observations will be add to the plot.
#' @return return a ggplot
#'
#' @examples
#'
#' library(readxl)
#'
#' raw_labels <- read_excel(
#' file.path(system.file(package="managecrownsdata"),
#' 'xlsx/labeling_file_Bouamir.xlsx')
#' )
#'
#' pivotLabels <- pivot_Labels(raw_labels)
#'
#' heatmap_Labels(pivotLabels,
#' Specie = NULL,
#' Genus = 'Desbordesia',
#' Family = NULL,
#' title = NULL)
#'
#' pivotLabels_simplify <- pivot_Labels(raw_labels, simplify_labels = TRUE)
#'
#' heatmap_Labels(pivotLabels,
#' Specie = NULL,
#' Genus = 'Desbordesia',
#' Family = NULL,
#' title = NULL,
#' simplify = TRUE)
#'
#' @export
#' @importFrom stringr str_split
#' @import dplyr
#' @import ggplot2


heatmap_Labels <-

   function(longLabels,
            Species = NULL,
            Genus = NULL,
            Family = NULL,
            title = NULL,
            simplify = FALSE,
            repro = FALSE
   ){

      longLabels <- longLabels %>%
         { if (!is.null(Species)) dplyr::filter (., species == Species) else . } %>%
         { if (!is.null(Genus)) dplyr::filter (., genus == Genus) else . } %>%
         { if (!is.null(Family)) dplyr::filter (., family == Family) else . }


      longLabels <- longLabels %>%
         dplyr::mutate(phenophase = dplyr::case_when(phenophase == 'NA' ~ NA, TRUE ~ phenophase)) %>%
         dplyr::group_by(id) %>%
         dplyr::mutate(na = dplyr::case_when(length(unique(phenophase)) == 1 &
                                                NA %in% (unique(phenophase)) ~ TRUE, TRUE ~ FALSE)) %>%
         dplyr::filter(na == FALSE) %>%
         dplyr::ungroup()

      ncol = 1

      if(!simplify) {

         x <- stringr::str_split(longLabels$phenophase, '\\;', simplify = T)

         longLabels$phenophase <- x[,1]
         if (ncol(x) == 2) { longLabels$repro <- x[,2] }
         ncol = ncol(x)

      }else{

         longLabels <-
            longLabels %>% mutate(
               repro = case_when(
                  PPFlo == 1  ~ 'fl',
                  PPFr == 1 ~ 'fr',
                  TRUE ~ NA
               )
            )
      }


      longLabels$id <- as.character(longLabels$id)
      longLabels$phenophase <- as.factor(longLabels$phenophase)
      longLabels$date <- as.factor(longLabels$date)


      if(is.null(title)) {
         title <- paste(Family, Genus, Species)
      }


      ggplot2::ggplot( longLabels, aes(date, id)) +

         {if (!simplify)    ggplot2::geom_tile(aes( fill = phenophase))} +
         {if (simplify)    ggplot2::geom_tile(aes( fill = PPfoliar1))} +

         {if (!simplify & ncol == 2)    ggplot2::geom_point ( aes(date, id, shape = repro, color = repro), size = 2 )} +
         {if (!simplify & ncol == 2)    scale_size(guide = 'none') } +

         {if (simplify)    ggplot2::geom_point ( aes(date, id, shape = repro, color = repro), size = 2 )} +
         {if (simplify)    scale_size(guide = 'none') } +

         ggplot2::scale_fill_manual ( values = color_label ) +
         ggplot2::scale_color_manual ( values = color_pheno ) +
         ggplot2::scale_shape_manual ( values = shape_pheno ) +

         ggplot2::ggtitle(title) +

         theme(
            panel.background = element_blank(),
            panel.border = element_blank(),
            panel.grid = element_blank(),
            axis.line = element_line(linewidth = 1),
            text = element_text(size = 12),
            axis.text = element_text(size = 10),
            axis.title.x = element_blank(),
            axis.title = element_text(size = 14, colour = "gray25"),
            legend.title = element_text(size = 14),
            legend.text = element_text(size = 14),
            legend.key = element_blank(),
            legend.background = element_rect(colour = 'darkgrey', fill = 'lightgrey', linewidth = 1.1),
            plot.title = element_text(hjust = 0.5, size = 20),
            axis.text.x = element_text(angle = 60, vjust = 0.5, hjust=1))

   }
