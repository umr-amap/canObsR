#' Plot the heatmap of Labels
#'
#' @description A fct function
#'
#'
#' @param longLabels tibble
#' @param Specie chr
#' @param Genus chr
#' @param Family chr
#' @param title chr
#'
#' @return return a ggplot
#'
#'
#' @export
#' @importFrom stringr str_split
#' @importFrom dplyr filter
#' @import ggplot2


plot_heatmapLabels <-

   function(longLabels, Specie = NULL, Genus = NULL, Family = NULL, title = NULL){

      longLabels <- longLabels %>%
         { if (!is.null(Specie)) dplyr::filter (., specie == Specie) else . } %>%
         { if (!is.null(Genus)) dplyr::filter (., genus == Genus) else . } %>%
         { if (!is.null(Family)) dplyr::filter (., family == Family) else . }

      x <- stringr::str_split(longLabels$phenophase, '\\;', simplify = T)

      longLabels$phenophase <- x[,1]
      if (ncol(x) == 2) { longLabels$repro <- x[,2] }

      longLabels$id <- as.character(longLabels$id)
      longLabels$phenophase <- as.factor(longLabels$phenophase)
      longLabels$date <- as.factor(longLabels$date)

      color <- c("D" = "yellow4",
                 "D?" = "yellow4",
                 "L/D" = "greenyellow",
                 "L/D?" = "greenyellow",
                 'L' = "lightgreen",
                 'D/F' = "orange",
                 'D/F?' = "orange",
                 "F" = "darkgreen",
                 "F?" = "red",
                 "L/F" = "green")

      color_pheno <- c('fl' = "yellow",
                       "fl?" = "brown",
                       "fr" = "pink3")

      shape_pheno <- c('fl' = 8,
                       "fl?" = 17,
                       "fr" = 21)

      if(is.null(title)) {
         title <- paste(Family, Genus, Specie)
      }


      ggplot2::ggplot( longLabels, aes(date, id, fill = phenophase)) +

         ggplot2::geom_tile() +
         {if (ncol(x) == 2)    ggplot2::geom_point ( aes(date, id, shape = repro, color = repro, size = 2) )} +
         {if (ncol(x) == 2)    scale_size(guide = 'none') } +

         ggplot2::scale_fill_manual ( values = color ) +
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
