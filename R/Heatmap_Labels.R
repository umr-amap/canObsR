#' Plot the heatmap of Labels
#'
#' @description A fct function
#'
#'
#' @param Labels tbl_df.
#' @param Species character. Specifying the species you want to filter
#' @param Genus character. Specifying the genus you want to filter
#' @param Family character. Specifying the family(ies) you want to filter
#' @param title character. The title of the plot
#' @param simplify logical. When TRUE, the plot will use simplified labels instead of raw labels.
#' @param repro logical. When TRUE, the flowers and fruits observations will be add to the plot.
#' @return ggplot
#'
#'
#' @export
#' @import stringr
#' @import dplyr
#' @import ggplot2


heatmap_Labels <-

   function(Labels,
            Species = NULL,
            Genus = NULL,
            Family = NULL,
            title = NULL,
            simplify = FALSE,
            repro = FALSE
   ){


      Labels <- Labels %>%
         { if (!is.null(Species)) dplyr::filter (., species == Species) else . } %>%
         { if (!is.null(Genus)) dplyr::filter (., genus == Genus) else . } %>%
         { if (!is.null(Family)) dplyr::filter (., family == Family) else . }

      if (nrow(Labels) == 0) {
         stop("This taxa does not exists in the data")
      }

      Labels <- Labels %>%
         mutate(phenophase = case_when(phenophase == 'NA' ~ NA, TRUE ~ phenophase)) %>%
         dplyr::group_by(id) %>%
         mutate(na = case_when(length(unique(phenophase)) == 1 &
                                                NA %in% (unique(phenophase)) ~ TRUE, TRUE ~ FALSE)) %>%
         dplyr::filter(na == FALSE) %>%
         ungroup()

      if (nrow(Labels) == 0) {
         stop("No label for this taxa")
      }

      ncol = 1

      if(!simplify) {

         x <- str_split(Labels$phenophase, '\\;', simplify = T)

         Labels$phenophase <- x[,1]
         if (ncol(x) == 2) { Labels$repro <- x[,2] }
         ncol = ncol(x)

      }else{

         Labels <-
            Labels %>% mutate(
               repro = case_when(
                  PPFlo == 1  ~ 'fl',
                  PPFr == 1 ~ 'fr',
                  TRUE ~ NA
               )
            )
      }


      Labels$id <- as.character(Labels$id)
      Labels$phenophase <- as.factor(Labels$phenophase)
      Labels$date <- as.factor(Labels$date)


      if(is.null(title)) {
         title <- paste(Family, Genus, Species)
      }


     ggplot( Labels, aes(date, id)) +

         {if (!simplify)   geom_tile(aes( fill = phenophase))} +
         {if (simplify)   geom_tile(aes( fill = PPfoliar1))} +

         {if (!simplify & ncol == 2)   geom_point ( aes(date, id, shape = repro, color = repro), size = 2 )} +
         {if (!simplify & ncol == 2)    scale_size(guide = 'none') } +

         {if (simplify)   geom_point ( aes(date, id, shape = repro, color = repro), size = 2 )} +
         {if (simplify)    scale_size(guide = 'none') } +

        scale_fill_manual ( values = color_label ) +
        scale_color_manual ( values = color_pheno ) +
        scale_shape_manual ( values = shape_pheno ) +

        ggtitle(title) +

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
