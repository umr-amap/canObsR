#' Plot the spectral signal over time
#'
#' @description A fct function
#'
#' @param data tbl_df. Labels Labels
#' @param Species character. Specifying the species you want to filter
#' @param Genus character. Specifying the genus you want to filter
#' @param Family character. Specifying the family(ies) you want to filter
#' @param Type character. Specifying the type(s) you want to filter
#' @param Metric character. Specifying the metric(s) you want to filter. By defaut 'mean'
#' @param Band character. Specifying the bande(s) you want to filter
#' @param facet_by character. Facetting by a variable. By defaut 'band'
#' @param slcted_id numeric. Highlighting an id
#' @param show_Labels logical. When TRUE, it shows the phenophase labels on the plot.
#' @param title character. The title of the plot
#'
#' @return return a ggplot
#'
#'
#' @export
#' @import dplyr
#' @import ggplot2
#' @importFrom stats quantile

plot_signal <- function(data,
                       Species = NULL,
                       Genus = NULL,
                       Family = NULL,
                       Type = NULL,
                       Metric = 'mean',
                       Band = NULL,
                       facet_by = 'band',
                       slcted_id = NULL,
                       show_Labels = FALSE,
                       title = NULL
) {


   data <- data %>%
      { if (!is.null(Species)) dplyr::filter (., species %in% Species) else . } %>%
      { if (!is.null(Genus)) dplyr::filter (., genus %in% Genus) else . } %>%
      { if (!is.null(Family)) dplyr::filter (., family %in% Family) else . } %>%
      { if (!is.null(Type)) dplyr::filter (., type %in% Type) else . } %>%
      { if (!is.null(Metric)) dplyr::filter (., metric %in% Metric) else . } %>%
      { if (!is.null(Band)) dplyr::filter (., band %in% Band) else . }


   if(!is.null(slcted_id)) {

      data <- data %>% mutate( highlight = ifelse(id==slcted_id, paste(slcted_id), "Other"))

      data <- data %>%
         group_by(id, band) %>%
         mutate(y_lvl = (quantile(value, na.rm = T, probs = 0.1) )) %>%
         ungroup()
   }


   ggplot (data) +

      {if ( is.null(slcted_id) & !is.null(facet_by))   geom_line (data = data, aes (x = date, y = value, group = id, colour = band) ) } +

      {if ( is.null(slcted_id) &  is.null(facet_by) )   geom_line (data = data, aes (x = date, y = value, group = id) ) } +


      {if ( !is.null(slcted_id) &  is.null(facet_by) )
         geom_line (data = dplyr::filter(data, highlight != paste(slcted_id) ),
                             aes(x = date, y = value, group = id),
                             colour = "lightgrey",
                             linewidth = 1)
      } +

      {if ( !is.null(slcted_id) &  !is.null(facet_by) )
         geom_line (data = dplyr::filter(data, highlight != paste(slcted_id) ),
                             aes(x = date, y = value, group = id),
                             colour = "lightgrey",
                             linewidth = 1)
      } +

      {if ( !is.null(slcted_id) &  is.null(facet_by) )
         geom_line(data = dplyr::filter(data, highlight == paste(slcted_id)),
                           aes(x = date, y = value, group = id),
                            size = 2.5)
      } +

      {if (!is.null(slcted_id) &  !is.null(facet_by) )
         geom_line(data = dplyr::filter(data, highlight == paste(slcted_id)),
                            aes(x = date, y = value, group = id, colour = band),
                            size = 2.5)
      } +


      {if (!is.null(slcted_id) & show_Labels)
         geom_point (data = dplyr::filter(data, highlight == paste(slcted_id) ),
                              aes(x = date, y = y_lvl, colour = phenophase),
                              na.rm=TRUE, fontface = "bold", size = 2)
      } +

      {if ( !is.null(facet_by) )  facet_grid( rows =  vars(get(facet_by)), scales = 'free') } +

      {if ( !is.null(slcted_id) )
         scale_fill_discrete()
      }  +

      ggtitle(title) +

      theme_classic()

}


# Définir les couleurs pour les labels
