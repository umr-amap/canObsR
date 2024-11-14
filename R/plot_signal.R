#' Plot the spectral signal over time
#'
#' @description A fct function
#'
#' @param data A \code{tibble} or \code{dataframe} of long data format for labels
#' @param Species chr Filtering the data at the species level
#' @param Genus chr Filtering the data at the genus level
#' @param Family chr Filtering the data at the family level
#' @param Type chr Filtering by the type
#' @param Metric chr Filtering by the metric. By defaut it is the mean.
#' @param Band chr Filtering by the band.
#' @param facet_by chr Facetting by a variable
#' @param slcted_id chr Highlighting an id
#' @param show_Labels description
#' @param title chr The title of the plot
#'
#' @return return a ggplot
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

      data <- data %>% dplyr::mutate( highlight = ifelse(id==slcted_id, paste(slcted_id), "Other"))

      data <- data %>%
         dplyr::group_by(id, band) %>%
         dplyr::mutate(y_lvl = (stats::quantile(value, na.rm = T, probs = 0.1) )) %>%
         dplyr::ungroup()
   }


   ggplot2::ggplot (data) +

      {if ( is.null(slcted_id) & !is.null(facet_by))   ggplot2::geom_line (data = data, ggplot2::aes (x = date, y = value, group = id, colour = band) ) } +

      {if ( is.null(slcted_id) &  is.null(facet_by) )   ggplot2::geom_line (data = data, ggplot2::aes (x = date, y = value, group = id) ) } +


      {if ( !is.null(slcted_id) &  is.null(facet_by) )
         ggplot2::geom_line (data = dplyr::filter(data, highlight != paste(slcted_id) ),
                             ggplot2::aes(x = date, y = value, group = id),
                             colour = "lightgrey",
                             linewidth = 1)
      } +

      {if ( !is.null(slcted_id) &  !is.null(facet_by) )
         ggplot2::geom_line (data = dplyr::filter(data, highlight != paste(slcted_id) ),
                             ggplot2::aes(x = date, y = value, group = id),
                             colour = "lightgrey",
                             linewidth = 1)
      } +

      {if ( !is.null(slcted_id) &  is.null(facet_by) )
         ggplot2::geom_line(data = dplyr::filter(data, highlight == paste(slcted_id)),
                            ggplot2::aes(x = date, y = value, group = id),
                            size = 2.5)
      } +

      {if (!is.null(slcted_id) &  !is.null(facet_by) )
         ggplot2::geom_line(data = dplyr::filter(data, highlight == paste(slcted_id)),
                            ggplot2::aes(x = date, y = value, group = id, colour = band),
                            size = 2.5)
      } +


      {if (!is.null(slcted_id) & show_Labels)
         ggplot2::geom_point (data = dplyr::filter(data, highlight == paste(slcted_id) ),
                              ggplot2::aes(x = date, y = y_lvl, colour = phenophase),
                              na.rm=TRUE, fontface = "bold", size = 2)
      } +

      {if ( !is.null(facet_by) )  ggplot2::facet_grid( rows =  vars(get(facet_by)), scales = 'free') } +

      {if ( !is.null(slcted_id) )
         ggplot2::scale_fill_discrete()
      }  +

      ggplot2::ggtitle(title) +

      theme_classic()

}


# Définir les couleurs pour les labels
