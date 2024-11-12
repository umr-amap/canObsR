#' Plot the spectral signal over time
#'
#' @description A fct function
#'
#' @param data A \code{tibble} or \code{dataframe} of long data format for labels
#' @param Species chr
#' @param Genus chr
#' @param Family chr
#' @param Type chr
#' @param Metric chr
#' @param Band chr
#' @param facet_by chr
#' @param slcted_id chr
#' @param title chr
#'
#' @return return a ggplot
#'
#' @export
#' @import dplyr
#' @import ggplot2

plot_signal <- function(data,
                       Species = NULL,
                       Genus = NULL,
                       Family = NULL,
                       Type = NULL,
                       Metric = 'mean',
                       Band = NULL,
                       facet_by = 'band',
                       slcted_id = 'all',
                       title = NULL
) {


   data1 <- data %>%
      { if (!is.null(Species)) dplyr::filter (., species %in% Species) else . } %>%
      { if (!is.null(Genus)) dplyr::filter (., genus %in% Genus) else . } %>%
      { if (!is.null(Family)) dplyr::filter (., family %in% Family) else . } %>%
      { if (!is.null(Type)) dplyr::filter (., type %in% Type) else . } %>%
      { if (!is.null(Metric)) dplyr::filter (., metric %in% Metric) else . } %>%
      { if (!is.null(Band)) dplyr::filter (., band %in% Band) else . }


   if(slcted_id != 'all') {

      data <- data %>% dplyr::mutate( highlight = ifelse(id==slcted_id, paste(slcted_id), "Other"))

      data <- data %>%
         dplyr::group_by(id, band) %>%
         dplyr::mutate(y_lvl = (quantile(value, na.rm = T, probs = 0.1) )) %>%
         dplyr::ungroup()
   }


   ggplot2::ggplot (data) +

      {if ( slcted_id == 'all' & !is.null(facet_by))   ggplot2::geom_line (data = data, ggplot2::aes (x = date, y = value, group = id, colour = band) ) } +

      {if ( slcted_id == 'all' &  is.null(facet_by) )   ggplot2::geom_line (data = data, ggplot2::aes (x = date, y = value, group = id) ) } +


      {if ( slcted_id != 'all' &  is.null(facet_by) )
         ggplot2::geom_line (data = dplyr::filter(data, highlight != paste(slcted_id) ),
                             ggplot2::aes(x = date, y = value, group = id),
                             colour = "lightgrey",
                             linewidth = 1)
      } +

      {if ( slcted_id != 'all' &  !is.null(facet_by) )
         ggplot2::geom_line (data = dplyr::filter(data, highlight != paste(slcted_id) ),
                             ggplot2::aes(x = date, y = value, group = id),
                             colour = "lightgrey",
                             linewidth = 1)
      } +

      {if ( slcted_id != 'all' &  is.null(facet_by) )
         ggplot2::geom_line(data = dplyr::filter(data, highlight == paste(slcted_id)),
                            ggplot2::aes(x = date, y = value, group = id),
                            size = 2.5)
      } +

      {if ( slcted_id != 'all' &  !is.null(facet_by) )
         ggplot2::geom_line(data = dplyr::filter(data, highlight == paste(slcted_id)),
                            ggplot2::aes(x = date, y = value, group = id, colour = band),
                            size = 2.5)
      } +


      {if (slcted_id != 'all' )
         ggplot2::geom_point (data = dplyr::filter(data, highlight == paste(slcted_id) ),
                              ggplot2::aes(x = date, y = y_lvl, colour = phenophase),
                              na.rm=TRUE, fontface = "bold", size = 2)
      } +

      {if ( !is.null(facet_by) )  ggplot2::facet_grid( rows =  vars(get(facet_by)), scales = 'free') } +

      {if ( slcted_id != 'all' )
         ggplot2::scale_fill_discrete()
      }  +

      ggplot2::ggtitle(plot_title) +

      theme_classic()

}


# Définir les couleurs pour les labels
