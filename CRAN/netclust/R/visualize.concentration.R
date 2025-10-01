#' @title Plot the Marginal Distributions of a Dirichlet Random Variable Under
#' Different Concentration Parameters
#'
#' @description Plots the marginal distributions of a Dirichlet random variable.
#' Useful for visualizing the effect of the concentration parameter on a
#' Dirichlet distribution with a set mean vector \code{p}.
#'
#' @param conc.values vector; different values for the concentration parameter
#' of a Dirichlet distribution.
#' @param p vector; the mean vector of the Dirichlet distribution.
#' @param nrow count; number of rows passed to \code{\link[ggplot2]{facet_wrap}}.
#' @param titl character string; title of the plot.
#'
#' @references
#'
#' Gamble, Laura J. (2021) \emph{Estimating the Size of Clustered Hidden
#' Populations}, Ph.D. Thesis, Department of Statistics, Oregon State
#' University.
#'
#' Wickham, H. (2016) \pkg{ggplot2}: Elegant Graphics for Data Analysis.
#' Springer-Verlag, New York. \url{https://ggplot2.tidyverse.org}.
#'
#' @examples
#'
#' # Visualize the effect of concentration parameters from 1 to 10 on a
#' # Dirichlet distribution with mean vector (0.5, 0.25, 0.25)
#' visualize.concentration(1:10, c(0.5, 0.25, 0.25))
#' visualize.concentration(1:10, c(0.5, 0.25, 0.25), nrow = 3)
#'
#' @export visualize.concentration

visualize.concentration <- function(conc.values, p,
                                    nrow = NULL, titl = NULL,
                                    low.col = "#E7AD2B", high.col = "darkblue"){
  x <- seq(0, 1, by = 0.01)
  facet.labels <- paste("Cluster", 1:length(p))
  graph.df <- data.frame("x" = NULL,
                         "f_x" = NULL,
                         "Variable" = NULL,
                         "Concentration" = NULL)
  for(i in 1:length(conc.values)){
    alph <- conc.values[i]*p
    for(j in 1:length(p)){
      graph.df <- rbind(graph.df,
                        data.frame("x" = x,
                                   "f_x" = dbeta(x,
                                                 shape1 = alph[j],
                                                 shape2 = sum(alph[-j])),
                                   "Variable" = facet.labels[j],
                                   "Concentration" = conc.values[i]))
    }
  }

  true.df <- data.frame("Variable" = facet.labels,
                        "Truth" = p,
                        "Concentration" = 1)
  max_y <- max(graph.df$f_x[graph.df$f_x != "Inf"])
  if(is.null(nrow)){
    ggplot(graph.df, aes(x = x, y = f_x, group = Concentration)) +
      geom_line(aes(color = Concentration)) +
      geom_segment(data = true.df, aes(x = Truth, y = 0,
                                       xend = Truth, yend = max_y)) +
      facet_wrap(~Variable) +
      scale_colour_gradient(low = low.col, high = high.col) +
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5)) +
      labs(title = titl,
           y = "Marginal Density")
  } else {
    ggplot(graph.df, aes(x = x, y = f_x, group = Concentration)) +
      geom_line(aes(color = Concentration)) +
      geom_segment(data = true.df, aes(x = Truth, y = 0,
                                       xend = Truth, yend = max_y)) +
      facet_wrap(~Variable, nrow = nrow) +
      scale_colour_gradient(low = low.col, high = high.col) +
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5)) +
      labs(title = titl,
           y = "Marginal Density",
           x = "p")
  }
}
