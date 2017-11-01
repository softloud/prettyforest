#' Forest plot.
#'
#' Analogous to the metafor::forest function, but using ggplot.
#'
#' @param ma A meta-analysis object produced by metafor.
#' @param null_centre Set the vertical line x-intercept representing null hypothesis for true effect.
#' @param title Optional character string for the plot.
#'
#' @export

ggforest <- function(ma, null_centre = 0, title = NULL) {
  ma %>%
    tidy_ma() %>%
    ggplot(aes(
      x = slab,
      y = yi,
      ymin = ci_lb,
      ymax = ci_ub
    )) +
    geom_hline(yintercept = null_centre,
               linetype = "dashed",
               colour = "black") +
    geom_pointrange(aes(size = weight),
                    alpha = 0.7) +
    theme_bw() +
    scale_size_continuous(range = c(0.5, 2)) +
    coord_flip() +g
    theme(axis.text.y = element_text(
      #angle = 30,
      size = 15
      )) +
    labs(
      title = title,
      size = TeX(
        "Weight: $\\frac{\\frac{1}{V_{\\theta'}}}{\\sum\\frac{1}{V_{\\theta'}}}}$"
      ),
      # x = "Study",
      x = "",
      y =
        TeX(
          "Interval $\\theta' \\pm \\Phi^{-1}(0.975)\\sqrt{V_{\\theta'}}$ with point $\\theta'$"
        )
    )
}

