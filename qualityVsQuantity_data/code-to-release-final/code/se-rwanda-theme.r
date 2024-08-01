##############################################
## rwanda_se_theme.R
##
## ggplot2 theme for Rwanda survey experiment paper
##

## see http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/#a-colorblind-friendly-palette
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73",
                "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

scale_color_tiedefn <- scale_color_manual(values=c("Acquaintance"=cbbPalette[4],
                                                   "Meal"=cbbPalette[6],
                                                   "Blended"=cbbPalette[8]),
                                          name="tie\ndefinition")

scale_fill_tiedefn <- scale_fill_manual(values=c("Acquaintance"=cbbPalette[4],
                                                   "Meal"=cbbPalette[6],
                                                   "Blended"=cbbPalette[8]),
                                          name="tie\ndefinition")

## see the source for ggplot2::theme_bw() to see how we're doing this
theme_se <- function(base_size = 12, base_family="") {
    theme_bw(base_size = base_size, base_family=base_family) %+replace%
      theme(panel.grid.minor=element_blank())
}


