library(knitr)
# opts_chunk$set(dev.args = list(family = "Palatino"))
# thm <- knit_theme$get("dusk")
thm <- knit_theme$get("solarized-light")
knit_theme$set(thm)

opts_chunk$set(tidy = FALSE,
               warning = FALSE,
               eval.after= "fig.cap",
               fig.width = 9,
               fig.height = 9 * 2/(1 + sqrt(5)),
               out.width = "11cm",
               fig.align = "center",
               # echo = FALSE,
               # results = "hide",
               # dev.args = list(family = "Helvetica-Narrow"),
               cache = FALSE)
options(width = 68)
