SELECT plot, abbr, layer, cov FROM table WHERE PLOT LIKE fü%

SELECT plot, abbr, layer, cov FROM table WHERE PLOT LIKE

prjs <- c("a", "b", "c")

test <- unique(table$plot)
paste(" OR PLOT LIKE", prjs, collapse = "")
prj1 OR LIKE prj2 OR LIKE prj_i