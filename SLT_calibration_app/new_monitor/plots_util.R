# ============================================================
# plot_util.R — SLT_calibration Monitor
# Generische Plot-Funktionen (nach Vorbild Klaus Frielers longgold_monitor)
#
# Hinweis: bivariate_plot_auto() nimmt var_x/var_y direkt als
# Argumente (statt intern auf input$bv_variable1/2 zuzugreifen
# wie im alten Monitor) — entkoppelt die Funktion von konkreten
# UI-Input-IDs, damit sie in app.R flexibel wiederverwendbar ist.
# ============================================================

library(ggplot2)
library(dplyr)
library(purrr)
library(corrr)   # install.packages("corrr") falls noch nicht installiert
library(cowplot)  # install.packages("cowplot") falls noch nicht installiert

def_colour1 <- "#1f77b4"
def_colour2 <- "#ff7f0e"
def_colour3 <- "#2ca02c"
uv_alpha   <- .7
bv_alpha   <- .7
default_text_size <- 16
correlation_background_color <- "gray92"

get_default_theme <- function(x_rotate = 0, keep_legend = FALSE) {
  t <- theme_minimal()
  t <- t + theme(strip.text = element_text(size = round(default_text_size * .75), hjust = 0))
  t <- t + theme(text = element_text(size = default_text_size))
  t <- t + theme(axis.title.x = element_text(size = default_text_size, vjust = -.5))
  t <- t + theme(plot.title = element_text(hjust = 0))
  t <- t + theme(panel.spacing.x = unit(0.5, "cm"))
  t <- t + theme(panel.spacing.y = unit(0.5, "cm"))
  if (x_rotate != 0) {
    t <- t + theme(axis.text.x = element_text(size = round(default_text_size * .85), angle = x_rotate, hjust = 1))
  } else {
    t <- t + theme(axis.text.x = element_text(size = round(default_text_size * .85)))
  }
  if (!keep_legend) t <- t + theme(legend.position = "none")
  t
}

is_valid <- function(x) {
  if ("character" %in% class(x)) {
    return(length(x) > 0 && nchar(x) > 0 && sum(is.na(x)) == 0)
  }
  invalid <- is.null(x) || length(x) == 0 || sum(is.na(x)) > 0 || sum(nchar(x)) == 0
  !invalid
}

is_valid_char <- function(x) is_valid(x) && is.character(x)

get_optimal_ncol <- function(data) {
  if ("group_var" %in% names(data)) {
    return(ifelse(dplyr::n_distinct(data$group_var) > 2, 2, 1))
  }
  1
}

add_group_var <- function(data, group_var, second = FALSE, remove_na = FALSE) {
  if (is_valid_char(group_var)) {
    if (second) {
      data$group_var2 <- data[[group_var]]
      if (remove_na) data <- data %>% filter(!is.na(group_var2))
    } else {
      data$group_var <- data[[group_var]]
      if (remove_na) data <- data %>% filter(!is.na(group_var))
    }
  }
  data
}

add_group_vars <- function(data, group_var, group_var2, remove_na = FALSE, remove_na2 = TRUE) {
  data <- add_group_var(data, group_var, remove_na = remove_na)
  add_group_var(data, group_var2, second = TRUE, remove_na = remove_na2)
}

add_facet_wrap <- function(plot_obj, data, scale = "free") {
  if ("group_var" %in% names(data)) {
    if (nrow(dplyr::count(data, group_var)) == 0) return(plot_obj)
    plot_obj <- plot_obj + facet_wrap(~group_var, ncol = get_optimal_ncol(data), scale = scale)
  }
  plot_obj
}

add_facet_grid <- function(plot_obj, data, scale = "free") {
  if (all(c("group_var", "group_var2") %in% names(data))) {
    if (nrow(dplyr::count(data, group_var)) == 0 || nrow(dplyr::count(data, group_var2)) == 0) return(plot_obj)
    plot_obj <- plot_obj + facet_grid(group_var ~ group_var2, scale = scale)
  }
  plot_obj
}

add_facet <- function(plot_obj, data, group_var, group_var2 = NULL, scale = "free") {
  if (is_valid_char(group_var2)) {
    if (is_valid_char(group_var)) plot_obj <- add_facet_grid(plot_obj, data, scale)
  } else if (is_valid_char(group_var)) {
    plot_obj <- add_facet_wrap(plot_obj, data, scale)
  }
  plot_obj
}

# ------------------------------------------------------------
# Univariate Plots
# ------------------------------------------------------------
univariate_plot_numeric <- function(data, var, group_var = NULL, group_var2 = NULL,
                                    scale = "fixed", remove_na = FALSE, remove_na2 = FALSE) {
  data <- add_group_vars(data, group_var, group_var2, remove_na = remove_na, remove_na2 = remove_na2)
  
  q <- ggplot(data, aes(x = .data[[var]])) +
    geom_histogram(fill = def_colour1, alpha = uv_alpha, colour = "white", bins = 20)
  q <- add_facet(q, data, group_var, group_var2, scale)
  q <- q + labs(x = var, y = "Häufigkeit")
  
  sum_data <- if (is_valid_char(group_var)) {
    data %>% group_by(group_var) %>% summarise(m = mean(.data[[var]], na.rm = TRUE), .groups = "drop")
  } else {
    data %>% summarise(m = mean(.data[[var]], na.rm = TRUE))
  }
  q <- q + geom_vline(data = sum_data, aes(xintercept = m), linewidth = 1.1, colour = "indianred")
  q + get_default_theme()
}

univariate_plot_categorial <- function(data, var, group_var = NULL, group_var2 = NULL,
                                       scale = "fixed", remove_na = FALSE, remove_na2 = FALSE,
                                       coord_flip = FALSE) {
  data <- data %>% mutate(!!var := factor(.data[[var]]))
  data <- add_group_vars(data, group_var, group_var2, remove_na = remove_na, remove_na2 = remove_na2)
  
  q <- ggplot(data, aes(x = .data[[var]])) +
    geom_bar(fill = def_colour1, alpha = uv_alpha, colour = "black")
  q <- add_facet(q, data, group_var, group_var2, scale)
  q <- q + labs(x = var, y = "Häufigkeit")
  if (coord_flip) q <- q + coord_flip()
  q + get_default_theme()
}

# ------------------------------------------------------------
# Bivariate Plots
# ------------------------------------------------------------
bivariate_plot_categorial_numeric <- function(data, var_x, var_y, group_var = NULL, group_var2 = NULL,
                                              scale = "fixed", remove_na = FALSE, remove_na2 = FALSE) {
  data <- data %>% filter(!is.na(.data[[var_x]])) %>% mutate(!!var_x := factor(.data[[var_x]]))
  data <- add_group_vars(data, group_var, group_var2, remove_na = remove_na, remove_na2 = remove_na2)
  
  q <- ggplot(data, aes(x = .data[[var_x]], y = .data[[var_y]])) +
    geom_boxplot(fill = def_colour1, alpha = .5) +
    geom_jitter(width = .15, alpha = bv_alpha)
  q <- add_facet(q, data, group_var, group_var2, scale)
  q <- q + labs(x = var_x, y = var_y)
  q + get_default_theme()
}

bivariate_plot_numeric_numeric <- function(data, var_x, var_y, add_regression = TRUE,
                                           group_var = NULL, group_var2 = NULL,
                                           scale = "fixed", remove_na = FALSE, remove_na2 = FALSE) {
  data <- add_group_vars(data, group_var, group_var2, remove_na = remove_na, remove_na2 = remove_na2)
  
  q <- ggplot(data, aes(x = .data[[var_x]], y = .data[[var_y]])) +
    geom_point(alpha = bv_alpha, colour = def_colour1)
  if (add_regression) q <- q + geom_smooth(method = "lm", colour = def_colour2, se = TRUE)
  q <- add_facet(q, data, group_var, group_var2, scale)
  q <- q + labs(x = var_x, y = var_y)
  q + get_default_theme()
}

bivariate_plot_categorial_categorial <- function(data, var_x, var_y, na_rm_x = TRUE, na_rm_y = TRUE) {
  if (na_rm_x) data <- data %>% filter(!is.na(.data[[var_x]]))
  if (na_rm_y) data <- data %>% filter(!is.na(.data[[var_y]]))
  
  sum_data <- data %>%
    dplyr::count(.data[[var_x]], .data[[var_y]]) %>%
    group_by(.data[[var_y]]) %>%
    mutate(rel_freq = n / sum(n)) %>%
    ungroup()
  
  ggplot(sum_data, aes(x = .data[[var_x]], y = rel_freq)) +
    geom_col(fill = def_colour1) +
    facet_wrap(as.formula(paste0("~", var_y))) +
    labs(x = var_x, y = paste("Relativer Anteil pro", var_y)) +
    get_default_theme(x_rotate = 45)
}

filter_non_numeric <- function(data, vars) {
  if (is.null(vars) || length(vars) == 0) return(character(0))
  vars[purrr::map_lgl(vars, ~ is.numeric(data[[.x]]))]
}

get_var_type <- function(var_data, v) {
  var_data %>% filter(variable == v) %>% pull(type)
}

bivariate_plot_auto <- function(data, var_x, var_y, var_data, group_var = NULL, group_var2 = NULL,
                                remove_na = TRUE, remove_na2 = TRUE) {
  type_x <- get_var_type(var_data, var_x)
  type_y <- get_var_type(var_data, var_y)
  sub_type <- sprintf("%s-%s", substr(type_x, 1, 3), substr(type_y, 1, 3))
  
  if (sub_type == "cat-cat") {
    bivariate_plot_categorial_categorial(data, var_x, var_y)
  } else if (sub_type == "cat-num") {
    bivariate_plot_categorial_numeric(data, var_x, var_y, group_var = group_var, group_var2 = group_var2,
                                      remove_na = remove_na, remove_na2 = remove_na2)
  } else if (sub_type == "num-cat") {
    bivariate_plot_categorial_numeric(data, var_y, var_x, group_var = group_var, group_var2 = group_var2,
                                      remove_na = remove_na, remove_na2 = remove_na2)
  } else {
    bivariate_plot_numeric_numeric(data, var_x, var_y, group_var = group_var, group_var2 = group_var2,
                                   remove_na = remove_na, remove_na2 = remove_na2)
  }
}

# ------------------------------------------------------------
# Korrelationsnetzwerk (corrr) — auf Wunsch aus altem Monitor übernommen
# ------------------------------------------------------------
plot_cor_network <- function(data, cor_vars, min_cor = .1, partial = FALSE,
                             output_type = c("matrix", "network", "text", "raw"),
                             legend = TRUE, text_size = 12, aggregate = TRUE) {
  cor_vars <- intersect(names(data), cor_vars)
  cor_vars <- filter_non_numeric(data, cor_vars)
  if (length(cor_vars) == 0) {
    messagef("plot_cor_network: keine Variablen übrig")
    return(NULL)
  }
  output_type <- match.arg(output_type)
  
  data_sub <- data %>% dplyr::select(id, dplyr::all_of(cor_vars))
  if (aggregate) {
    data_sub <- data_sub %>%
      group_by(id) %>%
      summarise(dplyr::across(everything(), ~ mean(.x, na.rm = TRUE)), .groups = "drop")
  }
  data_sub <- data_sub %>% dplyr::select(-id)
  
  res_cor <- corrr::correlate(data_sub, quiet = TRUE)
  
  if (output_type == "network") {
    q <- tryCatch({
      res_cor %>% corrr::network_plot(min_cor = min_cor, colours = c("indianred4", "white", "skyblue3"), legend = legend)
    }, error = function(e) { message("Fehler bei network_plot"); NULL })
    if (!is.null(q)) {
      q <- q + theme(text = element_text(size = text_size),
                     panel.background = element_rect(fill = correlation_background_color))
    }
    q
  } else if (output_type == "matrix") {
    q <- tryCatch({
      res_cor %>% corrr::rplot(print_cor = TRUE, colours = c("indianred4", "white", "skyblue3"), legend = legend)
    }, error = function(e) { message("Fehler bei rplot"); NULL })
    if (!is.null(q)) {
      q <- q + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
      q <- q + theme(text = element_text(size = text_size), panel.background = element_rect(fill = correlation_background_color))
    }
    q
  } else if (output_type == "text") {
    res_cor %>% corrr::fashion() %>% as_tibble() %>% rename(Variable = term)
  } else {
    res_cor
  }
}

plot_cor_networks_with_grouping <- function(data, cor_vars, grouping_var = NULL, min_cor = .1,
                                            partial = FALSE,
                                            output_type = c("matrix", "network", "text", "raw"),
                                            min_group_size = 10, aggregate = TRUE, ncol = NULL) {
  cor_vars <- filter_non_numeric(data, cor_vars)
  if (length(cor_vars) == 0) return(NULL)
  
  if (is.null(grouping_var) || grouping_var == "--" || !(grouping_var %in% names(data))) {
    return(plot_cor_network(data, cor_vars, min_cor = min_cor, partial = partial,
                            output_type = output_type, aggregate = aggregate, legend = TRUE))
  }
  
  output_type <- match.arg(output_type)
  data <- data %>% filter(!is.na(.data[[grouping_var]]))
  
  group_counts <- data %>% dplyr::count(.data[[grouping_var]])
  groups <- group_counts %>% filter(n >= min_group_size) %>% pull(.data[[grouping_var]]) %>% unique()
  
  if (length(groups) == 0) {
    messagef("plot_cor_networks_with_grouping: keine Gruppe erreicht min_group_size = %d", min_group_size)
    return(NULL)
  }
  
  plots <- purrr::map(groups, function(g) {
    group_n <- group_counts %>% filter(.data[[grouping_var]] == g) %>% pull(n)
    title <- sprintf("%s (N = %s)", g, group_n[1])
    q <- plot_cor_network(data %>% filter(.data[[grouping_var]] == g), cor_vars,
                          min_cor = min_cor, partial = partial, legend = FALSE, output_type = output_type)
    if (is.null(q)) return(NULL)
    if (output_type %in% c("network", "matrix")) {
      q + labs(title = title) +
        theme(text = element_text(size = 12),
              panel.border = element_rect(colour = "gray64", fill = NA, linewidth = .25),
              plot.margin = margin(5, 5, 5, 5))
    } else {
      q %>% mutate(Group = g) %>% select(Group, everything())
    }
  })
  
  if (output_type %in% c("network", "matrix")) {
    cowplot::plot_grid(plotlist = plots, ncol = ncol)
  } else if (output_type == "text") {
    suppressWarnings(bind_rows(plots))
  } else {
    suppressWarnings(plots)
  }
}