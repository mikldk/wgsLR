library(tidyverse)
library(ggraph)
library(tidygraph)
library(caracas)

haps <- c("0/0", "0/1", "1/0", "1/1")

add_group <- function(d) {
  d |> 
    mutate(group = case_when(
      XT_MA == XR_MA ~ "diag",
      abs(XT_MA - XR_MA) == 1L ~ "off-diag",
      abs(XT_MA - XR_MA) == 2L ~ "corners",
      TRUE ~ "unknown"
    ))
}



################################################################################
# Single
################################################################################

single_cases <- expand.grid(Z = haps, X = haps) 
d_formulas_single <- single_cases |> 
  separate(Z, into = c("Z1", "Z2"), sep = "/", remove = FALSE) |>
  separate(X, into = c("X1", "X2"), sep = "/", remove = FALSE) |> 
  rowwise() |> 
  mutate(num_no_err = sum(X1 == Z1) + sum(X2 == Z2)) |>
  mutate(num_err = 2L - num_no_err) |> 
  #mutate(expr = paste0("((1-w)^", num_no_err, ")*(w^", num_err, ")")) |>
  mutate(expr = case_when(
    num_no_err == 1L & num_err == 1L ~ paste0("w*(1-w)"),
    
    num_no_err == 0L ~ paste0("w^", num_err),
    num_err  == 0L ~ paste0("(1-w)^", num_no_err),
    #TRUE ~ paste0("((1-w)^", num_no_err, ")*(w^", num_err, ")"))) |> 
    
    num_no_err == 1L ~ paste0("(1-w)", " * w^", num_err, ""),
    num_err == 1L ~ paste0("(1-w)^", num_no_err, " * w", ""),
    
    TRUE ~ paste0("(1-w)^", num_no_err, " * w^", num_err, ""))) |> 
  ungroup() |> 
  mutate(Z_MA = as.integer(Z1) + as.integer(Z2),
         X_MA = as.integer(X1) + as.integer(X2))



#############

add_expr_w <- function(d) {
  d |> 
    mutate(expr = case_when(
      num_no_err == 1L & num_err == 1L ~ paste0("w*(1-w)"),
      
      num_no_err == 0L ~ paste0("w^", num_err),
      num_err  == 0L ~ paste0("(1-w)^", num_no_err),
      
      num_no_err == 1L ~ paste0("(1-w)", " * w^", num_err, ""),
      num_err == 1L ~ paste0("(1-w)^", num_no_err, " * w", ""),
      
      #TRUE ~ paste0("((1-w)^", num_no_err, ")*(w^", num_err, ")"))) |>
      TRUE ~ paste0("(1-w)^", num_no_err, " * w^", num_err, "")))
}

add_expr_wTwR <- function(d) {
  d |> 
    #mutate(expr_D = paste0("((1-wT)^", num_no_err_D, ")*(wT^", num_err_D, ")")) |>
    mutate(expr_D = case_when(
      num_no_err_D == 1L & num_err_D == 1L ~ paste0("wT*(1-wT)"),
      
      num_no_err_D == 0L ~ paste0("wT^", num_err_D),
      num_err_D  == 0L ~ paste0("(1-wT)^", num_no_err_D),
      
      num_no_err_D == 1L ~ paste0("(1-wT)", " * wT^", num_err_D, ""),
      num_err_D == 1L ~ paste0("(1-wT)^", num_no_err_D, " * wT", ""),
      
      num_no_err_D == 1L ~ paste0("(1-wT)", " * wT^", num_err_D, ""),
      num_err_D == 1L ~ paste0("(1-wT)^", num_no_err_D, " * wT", ""),
      
      #TRUE ~ paste0("((1-wT)^", num_no_err_D, ")*(wT^", num_err_D, ")"))) |>
      TRUE ~ paste0("(1-wT)^", num_no_err_D, " * wT^", num_err_D, ""))) |> 
      
      #mutate(expr_S = paste0("((1-wR)^", num_no_err_S, ")*(wR^", num_err_S, ")")) |>
      mutate(expr_S = case_when(
        num_no_err_S == 1L & num_err_S == 1L ~ paste0("wR*(1-wR)"),
        
        num_no_err_S == 0L ~ paste0("wR^", num_err_S),
        num_err_S  == 0L ~ paste0("(1-wR)^", num_no_err_S),
        
        num_no_err_S == 1L ~ paste0("(1-wR)", " * wR^", num_err_S, ""),
        num_err_S == 1L ~ paste0("(1-wR)^", num_no_err_S, " * wR", ""),
        
        #TRUE ~ paste0("((1-wR)^", num_no_err_S, ")*(wR^", num_err_S, ")"))) |>
        TRUE ~ paste0("(1-wR)^", num_no_err_S, " * wR^", num_err_S, ""))) |> 
      
      mutate(expr = paste0(expr_D, "*", expr_S))
}

################################################################################
# Hp
################################################################################

H_p_cases <- expand.grid(Z = haps, XT = haps, XR = haps) 

d_formulas_Hp_w <- H_p_cases |> 
  separate(Z, into = c("Z1", "Z2"), sep = "/", remove = FALSE) |>
  separate(XT, into = c("XT1", "XT2"), sep = "/", remove = FALSE) |> 
  separate(XR, into = c("XR1", "XR2"), sep = "/", remove = FALSE) |> 
  rowwise() |> 
  mutate(num_no_err = sum(XT1 == Z1) + sum(XT2 == Z2) +
                      sum(XR1 == Z1) + sum(XR2 == Z2)) |>
  mutate(num_err = 4L - num_no_err) |> 
  #mutate(expr = paste0("((1-w)^", num_no_err, ")*(w^", num_err, ")")) |>
  
  add_expr_w() |> 
  
  ungroup() |> 
  mutate(Z_MA = as.integer(Z1) + as.integer(Z2),
         XT_MA = as.integer(XT1) + as.integer(XT2),
         XR_MA = as.integer(XR1) + as.integer(XR2)) |> 
  add_group()

d_prob_Hp_w <- d_formulas_Hp_w |> 
  select(Z_MA, XT_MA, XR_MA, expr) |> 
  
  # p_1 is for both 0/1 and 1/0, so only take half for those cases
  mutate(weight = ifelse(Z_MA == 1L, 2, 1)) |> 
  
  mutate(caracas_sym = paste0("(p_", Z_MA, "/", weight, ") * ", expr)) |>
  
  group_by(XT_MA, XR_MA) |> 
  summarise(caracas_sym = paste0(caracas_sym, collapse = " + "),
            .groups = "drop") |>
  rowwise() |>
  mutate(caracas_sym = list(as_sym(caracas_sym))) |>
  mutate(expr = list(as_expr(caracas_sym)),
         expr_chr = as.character(caracas_sym),
         expr_tex = tex(caracas_sym))



d_formulas_Hp_wTwR <- H_p_cases |> 
  separate(Z, into = c("Z1", "Z2"), sep = "/", remove = FALSE) |>
  separate(XT, into = c("XT1", "XT2"), sep = "/", remove = FALSE) |> 
  separate(XR, into = c("XR1", "XR2"), sep = "/", remove = FALSE) |> 
  rowwise() |> 
  mutate(num_no_err_D = sum(XT1 == Z1) + sum(XT2 == Z2)) |>
  mutate(num_no_err_S = sum(XR1 == Z1) + sum(XR2 == Z2)) |>
  mutate(num_err_D = 2L - num_no_err_D) |>
  mutate(num_err_S = 2L - num_no_err_S) |> 
  
  add_expr_wTwR() |> 
  
  ungroup() |> 
  mutate(Z_MA = as.integer(Z1) + as.integer(Z2),
         XT_MA = as.integer(XT1) + as.integer(XT2),
         XR_MA = as.integer(XR1) + as.integer(XR2)) |> 
  add_group()


d_prob_Hp_wTwR <- d_formulas_Hp_wTwR |> 
  select(Z_MA, XT_MA, XR_MA, expr) |> 
  
  # p_1 is for both 0/1 and 1/0, so only take half for those cases
  mutate(weight = ifelse(Z_MA == 1L, 2, 1)) |> 
  
  mutate(caracas_sym = paste0("(p_", Z_MA, "/", weight, ") * ", expr)) |>
  
  group_by(XT_MA, XR_MA) |> 
  summarise(caracas_sym = paste0(caracas_sym, collapse = " + "),
            .groups = "drop") |>
  rowwise() |>
  mutate(caracas_sym = list(simplify(as_sym(caracas_sym)))) |>
  mutate(expr = list(as_expr(caracas_sym)),
         expr_chr = as.character(caracas_sym),
         expr_tex = tex(caracas_sym))


################################################################################
# Hd
################################################################################

H_d_cases <- expand.grid(ZT = haps, ZR = haps, XT = haps, XR = haps) 

d_formulas_Hd_w <- H_d_cases |> 
  separate(ZT, into = c("ZT1", "ZT2"), sep = "/", remove = FALSE) |>
  separate(ZR, into = c("ZR1", "ZR2"), sep = "/", remove = FALSE) |> 
  separate(XT, into = c("XT1", "XT2"), sep = "/", remove = FALSE) |> 
  separate(XR, into = c("XR1", "XR2"), sep = "/", remove = FALSE) |> 
  rowwise() |> 
  mutate(num_no_err = sum(XT1 == ZT1) + sum(XT2 == ZT2) +
                      sum(XR1 == ZR1) + sum(XR2 == ZR2)) |>
  mutate(num_err = 4L - num_no_err) |> 
  #mutate(expr = paste0("((1-w)^", num_no_err, ")*(w^", num_err, ")")) |>
  
  add_expr_w() |> 
  
  ungroup() |> 
  mutate(ZT_MA = as.integer(ZT1) + as.integer(ZT2),
         ZR_MA = as.integer(ZR1) + as.integer(ZR2),
         XT_MA = as.integer(XT1) + as.integer(XT2),
         XR_MA = as.integer(XR1) + as.integer(XR2)) |> 
  add_group()

d_prob_Hd_w <- d_formulas_Hd_w |> 
  select(ZT_MA, ZR_MA, XT_MA, XR_MA, expr) |> 
  
  # p_1 is for both 0/1 and 1/0, so only take half for those cases
  mutate(weight_D = ifelse(ZT_MA == 1L, 2, 1),
         weight_S = ifelse(ZR_MA == 1L, 2, 1)) |> 
  
  mutate(caracas_sym = paste0("(p_", ZT_MA, "/", weight_D, ") * (", 
                        "p_", ZR_MA, "/", weight_S, ") * ", expr)) |>
  
  group_by(XT_MA, XR_MA) |> 
  summarise(caracas_sym = paste0(caracas_sym, collapse = " + "),
            .groups = "drop") |>
  rowwise() |>
  mutate(caracas_sym = list(as_sym(caracas_sym))) |>
  mutate(expr = list(as_expr(caracas_sym)),
         expr_chr = as.character(caracas_sym),
         expr_tex = tex(caracas_sym))


##

d_formulas_Hd_wTwR <- H_d_cases |> 
  separate(ZT, into = c("ZT1", "ZT2"), sep = "/", remove = FALSE) |>
  separate(ZR, into = c("ZR1", "ZR2"), sep = "/", remove = FALSE) |> 
  separate(XT, into = c("XT1", "XT2"), sep = "/", remove = FALSE) |> 
  separate(XR, into = c("XR1", "XR2"), sep = "/", remove = FALSE) |> 
  rowwise() |> 
  mutate(num_no_err_D = sum(XT1 == ZT1) + sum(XT2 == ZT2)) |>
  mutate(num_no_err_S = sum(XR1 == ZR1) + sum(XR2 == ZR2)) |>
  mutate(num_err_D = 2L - num_no_err_D) |>
  mutate(num_err_S = 2L - num_no_err_S) |> 
  
  add_expr_wTwR() |> 
 
  ungroup() |> 
  mutate(ZT_MA = as.integer(ZT1) + as.integer(ZT2),
         ZR_MA = as.integer(ZR1) + as.integer(ZR2),
         XT_MA = as.integer(XT1) + as.integer(XT2),
         XR_MA = as.integer(XR1) + as.integer(XR2)) |> 
  add_group()

d_prob_Hd_wTwR <- d_formulas_Hd_wTwR |> 
  select(ZT_MA, ZR_MA, XT_MA, XR_MA, expr) |> 
  
  # p_1 is for both 0/1 and 1/0, so only take half for those cases
  mutate(weight_D = ifelse(ZT_MA == 1L, 2, 1),
         weight_S = ifelse(ZR_MA == 1L, 2, 1)) |> 
  
  mutate(caracas_sym = paste0("(p_", ZT_MA, "/", weight_D, ") * (", 
                              "p_", ZR_MA, "/", weight_S, ") * ", expr)) |>
  
  group_by(XT_MA, XR_MA) |> 
  summarise(caracas_sym = paste0(caracas_sym, collapse = " + "),
            .groups = "drop") |>
  rowwise() |>
  mutate(caracas_sym = list(simplify(as_sym(caracas_sym)))) |>
  mutate(expr = list(as_expr(caracas_sym)),
         expr_chr = as.character(caracas_sym),
         expr_tex = tex(caracas_sym))


################################################################################
# Single graph
################################################################################

y_weights_single <- c(0.5, rep(1, length(haps) - 1), 0.5)
y_weights_qs_single <- (cumsum(y_weights_single) / sum(y_weights_single))[-length(y_weights_single)]
ys_single <- round(quantile(seq_len(length(haps)^2), y_weights_qs_single, names = FALSE))

d_edges_root_single <- expand.grid(Z = haps, X = NA) |>
  mutate(from = "Z") |>
  mutate(to = paste0("Z=", Z)) |> 
  select(from, to) |> 
  mutate(x = 2,
         y = ys_single)

d_edges_single <- bind_rows(
  d_formulas_single |> 
    mutate(from = paste0("Z=", Z)) |> 
    mutate(to = paste0("Z=", Z, ", X=", X)) |> 
    arrange(Z, X) |> 
    mutate(x = 3, y = row_number()) |> 
    select(from, to, x, y, expr),
  d_edges_root_single)

graph_single <- as_tbl_graph(d_edges_single, directed = TRUE)
graph_single <- graph_single |>
  activate(nodes) |>
  left_join(d_edges_single |> select(name = to, x, y), by = "name") |>
  left_join(d_edges_single |> select(name = to, expr), by = "name") |> 
  
  #mutate(label = ifelse(is.na(expr), name, paste0(name, ": ", expr))) |> 
  mutate(shortname = gsub("^(Z[^X]*)(X.*)$", "\\2", name)) |> 
  mutate(label = ifelse(is.na(expr), shortname, paste0(shortname, ": ", expr))) |> 
  
  # Root:
  mutate(x = ifelse(is.na(x), 1, x), 
         y = ifelse(is.na(y), floor(nrow(d_formulas_single)/2)+0.5, y)) |> 
  mutate(leaf = grepl("X", name)) |> 
  
  # Expression label
  mutate(shortname2 = gsub("([DS]{1})=", "\\[\\1\\]=", shortname)) |> 
  mutate(shortname2 = gsub(",", ", ',', ", shortname2, fixed = TRUE)) |> 
  mutate(label_expr = case_when(
    !leaf ~ paste0("paste(", shortname2, ")"),
    leaf ~ paste0("paste(", shortname2, ", ':', ", expr, ")"))) |> 
  mutate(label_expr = gsub("=", ", '=', ", label_expr, fixed = TRUE)) |> 
  select(-shortname2) 
  
  # Adjusting x:
  # mutate(x = case_when(
  #   abs(x - 2) < 1e-6 ~ 1.7,
  #   #abs(x - 3) < 1e-6 ~ 1.4,
  #   TRUE ~ x
  # ))

if (FALSE) {
  ggraph(graph_single, x = x, y = y) + 
    geom_edge_link() +
    geom_node_label(aes(filter = !leaf, label = label), size = 2) +
    geom_node_label(aes(filter = leaf, label = label_expr), size = 2.5, hjust = 0, parse = TRUE) +
    theme_void() +
    theme(legend.position = "bottom") +
    labs(fill = NULL) +
    guides(fill = guide_legend(override.aes = list(color = NULL))) +
    coord_cartesian(xlim = c(1, 3.5))  
}



################################################################################
# Hp graph
################################################################################
y_weights_Hp <- c(0.5, rep(1, length(haps) - 1), 0.5)
y_weights_qs_Hp <- (cumsum(y_weights_Hp) / sum(y_weights_Hp))[-length(y_weights_Hp)]
ys_Hp <- round(quantile(seq_len(length(haps)^3), y_weights_qs_Hp, names = FALSE))

d_edges_root_Hp <- expand.grid(Z = haps, XT = NA, XR = NA) |>
  mutate(from = "Z") |>
  mutate(to = paste0("Z=", Z)) |> 
  select(from, to) |> 
  mutate(x = 2,
         y = ys_Hp)

d_edges_Hp_w <- bind_rows(
  d_formulas_Hp_w |> 
    mutate(from = paste0("Z=", Z)) |> 
    mutate(to = paste0("Z=", Z, ", XT=", XT, ", XR=", XR)) |> 
    arrange(Z, XT, XR) |> 
    mutate(x = 3, y = row_number()) |> 
    select(from, to, x, y, group, expr),
  d_edges_root_Hp)

graph_Hp_w <- as_tbl_graph(d_edges_Hp_w, directed = TRUE)
graph_Hp_w <- graph_Hp_w |>
  activate(nodes) |>
  left_join(d_edges_Hp_w |> select(name = to, x, y), by = "name") |>
  left_join(d_edges_Hp_w |> select(name = to, group, expr), by = "name") |> 
  
  #mutate(label = ifelse(is.na(expr), name, paste0(name, ": ", expr))) |> 
  mutate(shortname = gsub("^(Z[^X]*)(X.*)$", "\\2", name)) |> 
  mutate(label = ifelse(is.na(expr), shortname, paste0(shortname, ": ", expr))) |> 
  
  # Root:
  mutate(x = ifelse(is.na(x), 1, x), 
         y = ifelse(is.na(y), round(nrow(d_edges_Hp_w)/2), y)) |> 
  mutate(leaf = grepl("XT|XR", name)) |> 
  
  # Expression label
  mutate(shortname2 = gsub("([DS]{1})=", "\\[\\1\\]=", shortname)) |> 
  mutate(shortname2 = gsub(",", ", ',', ", shortname2, fixed = TRUE)) |> 
  mutate(label_expr = case_when(
    !leaf ~ paste0("paste(", shortname2, ")"),
    leaf ~ paste0("paste(", shortname2, ", ':', ", expr, ")"))) |> 
  mutate(label_expr = gsub("=", ", '=', ", label_expr, fixed = TRUE)) |> 
  select(-shortname2)# |> 
  
  # # Adjusting x:
  # mutate(x = case_when(
  #   abs(x - 2) < 1e-6 ~ 1.2,
  #   #abs(x - 3) < 1e-6 ~ 1.4,
  #   TRUE ~ x
  # ))

if (FALSE) {
  ggraph(graph_Hp_w, x = x, y = y) + 
    geom_edge_link() +
    #geom_node_label(aes(filter = !leaf, label = label), size = 2) +
    geom_node_label(aes(filter = !leaf, label = label_expr), size = 2, parse = TRUE) +
    geom_node_label(aes(filter = leaf, label = label_expr, fill = group), 
                    size = 2.5, hjust = 0, 
                    parse = TRUE) +
    theme_void() +
    theme(legend.position = "bottom") +
    labs(fill = NULL) +
    guides(fill = guide_legend(override.aes = list(color = NULL))) +
    coord_cartesian(xlim = c(1, 3.5))  
  
  
  subgraph_Hp_w <- graph_Hp_w |> 
    activate(nodes) |> 
    filter(grepl("Z=0/0", name))
  ggraph(subgraph_Hp_w, x = x, y = y) + 
    geom_edge_link() +
    #geom_node_label(aes(filter = !leaf, label = label), size = 2) +
    geom_node_label(aes(filter = !leaf, label = label_expr), size = 2, parse = TRUE) +
    geom_node_label(aes(filter = leaf, label = label_expr, fill = group), size = 2.5, hjust = 0, parse = TRUE) +
    theme_void() +
    theme(legend.position = "bottom") +
    labs(fill = NULL) +
    guides(fill = guide_legend(override.aes = list(color = NULL))) +
    coord_cartesian(xlim = c(1, 3.5))  
}

###


d_edges_Hp_wTwR <- bind_rows(
  d_formulas_Hp_wTwR |> 
    mutate(from = paste0("Z=", Z)) |> 
    mutate(to = paste0("Z=", Z, ", XT=", XT, ", XR=", XR)) |> 
    arrange(Z, XT, XR) |> 
    mutate(x = 3, y = row_number()) |> 
    select(from, to, x, y, group, expr),
  d_edges_root_Hp)

graph_Hp_wTwR <- as_tbl_graph(d_edges_Hp_wTwR, directed = TRUE)
graph_Hp_wTwR <- graph_Hp_wTwR |>
  activate(nodes) |>
  left_join(d_edges_Hp_wTwR |> select(name = to, x, y), by = "name") |>
  left_join(d_edges_Hp_wTwR |> select(name = to, group, expr), by = "name") |> 
  
  #mutate(label = ifelse(is.na(expr), name, paste0(name, ": ", expr))) |> 
  mutate(shortname = gsub("^(Z[^X]*)(X.*)$", "\\2", name)) |> 
  mutate(label = ifelse(is.na(expr), shortname, paste0(shortname, ": ", expr))) |> 
  
  # Root:
  mutate(x = ifelse(is.na(x), 1, x), 
         y = ifelse(is.na(y), round(nrow(d_edges_Hp_wTwR)/2), y)) |> 
  mutate(leaf = grepl("XT|XR", name)) |> 
  
  # Expression label
  mutate(shortname2 = gsub("([DS]{1})=", "\\[\\1\\]=", shortname)) |>
  
  mutate(shortname2 = gsub(",", ", ',', ", shortname2, fixed = TRUE)) |> 
  mutate(label_expr = case_when(
    !leaf ~ paste0("paste(", shortname2, ")"),
    leaf ~ paste0("paste(", shortname2, ", ':', ", expr, ")"))) |> 
  mutate(label_expr = gsub("=", ", '=', ", label_expr, fixed = TRUE)) |> 
  
  # w_T and w_R
  mutate(label_expr = gsub("wT", "w[t]", label_expr, fixed = TRUE)) |>
  mutate(label_expr = gsub("wR", "w[r]", label_expr, fixed = TRUE)) |> 
  
  select(-shortname2)

if (FALSE) {
  ggraph(graph_Hp_wTwR, x = x, y = y) + 
    geom_edge_link() +
    #geom_node_label(aes(filter = !leaf, label = label), size = 2) +
    geom_node_label(aes(filter = !leaf, label = label_expr), size = 2, parse = TRUE) +
    geom_node_label(aes(filter = leaf, label = label_expr, fill = group), 
                    size = 2.5, hjust = 0, 
                    parse = TRUE) +
    theme_void() +
    theme(legend.position = "bottom") +
    labs(fill = NULL) +
    guides(fill = guide_legend(override.aes = list(color = NULL))) +
    coord_cartesian(xlim = c(1, 3.5))  
  
  
  subgraph_Hp_wTwR <- graph_Hp_wTwR |> 
    activate(nodes) |> 
    filter(grepl("Z=0/0", name))
  ggraph(subgraph_Hp_wTwR, x = x, y = y) + 
    geom_edge_link() +
    #geom_node_label(aes(filter = !leaf, label = label), size = 2) +
    geom_node_label(aes(filter = !leaf, label = label_expr), size = 2, parse = TRUE) +
    geom_node_label(aes(filter = leaf, label = label_expr, fill = group), size = 2.5, hjust = 0, parse = TRUE) +
    theme_void() +
    theme(legend.position = "bottom") +
    labs(fill = NULL) +
    guides(fill = guide_legend(override.aes = list(color = NULL))) +
    coord_cartesian(xlim = c(1, 3.5))  
}


# 2025-04-15
if (FALSE) {
  d_tmp <- wgsLR::d_formulas_Hp_w |> 
    filter(Z == "0/0") |> 
    group_by(group) |>
    summarise(expr = paste0(expr, collapse = " + "),
              n = n(),
              .groups = "drop") |>
    rowwise() |>
    mutate(expr = list(as_sym(expr))) |> 
    mutate(expr_chr = as.character(expr))
  d_tmp
  x <- d_tmp |> pull(expr)
  x <- x[[1]] + x[[2]] + x[[3]]
  simplify(x)
  
  
  
  
  
  d_tmp <- wgsLR::d_formulas_Hp_wTwR |> 
    filter(Z == "0/0") |> 
    group_by(group) |>
    summarise(expr = paste0(expr, collapse = " + "),
              n = n(),
              .groups = "drop") |>
    rowwise() |>
    mutate(expr = list(as_sym(expr))) |> 
    mutate(expr_chr = as.character(expr))
  d_tmp
  x <- d_tmp |> pull(expr)
  x <- x[[1]] + x[[2]] + x[[3]]
  simplify(x)
  
  
  d_tmp <- wgsLR::d_formulas_Hp_wTwR |> 
    mutate(genotype_prob = paste0("p_", Z_MA)) |> 
    group_by(group) |>
    summarise(#expr = paste0(expr, collapse = " + "),
      expr = paste0(genotype_prob, "*", expr, collapse = " + "),
              n = n(),
              .groups = "drop") |>
    rowwise() |>
    mutate(expr = list(as_sym(expr))) |> 
    mutate(expr_chr = as.character(expr))
  d_tmp
  x <- d_tmp |> pull(expr)
  x <- x[[1]] + x[[2]] + x[[3]]
  simplify(x)
  
  
  q <- 0.2
  cases <- sample_data_Hp_wTwR(n = 100000, wT = 1e-2, wR = 1e-4, p = c(q^2, 2*q*(1-q), (1-q)^2))
  table(X_D = cases$X_S, X_S = cases$X_D)
  
  set.seed(1)
  qs <- runif(10000, min = 0.1, max = 0.4)
  ps <- lapply(qs, \(q) c(q^2, 2*q*(1-q), (1-q)^2))
  cases <- sample_data_Hp_wTwR(n = 1, wT = 1e-2, wR = 1e-4, p = ps)
  table(X_D = cases$X_S, X_S = cases$X_D)
  
  
  
}


################################################################################
# Hd graph
################################################################################
y_weights_Hd <- c(0.5, rep(1, length(haps)^2 - 1), 0.5)
y_weights_qs_Hd <- (cumsum(y_weights_Hd) / sum(y_weights_Hd))[-length(y_weights_Hd)]
ys_Hd <- round(quantile(seq_len(length(haps)^4), y_weights_qs_Hd, names = FALSE))

d_edges_root_Hd_w <- expand.grid(ZR = haps, ZT = haps, XT = NA, XR = NA) |>
  mutate(from = "Z") |>
  mutate(to = paste0("ZT=", ZT, ", ZR=", ZR)) |> 
  select(from, to) |> 
  mutate(x = 2,
         y = ys_Hd)
d_edges_root_Hd_w

d_edges_Hd_w <- bind_rows(
  d_formulas_Hd_w |> 
    mutate(from = paste0("ZT=", ZT, ", ZR=", ZR)) |> 
    mutate(to = paste0("ZR=", ZR, ", ZT=", ZT, ", XT=", XT, ", XR=", XR)) |> 
    arrange(ZT, ZR, XT, XR) |> 
    mutate(x = 3, y = row_number()) |> 
    select(from, to, x, y, group, expr),
  d_edges_root_Hd_w)

graph_Hd_w <- as_tbl_graph(d_edges_Hd_w, directed = TRUE)
graph_Hd_w <- graph_Hd_w |>
  activate(nodes) |>
  left_join(d_edges_Hd_w |> select(name = to, x, y), by = "name") |>
  left_join(d_edges_Hd_w |> select(name = to, group, expr), by = "name") |> 
  
  #mutate(label = ifelse(is.na(expr), name, paste0(name, ": ", expr))) |>
  mutate(shortname = gsub("^(Z[^X]*)(X.*)$", "\\2", name)) |> 
  mutate(label = ifelse(is.na(expr), shortname, paste0(shortname, ": ", expr))) |> 
  
  # Root:
  mutate(x = ifelse(is.na(x), 1, x), 
         y = ifelse(is.na(y), round(nrow(d_edges_Hd_w)/2), y)) |> 
  mutate(leaf = grepl("XT|XR", name)) |> 
  
  # Expression label
  # FIXME: ZT / ZR parse?
  mutate(shortname2 = gsub("([DS]{1})=", "\\[\\1\\]=", shortname)) |> 
  mutate(shortname2 = gsub(",", ", ',', ", shortname2, fixed = TRUE)) |> 
  mutate(label_expr = case_when(
    !leaf ~ paste0("paste(", shortname2, ")"),
    leaf ~ paste0("paste(", shortname2, ", ':', ", expr, ")"))) |> 
  mutate(label_expr = gsub("=", ", '=', ", label_expr, fixed = TRUE)) |> 
  select(-shortname2)# |> 
  
  # # Adjusting x:
  # mutate(x = case_when(
  #   abs(x - 2) < 1e-6 ~ 1.2,
  #   #abs(x - 3) < 1e-6 ~ 1.4,
  #   TRUE ~ x
  # ))

if (FALSE) {
  ggraph(graph_Hd_w, x = x, y = y) + 
    geom_edge_link() +
    #geom_node_label(aes(filter = !leaf, label = label), size = 2) +
    geom_node_label(aes(filter = !leaf, label = label_expr), size = 2, parse = TRUE) +
    geom_node_label(aes(filter = leaf, label = label_expr, fill = group), size = 2.5, hjust = 0, 
                    parse = TRUE) +
    theme_void() +
    theme(legend.position = "bottom") +
    labs(fill = NULL) +
    guides(fill = guide_legend(override.aes = list(color = NULL))) +
    coord_cartesian(xlim = c(1, 3.5))  
  
  
  subgraph_Hd_w <- graph_Hd_w |> 
    activate(nodes) |> 
    filter(grepl("ZT=0/0", name), grepl("ZR=0/0", name))
  ggraph(subgraph_Hd_w, x = x, y = y) + 
    geom_edge_link() +
    #geom_node_label(aes(filter = !leaf, label = label), size = 2) +
    geom_node_label(aes(filter = !leaf, label = label_expr), size = 2, parse = TRUE) +
    geom_node_label(aes(filter = leaf, label = label_expr, fill = group), size = 2.5, hjust = 0, parse = TRUE) +
    theme_void() +
    theme(legend.position = "bottom") +
    labs(fill = NULL) +
    guides(fill = guide_legend(override.aes = list(color = NULL))) +
    coord_cartesian(xlim = c(1, 3.5))  
}

############


d_edges_Hd_wTwR <- bind_rows(
  d_formulas_Hd_wTwR |> 
    mutate(from = paste0("ZT=", ZT, ", ZR=", ZR)) |> 
    mutate(to = paste0("ZR=", ZR, ", ZT=", ZT, ", XT=", XT, ", XR=", XR)) |> 
    arrange(ZT, ZR, XT, XR) |> 
    mutate(x = 3, y = row_number()) |> 
    select(from, to, x, y, group, expr),
  d_edges_root_Hd_w)

graph_Hd_wTwR <- as_tbl_graph(d_edges_Hd_wTwR, directed = TRUE)
graph_Hd_wTwR <- graph_Hd_wTwR |>
  activate(nodes) |>
  left_join(d_edges_Hd_wTwR |> select(name = to, x, y), by = "name") |>
  left_join(d_edges_Hd_wTwR |> select(name = to, group, expr), by = "name") |> 
  
  #mutate(label = ifelse(is.na(expr), name, paste0(name, ": ", expr))) |>
  mutate(shortname = gsub("^(Z[^X]*)(X.*)$", "\\2", name)) |> 
  mutate(label = ifelse(is.na(expr), shortname, paste0(shortname, ": ", expr))) |> 
  
  # Root:
  mutate(x = ifelse(is.na(x), 1, x), 
         y = ifelse(is.na(y), round(nrow(d_edges_Hd_w)/2), y)) |> 
  mutate(leaf = grepl("XT|XR", name)) |> 
  
  # Expression label
  mutate(shortname2 = gsub("([DS]{1})=", "\\[\\1\\]=", shortname)) |> 
  mutate(shortname2 = gsub(",", ", ',', ", shortname2, fixed = TRUE)) |> 
  mutate(label_expr = case_when(
    !leaf ~ paste0("paste(", shortname2, ")"),
    leaf ~ paste0("paste(", shortname2, ", ':', ", expr, ")"))) |> 
  mutate(label_expr = gsub("=", ", '=', ", label_expr, fixed = TRUE)) |> 
  
  # w_t and w_r
  mutate(label_expr = gsub("wT", "w[t]", label_expr, fixed = TRUE)) |>
  mutate(label_expr = gsub("wR", "w[r]", label_expr, fixed = TRUE)) |> 
  
  select(-shortname2)# |> 

# # Adjusting x:
# mutate(x = case_when(
#   abs(x - 2) < 1e-6 ~ 1.2,
#   #abs(x - 3) < 1e-6 ~ 1.4,
#   TRUE ~ x
# ))

if (FALSE) {
  ggraph(graph_Hd_wTwR, x = x, y = y) + 
    geom_edge_link() +
    #geom_node_label(aes(filter = !leaf, label = label), size = 2) +
    geom_node_label(aes(filter = !leaf, label = label_expr), size = 2, parse = TRUE) +
    geom_node_label(aes(filter = leaf, label = label_expr, fill = group), size = 2.5, hjust = 0, 
                    parse = TRUE) +
    theme_void() +
    theme(legend.position = "bottom") +
    labs(fill = NULL) +
    guides(fill = guide_legend(override.aes = list(color = NULL))) +
    coord_cartesian(xlim = c(1, 3.5))  
  
  
  subgraph_Hd_wTwR <- graph_Hd_wTwR |> 
    activate(nodes) |> 
    filter(grepl("ZT=0/0", name), grepl("ZR=0/0", name))
  ggraph(subgraph_Hd_wTwR, x = x, y = y) + 
    geom_edge_link() +
    #geom_node_label(aes(filter = !leaf, label = label), size = 2) +
    geom_node_label(aes(filter = !leaf, label = label_expr), size = 2, parse = TRUE) +
    geom_node_label(aes(filter = leaf, label = label_expr, fill = group), size = 2.5, hjust = 0, parse = TRUE) +
    theme_void() +
    theme(legend.position = "bottom") +
    labs(fill = NULL) +
    guides(fill = guide_legend(override.aes = list(color = NULL))) +
    coord_cartesian(xlim = c(1, 3.5))  
}


################################################################################
# LR
################################################################################
d_LR_w <- d_prob_Hp_w |> 
  select(XT_MA, XR_MA, caracas_sym_Hp = caracas_sym) |> 
  inner_join(d_prob_Hd_w |> 
               select(XT_MA, XR_MA, caracas_sym_Hd = caracas_sym),
             by = c("XT_MA", "XR_MA")) |> 
  rowwise() |> 
  mutate(caracas_sym = list(caracas_sym_Hp / caracas_sym_Hd)) |> 
  select(-caracas_sym_Hp, -caracas_sym_Hd) |> 
  mutate(expr = list(as_expr(caracas_sym)),
         expr_chr = as.character(caracas_sym),
         expr_tex = tex(caracas_sym))
#d_LR_w

###

d_LR_wTwR <- d_prob_Hp_wTwR |> 
  select(XT_MA, XR_MA, caracas_sym_Hp = caracas_sym) |> 
  inner_join(d_prob_Hd_wTwR |> 
               select(XT_MA, XR_MA, caracas_sym_Hd = caracas_sym),
             by = c("XT_MA", "XR_MA")) |> 
  rowwise() |> 
  mutate(caracas_sym = list(simplify(caracas_sym_Hp / caracas_sym_Hd))) |> 
  select(-caracas_sym_Hp, -caracas_sym_Hd) |> 
  mutate(expr = list(as_expr(caracas_sym)),
         expr_chr = as.character(caracas_sym),
         expr_tex = tex(caracas_sym))


################################################################################
# Tables for simulations
################################################################################

# Hp:
d_probtable_Hp_w <- d_formulas_Hp_w |> 
  select(Z, Z1, Z2, XT, XR, expr) |> 
  mutate(Z012 = case_when(
    Z == "0/0" ~ 0L,
    Z == "1/1" ~ 2L,
    TRUE ~ 1L
  ),
  XT012 = case_when(
    XT == "0/0" ~ 0L,
    XT == "1/1" ~ 2L,
    TRUE ~ 1L
  ),
  XR012 = case_when(
    XR == "0/0" ~ 0L,
    XR == "1/1" ~ 2L,
    TRUE ~ 1L
  )) |> 
  # mutate(genoprob = case_when(
  #   Z == "0/0" ~ "q0",
  #   Z == "0/1" ~ "(q1/2)",
  #   Z == "1/0" ~ "(q1/2)",
  #   Z == "1/1" ~ "q2"
  # )) |> 
  # mutate(sampleprob = paste0(genoprob, "*", expr)) |> 
  group_by(Z012) |> 
  mutate(constant = n()/16) |> 
  ungroup() |> 
  mutate(expr = paste0("(", expr, ")/", constant)) |> 
  select(Z, Z1, Z2, Z012, XT, XT012, XR, XR012, expr) |> #, sampleprob) |> 
  as.data.frame()


## Test:
x <- split(d_probtable_Hp_w, d_probtable_Hp_w$Z012)
xx <- lapply(x, \(y1) lapply(y1$expr, \(y2) parse(text = y2)))

xxx <- lapply(xx, \(y1) lapply(y1, \(y2) eval(y2, list(w = 0.3))))
lapply(xxx, \(y1) y1 |> unlist() |> sum())

xxx <- lapply(xx, \(y1) lapply(y1, \(y2) eval(y2, list(w = 0.1))))
lapply(xxx, \(y1) y1 |> unlist() |> sum())



# Hd:
d_probtable_Hd_w <- d_formulas_Hd_w |> 
  select(ZT, ZT1, ZT2, ZR, ZR1, ZR2, XT, XR, expr) |> 
  mutate(
  ZT012 = case_when(
    ZT == "0/0" ~ 0L,
    ZT == "1/1" ~ 2L,
    TRUE ~ 1L
  ),
  ZR012 = case_when(
    ZR == "0/0" ~ 0L,
    ZR == "1/1" ~ 2L,
    TRUE ~ 1L
  ),
  XT012 = case_when(
    XT == "0/0" ~ 0L,
    XT == "1/1" ~ 2L,
    TRUE ~ 1L
  ),
  XR012 = case_when(
    XR == "0/0" ~ 0L,
    XR == "1/1" ~ 2L,
    TRUE ~ 1L
  )) |> 
  #mutate(XR = XR + 1L) |> # index
  #mutate(XT = XT + 1L) |> # index
  # mutate(genoprobD = case_when(
  #   ZT == "0/0" ~ "q0",
  #   ZT == "0/1" ~ "(q1/2)",
  #   ZT == "1/0" ~ "(q1/2)",
  #   ZT == "1/1" ~ "q2"
  # )) |> 
  # mutate(genoprobS = case_when(
  #   ZR == "0/0" ~ "q0",
  #   ZR == "0/1" ~ "(q1/2)",
  #   ZR == "1/0" ~ "(q1/2)",
  #   ZR == "1/1" ~ "q2"
  # )) |> 
  # mutate(sampleprob = paste0(genoprobD, "*", genoprobS, "*", expr)) |> 
  group_by(ZT012, ZR012) |> 
  mutate(constant = n()/16) |> 
  ungroup() |> 
  mutate(expr = paste0("(", expr, ")/", constant)) |> 
  select(ZT, ZT1, ZT2, ZT012, ZR, ZR1, ZR2, ZR012, XT, XT012, XR, XR012, expr) |> #, sampleprob) |> 
  as.data.frame()

## Test:
# x <- lapply(d_probtable_Hd$sampleprob, \(y) parse(text = y))
# lapply(x, \(y) eval(y, list(w = 0.3, q0 = 0.1, q1 = 0.8, q2 = 0.1))) |> unlist() |> sum()
# lapply(x, \(y) eval(y, list(w = 0.3, q0 = 0.9, q1 = 0.1, q2 = 0.0))) |> unlist() |> sum()
# 
# x <- lapply(subset(d_probtable_Hd, ZT == "0/0" & ZR == "0/0")$expr, \(y) parse(text = y))
# lapply(x, \(y) eval(y, list(w = 0.3))) |> unlist() |> sum()

x <- split(d_probtable_Hd_w, interaction(d_probtable_Hd_w$ZT012, d_probtable_Hd_w$ZR012))
length(x)
names(x)
x[[1]]; nrow(x[[1]])
x[[2]]; nrow(x[[2]])
str(x, 1)
xx <- lapply(x, \(y1) lapply(y1$expr, \(y2) parse(text = y2)))

xxx <- lapply(xx, \(y1) lapply(y1, \(y2) eval(y2, list(w = 0.3))))
lapply(xxx, \(y1) y1 |> unlist() |> sum()) |> unlist()

xxx <- lapply(xx, \(y1) lapply(y1, \(y2) eval(y2, list(w = 0.1))))
lapply(xxx, \(y1) y1 |> unlist() |> sum()) |> unlist()


###

d_probtable_Hp_wTwR <- d_formulas_Hp_wTwR |> 
  select(Z, Z1, Z2, XT, XR, expr) |> 
  mutate(Z012 = case_when(
    Z == "0/0" ~ 0L,
    Z == "1/1" ~ 2L,
    TRUE ~ 1L
  ),
  XT012 = case_when(
    XT == "0/0" ~ 0L,
    XT == "1/1" ~ 2L,
    TRUE ~ 1L
  ),
  XR012 = case_when(
    XR == "0/0" ~ 0L,
    XR == "1/1" ~ 2L,
    TRUE ~ 1L
  )) |> 
  # mutate(genoprob = case_when(
  #   Z == "0/0" ~ "q0",
  #   Z == "0/1" ~ "(q1/2)",
  #   Z == "1/0" ~ "(q1/2)",
  #   Z == "1/1" ~ "q2"
  # )) |> 
  # mutate(sampleprob = paste0(genoprob, "*", expr)) |> 
  group_by(Z012) |> 
  mutate(constant = n()/16) |> 
  ungroup() |> 
  mutate(expr = paste0("(", expr, ")/", constant)) |> 
  select(Z, Z1, Z2, Z012, XT, XT012, XR, XR012, expr) |> #, sampleprob) |> 
  as.data.frame()


## Test:
x <- split(d_probtable_Hp_wTwR, d_probtable_Hp_wTwR$Z012)
xx <- lapply(x, \(y1) lapply(y1$expr, \(y2) parse(text = y2)))

xxx <- lapply(xx, \(y1) lapply(y1, \(y2) eval(y2, list(wT = 0.3, wR = 0.7))))
lapply(xxx, \(y1) y1 |> unlist() |> sum())

xxx <- lapply(xx, \(y1) lapply(y1, \(y2) eval(y2, list(wT = 0.1, wR = 0.0001))))
lapply(xxx, \(y1) y1 |> unlist() |> sum())



# Hd:
d_probtable_Hd_wTwR <- d_formulas_Hd_wTwR |> 
  select(ZT, ZT1, ZT2, ZR, ZR1, ZR2, XT, XR, expr) |> 
  mutate(
    ZT012 = case_when(
      ZT == "0/0" ~ 0L,
      ZT == "1/1" ~ 2L,
      TRUE ~ 1L
    ),
    ZR012 = case_when(
      ZR == "0/0" ~ 0L,
      ZR == "1/1" ~ 2L,
      TRUE ~ 1L
    ),
    XT012 = case_when(
      XT == "0/0" ~ 0L,
      XT == "1/1" ~ 2L,
      TRUE ~ 1L
    ),
    XR012 = case_when(
      XR == "0/0" ~ 0L,
      XR == "1/1" ~ 2L,
      TRUE ~ 1L
    )) |> 
  #mutate(XR = XR + 1L) |> # index
  #mutate(XT = XT + 1L) |> # index
  # mutate(genoprobD = case_when(
  #   ZT == "0/0" ~ "q0",
  #   ZT == "0/1" ~ "(q1/2)",
  #   ZT == "1/0" ~ "(q1/2)",
  #   ZT == "1/1" ~ "q2"
  # )) |> 
  # mutate(genoprobS = case_when(
  #   ZR == "0/0" ~ "q0",
  #   ZR == "0/1" ~ "(q1/2)",
#   ZR == "1/0" ~ "(q1/2)",
#   ZR == "1/1" ~ "q2"
# )) |> 
# mutate(sampleprob = paste0(genoprobD, "*", genoprobS, "*", expr)) |> 
group_by(ZT012, ZR012) |> 
  mutate(constant = n()/16) |> 
  ungroup() |> 
  mutate(expr = paste0("(", expr, ")/", constant)) |> 
  select(ZT, ZT1, ZT2, ZT012, ZR, ZR1, ZR2, ZR012, XT, XT012, XR, XR012, expr) |> #, sampleprob) |> 
  as.data.frame()

## Test:
# x <- lapply(d_probtable_Hd$sampleprob, \(y) parse(text = y))
# lapply(x, \(y) eval(y, list(w = 0.3, q0 = 0.1, q1 = 0.8, q2 = 0.1))) |> unlist() |> sum()
# lapply(x, \(y) eval(y, list(w = 0.3, q0 = 0.9, q1 = 0.1, q2 = 0.0))) |> unlist() |> sum()
# 
# x <- lapply(subset(d_probtable_Hd, ZT == "0/0" & ZR == "0/0")$expr, \(y) parse(text = y))
# lapply(x, \(y) eval(y, list(w = 0.3))) |> unlist() |> sum()

x <- split(d_probtable_Hd_wTwR, interaction(d_probtable_Hd_wTwR$ZT012, d_probtable_Hd_wTwR$ZR012))
length(x)
names(x)
x[[1]]; nrow(x[[1]])
x[[2]]; nrow(x[[2]])
str(x, 1)
xx <- lapply(x, \(y1) lapply(y1$expr, \(y2) parse(text = y2)))

xxx <- lapply(xx, \(y1) lapply(y1, \(y2) eval(y2, list(wT = 0.3, wR = 0.7))))
lapply(xxx, \(y1) y1 |> unlist() |> sum()) |> unlist()

xxx <- lapply(xx, \(y1) lapply(y1, \(y2) eval(y2, list(wT = 0.1, wR = 0.0001))))
lapply(xxx, \(y1) y1 |> unlist() |> sum()) |> unlist()



################################################################################
# Finalise
################################################################################

# caracas_symbol cannot be shared accross R sessions
d_prob_Hp_w <- d_prob_Hp_w |> select(-caracas_sym)
d_prob_Hd_w <- d_prob_Hd_w |> select(-caracas_sym)
d_LR_w <- d_LR_w |> select(-caracas_sym)

d_prob_Hp_wTwR <- d_prob_Hp_wTwR |> select(-caracas_sym)
d_prob_Hd_wTwR <- d_prob_Hd_wTwR |> select(-caracas_sym)
d_LR_wTwR <- d_LR_wTwR |> select(-caracas_sym)

###
##########################################

finalise_df_rename <- function(d) {
  d |> 
    rename(xT = XT_MA,
           xR = XR_MA) 
}

finalise_df_fx_formula <- function(d) {
  d |> 
    mutate(expr_tex = gsub("wT", "w_t", expr_tex, fixed = TRUE),
           expr_tex = gsub("wR", "w_r", expr_tex, fixed = TRUE))
}

d_LR_w <- d_LR_w |> finalise_df_rename() |> finalise_df_fx_formula()
d_LR_wTwR <- d_LR_wTwR |> finalise_df_rename() |> finalise_df_fx_formula()
##########################################

usethis::use_data(d_formulas_single, overwrite = TRUE, version = 3, compress = "bzip2")
usethis::use_data(graph_single, overwrite = TRUE, version = 3, compress = "bzip2")

##########################################

usethis::use_data(d_prob_Hp_w, overwrite = TRUE, version = 3, compress = "bzip2")
usethis::use_data(d_formulas_Hp_w, overwrite = TRUE, version = 3, compress = "bzip2")
usethis::use_data(graph_Hp_w, overwrite = TRUE, version = 3, compress = "bzip2")

usethis::use_data(d_prob_Hd_w, overwrite = TRUE, version = 3, compress = "bzip2")
usethis::use_data(d_formulas_Hd_w, overwrite = TRUE, version = 3, compress = "bzip2")
usethis::use_data(graph_Hd_w, overwrite = TRUE, version = 3, compress = "bzip2")

usethis::use_data(d_LR_w, overwrite = TRUE, version = 3, compress = "bzip2")

usethis::use_data(d_probtable_Hp_w, overwrite = TRUE, version = 3, compress = "bzip2")
usethis::use_data(d_probtable_Hd_w, overwrite = TRUE, version = 3, compress = "bzip2")

#rm(list = ls())
##########################################

usethis::use_data(d_prob_Hp_wTwR, overwrite = TRUE, version = 3, compress = "bzip2")
usethis::use_data(d_formulas_Hp_wTwR, overwrite = TRUE, version = 3, compress = "bzip2")
usethis::use_data(graph_Hp_wTwR, overwrite = TRUE, version = 3, compress = "bzip2")

usethis::use_data(d_prob_Hd_wTwR, overwrite = TRUE, version = 3, compress = "bzip2")
usethis::use_data(d_formulas_Hd_wTwR, overwrite = TRUE, version = 3, compress = "bzip2")
usethis::use_data(graph_Hd_wTwR, overwrite = TRUE, version = 3, compress = "bzip2")

usethis::use_data(d_LR_wTwR, overwrite = TRUE, version = 3, compress = "bzip2")

usethis::use_data(d_probtable_Hp_wTwR, overwrite = TRUE, version = 3, compress = "bzip2")
usethis::use_data(d_probtable_Hd_wTwR, overwrite = TRUE, version = 3, compress = "bzip2")

