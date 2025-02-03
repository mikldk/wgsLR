library(tidyverse)
library(ggraph)
library(tidygraph)
library(caracas)

haps <- c("0/0", "0/1", "1/0", "1/1")

add_group <- function(d) {
  d |> 
    mutate(group = case_when(
      XD_MA == XS_MA ~ "diag",
      abs(XD_MA - XS_MA) == 1L ~ "off-diag",
      abs(XD_MA - XS_MA) == 2L ~ "corners",
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

add_expr_wDwS <- function(d) {
  d |> 
    #mutate(expr_D = paste0("((1-wD)^", num_no_err_D, ")*(wD^", num_err_D, ")")) |>
    mutate(expr_D = case_when(
      num_no_err_D == 1L & num_err_D == 1L ~ paste0("wD*(1-wD)"),
      
      num_no_err_D == 0L ~ paste0("wD^", num_err_D),
      num_err_D  == 0L ~ paste0("(1-wD)^", num_no_err_D),
      
      num_no_err_D == 1L ~ paste0("(1-wD)", " * wD^", num_err_D, ""),
      num_err_D == 1L ~ paste0("(1-wD)^", num_no_err_D, " * wD", ""),
      
      num_no_err_D == 1L ~ paste0("(1-wD)", " * wD^", num_err_D, ""),
      num_err_D == 1L ~ paste0("(1-wD)^", num_no_err_D, " * wD", ""),
      
      #TRUE ~ paste0("((1-wD)^", num_no_err_D, ")*(wD^", num_err_D, ")"))) |>
      TRUE ~ paste0("(1-wD)^", num_no_err_D, " * wD^", num_err_D, ""))) |> 
      
      #mutate(expr_S = paste0("((1-wS)^", num_no_err_S, ")*(wS^", num_err_S, ")")) |>
      mutate(expr_S = case_when(
        num_no_err_S == 1L & num_err_S == 1L ~ paste0("wS*(1-wS)"),
        
        num_no_err_S == 0L ~ paste0("wS^", num_err_S),
        num_err_S  == 0L ~ paste0("(1-wS)^", num_no_err_S),
        
        num_no_err_S == 1L ~ paste0("(1-wS)", " * wS^", num_err_S, ""),
        num_err_S == 1L ~ paste0("(1-wS)^", num_no_err_S, " * wS", ""),
        
        #TRUE ~ paste0("((1-wS)^", num_no_err_S, ")*(wS^", num_err_S, ")"))) |>
        TRUE ~ paste0("(1-wS)^", num_no_err_S, " * wS^", num_err_S, ""))) |> 
      
      mutate(expr = paste0(expr_D, "*", expr_S))
}

################################################################################
# Hp
################################################################################

H_p_cases <- expand.grid(Z = haps, XD = haps, XS = haps) 

d_formulas_Hp_w <- H_p_cases |> 
  separate(Z, into = c("Z1", "Z2"), sep = "/", remove = FALSE) |>
  separate(XD, into = c("XD1", "XD2"), sep = "/", remove = FALSE) |> 
  separate(XS, into = c("XS1", "XS2"), sep = "/", remove = FALSE) |> 
  rowwise() |> 
  mutate(num_no_err = sum(XD1 == Z1) + sum(XD2 == Z2) +
                      sum(XS1 == Z1) + sum(XS2 == Z2)) |>
  mutate(num_err = 4L - num_no_err) |> 
  #mutate(expr = paste0("((1-w)^", num_no_err, ")*(w^", num_err, ")")) |>
  
  add_expr_w() |> 
  
  ungroup() |> 
  mutate(Z_MA = as.integer(Z1) + as.integer(Z2),
         XD_MA = as.integer(XD1) + as.integer(XD2),
         XS_MA = as.integer(XS1) + as.integer(XS2)) |> 
  add_group()

d_prob_Hp_w <- d_formulas_Hp_w |> 
  select(Z_MA, XD_MA, XS_MA, expr) |> 
  
  # p_1 is for both 0/1 and 1/0, so only take half for those cases
  mutate(weight = ifelse(Z_MA == 1L, 2, 1)) |> 
  
  mutate(caracas_sym = paste0("(p_", Z_MA, "/", weight, ") * ", expr)) |>
  
  group_by(XD_MA, XS_MA) |> 
  summarise(caracas_sym = paste0(caracas_sym, collapse = " + "),
            .groups = "drop") |>
  rowwise() |>
  mutate(caracas_sym = list(as_sym(caracas_sym))) |>
  mutate(expr = list(as_expr(caracas_sym)),
         expr_chr = as.character(caracas_sym),
         expr_tex = tex(caracas_sym))



d_formulas_Hp_wDwS <- H_p_cases |> 
  separate(Z, into = c("Z1", "Z2"), sep = "/", remove = FALSE) |>
  separate(XD, into = c("XD1", "XD2"), sep = "/", remove = FALSE) |> 
  separate(XS, into = c("XS1", "XS2"), sep = "/", remove = FALSE) |> 
  rowwise() |> 
  mutate(num_no_err_D = sum(XD1 == Z1) + sum(XD2 == Z2)) |>
  mutate(num_no_err_S = sum(XS1 == Z1) + sum(XS2 == Z2)) |>
  mutate(num_err_D = 2L - num_no_err_D) |>
  mutate(num_err_S = 2L - num_no_err_S) |> 
  
  add_expr_wDwS() |> 
  
  ungroup() |> 
  mutate(Z_MA = as.integer(Z1) + as.integer(Z2),
         XD_MA = as.integer(XD1) + as.integer(XD2),
         XS_MA = as.integer(XS1) + as.integer(XS2)) |> 
  add_group()


d_prob_Hp_wDwS <- d_formulas_Hp_wDwS |> 
  select(Z_MA, XD_MA, XS_MA, expr) |> 
  
  # p_1 is for both 0/1 and 1/0, so only take half for those cases
  mutate(weight = ifelse(Z_MA == 1L, 2, 1)) |> 
  
  mutate(caracas_sym = paste0("(p_", Z_MA, "/", weight, ") * ", expr)) |>
  
  group_by(XD_MA, XS_MA) |> 
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

H_d_cases <- expand.grid(ZD = haps, ZS = haps, XD = haps, XS = haps) 

d_formulas_Hd_w <- H_d_cases |> 
  separate(ZD, into = c("ZD1", "ZD2"), sep = "/", remove = FALSE) |>
  separate(ZS, into = c("ZS1", "ZS2"), sep = "/", remove = FALSE) |> 
  separate(XD, into = c("XD1", "XD2"), sep = "/", remove = FALSE) |> 
  separate(XS, into = c("XS1", "XS2"), sep = "/", remove = FALSE) |> 
  rowwise() |> 
  mutate(num_no_err = sum(XD1 == ZD1) + sum(XD2 == ZD2) +
                      sum(XS1 == ZS1) + sum(XS2 == ZS2)) |>
  mutate(num_err = 4L - num_no_err) |> 
  #mutate(expr = paste0("((1-w)^", num_no_err, ")*(w^", num_err, ")")) |>
  
  add_expr_w() |> 
  
  ungroup() |> 
  mutate(ZD_MA = as.integer(ZD1) + as.integer(ZD2),
         ZS_MA = as.integer(ZS1) + as.integer(ZS2),
         XD_MA = as.integer(XD1) + as.integer(XD2),
         XS_MA = as.integer(XS1) + as.integer(XS2)) |> 
  add_group()

d_prob_Hd_w <- d_formulas_Hd_w |> 
  select(ZD_MA, ZS_MA, XD_MA, XS_MA, expr) |> 
  
  # p_1 is for both 0/1 and 1/0, so only take half for those cases
  mutate(weight_D = ifelse(ZD_MA == 1L, 2, 1),
         weight_S = ifelse(ZS_MA == 1L, 2, 1)) |> 
  
  mutate(caracas_sym = paste0("(p_", ZD_MA, "/", weight_D, ") * (", 
                        "p_", ZS_MA, "/", weight_S, ") * ", expr)) |>
  
  group_by(XD_MA, XS_MA) |> 
  summarise(caracas_sym = paste0(caracas_sym, collapse = " + "),
            .groups = "drop") |>
  rowwise() |>
  mutate(caracas_sym = list(as_sym(caracas_sym))) |>
  mutate(expr = list(as_expr(caracas_sym)),
         expr_chr = as.character(caracas_sym),
         expr_tex = tex(caracas_sym))


##

d_formulas_Hd_wDwS <- H_d_cases |> 
  separate(ZD, into = c("ZD1", "ZD2"), sep = "/", remove = FALSE) |>
  separate(ZS, into = c("ZS1", "ZS2"), sep = "/", remove = FALSE) |> 
  separate(XD, into = c("XD1", "XD2"), sep = "/", remove = FALSE) |> 
  separate(XS, into = c("XS1", "XS2"), sep = "/", remove = FALSE) |> 
  rowwise() |> 
  mutate(num_no_err_D = sum(XD1 == ZD1) + sum(XD2 == ZD2)) |>
  mutate(num_no_err_S = sum(XS1 == ZS1) + sum(XS2 == ZS2)) |>
  mutate(num_err_D = 2L - num_no_err_D) |>
  mutate(num_err_S = 2L - num_no_err_S) |> 
  
  add_expr_wDwS() |> 
 
  ungroup() |> 
  mutate(ZD_MA = as.integer(ZD1) + as.integer(ZD2),
         ZS_MA = as.integer(ZS1) + as.integer(ZS2),
         XD_MA = as.integer(XD1) + as.integer(XD2),
         XS_MA = as.integer(XS1) + as.integer(XS2)) |> 
  add_group()

d_prob_Hd_wDwS <- d_formulas_Hd_wDwS |> 
  select(ZD_MA, ZS_MA, XD_MA, XS_MA, expr) |> 
  
  # p_1 is for both 0/1 and 1/0, so only take half for those cases
  mutate(weight_D = ifelse(ZD_MA == 1L, 2, 1),
         weight_S = ifelse(ZS_MA == 1L, 2, 1)) |> 
  
  mutate(caracas_sym = paste0("(p_", ZD_MA, "/", weight_D, ") * (", 
                              "p_", ZS_MA, "/", weight_S, ") * ", expr)) |>
  
  group_by(XD_MA, XS_MA) |> 
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

d_edges_root_Hp <- expand.grid(Z = haps, XD = NA, XS = NA) |>
  mutate(from = "Z") |>
  mutate(to = paste0("Z=", Z)) |> 
  select(from, to) |> 
  mutate(x = 2,
         y = ys_Hp)

d_edges_Hp_w <- bind_rows(
  d_formulas_Hp_w |> 
    mutate(from = paste0("Z=", Z)) |> 
    mutate(to = paste0("Z=", Z, ", XD=", XD, ", XS=", XS)) |> 
    arrange(Z, XD, XS) |> 
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
  mutate(leaf = grepl("XD|XS", name)) |> 
  
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


d_edges_Hp_wDwS <- bind_rows(
  d_formulas_Hp_wDwS |> 
    mutate(from = paste0("Z=", Z)) |> 
    mutate(to = paste0("Z=", Z, ", XD=", XD, ", XS=", XS)) |> 
    arrange(Z, XD, XS) |> 
    mutate(x = 3, y = row_number()) |> 
    select(from, to, x, y, group, expr),
  d_edges_root_Hp)

graph_Hp_wDwS <- as_tbl_graph(d_edges_Hp_wDwS, directed = TRUE)
graph_Hp_wDwS <- graph_Hp_wDwS |>
  activate(nodes) |>
  left_join(d_edges_Hp_wDwS |> select(name = to, x, y), by = "name") |>
  left_join(d_edges_Hp_wDwS |> select(name = to, group, expr), by = "name") |> 
  
  #mutate(label = ifelse(is.na(expr), name, paste0(name, ": ", expr))) |> 
  mutate(shortname = gsub("^(Z[^X]*)(X.*)$", "\\2", name)) |> 
  mutate(label = ifelse(is.na(expr), shortname, paste0(shortname, ": ", expr))) |> 
  
  # Root:
  mutate(x = ifelse(is.na(x), 1, x), 
         y = ifelse(is.na(y), round(nrow(d_edges_Hp_wDwS)/2), y)) |> 
  mutate(leaf = grepl("XD|XS", name)) |> 
  
  # Expression label
  mutate(shortname2 = gsub("([DS]{1})=", "\\[\\1\\]=", shortname)) |>
  
  mutate(shortname2 = gsub(",", ", ',', ", shortname2, fixed = TRUE)) |> 
  mutate(label_expr = case_when(
    !leaf ~ paste0("paste(", shortname2, ")"),
    leaf ~ paste0("paste(", shortname2, ", ':', ", expr, ")"))) |> 
  mutate(label_expr = gsub("=", ", '=', ", label_expr, fixed = TRUE)) |> 
  
  # w_D and w_S
  mutate(label_expr = gsub("wD", "w[D]", label_expr, fixed = TRUE)) |>
  mutate(label_expr = gsub("wS", "w[S]", label_expr, fixed = TRUE)) |> 
  
  select(-shortname2)

if (FALSE) {
  ggraph(graph_Hp_wDwS, x = x, y = y) + 
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
  
  
  subgraph_Hp_wDwS <- graph_Hp_wDwS |> 
    activate(nodes) |> 
    filter(grepl("Z=0/0", name))
  ggraph(subgraph_Hp_wDwS, x = x, y = y) + 
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
# Hd graph
################################################################################
y_weights_Hd <- c(0.5, rep(1, length(haps)^2 - 1), 0.5)
y_weights_qs_Hd <- (cumsum(y_weights_Hd) / sum(y_weights_Hd))[-length(y_weights_Hd)]
ys_Hd <- round(quantile(seq_len(length(haps)^4), y_weights_qs_Hd, names = FALSE))

d_edges_root_Hd_w <- expand.grid(ZS = haps, ZD = haps, XD = NA, XS = NA) |>
  mutate(from = "Z") |>
  mutate(to = paste0("ZD=", ZD, ", ZS=", ZS)) |> 
  select(from, to) |> 
  mutate(x = 2,
         y = ys_Hd)
d_edges_root_Hd_w

d_edges_Hd_w <- bind_rows(
  d_formulas_Hd_w |> 
    mutate(from = paste0("ZD=", ZD, ", ZS=", ZS)) |> 
    mutate(to = paste0("ZS=", ZS, ", ZD=", ZD, ", XD=", XD, ", XS=", XS)) |> 
    arrange(ZD, ZS, XD, XS) |> 
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
  mutate(leaf = grepl("XD|XS", name)) |> 
  
  # Expression label
  # FIXME: ZD / ZS parse?
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
    filter(grepl("ZD=0/0", name), grepl("ZS=0/0", name))
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


d_edges_Hd_wDwS <- bind_rows(
  d_formulas_Hd_wDwS |> 
    mutate(from = paste0("ZD=", ZD, ", ZS=", ZS)) |> 
    mutate(to = paste0("ZS=", ZS, ", ZD=", ZD, ", XD=", XD, ", XS=", XS)) |> 
    arrange(ZD, ZS, XD, XS) |> 
    mutate(x = 3, y = row_number()) |> 
    select(from, to, x, y, group, expr),
  d_edges_root_Hd_w)

graph_Hd_wDwS <- as_tbl_graph(d_edges_Hd_wDwS, directed = TRUE)
graph_Hd_wDwS <- graph_Hd_wDwS |>
  activate(nodes) |>
  left_join(d_edges_Hd_wDwS |> select(name = to, x, y), by = "name") |>
  left_join(d_edges_Hd_wDwS |> select(name = to, group, expr), by = "name") |> 
  
  #mutate(label = ifelse(is.na(expr), name, paste0(name, ": ", expr))) |>
  mutate(shortname = gsub("^(Z[^X]*)(X.*)$", "\\2", name)) |> 
  mutate(label = ifelse(is.na(expr), shortname, paste0(shortname, ": ", expr))) |> 
  
  # Root:
  mutate(x = ifelse(is.na(x), 1, x), 
         y = ifelse(is.na(y), round(nrow(d_edges_Hd_w)/2), y)) |> 
  mutate(leaf = grepl("XD|XS", name)) |> 
  
  # Expression label
  mutate(shortname2 = gsub("([DS]{1})=", "\\[\\1\\]=", shortname)) |> 
  mutate(shortname2 = gsub(",", ", ',', ", shortname2, fixed = TRUE)) |> 
  mutate(label_expr = case_when(
    !leaf ~ paste0("paste(", shortname2, ")"),
    leaf ~ paste0("paste(", shortname2, ", ':', ", expr, ")"))) |> 
  mutate(label_expr = gsub("=", ", '=', ", label_expr, fixed = TRUE)) |> 
  
  # w_D and w_S
  mutate(label_expr = gsub("wD", "w[D]", label_expr, fixed = TRUE)) |>
  mutate(label_expr = gsub("wS", "w[S]", label_expr, fixed = TRUE)) |> 
  
  select(-shortname2)# |> 

# # Adjusting x:
# mutate(x = case_when(
#   abs(x - 2) < 1e-6 ~ 1.2,
#   #abs(x - 3) < 1e-6 ~ 1.4,
#   TRUE ~ x
# ))

if (FALSE) {
  ggraph(graph_Hd_wDwS, x = x, y = y) + 
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
  
  
  subgraph_Hd_wDwS <- graph_Hd_wDwS |> 
    activate(nodes) |> 
    filter(grepl("ZD=0/0", name), grepl("ZS=0/0", name))
  ggraph(subgraph_Hd_wDwS, x = x, y = y) + 
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
d_prob_LR_w <- d_prob_Hp_w |> 
  select(XD_MA, XS_MA, caracas_sym_Hp = caracas_sym) |> 
  inner_join(d_prob_Hd_w |> 
               select(XD_MA, XS_MA, caracas_sym_Hd = caracas_sym),
             by = c("XD_MA", "XS_MA")) |> 
  rowwise() |> 
  mutate(caracas_sym = list(caracas_sym_Hp / caracas_sym_Hd)) |> 
  select(-caracas_sym_Hp, -caracas_sym_Hd) |> 
  mutate(expr = list(as_expr(caracas_sym)),
         expr_chr = as.character(caracas_sym),
         expr_tex = tex(caracas_sym))
#d_prob_LR

###

d_prob_LR_wDwS <- d_prob_Hp_wDwS |> 
  select(XD_MA, XS_MA, caracas_sym_Hp = caracas_sym) |> 
  inner_join(d_prob_Hd_wDwS |> 
               select(XD_MA, XS_MA, caracas_sym_Hd = caracas_sym),
             by = c("XD_MA", "XS_MA")) |> 
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
  select(Z, Z1, Z2, XD, XS, expr) |> 
  mutate(Z012 = case_when(
    Z == "0/0" ~ 0L,
    Z == "1/1" ~ 2L,
    TRUE ~ 1L
  ),
  XD012 = case_when(
    XD == "0/0" ~ 0L,
    XD == "1/1" ~ 2L,
    TRUE ~ 1L
  ),
  XS012 = case_when(
    XS == "0/0" ~ 0L,
    XS == "1/1" ~ 2L,
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
  select(Z, Z1, Z2, Z012, XD, XD012, XS, XS012, expr) |> #, sampleprob) |> 
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
  select(ZD, ZD1, ZD2, ZS, ZS1, ZS2, XD, XS, expr) |> 
  mutate(
  ZD012 = case_when(
    ZD == "0/0" ~ 0L,
    ZD == "1/1" ~ 2L,
    TRUE ~ 1L
  ),
  ZS012 = case_when(
    ZS == "0/0" ~ 0L,
    ZS == "1/1" ~ 2L,
    TRUE ~ 1L
  ),
  XD012 = case_when(
    XD == "0/0" ~ 0L,
    XD == "1/1" ~ 2L,
    TRUE ~ 1L
  ),
  XS012 = case_when(
    XS == "0/0" ~ 0L,
    XS == "1/1" ~ 2L,
    TRUE ~ 1L
  )) |> 
  #mutate(XS = XS + 1L) |> # index
  #mutate(XD = XD + 1L) |> # index
  # mutate(genoprobD = case_when(
  #   ZD == "0/0" ~ "q0",
  #   ZD == "0/1" ~ "(q1/2)",
  #   ZD == "1/0" ~ "(q1/2)",
  #   ZD == "1/1" ~ "q2"
  # )) |> 
  # mutate(genoprobS = case_when(
  #   ZS == "0/0" ~ "q0",
  #   ZS == "0/1" ~ "(q1/2)",
  #   ZS == "1/0" ~ "(q1/2)",
  #   ZS == "1/1" ~ "q2"
  # )) |> 
  # mutate(sampleprob = paste0(genoprobD, "*", genoprobS, "*", expr)) |> 
  group_by(ZD012, ZS012) |> 
  mutate(constant = n()/16) |> 
  ungroup() |> 
  mutate(expr = paste0("(", expr, ")/", constant)) |> 
  select(ZD, ZD1, ZD2, ZD012, ZS, ZS1, ZS2, ZS012, XD, XD012, XS, XS012, expr) |> #, sampleprob) |> 
  as.data.frame()

## Test:
# x <- lapply(d_probtable_Hd$sampleprob, \(y) parse(text = y))
# lapply(x, \(y) eval(y, list(w = 0.3, q0 = 0.1, q1 = 0.8, q2 = 0.1))) |> unlist() |> sum()
# lapply(x, \(y) eval(y, list(w = 0.3, q0 = 0.9, q1 = 0.1, q2 = 0.0))) |> unlist() |> sum()
# 
# x <- lapply(subset(d_probtable_Hd, ZD == "0/0" & ZS == "0/0")$expr, \(y) parse(text = y))
# lapply(x, \(y) eval(y, list(w = 0.3))) |> unlist() |> sum()

x <- split(d_probtable_Hd_w, interaction(d_probtable_Hd_w$ZD012, d_probtable_Hd_w$ZS012))
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

d_probtable_Hp_wDwS <- d_formulas_Hp_wDwS |> 
  select(Z, Z1, Z2, XD, XS, expr) |> 
  mutate(Z012 = case_when(
    Z == "0/0" ~ 0L,
    Z == "1/1" ~ 2L,
    TRUE ~ 1L
  ),
  XD012 = case_when(
    XD == "0/0" ~ 0L,
    XD == "1/1" ~ 2L,
    TRUE ~ 1L
  ),
  XS012 = case_when(
    XS == "0/0" ~ 0L,
    XS == "1/1" ~ 2L,
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
  select(Z, Z1, Z2, Z012, XD, XD012, XS, XS012, expr) |> #, sampleprob) |> 
  as.data.frame()


## Test:
x <- split(d_probtable_Hp_wDwS, d_probtable_Hp_wDwS$Z012)
xx <- lapply(x, \(y1) lapply(y1$expr, \(y2) parse(text = y2)))

xxx <- lapply(xx, \(y1) lapply(y1, \(y2) eval(y2, list(wD = 0.3, wS = 0.7))))
lapply(xxx, \(y1) y1 |> unlist() |> sum())

xxx <- lapply(xx, \(y1) lapply(y1, \(y2) eval(y2, list(wD = 0.1, wS = 0.0001))))
lapply(xxx, \(y1) y1 |> unlist() |> sum())



# Hd:
d_probtable_Hd_wDwS <- d_formulas_Hd_wDwS |> 
  select(ZD, ZD1, ZD2, ZS, ZS1, ZS2, XD, XS, expr) |> 
  mutate(
    ZD012 = case_when(
      ZD == "0/0" ~ 0L,
      ZD == "1/1" ~ 2L,
      TRUE ~ 1L
    ),
    ZS012 = case_when(
      ZS == "0/0" ~ 0L,
      ZS == "1/1" ~ 2L,
      TRUE ~ 1L
    ),
    XD012 = case_when(
      XD == "0/0" ~ 0L,
      XD == "1/1" ~ 2L,
      TRUE ~ 1L
    ),
    XS012 = case_when(
      XS == "0/0" ~ 0L,
      XS == "1/1" ~ 2L,
      TRUE ~ 1L
    )) |> 
  #mutate(XS = XS + 1L) |> # index
  #mutate(XD = XD + 1L) |> # index
  # mutate(genoprobD = case_when(
  #   ZD == "0/0" ~ "q0",
  #   ZD == "0/1" ~ "(q1/2)",
  #   ZD == "1/0" ~ "(q1/2)",
  #   ZD == "1/1" ~ "q2"
  # )) |> 
  # mutate(genoprobS = case_when(
  #   ZS == "0/0" ~ "q0",
  #   ZS == "0/1" ~ "(q1/2)",
#   ZS == "1/0" ~ "(q1/2)",
#   ZS == "1/1" ~ "q2"
# )) |> 
# mutate(sampleprob = paste0(genoprobD, "*", genoprobS, "*", expr)) |> 
group_by(ZD012, ZS012) |> 
  mutate(constant = n()/16) |> 
  ungroup() |> 
  mutate(expr = paste0("(", expr, ")/", constant)) |> 
  select(ZD, ZD1, ZD2, ZD012, ZS, ZS1, ZS2, ZS012, XD, XD012, XS, XS012, expr) |> #, sampleprob) |> 
  as.data.frame()

## Test:
# x <- lapply(d_probtable_Hd$sampleprob, \(y) parse(text = y))
# lapply(x, \(y) eval(y, list(w = 0.3, q0 = 0.1, q1 = 0.8, q2 = 0.1))) |> unlist() |> sum()
# lapply(x, \(y) eval(y, list(w = 0.3, q0 = 0.9, q1 = 0.1, q2 = 0.0))) |> unlist() |> sum()
# 
# x <- lapply(subset(d_probtable_Hd, ZD == "0/0" & ZS == "0/0")$expr, \(y) parse(text = y))
# lapply(x, \(y) eval(y, list(w = 0.3))) |> unlist() |> sum()

x <- split(d_probtable_Hd_wDwS, interaction(d_probtable_Hd_wDwS$ZD012, d_probtable_Hd_wDwS$ZS012))
length(x)
names(x)
x[[1]]; nrow(x[[1]])
x[[2]]; nrow(x[[2]])
str(x, 1)
xx <- lapply(x, \(y1) lapply(y1$expr, \(y2) parse(text = y2)))

xxx <- lapply(xx, \(y1) lapply(y1, \(y2) eval(y2, list(wD = 0.3, wS = 0.7))))
lapply(xxx, \(y1) y1 |> unlist() |> sum()) |> unlist()

xxx <- lapply(xx, \(y1) lapply(y1, \(y2) eval(y2, list(wD = 0.1, wS = 0.0001))))
lapply(xxx, \(y1) y1 |> unlist() |> sum()) |> unlist()



################################################################################
# Finalise
################################################################################
# caracas_symbol cannot be shared accross R sessions
d_prob_Hp_w <- d_prob_Hp_w |> select(-caracas_sym)
d_prob_Hd_w <- d_prob_Hd_w |> select(-caracas_sym)
d_prob_LR_w <- d_prob_LR_w |> select(-caracas_sym)

d_prob_Hp_wDwS <- d_prob_Hp_wDwS |> select(-caracas_sym)
d_prob_Hd_wDwS <- d_prob_Hd_wDwS |> select(-caracas_sym)
d_prob_LR_wDwS <- d_prob_LR_wDwS |> select(-caracas_sym)

###

usethis::use_data(d_formulas_single, overwrite = TRUE, version = 3, compress = "bzip2")
usethis::use_data(graph_single, overwrite = TRUE, version = 3, compress = "bzip2")

##########################################

usethis::use_data(d_prob_Hp_w, overwrite = TRUE, version = 3, compress = "bzip2")
usethis::use_data(d_formulas_Hp_w, overwrite = TRUE, version = 3, compress = "bzip2")
usethis::use_data(graph_Hp_w, overwrite = TRUE, version = 3, compress = "bzip2")

usethis::use_data(d_prob_Hd_w, overwrite = TRUE, version = 3, compress = "bzip2")
usethis::use_data(d_formulas_Hd_w, overwrite = TRUE, version = 3, compress = "bzip2")
usethis::use_data(graph_Hd_w, overwrite = TRUE, version = 3, compress = "bzip2")

usethis::use_data(d_prob_LR_w, overwrite = TRUE, version = 3, compress = "bzip2")

usethis::use_data(d_probtable_Hp_w, overwrite = TRUE, version = 3, compress = "bzip2")
usethis::use_data(d_probtable_Hd_w, overwrite = TRUE, version = 3, compress = "bzip2")

#rm(list = ls())
##########################################

usethis::use_data(d_prob_Hp_wDwS, overwrite = TRUE, version = 3, compress = "bzip2")
usethis::use_data(d_formulas_Hp_wDwS, overwrite = TRUE, version = 3, compress = "bzip2")
usethis::use_data(graph_Hp_wDwS, overwrite = TRUE, version = 3, compress = "bzip2")

usethis::use_data(d_prob_Hd_wDwS, overwrite = TRUE, version = 3, compress = "bzip2")
usethis::use_data(d_formulas_Hd_wDwS, overwrite = TRUE, version = 3, compress = "bzip2")
usethis::use_data(graph_Hd_wDwS, overwrite = TRUE, version = 3, compress = "bzip2")

usethis::use_data(d_prob_LR_wDwS, overwrite = TRUE, version = 3, compress = "bzip2")

usethis::use_data(d_probtable_Hp_wDwS, overwrite = TRUE, version = 3, compress = "bzip2")
usethis::use_data(d_probtable_Hd_wDwS, overwrite = TRUE, version = 3, compress = "bzip2")

