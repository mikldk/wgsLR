library(tidyverse)
library(ggraph)
library(tidygraph)
library(caracas)

#haps <- c("0/0", "0/1", "1/0", "1/1")
haps <- c("0/0", "0/1", "1/1")

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

W_mat <- matrix(NA_character_, 3, 3)
diag(W_mat) <- "(1-w)^2"
W_mat[3, 1] <- W_mat[1, 3] <- "w^2"
W_mat[2, 1] <- W_mat[1, 2] <- W_mat[2, 3] <- W_mat[3, 2] <- "w*(1-w)"
W_mat

# W_mat <- matrix(NA_character_, 3, 3)
# W_mat[1, 1] <- "(1-w)^2"
# W_mat[2, 2] <- "2*(1-w)^2 + 2*w^2"
# W_mat[3, 3] <- "(1-w)^2"
# W_mat[3, 1] <- W_mat[1, 3] <- "w^2"
# W_mat[2, 1] <- W_mat[1, 2] <- W_mat[2, 3] <- W_mat[3, 2] <- "2*w*(1-w)"
# W_mat

# W_mat <- matrix(NA_character_, 3, 3)
# W_mat[1, 1] <- "(1-w)^2"
# W_mat[2, 2] <- "(1-w)^2 + w^2"
# W_mat[3, 3] <- "(1-w)^2"
# W_mat[3, 1] <- "w^2"
# W_mat[1, 3] <- "w^2"
# W_mat[2, 1] <- "2*w*(1-w)"
# W_mat[2, 3] <- "2*w*(1-w)"
# W_mat[1, 2] <- "w*(1-w)"
# W_mat[3, 2] <- "w*(1-w)"
# W_mat

W_mat <- matrix(NA_character_, 3, 3)
W_mat[1, 1] <- "(1-w)^2"
W_mat[2, 1] <- "2*w*(1-w)"
W_mat[3, 1] <- "w^2"

W_mat[1, 2] <- "(r^2)*w*(1-w)"
W_mat[2, 2] <- "(2*r*(1-r))*((1-w)^2 + w^2)"
W_mat[3, 2] <- "((1-r)^2)*w*(1-w)"

W_mat[1, 3] <- "w^2"
W_mat[2, 3] <- "2*w*(1-w)"
W_mat[3, 3] <- "(1-w)^2"

W_mat

W_mat_car <- as_sym(W_mat)
W_mat_car
colSums_(W_mat_car) |> simplify()


single_cases <- expand.grid(Z = haps, X = haps) 
d_formulas_single <- single_cases |> 
  separate(Z, into = c("Z1", "Z2"), sep = "/", remove = FALSE) |>
  separate(X, into = c("X1", "X2"), sep = "/", remove = FALSE) |> 
  mutate(Z_MA = as.integer(Z1) + as.integer(Z2),
         X_MA = as.integer(X1) + as.integer(X2)) |> 
  #rowwise() |> 
  #mutate(num_no_err = sum(X1 == Z1) + sum(X2 == Z2)) |>
  #mutate(num_err = 2L - num_no_err) |> 
  rowwise() |> 
  mutate(expr = W_mat[Z_MA + 1L, X_MA + 1L]) |> 
  
  #mutate(expr = paste0("((1-w)^", num_no_err, ")*(w^", num_err, ")")) |>
  # mutate(expr = case_when(
  # 
  #   # num_no_err == 1L & num_err == 1L ~ paste0("w*(1-w)"),
  #   # num_no_err == 0L ~ paste0("w^", num_err),
  #   # num_err  == 0L ~ paste0("(1-w)^", num_no_err),
  #   
  #   
  #   num_no_err == 1L & num_err == 1L ~ paste0("w*(1-w)"),
  #   num_no_err == 0L ~ paste0("w^", num_err),
  #   num_err  == 0L ~ paste0("(1-w)^", num_no_err),
  #   
  #   
  #   TRUE ~ "UNKNOWN")) |> 
  # 
  # mutate(expr = case_when(
  #   X_MA > Z_MA ~ paste0("r1*", expr),
  #   X_MA < Z_MA ~ paste0("r2*", expr),
  #   TRUE ~ expr
  # )) |> 
  ungroup() 
d_formulas_single

d_formulas_single |> 
  #rowwise() |>
  #mutate(caracas_sym = list(as_sym(expr))) |> 
  # mutate(
  #   n = case_when(
  #     X_MA == 0 ~ 1/3,
  #     X_MA == 1 ~ 2/3,
  #     X_MA == 2 ~ 1/3
  #   )) |>
  group_by(X_MA) |> 
  summarise(
    #expr = as.character(sum(caracas_sym[[1L]])),
    #n = n(),
    #n = sum(n),
    
    expr = paste0( paste0("(", expr, ")"), collapse = "+"), 
    .groups = "drop"
  ) |> 
  #mutate(expr = paste0("(", expr, ")/(", n, ")")) |>
  rowwise() |> 
  mutate(expr_car = list(as_sym(expr))) |> 
  mutate(expr_sim = list(expr_car |> simplify())) |> 
  mutate(expr_car = as.character(expr_car)) |> 
  mutate(expr_chr = as.character(expr_sim)) |> 
  #select(X_MA, expr_car, expr_chr) |>
  select(X_MA, expr_chr) |> 
  print(width = Inf)

d_formulas_single |> filter(X_MA == 0) |> pull(expr) |> as_sym() |> sum() |> simplify()
d_formulas_single |> filter(X_MA == 1) |> pull(expr) |> as_sym() |> sum() |> simplify()
d_formulas_single |> filter(X_MA == 2) |> pull(expr) |> as_sym() |> sum() |> simplify()


#d_formulas_single |>   add_group()

####

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


p <- ggraph(graph_single, x = x, y = y) + 
  geom_edge_link() +
  geom_node_label(aes(filter = !leaf, label = label), size = 2) +
  geom_node_label(aes(filter = leaf, label = label_expr), size = 2.5, hjust = 0, parse = TRUE) +
  theme_void() +
  theme(legend.position = "bottom") +
  labs(fill = NULL) +
  guides(fill = guide_legend(override.aes = list(color = NULL))) +
  coord_cartesian(xlim = c(1, 3.5))  
print(p)

