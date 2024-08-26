#' Enumeration of possibilities for single observation
"d_formulas_single"

#' Graph for enumeration of possibilities for single observation
"graph_single"

#' Enumeration of possibilities under Hp
"d_formulas_Hp"

#' Formulas under Hp grouped by XD and XS
"d_prob_Hp"

#' Graph for enumeration of possibilities under Hp
"graph_Hp"

#' Enumeration of possibilities under Hd
"d_formulas_Hd"

#' Formulas under Hd grouped by XD and XS
"d_prob_Hd"

#' Graph for enumeration of possibilities under Hd
"graph_Hd"

#' Formulas for LR grouped by XD and XS
"d_prob_LR"

#' Probabilties for efficient sampling under Hp
"d_probtable_Hp"

#' Probabilties for efficient sampling under Hd
"d_probtable_Hd"

#' Marker candidates (v1)
#' 
#' Data from The Genome Aggregation Database (gnomAD) for 
#' binary SNP loci with `rs` names, i.e., 
#' where the `rs` column starts with `rs`, 
#' there is one character in the `ref` column and one character in the `alt` column.
#' 
#' The loci are filtered by MAF values for AFR, EAS, and NFE populations. 
#' The MAF values must be within 0.1, 0.15, 0.2 from 0.5.
#' 
#' @source <https://gnomad.broadinstitute.org/>
"marker_candidates_v1"

# VCF example data
# 
# Based on marker_candidates_v1 and markers not in that, too
#"vcf_example_data_v1"
