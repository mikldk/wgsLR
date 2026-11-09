#' Convert GT field from 0/0, 0/1, 0|1 etc. to 0, 1, 2
#' 
#' @examples
#' GT_convert_to_012("0/0")
#' GT_convert_to_012("1|0")
#' GT_convert_to_012("1/0")
#' GT_convert_to_012("0/1")
#' GT_convert_to_012("1/1")
#' # GT_convert_to_012("1/2")
#' 
#' @export
GT_convert_to_012 <- function(x) {
  x <- gsub("|", "/", x, fixed = TRUE)
  
  x[x == "1/0"] <- "0/1"
  
  x_unq <- unique(x)
  if (length(x_unq) >= 4L) {
    
  }
  
  if (isTRUE(any(is.na(match(x_unq, c("0/0", "0/1", "1/1")))))) {
    stop("Only expected 0/0, 0/1, 1/1 (after unphasing etc.).")
  }
  
  new_x <- rep(NA_integer_, length(x))
  new_x[x == "0/0"] <- 0L
  new_x[x == "0/1"] <- 1L
  new_x[x == "1/1"] <- 2L
  
  if (any(is.na(new_x))) {
    stop("Unexpected NA's")
  }
  
  new_x
}


#' Get profile sets from cM threshold
#' 
#' @param d dataset with columns `Chr`, `Position` and `cM`
#' @param cM_thres minimal `cM` between markers
#' @param n number of random profile sets to generate
#' 
#' @export
generate_profilesets <- function(d, cM_thres, n) {
  stopifnot(all(c("Chr", "Position", "cM") %in% colnames(d)))
  
  chromlst <- split(d, d$Chr)
  
  if (length(chromlst) == 0L) {
    stop("No chromosomes?")
  }
  
  res_lst <- lapply(seq_len(n), \(marker_set_no) {
      
    y <- lapply(chromlst, \(x) {
        idx_chosen <- sample(x = seq_len(nrow(x)), size = 1L)
        idx_pos <- x$Position[idx_chosen]
        idx_cM <- x$cM[idx_chosen]
        x_cand_left <- subset(x, abs(cM - idx_cM) >= cM_thres)
        
        SNPs_chosen <- c(idx_pos)
        
        while (nrow(x_cand_left) >= 1L) {
          idx_chosen <- sample(x = seq_len(nrow(x_cand_left)), size = 1L)
          idx_pos <- x_cand_left$Position[idx_chosen]
          idx_cM <- x_cand_left$cM[idx_chosen]
          x_cand_left <- subset(x_cand_left, abs(cM - idx_cM) >= cM_thres)
          
          SNPs_chosen <- c(SNPs_chosen, idx_pos)
        }
        
        x_chosen <- subset(x, Position %in% SNPs_chosen)
        x_chosen
      })
      
    do.call(rbind, y)
  }) 
  
  res_lst
}

