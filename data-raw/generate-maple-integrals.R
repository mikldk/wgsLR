if (FALSE) {
  # maple
  # interface(prettyprint = 0);
  # 
  # f := wD -> (wD^(a-1) * ((1/2) - wD)^(b-1)) / ((1/2)^(a+b-1) * Beta(a, b));
  # f(2.3);
  # 
  # n := wD -> p_0*(wD - 1)^2*(wS - 1)^2 + p_1*wD*wS*(wD - 1)*(wS - 1) + p_2*wD^2*wS^2;
  # n(0.2);
  # 
  # res := int(n(wD)*f(wD), wD = 0..1/2);
}

library(tidyverse)

cat("
#maple
interface(prettyprint=0);

# Beta density on (0, 1/2)
f := wD -> (wD^(a-1) * ((1/2) - wD)^(b-1)) / ((1/2)^(a+b-1) * Beta(a, b));
f(1/2);

")



 for (xD in 0L:2L) {
  for (xS in 0L:2L) {
    
    e_Hp <- d_prob_Hp_wDwS |> filter(XD_MA == xD, XS_MA == xS) |> pull(expr_chr)
    e_Hd <- d_prob_Hd_wDwS |> filter(XD_MA == xD, XS_MA == xS) |> pull(expr_chr)
    
    n_nm <- paste0("n_xD", xD, "_xS", xS)
    d_nm <- paste0("d_xD", xD, "_xS", xS)
    
    cat(n_nm, " := wD -> ", e_Hp, ";\n", sep = "")
    cat(d_nm, " := wD -> ", e_Hd, ";\n", sep = "")
    
    cat("\n")
    
    cat("int_Hp_xD", xD, "_xS", xS, " := int(", n_nm, "(wD) * f(wD), wD = 0..1/2);\n", sep = "")
    cat("int_Hd_xD", xD, "_xS", xS, " := int(", d_nm, "(wD) * f(wD), wD = 0..1/2);\n", sep = "")
    
    cat("\n\n")
  }
}


out <- paste0("
allbody := \"\";
")

for (xD in 0L:2L) {
  for (xS in 0L:2L) {
    
    int_n_nm <- paste0("int_Hp_xD", xD, "_xS", xS)
    int_d_nm <- paste0("int_Hd_xD", xD, "_xS", xS)
    
    out <- paste0(out, "
    
    body := convert(", int_n_nm, ", string);
    body := StringTools:-SubstituteAll(body, \"GAMMA\", \"gamma\");
    body := StringTools:-SubstituteAll(body, \"Beta\", \"beta\");
    body := cat(\"\\n\\n", int_n_nm, " <- function(wS, p_0, p_1, p_2, a, b) {\n\", body, \"\n}\n\");
    body := StringTools:-SubstituteAll(body, \"+\", \" + \");
    body := StringTools:-WrapText(body, 100);
    allbody := cat(allbody, body);
    
    body := convert(", int_d_nm, ", string);
    body := StringTools:-SubstituteAll(body, \"GAMMA\", \"gamma\");
    body := StringTools:-SubstituteAll(body, \"Beta\", \"beta\");
    body := cat(\"\\n\\n", int_d_nm, " <- function(wS, p_0, p_1, p_2, a, b) {\n\", body, \"\n}\n\");
    body := StringTools:-SubstituteAll(body, \"+\", \" + \");
    body := StringTools:-WrapText(body, 100);
    allbody := cat(allbody, body);
    
    
    ")

    
    #cat("int_Hp_xD", xD, "_xS", xS, ";\n", sep = "")
    #cat("int_Hd_xD", xD, "_xS", xS, ";\n", sep = "")
    #cat("\n\n")
  }
}

out <- paste0(out, "
printf(allbody);

FileTools[Text]:-WriteLine(\"/nfs/home/math.aau.dk/mikl/maple-out.txt\", allbody);\n
FileTools[Text]:-Close(\"/nfs/home/math.aau.dk/mikl/maple-out.txt\");\n
\n
")

cat(out, file = "~/maple.txt")


# 

# quit;



