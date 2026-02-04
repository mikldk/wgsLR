if (FALSE) {
  # maple
  # interface(prettyprint = 0);
  # 
  # f := wT -> (wT^(a-1) * ((1/2) - wT)^(b-1)) / ((1/2)^(a+b-1) * Beta(a, b));
  # f(2.3);
  # 
  # n := wT -> p_0*(wT - 1)^2*(wR - 1)^2 + p_1*wT*wR*(wT - 1)*(wR - 1) + p_2*wT^2*wR^2;
  # n(0.2);
  # 
  # res := int(n(wT)*f(wT), wT = 0..1/2);
}

library(tidyverse)

cat("
#maple
interface(prettyprint=0);

# Beta density on (0, 1/2)
f := wT -> (wT^(a-1) * ((1/2) - wT)^(b-1)) / ((1/2)^(a+b-1) * Beta(a, b));
f(1/2);

")



 for (xT in 0L:2L) {
  for (xR in 0L:2L) {
    
    e_Hp <- d_prob_Hp_wTwR |> filter(XT_MA == xT, XR_MA == xR) |> pull(expr_chr)
    e_Ha <- d_prob_Ha_wTwR |> filter(XT_MA == xT, XR_MA == xR) |> pull(expr_chr)
    
    n_nm <- paste0("n_xT", xT, "_xR", xR)
    d_nm <- paste0("d_xT", xT, "_xR", xR)
    
    cat(n_nm, " := wT -> ", e_Hp, ";\n", sep = "")
    cat(d_nm, " := wT -> ", e_Ha, ";\n", sep = "")
    
    cat("\n")
    
    cat("int_Hp_xT", xT, "_xR", xR, " := int(", n_nm, "(wT) * f(wT), wT = 0..1/2);\n", sep = "")
    cat("int_Ha_xT", xT, "_xR", xR, " := int(", d_nm, "(wT) * f(wT), wT = 0..1/2);\n", sep = "")
    
    cat("\n\n")
  }
}


out <- paste0("
allbody := \"\";
")

for (xT in 0L:2L) {
  for (xR in 0L:2L) {
    
    int_n_nm <- paste0("int_Hp_xT", xT, "_xR", xR)
    int_d_nm <- paste0("int_Ha_xT", xT, "_xR", xR)
    
    out <- paste0(out, "
    
    body := convert(", int_n_nm, ", string);
    body := StringTools:-SubstituteAll(body, \"GAMMA\", \"gamma\");
    body := StringTools:-SubstituteAll(body, \"Beta\", \"beta\");
    body := cat(\"\\n\\n", int_n_nm, " <- function(wR, p_0, p_1, p_2, a, b) {\n\", body, \"\n}\n\");
    body := StringTools:-SubstituteAll(body, \"+\", \" + \");
    body := StringTools:-WrapText(body, 100);
    allbody := cat(allbody, body);
    
    body := convert(", int_d_nm, ", string);
    body := StringTools:-SubstituteAll(body, \"GAMMA\", \"gamma\");
    body := StringTools:-SubstituteAll(body, \"Beta\", \"beta\");
    body := cat(\"\\n\\n", int_d_nm, " <- function(wR, p_0, p_1, p_2, a, b) {\n\", body, \"\n}\n\");
    body := StringTools:-SubstituteAll(body, \"+\", \" + \");
    body := StringTools:-WrapText(body, 100);
    allbody := cat(allbody, body);
    
    
    ")

    
    #cat("int_Hp_xT", xT, "_xR", xR, ";\n", sep = "")
    #cat("int_Ha_xT", xT, "_xR", xR, ";\n", sep = "")
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



