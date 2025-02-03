# wgsLR 0.0.2

* Added a `NEWS.md` file to track changes to the package.
* Added objects related to sample specific error probabilities: 
    - `d_formulas_Hd_wDwS`
    - `d_formulas_Hp_wDwS`
    - `d_prob_Hp_wDwS`
* Renamed objects to allow for sample-specific $w$ (cf. below), e.g.:
    - `d_formulas_Hd` renamed to `d_formulas_Hd_w`
    - `d_formulas_Hp` renamed to `d_formulas_Hp_w`
    - `d_prob_Hp` renamed to `d_prob_Hp_w`