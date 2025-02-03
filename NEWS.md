# wgsLR 0.0.2

* Added a `NEWS.md` file to track changes to the package.
* Include functionality for sample specific error rates:
    + Convention: postfix `_w` for same error rate in both samples and postfix `_wDwS` to allow for 
      different error rates in donor (trace) and suspect (reference) sample.
    + Added functions and objects related to sample specific error probabilities: 
        - `calc_LRs_wDwS()` renamed to `calc_LRs_w()`
        - `sample_data_Hp_wDwS()`, `sample_data_Hd_wDwS()`
        - `add_errors_Hp_wDwS()`, `add_errors_Hd_wDwS()`
        - `d_formulas_Hd_wDwS`, `d_formulas_Hp_wDwS`, `d_prob_Hp_wDwS`
    + Renamed functions and objects to allow for sample-specific $w$ (cf. below), i.e.:
        - `calc_LRs()` renamed to `calc_LRs_w()`
        - `sample_data_Hp()` renamed to `sample_data_Hp_w()`
        - `sample_data_Hd()` renamed to `sample_data_Hd_w()`
        - `add_errors_Hp()` renamed to `add_errors_Hp_w()`
        - `add_errors_Hd()` renamed to `add_errors_Hd_w()`
        - `d_formulas_Hd` renamed to `d_formulas_Hd_w`
        - `d_formulas_Hp` renamed to `d_formulas_Hp_w`
        - `d_prob_Hp` renamed to `d_prob_Hp_w`
