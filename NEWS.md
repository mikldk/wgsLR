# wgsLR 0.0.3

* Adding `calc_WoE_wTwR_mleH2_wT_num()

# wgsLR 0.0.2
* Added a `NEWS.md` file to track changes to the package.
* Renaming: 
    + `xT` for trace sample (instead of `xD`)
    + `xR` for reference sample (instead of `xS`)
* Include functionality for sample specific error rates:
    + `wT` for **T**race sample and `wR` for **R**eference sample
    + Convention: postfix `_w` for same error rate in both samples and postfix `_wTwR` to allow for 
      different error rates in donor (trace) and suspect (reference) sample.
    + Added functions and objects related to sample specific error probabilities: 
        - `calc_LRs_wTwR()` renamed to `calc_LRs_w()`
        - `sample_data_Hp_wTwR()`, `sample_data_Hd_wTwR()`
        - `add_errors_Hp_wTwR()`, `add_errors_Hd_wTwR()`
        - `d_formulas_Hd_wTwR`, `d_formulas_Hp_wTwR`, 
        - `d_prob_Hp_wTwR`, `d_prob_Hd_wTwR`, 
        - `d_LR_wTwR`
        - `calc_WoE_wTwR_integrate_wT_mc()`
        - `calc_WoE_wTwR_integrate_wT_num()`
        - `calc_WoE_wTwR_profilemax_wT_num()`
    + Renamed functions and objects to allow for sample-specific $w$ (cf. below), i.e.:
        - `calc_LRs()` renamed to `calc_LRs_w()`
        - `sample_data_Hp()` renamed to `sample_data_Hp_w()`
        - `sample_data_Hd()` renamed to `sample_data_Hd_w()`
        - `add_errors_Hp()` renamed to `add_errors_Hp_w()`
        - `add_errors_Hd()` renamed to `add_errors_Hd_w()`
        - `d_formulas_Hd` renamed to `d_formulas_Hd_w`
        - `d_formulas_Hp` renamed to `d_formulas_Hp_w`
        - `d_prob_Hp` renamed to `d_prob_Hp_w`
        - `d_prob_Hd` renamed to `d_prob_Hd_w`
        - `d_prob_LR` renamed to `d_LR_w`
