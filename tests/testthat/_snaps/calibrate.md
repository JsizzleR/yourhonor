# calibration printing shows intervals and interpretations

    Code
      print(cal)
    Message
      
      -- yourhonor judge calibration -------------------------------------------------
      8 usable items of 8 - descriptive judge-vs-human agreement:
      * cohen_kappa: 0.58 [0.09, 1.00] (moderate)
      * krippendorff_alpha: 0.60 [0.07, 1.00] (moderate)
      * gwet_ac1: 0.65 [0.22, 1.00] (substantial)
      2 disagreements; inspect `confusion_metrics()` before use.
      i No deployment decision is implied; call report() with a policy to assess one.

---

    Code
      print(multi)
    Message
      
      -- yourhonor judge calibration -------------------------------------------------
      7 usable items of 8 - descriptive judge-vs-human agreement:
      * cohen_kappa: 0.55 [-0.01, 0.85] (moderate)
      * krippendorff_alpha: 0.57 [-0.05, 0.85] (moderate)
      * gwet_ac1: 0.59 [0.12, 0.89] (moderate)
      2 disagreements; inspect `confusion_metrics()` before use.
      Human reliability benchmark (3 raters): gwet_ac1 = 0.58.
      1 item had fewer than two observed human ratings.
      1 item excluded from judge-consensus agreement.
      i No deployment decision is implied; call report() with a policy to assess one.

