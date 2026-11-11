# policies print their gates

    Code
      print(minimal)
    Message
      
      -- yourhonor acceptance policy -------------------------------------------------
      Agreement coefficient: the calibration's Gwet coefficient (gwet_ac1 unweighted,
      gwet_ac2 weighted).
      Confidence level: 0.95.
      Calibration:
      * agreement lower >= 0.9
      * usable items >= 50
      * class support >= 15
      * recall lower >= 0.8
      * missing rate <= 0
      Stability: outside the policy.
      Bias: outside the policy.
      Assess it with `report(calibration, policy = <this policy>)`.

---

    Code
      print(full)
    Message
      
      -- yourhonor acceptance policy -------------------------------------------------
      Agreement coefficient: gwet_ac1.
      Confidence level: 0.9.
      Calibration:
      * agreement lower >= 0.9
      * usable items >= 50
      * class support >= 15
      * recall (I) lower >= 0.8
      * recall (C) lower >= 0.85
      * precision lower >= 0.7
      * missing rate <= 0
      * judge abstention rate upper <= 0.05
      * human reliability lower >= 0.6
      Stability:
      * pairwise disagreement upper <= 0.1
      * items >= 150
      * runs >= 3
      * missing rate <= 0.01
      Bias (required probes: "verbosity" and "position"):
      * scored items >= 20
      * flip rate upper <= 0.1
      * directional rate upper <= 0.1
      * missing rate <= 0.01
      Assess it with `report(calibration, policy = <this policy>)`.

