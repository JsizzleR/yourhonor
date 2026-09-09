# bias objects print a heading and the probe table

    Code
      print(res)
    Message
      
      -- yourhonor bias probes --
      
    Output
      # A tibble: 2 x 32
        probe     flip_rate conf_low conf_high direction        directional_rate
        <chr>         <dbl>    <dbl>     <dbl> <chr>                       <dbl>
      1 verbosity         1    0.832     1     favors_perturbed                1
      2 bandwagon         0    0         0.168 none                            0
      # i 26 more variables: directional_conf_low <dbl>, directional_conf_high <dbl>,
      #   position_preference <dbl>, display_first_rate <dbl>,
      #   position_conf_low <dbl>, position_conf_high <dbl>, statistic <int>,
      #   n_discordant <int>, n_scored <int>, n_directional_items <int>,
      #   n_input <int>, n_unscored <int>, unscored_rate <dbl>,
      #   n_items_missing <int>, n_judgments <int>, n_judgments_missing <int>,
      #   missing_rate <dbl>, n_comparisons <int>, p_value <dbl>, ...

