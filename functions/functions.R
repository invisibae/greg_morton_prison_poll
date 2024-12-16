# Functions
clean_poll_questions <-
  function(x, quesiton = "eligible_to_vote") {

    x %>%
      # We want to organize responses by facility to so they are easier to join to each other later
      pivot_wider(
        names_from = quesiton,
        values_from = c("count", "n_respondents", "pct_of_respondents")
      ) %>%
      clean_names() %>%
      group_by(state, facility_name) %>%
      # sanity check on percentages
      mutate(count_check = sum(across(starts_with("count")), na.rm = T),
             respondents = rowMeans(across(starts_with("n_respondents")), na.rm = T),
             pct_sum = rowSums(across(starts_with("pct_of_respondents")), na.rm = T),
             pct_check = count_check / respondents) %>%
      select(
        -starts_with("n_respondents"),
      ) %>%
      ungroup() %>%
      # replace all pct NA's with 0s
      # hesitant to do this for counts because I want to treat NAs and 0s differently
      mutate(across(starts_with("pct_of_respondents"), ~replace_na(., 0)))

  }
