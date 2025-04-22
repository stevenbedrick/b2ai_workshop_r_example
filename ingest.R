# Perform tidying on phenotype dataframe, e.g. turn age into numbers, etc.
clean_phenotype_cols <- function(input.raw.phenotype.df) {
  input.raw.phenotype.df |> mutate(
    age=case_when(age=="90 and above" ~ NA, .default=as.numeric(age)),# This loses us two people; that's fine for now
    is_control_participant=case_match(is_control_participant,"No"~FALSE, "Yes"~TRUE)
  )
}

# Compute a long dataframe of participants and their protocol eligibility information, as well as case/control status
compute_eligibility <- function(input.raw.phenotype.df) {
  input.raw.phenotype.df |> 
    select(participant_id, redcap_repeat_instrument, redcap_repeat_instance, 
           age, sex_at_birth, is_control_participant, starts_with("eligible")) |> 
    pivot_longer(starts_with("eligible"), names_to="dummy_col", values_to="eligible_protocol", values_drop_na = TRUE) |> 
    select(-dummy_col) |> mutate(eligible_protocol=as.factor(eligible_protocol))
}

# Compute a long dataframe of participants and condition + case/control status
compute_conditions <- function(input.raw.phenotype.df) {
  input.raw.phenotype.df %>% select(participant_id, age, sex_at_birth, is_control_participant, laryng_cancer:osa) %>% 
    pivot_longer(laryng_cancer:osa, names_to="condition", values_to="is_checked") %>% 
    mutate(is_checked=case_when(is_checked=="Checked"~TRUE, .default=FALSE))
}