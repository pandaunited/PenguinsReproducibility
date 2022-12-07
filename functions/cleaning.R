# Cleaning function, allows us to keep a safe copy of code

cleaning <- function(penguins_raw){
  penguins_raw %>%
    clean_names() %>%
    remove_empty(c("rows", "cols")) %>%
    select(-starts_with("delta")) %>%
    select(-comments)
}

#Function to subset the data and remove NA values for culmen length and culmen depth

remove_empty_culmen <- function(penguins_clean){
  penguins_clean %>%
    filter(!is.na(culmen_length_mm)) %>%
    filter(!is.na(culmen_depth_mm)) %>%
    select(species, culmen_length_mm, culmen_depth_mm)
}