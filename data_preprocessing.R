library(tidyverse)
library(readxl)
library(janitor)

# Loading the data
pc_wcst <- read_xlsx("E:/thesis_2024/AB_testing_EF_tests_sesol.xlsx", sheet = 1)
phone_wcst <- read_xlsx("E:/thesis_2024/AB_testing_EF_tests_sesol.xlsx", sheet = 4)

pc_cw <- read_xlsx("E:/thesis_2024/AB_testing_EF_tests_sesol.xlsx", sheet = 2)
phone_cw <- read_xlsx("E:/thesis_2024/AB_testing_EF_tests_sesol.xlsx", sheet = 5)

pc_tol <- read_xlsx("E:/thesis_2024/AB_testing_EF_tests_sesol.xlsx", sheet = 3)
phone_tol <- read_xlsx("E:/thesis_2024/AB_testing_EF_tests_sesol.xlsx", sheet = 6)

# Merging to create a vector of all available names, necessary for creating a uid column
merged_wcst <- inner_join(pc_wcst, phone_wcst, by = "firstName") %>%
  select("firstName")

merged_cw <- inner_join(pc_cw, phone_cw, by = "firstName") %>%
  select("firstName")

merged_tol <- inner_join(pc_tol, phone_tol, by = "firstName") %>%
  select("firstName")

all_firstNames <- union(union(merged_wcst$firstName, merged_cw$firstName), merged_tol$firstName)

# Create a new data frame with all first names
all_names <- data.frame(firstName = all_firstNames)

all_names <- all_names %>%
  mutate(uid = row_number())


# For WCST 

# Merging only by firstName to get matching rows to filter by 
shared_names_wcst <- inner_join(pc_wcst, phone_wcst, by = "firstName") %>%
  select(firstName)

pc_wcst_filtered <- pc_wcst %>%
  filter(firstName %in% shared_names_wcst$firstName)

pc_wcst_final <- pc_wcst_filtered %>%
  left_join(all_names, by = "firstName")


phone_wcst_filtered <- phone_wcst %>%
  filter(firstName %in% shared_names_wcst$firstName)

phone_wcst_final <- phone_wcst_filtered %>%
  left_join(all_names, by = "firstName")

# Sort the vectors of first names in both data frames
phone_wcst_names <- sort(phone_wcst_final$firstName)
pc_wcst_names <- sort(pc_wcst_final$firstName)

# Check if the sorted vectors of first names are identical
is_identical <- all(phone_wcst_names == pc_wcst_names)

# Print the result
if (is_identical) {
  print("The data frames have a 100% match in firstName.")
} else {
  print("The data frames do not have a 100% match in firstName.")
}

# Last bit of preprocessing
pc_wcst_final <- pc_wcst_final %>%
  select(c(uid, firstName, totalTime, correctAnswers, wrongAnswers, administrated))

writexl::write_xlsx(pc_wcst_final, path = "E:/thesis_2024/wcst_pc.xlsx")


phone_wcst_final <- phone_wcst_final %>%
  select(c(uid, firstName, totalTime, correctAnswers, wrongAnswers, administrated))

writexl::write_xlsx(phone_wcst_final, path = "E:/thesis_2024/wcst_phone.xlsx")




# For CW

# Merging only by firstName to get matching rows to filter by 
shared_names_cw <- inner_join(pc_cw, phone_cw, by = "firstName") %>%
  select(firstName)

pc_cw_filtered <- pc_cw %>%
  filter(firstName %in% shared_names_cw$firstName)

pc_cw_final <- pc_cw_filtered %>%
  left_join(all_names, by = "firstName")

phone_cw_filtered <- phone_cw %>%
  filter(firstName %in% shared_names_cw$firstName)

phone_cw_final <- phone_cw_filtered %>%
  left_join(all_names, by = "firstName")

# Sort the vectors of first names in both data frames
phone_cw_names <- sort(phone_cw_final$firstName)
pc_cw_names <- sort(pc_cw_final$firstName)

# Check if the sorted vectors of first names are identical
is_identical <- all(phone_cw_names == pc_cw_names)

# Print the result
if (is_identical) {
  print("The data frames have a 100% match in firstName.")
} else {
  print("The data frames do not have a 100% match in firstName.")
}


# Last bit of preprocessing
pc_cw_final <- pc_cw_final %>%
  select(c(uid, firstName, totalTime, correctAnswers, wrongAnswers, administrated))

writexl::write_xlsx(pc_cw_final, path = "E:/thesis_2024/cw_pc.xlsx")


phone_cw_final <- phone_cw_final %>%
  select(c(uid, firstName, totalTime, correctAnswers, wrongAnswers, administrated))

writexl::write_xlsx(phone_cw_final, path = "E:/thesis_2024/cw_phone.xlsx")



# For ToL

# Merging only by firstName to get matching rows to filter by 
shared_names_tol <- inner_join(pc_tol, phone_tol, by = "firstName") %>%
  select(firstName)

phone_tol_filtered <- phone_tol %>%
  filter(firstName %in% shared_names_tol$firstName)

phone_tol_final <- phone_tol_filtered %>%
  left_join(all_names, by = "firstName")

pc_tol_filtered <- pc_tol %>%
  filter(firstName %in% shared_names_tol$firstName)

pc_tol_final <- pc_tol_filtered %>%
  left_join(all_names, by = "firstName")

# Sort the vectors of first names in both data frames
phone_tol_names <- sort(phone_tol_final$firstName)
pc_tol_names <- sort(pc_tol_final$firstName)

# Check if the sorted vectors of first names are identical
is_identical <- all(phone_tol_names == pc_tol_names)

# Print the result
if (is_identical) {
  print("The data frames have a 100% match in firstName.")
} else {
  print("The data frames do not have a 100% match in firstName.")
}


# Last bit of preprocessing
pc_tol_final <- pc_tol_final %>%
  select(c(uid, firstName, totalTime, correctAnswers, wrongAnswers, numberAttempts, administrated, finalScore))

writexl::write_xlsx(pc_tol_final, path = "E:/thesis_2024/tol_pc.xlsx")


phone_tol_final <- phone_tol_final %>%
  select(c(uid, firstName, totalTime, correctAnswers, wrongAnswers, numberAttempts, administrated, finalScore))

writexl::write_xlsx(phone_tol_final, path = "E:/thesis_2024/tol_phone.xlsx")


# Phase two Data
data <- read.csv("E:/thesis_2024/cohrex_res.csv") %>%
  filter(Project == "Hampus Thesis") %>%
  clean_names() %>%
  select(username:tower_of_london_total_attempts)

writexl::write_xlsx(data, path = "E:/thesis_2024/phase_two_data.xlsx")




