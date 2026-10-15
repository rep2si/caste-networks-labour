require(tidyverse)

#' Recode Jati/Caste to Reduced Categories
#'
#' @param group A vector of jati/caste names.
#' @param whether the desired result is of two categories, or more (options are "two" or "more" [or if put in anything else, like "most" will have a further division just for NP!])
#'
#' @return Factor. Simplified caste breakdown *with all unspecified categories carried through! Need to clean further!
reduce_caste <- function(group, twoormore = "more") {
  reduced <- case_match(
    as.character(group),
    # unique NP divisions
    c("Acharya / Upadhaya (Bahun)") ~ "Bahun", # Brahmins, will go with GC
    c("Basnet", "Budathoki", "Chuhan", "Acharya (Khatri)", "Khatri", "Paudel", "Raut", "Thapa") ~ "Chhetri", # will go with GC
    c("Bhujel", "Magar", "Tamang") ~ "Janajati", # will go with OBC
    c("Thakur", "Brahmin", "Sharma", "Agarwal", "Gaur", "Goswami", "Goyal",
      "Gupta", "Sikh", "Sindhi", "Sonar", "Srivastava", "Sunar", "thakur", "Vaishya",
      "Bhaskar", "Diwakar" ) ~ "GC",
    c(
      # NP
      "Bishwakarma", "Nepali / Pariyar", "Sarki",
      # UP
      "Gautam", "Pasi", "Dhanuk", "Kori", "Maurya", "Sonkar", "Dhobi",
      # TN
      "Arundhathiyar", "Pallar", "Paraiyar", "SC"
      ) ~ "SC",
    c(
      # UP
      "Pal", "Prajapati (Kumhar)", "Sharma (Badhai)", "Yadav",
      "Nishad", "Nai", "Kashyap", "Katheriya", "Katiyar",
      "Kushwaha", "Khushwaha", "Lodhi", "Lohar", "Patel", "Sachan",
      "Sahu", "Saini", "Teli", "Verma", "Vishwakarma", "Baghel", "Chaurasiya",
      "Jaat",
      # TN
      "Aasaari",
      "Agamudaiyaan",
      "Hindu Vellalar",
      "Hindu Yaathavar",
      "Jaanaan",
      "Kallar",
      "Kulaalar",
      "Maravar",
      "Naayakkar",
      "Pandaaram",
      "Pillamaar",
      "RC Vellalar",
      "RC Yaathavar",
      "BC") ~ "OBC",
      "Muslim" ~ "Muslim",
      .default = group
  )
  if(twoormore == "more") {
    reduced <- case_match(
      reduced,
      c("Bahun", "Chhetri") ~ "GC",
      c("Janajati") ~ "OBC",
      .default = reduced
    )
  } else if(twoormore == "two") {
    reduced <- case_match(
      reduced,
      c("Bahun", "Chhetri", "GC", "OBC") ~ "All others",
      c("SC", "Muslim") ~ "Dalit and Muslim"
    )
  }
  reduced <- as.factor(reduced)

  return(reduced)
}
