representation_scenarios <- list(
  balanced = list(
    age_group = c(
      "18-29" = 0.25,
      "30-44" = 0.25,
      "45-59" = 0.25,
      "60+" = 0.25
    ),

    gender = c(
      Female = 0.50,
      Male = 0.48,
      Non_binary = 0.02
    ),

    tenure = c(
      Homeowner = 0.33,
      Renter = 0.33,
      Other = 0.34
    )
  ),

  mildly_skewed = list(
    age_group = c(
      "18-29" = 0.35,
      "30-44" = 0.30,
      "45-59" = 0.20,
      "60+" = 0.15
    ),

    gender = c(
      Female = 0.55,
      Male = 0.43,
      Non_binary = 0.02
    ),

    tenure = c(
      Homeowner = 0.40,
      Renter = 0.45,
      Other = 0.15
    )
  ),

  highly_skewed = list(
    age_group = c(
      "18-29" = 0.50,
      "30-44" = 0.30,
      "45-59" = 0.15,
      "60+" = 0.05
    ),

    gender = c(
      Female = 0.65,
      Male = 0.33,
      Non_binary = 0.02
    ),

    tenure = c(
      Homeowner = 0.15,
      Renter = 0.75,
      Other = 0.10
    )
  ),

  extreme_underrepresentation = list(
    age_group = c(
      "18-29" = 0.70,
      "30-44" = 0.20,
      "45-59" = 0.08,
      "60+" = 0.02
    ),

    gender = c(
      Female = 0.80,
      Male = 0.19,
      Non_binary = 0.01
    ),

    tenure = c(
      Homeowner = 0.05,
      Renter = 0.90,
      Other = 0.05
    )
  )
)
