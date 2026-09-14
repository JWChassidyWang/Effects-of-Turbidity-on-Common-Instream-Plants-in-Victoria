# Libraries
library(readxl)
library(dplyr)
library(stringr)
library(emmeans)

# Function to prepare data
prepare_data <- function(sheet_name) {
  
  read_excel("Plant Data.xlsx", sheet = sheet_name) %>%
    
    mutate(
      # Extract identifiers from Plant ID
      Treatment = word(Plant, 1, sep = "_"),
    ) %>%
    
    # Ensure correct data types for modeling
    mutate(
      Plant = factor(Plant),
      Treatment = factor(
        Treatment,
        levels = c("TC", "T100", "T200", "T300")),
      
      Growth_Abv = Above - Initial_Above,
      Growth_Blw = Below - Initial_Below,
    )
}

# Prepare VA and CP datasets
df_VA <- prepare_data("Biomass_VA")
df_CP <- prepare_data("Biomass_CP")

# Make an ANOVA planned comparison function
bio_anova <- function(data, response) {
  
  # Above/Below as variable
  formula <- as.formula(paste(response, "~ Treatment"))
  # Fit ANOVA
  model <- lm(formula, data = data)
  
  # Estimate group means
  emm <- emmeans(model, "Treatment")
  
  # Compare each treatment vs control
  contrasts <- contrast(emm, method = "trt.vs.ctrl", ref = "TC") %>%
    summary(infer = TRUE, adjust = "dunnett")  # infer=TRUE gives CI and p-values

  return(contrasts)
}

# Apply the ANOVA function to all datasets
# Above-ground biomass
VA_above <- bio_anova(df_VA, "Growth_Abv")
CP_above <- bio_anova(df_CP, "Growth_Abv")

# Below-ground biomass
VA_below <- bio_anova(df_VA, "Growth_Blw")
CP_below <- bio_anova(df_CP, "Growth_Blw")

# Show summaries
summary(VA_above)
summary(CP_above)
summary(VA_below)
summary(CP_below)
