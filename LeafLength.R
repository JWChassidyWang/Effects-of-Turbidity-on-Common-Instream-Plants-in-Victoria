# Libraries
library(readxl)
library(dplyr)
library(stringr)
library(lme4)
library(emmeans)
library(lmerTest)

# Function to prepare data
prepare_data <- function(sheet_name) {
  
  read_excel("Plant Data.xlsx", sheet = sheet_name) %>%
    
    mutate(
      # Extract identifiers from Plant ID
      Treatment = word(Plant, 1, sep = "_"),
      
      # Extract week number from Date
      Week = as.integer(str_extract(Date, "\\d+")),
    ) %>%
    
    # Ensure correct data types for modeling
    mutate(
      Plant = factor(Plant),
      Treatment = factor(
        Treatment,
        levels = c("TC", "T100", "T200", "T300"),
        ordered = FALSE,
      ),
    )
}

# Prepare VA and CP datasets
df_VA <- prepare_data("LeafLength_VA")
df_CP <- prepare_data("LeafLength_CP")

# Set control as reference
df_VA$Treatment <- relevel(df_VA$Treatment, ref = "TC")
df_CP$Treatment <- relevel(df_CP$Treatment, ref = "TC")

# Fit mixed-effects models
# Fixed effects: Treatment * Week: considering the growth across weeks (growth slope)
# Random effects: (Week | Plant): allowing each plant has its own baseline and slope
Height_VA <- lmer(Value ~ Treatment * Week + (Week | Plant), data = df_VA)
Height_CP <- lmer(Value ~ Treatment * Week + (Week | Plant), data = df_CP)

summary(Height_VA)
summary(Height_CP)


# Perform comparison with control group
summary_VA <- emtrends(Height_VA, ~ Treatment, var = "Week")
contrast(summary_VA, method = "trt.vs.ctrl", ref = "TC", adjust = "dunnett")

summary_CP <- emtrends(Height_CP, ~ Treatment, var = "Week")
contrast(summary_CP, method = "trt.vs.ctrl", ref = "TC", adjust = "dunnett")

