## 01_recession_probability_plot.R
## Time series of predicted recession probability with shading.

## (Optional) load your requirements script if you want
## source("00_requirements.R")

library(dplyr)
library(ggplot2)

# --- Load data --------------------------------------------

recession_indicators <- read.csv("recession_indicators.csv",
                                 stringsAsFactors = FALSE)

recession_indicators$date <- as.Date(recession_indicators$date)

# Remove index column if present
recession_indicators <- recession_indicators |>
  dplyr::select(-dplyr::any_of("Unnamed..0"))

# --- Feature engineering ---------------------------------

recession_indicators <- recession_indicators |>
  arrange(date) |>
  mutate(
    UnempChange   = UnempRate - dplyr::lag(UnempRate),
    SP500_Return  = (GSPC.Close / dplyr::lag(GSPC.Close)) - 1
  )

model_data <- recession_indicators |>
  filter(!is.na(UnempChange), !is.na(SP500_Return))

# --- Logistic regression model ----------------------------

recess_model <- glm(
  Recess ~ UnempChange + SP500_Return,
  data   = model_data,
  family = binomial(link = "logit")
)

model_data$Recess_Prob <- as.numeric(predict(recess_model,
                                             type = "response"))

# Join predictions back to full data
recession_indicators <- recession_indicators |>
  left_join(model_data |> select(date, Recess_Prob),
            by = "date")

# Last predicted point for "Now" label
last_point <- recession_indicators |>
  filter(!is.na(Recess_Prob)) |>
  arrange(date) |>
  dplyr::slice(dplyr::n())

now_label <- paste0(
  "Now: ",
  round(last_point$Recess_Prob * 100, 1),
  "%"
)

# --- Plot -------------------------------------------------

p1 <- ggplot(recession_indicators, aes(x = date)) +
  # Recession shading (0/1 indicator)
  geom_ribbon(aes(ymin = 0,
                  ymax = ifelse(Recess == 1, 1, NA_real_)),
              fill = "grey80", alpha = 0.5) +
  # Gold recession probability line
  geom_line(aes(y = Recess_Prob),
            linewidth = 1.1,
            colour = "#d4af37",
            na.rm = TRUE) +
  # "Now" point and label
  geom_point(data = last_point,
             aes(y = Recess_Prob),
             size = 3,
             colour = "#d4af37") +
  geom_text(
    data = last_point,
    aes(y = Recess_Prob, label = now_label),
    vjust = -1,
    hjust = 1,
    size = 3.5
  ) +
  scale_y_continuous(
    "Predicted probability of recession",
    limits = c(0, 1),
    breaks = seq(0, 1, by = 0.1),
    labels = function(x) paste0(x * 100, "%")
  ) +
  scale_x_date("Date") +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank()
  )

# Save to file (you can change to .pdf if you want)
ggsave("plot1_recession_probability.png",
       p1, width = 9, height = 5, dpi = 300)

