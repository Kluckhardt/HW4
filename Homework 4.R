install.packages("ggplot2")
install.packages("dplyr")
install.packages("lubridate")

library(ggplot2)
library(dplyr)
library(lubridate)
library(MASS)
cars93 <- MASS::Cars93
ggplot(cars93, aes(x = Price, y = Fuel.tank.capacity)) +
  geom_point(color = "grey60") +
  geom_smooth(se = FALSE, method = "loess", formula = y ~ x, color = "#0072B2") +
  scale_x_continuous(
    name = "price (USD)",
    breaks = c(20, 40, 60),
    labels = c("$20,000", "$40,000", "$60,000")
  ) +
  scale_y_continuous(name = "fuel-tank capacity\n(US gallons)")
ggplot(cars93, aes(x = Price, y = Fuel.tank.capacity)) +
  geom_point(color = "grey60") +
  geom_smooth(method = "lm", se = TRUE, formula = y ~ x, color = "#8fe388") +
  labs(title = "Smoothing with lm (Linear Model)") +
  theme(plot.title = element_text(size = 14, color = "#8fe388"))
ggplot(cars93, aes(x = Price, y = Fuel.tank.capacity)) +
  geom_point(color = "grey60") +
  geom_smooth(method = "glm", se = TRUE, formula = y ~ x, color = "#8fe388") +
  labs(title = "Smoothing with lm (Linear Model)") +
  theme(plot.title = element_text(size = 14, color = "#8fe388"))
ggplot(cars93, aes(x = Price, y = Fuel.tank.capacity)) +
  geom_point(color = "grey60") +
  geom_smooth(method = "glm", se = TRUE, formula = y ~ x, color = "#fe8d6d") +
  labs(title = "Smoothing with glm (Generalized Linear Model)") +
  theme(plot.title = element_text(size = 14, color = "#fe8d6d"))
ggplot(cars93, aes(x = Price, y = Fuel.tank.capacity)) +
  geom_point(color = "grey60") +
  geom_smooth(method = "lm", se = TRUE, formula = y ~ x, color = "#8fe388") +
  labs(title = "Smoothing with lm (Linear Model)") +
  theme(plot.title = element_text(size = 14, color = "#8fe388"))
ggplot(cars93, aes(x = Price, y = Fuel.tank.capacity)) +
  geom_point(color = "grey60") +
  geom_smooth(method = "glm", se = TRUE, formula = y ~ x, color = "#fe8d6d") +
  labs(title = "Smoothing with glm (Generalized Linear Model)") +
  theme(plot.title = element_text(size = 14, color = "#fe8d6d"))
ggplot(cars93, aes(x = Price, y = Fuel.tank.capacity)) +
  geom_point(color = "grey60") +
  geom_smooth(se = FALSE, method = "glm", formula = y ~ x, color = "#0072B2") +
  scale_x_continuous(
    name = "price (USD)",
    breaks = c(20, 40, 60),
    labels = c("$20,000", "$40,000", "$60,000")
  ) +
  scale_y_continuous(name = "fuel-tank capacity\n(US gallons)")
ggplot(cars93, aes(x = Price, y = Fuel.tank.capacity)) +
  geom_point(color = "grey60") +
  geom_smooth(se = FALSE, method = "lm", formula = y ~ x, color = "#0072B2") +
  scale_x_continuous(
    name = "price (USD)",
    breaks = c(20, 40, 60),
    labels = c("$20,000", "$40,000", "$60,000")
  ) +
  scale_y_continuous(name = "fuel-tank capacity\n(US gallons)")
ggplot(cars93, aes(x = Price, y = Fuel.tank.capacity)) +
  geom_point(color = "grey60") +
  geom_smooth(se = FALSE, method = "gam", formula = y ~ x, color = "#0072B2") +
  scale_x_continuous(
    name = "price (USD)",
    breaks = c(20, 40, 60),
    labels = c("$20,000", "$40,000", "$60,000")
  ) +
  scale_y_continuous(name = "fuel-tank capacity\n(US gallons)")
ggplot(cars93, aes(x = Price, y = Fuel.tank.capacity)) +
  geom_point(color = "grey60") +
  geom_smooth(se = TRUE, method = "glm", formula = y ~ x, color = "#8fe388") +
  labs(title = "Smoothing with glm (Generalized Linear Model)") +
  theme(plot.title = element_text(size = 14, color = "#8fe388"))
load("./preprint_growth.rda")

## Homework part 2

load("C:/Users/Luckh/Downloads/preprint_growth.rda")
head(preprint_growth)
preprint_growth %>% filter(archive == "bioRxiv") %>%
  filter(count > 0)-> biorxiv_growth
preprints<-preprint_growth %>% filter(archive %in%
                                        c("bioRxiv", "arXiv q-bio", "PeerJ Preprints")) %>%filter(count > 0) %>%
  mutate(archive = factor(archive, levels = c("bioRxiv", "arXiv q-bio", "PeerJ Preprints")))
1
preprints_final <- filter(preprints, date == ymd("2017-01-01"))
ggplot(preprints) +
  aes(date, count, color = archive, fill = archive) +
  geom_line(size = 1) +
  scale_y_continuous(
    limits = c(0, 600), expand = c(0, 0),
    name = "preprints / month",
    sec.axis = dup_axis( #this part is for the second y axis
      breaks = preprints_final$count, #and we use the counts to position our labels
      labels = c("arXivq-bio", "PeerJPreprints", "bioRxiv"),
      name = NULL)
  ) +
  scale_x_date(name = "year",
               limits = c(min(biorxiv_growth$date), ymd("2017-01-01"))) +
  scale_color_manual(values = c("#0072b2", "#D55E00", "#009e73"),
                     name = NULL) +
  theme(legend.position = "none")
preprint_full <- preprint_growth %>%
  drop_na(count) %>%
  filter(count > 0, year(date) > 2004)
selected_preprints <- preprint_full %>%
  filter(archive %in% c("bioRxiv", "F1000Research")) %>%
  mutate(archive = factor(archive, levels = c("bioRxiv", "F1000Research")))
ggplot(selected_preprints, aes(x = date, y = count, color = archive)) +
  geom_line(size = 1) +
  scale_color_manual(values = c("bioRxiv" = "#7c6bea", "F1000Research" = "#fe8d6d"),
                     name = "Archive") +
  scale_x_date(name = "Date", limits = c(ymd("2014-02-01"), NA)) +
  ylab("Monthly Preprint Count") +
  ggtitle("Preprint Counts") +
  theme_minimal() +
  theme(
    legend.position = "right",
    plot.title = element_text(size = 14, color = "#7c6bea")
  )

