# ============================================================
# Reproduces:
#   Plot 1 - "TT4 Levels: Patients On vs Not On Thyroxine"
#   Plot 2 - "TSH by Antithyroid Medication"
# Dataset: thyroidDF.csv
# ============================================================

library(ggplot2)
library(scales)

# Set this to wherever thyroidDF.csv lives on your machine
setwd("C:/Users/USER/Desktop/PRACTICE/biostatistics/new/R work")
df <- read.csv("thyroidDF.csv", stringsAsFactors = FALSE)

# TSH/TT4 come in as character because of blank cells -> force numeric
df$TT4 <- as.numeric(df$TT4)
df$TSH <- as.numeric(df$TSH)

# ============================================================
# PLOT 1 - TT4 by thyroxine status (Welch's t-test)
# ============================================================
d1 <- subset(df, !is.na(TT4) & on_thyroxine %in% c("f", "t"))
d1$on_thyroxine <- factor(d1$on_thyroxine, levels = c("f", "t"), labels = c("No", "Yes"))

tt4_test <- t.test(TT4 ~ on_thyroxine, data = d1)
print(tt4_test)                          # t(1469.7) = 17.69, p < 0.001

means1 <- aggregate(TT4 ~ on_thyroxine, data = d1, FUN = mean)
means1$TT4 <- round(means1$TT4, 1)       # 105.5 / 128.7

p1 <- ggplot(d1, aes(x = on_thyroxine, y = TT4, fill = on_thyroxine)) +
  geom_jitter(width = 0.15, color = "grey55", alpha = 0.25, size = 0.7) +
  geom_boxplot(width = 0.45, outlier.shape = NA, alpha = 0.95, color = "black") +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 4, color = "black") +
  geom_text(data = means1,
            aes(x = on_thyroxine, y = TT4 + 30, label = paste0("Mean = ", TT4)),
            inherit.aes = FALSE, fontface = "bold", size = 3.3) +
  # significance bracket
  annotate("segment", x = 1, xend = 2, y = 350, yend = 350) +
  annotate("segment", x = 1, xend = 1, y = 340, yend = 350) +
  annotate("segment", x = 2, xend = 2, y = 340, yend = 350) +
  annotate("text", x = 1.5, y = 368, label = "p < 0.001 ***", fontface = "bold", size = 3.3) +
  scale_fill_manual(values = c("No" = "#2C5C8A", "Yes" = "#E0822E")) +
  scale_y_continuous(limits = c(0, 620), expand = expansion(mult = c(0, 0.02))) +
  labs(title = "TT4 Levels: Patients On vs Not On Thyroxine",
       subtitle = paste0("Welch's t(", round(tt4_test$parameter, 1), ") = ",
                         round(abs(tt4_test$statistic), 2),
                         ", p < 0.001 \u2014 Significant difference"),
       x = "On Thyroxine?", y = "TT4 Level (\u00b5g/dL)") +
  theme_classic(base_size = 12) +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold", size = 12),
        plot.subtitle = element_text(color = "grey35", size = 9))

p1
ggsave("TT4_by_thyroxine.png", p1, width = 6.0, height = 4.6, dpi = 200)

# ============================================================
# PLOT 2 - TSH by antithyroid medication (Mann-Whitney U test)
# ============================================================
d2 <- subset(df, !is.na(TSH) & TSH > 0 & on_antithyroid_meds %in% c("f", "t"))
d2$on_antithyroid_meds <- factor(d2$on_antithyroid_meds, levels = c("f", "t"),
                                 labels = c("Not on Antithyroid Meds", "On Antithyroid Meds"))

tsh_test <- wilcox.test(TSH ~ on_antithyroid_meds, data = d2)
print(tsh_test)                          # p = 0.0017

p2 <- ggplot(d2, aes(x = on_antithyroid_meds, y = TSH, fill = on_antithyroid_meds)) +
  geom_boxplot() +
  scale_y_log10(breaks = 10^(-2:2), labels = label_scientific()) +
  labs(title = "TSH by Antithyroid Medication",
       x = "", y = "TSH (log scale)") +
  theme_gray(base_size = 12) +
  theme(legend.title = element_text(size = 10),
        axis.text.x = element_text(size = 9))

p2
ggsave("TSH_by_antithyroid.png", p2, width = 6.4, height = 4.5, dpi = 200)