library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(grid)

# 1. 데이터 불러오기
raw <- read_excel(
  file.choose(),
  sheet = 1,
  skip = 18,
  col_names = c("date", "time", "A_in", "A_out", "B_in", "B_out")
)

# 2. 날짜/시간 정리
raw2 <- raw %>%
  mutate(
    date = as.Date(date),
    time_chr = format(time, "%H:%M"),
    datetime = ymd_hm(paste(date, time_chr)),
    year = factor(year(datetime), levels = c(2024, 2025))
  ) %>%
  arrange(datetime) %>%
  mutate(
    day_gap = as.numeric(date - lag(date)),
    block = cumsum(ifelse(is.na(day_gap) | day_gap >= 2, 1, 0))
  )

# 3. 블록 내부 x축: 실제 경과시간 반영
raw2 <- raw2 %>%
  group_by(year, block) %>%
  arrange(datetime, .by_group = TRUE) %>%
  mutate(
    x_within = as.numeric(difftime(datetime, first(datetime), units = "hours"))
  ) %>%
  ungroup()

# 4. 블록 사이 간격
gap_hours <- 8

block_info <- raw2 %>%
  group_by(year, block) %>%
  summarise(block_width = max(x_within), .groups = "drop") %>%
  group_by(year) %>%
  arrange(block, .by_group = TRUE) %>%
  mutate(
    start = lag(cumsum(block_width + gap_hours), default = 0)
  ) %>%
  ungroup()

raw2 <- raw2 %>%
  left_join(block_info, by = c("year", "block")) %>%
  mutate(
    x = start + x_within,
    x_plot = ifelse(year == 2024, x, x + 1000)
  )

# 5. long 형태 변환
df_long <- bind_rows(
  raw2 %>%
    transmute(
      year, block, x = x_plot, date,
      system = "System A",
      Influent = A_in,
      Effluent = A_out
    ),
  raw2 %>%
    transmute(
      year, block, x = x_plot, date,
      system = "System B",
      Influent = B_in,
      Effluent = B_out
    )
)

plot_df <- df_long %>%
  pivot_longer(
    cols = c(Influent, Effluent),
    names_to = "type",
    values_to = "TAN"
  ) %>%
  mutate(
    type = factor(type, levels = c("Influent", "Effluent")),
    system = factor(system, levels = c("System A", "System B"))
  )

# 6. x축 라벨: 날짜만 표시 (연도 없이 월-일)
date_break_df <- raw2 %>%
  group_by(year, block, date) %>%
  summarise(x = min(x_plot), .groups = "drop") %>%
  arrange(year, x)

x_breaks <- date_break_df$x
x_labels <- format(date_break_df$date, "%m-%d")

# 7. 패널 안쪽 제목
anno_df <- data.frame(
  year = factor(c(2024, 2025, 2024, 2025), levels = c(2024, 2025)),
  system = factor(c("System A", "System A", "System B", "System B"),
                  levels = c("System A", "System B")),
  x = c(
    mean(range(plot_df$x[plot_df$year == 2024])),
    mean(range(plot_df$x[plot_df$year == 2025])),
    mean(range(plot_df$x[plot_df$year == 2024])),
    mean(range(plot_df$x[plot_df$year == 2025]))
  ),
  y = c(0.87, 0.87, 0.87, 0.87),
  lab = c("System A", "System A", "System B", "System B")
)

# 8. 그래프
p <- ggplot(plot_df, aes(x = x, y = TAN, group = interaction(type, block))) +
  geom_line(aes(linetype = type), color = "black", linewidth = 0.45) +
  geom_point(aes(shape = type), color = "black", size = 1.6, stroke = 0.2) +
  geom_text(
    data = anno_df,
    aes(x = x, y = y, label = lab),
    inherit.aes = FALSE,
    size = 7
  ) +
  facet_grid(system ~ year, scales = "free_x") +
  scale_linetype_manual(values = c("Influent" = "solid", "Effluent" = "dashed")) +
  scale_shape_manual(values = c("Influent" = 17, "Effluent" = 25)) +
  scale_x_continuous(
    breaks = x_breaks,
    labels = x_labels,
    expand = expansion(mult = c(0.02, 0.02))
  ) +
  labs(
    x = "Date",
    y = expression("TAN concentration (mg.L"^{-1}*")")
  ) +
  coord_cartesian(ylim = c(0, 0.95), clip = "off") +
  theme_bw(base_size = 14) +
  theme(
    strip.background = element_blank(),
    strip.text.x = element_text(size = 18, face = "plain"),
    strip.text.y = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line(color = "black", linewidth = 0.8),
    axis.ticks = element_line(color = "black", linewidth = 0.6),
    axis.ticks.length = unit(-0.15, "cm"),
    panel.grid.major = element_line(color = "grey80", linewidth = 0.5, linetype = "dashed"),
    panel.grid.minor = element_blank(),
    legend.position = c(0.005, 1.02),
    legend.justification = c(0, 1),
    legend.title = element_blank(),
    legend.background = element_blank(),
    legend.key = element_blank(),
    legend.text = element_text(size = 13),
    axis.title.x = element_text(size = 20, face = "plain"),
    axis.title.y = element_text(size = 18, face = "plain"),
    axis.text.x = element_text(size = 12, color = "black"),
    axis.text.y = element_text(size = 12, color = "black"),
    panel.spacing.x = unit(1.0, "lines"),
    panel.spacing.y = unit(0.9, "lines"),
    plot.margin = margin(18, 18, 10, 18)
  )

p

# 9. 저장
ggsave(
  "TAN_figure_date_axis.png",
  plot = p,
  width = 16,
  height = 6.2,
  dpi = 600,
  bg = "white"
)
