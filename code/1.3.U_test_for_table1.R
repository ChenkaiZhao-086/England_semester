dat <- LTLA_dat_combine %>%
  left_join(., Tab1_case_mean) %>%
  mutate(
    any_hall = if_else(hall_num > 0, "LTLA with hall(s)", "LTLA without hall"), # nolint: line_length_linter.
    Prosperity = as.numeric(Prosperity)
  )


g1 <- dat %>%
  filter(any_hall == "LTLA with hall(s)") %>%
  dplyr::select(mean, GDP_pc, Pop_Den_km2, Prosperity)
g2 <- dat %>%
  filter(any_hall == "LTLA without hall") %>%
  dplyr::select(mean, GDP_pc, Pop_Den_km2, Prosperity)

wilcox.test(g1$mean, g2$mean, paired = FALSE, alternative = "two.sided")
wilcox.test(g1$GDP_pc, g2$GDP_pc, paired = FALSE, alternative = "two.sided")
wilcox.test(
  g1$Pop_Den_km2,
  g2$Pop_Den_km2,
  paired = FALSE,
  alternative = "two.sided"
)
wilcox.test(
  g1$Prosperity,
  g2$Prosperity,
  paired = FALSE,
  alternative = "two.sided"
)
