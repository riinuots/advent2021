test = do_traj(24, 95, testing = TRUE)
max = slice_max(test, y_traj, with_ties = FALSE)

test %>% 
  ggplot() +
  geom_rect(data = area, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), fill = "orange") +
  geom_line(aes(x_traj, y_traj)) +
  geom_label(data = max, aes(x_traj, y_traj, label = y_traj), fill = "white") +
  theme_classic()
  #coord_cartesian(xlim = c(200, 400), ylim = c(-96, 100))

ggsave("solutions/day17/trajectory.png")
