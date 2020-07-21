



d <- expand.grid(R0 = c(1.1, 1.3, 1.5),
                 contact_adhere = c(0.5, 0.7, 0.9),
                 iso_adhere = c(0.5, 0.7, 0.9))

d$p <- runif(nrow(d))

ggplot(d, aes(contact_adhere, iso_adhere, fill = p)) + 
  geom_tile() +
  facet_wrap(~ R0) + 
  coord_equal()



d <- expand.grid(R0 = c(1.1, 1.3, 1.5),
                 contact_adhere = c(0.5, 0.7, 0.9),
                 trade_off = c(0, 1, 2),
                 coverage = seq(0.4, 1, 0.1))
d$p <- runif(nrow(d))
d$iso_adhere = plogis(0.5 * d$trade_off - d$trade_off * d$contact_adhere)

ggplot(d, aes(contact_adhere, iso_adhere, colour = factor(trade_off))) + 
  geom_line()


ggplot(d , aes(coverage, p, colour = factor(contact_adhere))) + 
  geom_line() +
  facet_wrap(trade_off ~ R0) 



ggplot(d , aes(contact_adhere, p, colour = coverage, group = coverage)) + 
  geom_line() +
  facet_grid(trade_off ~ R0) 



# Work out trade-offs

# Original

dd = data.frame(self_report = c(0.6, 0.9),
                self_iso = c(0.9, 0.6))

ggplot(dd, aes(self_iso, self_report)) + 
  geom_point() + 
  geom_line(arrow = arrow(length = unit(0.03, "npc"))) +
  xlim(0.2, 1) +
  ylim(0.2, 1)



# New, single trade-off

dd2 <- tibble(self_iso = seq(0.4, 0.9, 0.1), 
              self_report = 1.3 - self_iso)

ggplot(dd2, aes(self_iso, self_report)) + 
  geom_point() + 
  geom_line(arrow = arrow(length = unit(0.03, "npc"))) +
  xlim(0.2, 1) +
  ylim(0.2, 1)


# Two more trade-offs.

dd3 <- tibble(self_iso = seq(0.4, 0.9, 0.1), 
              self_report = -0.4 + 1.3 - 1.2 * (self_iso - 0.4),
              trade_off = 1.2)
dd4 <- tibble(self_iso = seq(0.4, 0.9, 0.1), 
              self_report = -0.4 + 1.3 - 0.8 * (self_iso - 0.4),
              trade_off = 0.8)

ddcomb <- rbind(cbind(dd2, trade_off = 1), dd3, dd4)

ggplot(ddcomb, aes(self_iso, self_report, colour = factor(trade_off), group = trade_off)) + 
  geom_point() + 
  geom_line(arrow = arrow(length = unit(0.03, "npc"))) +
  xlim(0.2, 1) +
  ylim(0.2, 1)


