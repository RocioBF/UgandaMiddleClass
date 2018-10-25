# This script is used to plot some economic indicators of 
# Uganda and compare them with Africa.

# Making the df long for the graph:
df.long <- melt(GNP_pc)

#Plot bar graph of per capita GNP of Africa and Uganda:
ggplot(df.long, aes(YEAR, value, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge") +
  xlab("year") + ylab("per capita GNP") +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black")
  )
# NOTE: export it in Device Size = 10 x 4 inches

# Plot the GDP growth per capita of Africa and Uganda:
gdp_growth_long <- melt(gdp_growth)

ggplot(gdp_growth, aes(Year)) +
  geom_point(aes(y = Uganda, colour = "Uganda")) +
  geom_point(aes(y = Africa, colour = "Africa")) +
  xlab("year") + ylab("GDP growth (%)") +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black")
  )


# Plot the primary enrollment ratio for Africa and Uganda:
ggplot(Primary_enrollment, aes(Year)) +
  geom_line(aes(y = Primary, colour = "Uganda")) +
  geom_line(aes(y = Africa, colour = "Africa")) +
  xlab("Year") +
  ylab("School enrollment, primary (% gross)") +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black")
  )
