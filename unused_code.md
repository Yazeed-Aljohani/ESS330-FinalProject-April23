## Understanding Public Restrictions

```{r, echo = FALSE}
library(readr)
library(readxl)
```


```{r, echo = FALSE}
#loading all dataset
govres <- read_excel("C:/Users/Joshua Puyear/Documents/csu-undergrad/ess-330-joshp-2025/github/ESS-330-FinalProject-April23/data/Gov_Responses2Covid19_last.xlsx", sheet = "Dataset")

govresdes<- read_excel("C:/Users/Joshua Puyear/Documents/csu-undergrad/ess-330-joshp-2025/github/ESS-330-FinalProject-April23/data/Gov_Responses2Covid19_last.xlsx", sheet = "Description")

govres_econ <- read_excel("C:/Users/Joshua Puyear/Documents/csu-undergrad/ess-330-joshp-2025/github/ESS-330-FinalProject-April23/data/Gov_Responses_Sources.xlsx", sheet = "Econ sources")

govres_pubhealth <- read_excel("C:/Users/Joshua Puyear/Documents/csu-undergrad/ess-330-joshp-2025/github/ESS-330-FinalProject-April23/data/Gov_Responses_Sources.xlsx", sheet = "Public health sources")

#View(govres)
#View(govres_source)
#view(govresdes)

```

```{r, echo = FALSE}
#names(govres)
#summary(govres)
```


```{r, echo = TRUE}
# Getting top 5 CO2 emitting countries by total GHG (excluding land use)

#filter to top 5 countries

#need to actually just select for the five most ghg emitting countries
top_emitters_res <- govres |>
  group_by(country) |>
  filter(country %in% c("China", "India", "Russia", "United States of America", "Japan"))

# Filtering data to include only those countries and select relevant variables
df2 <- top_emitters_res %>% 
  select(country, date, Rigidity_Public_Health, Economic_Measures) %>% 
  mutate(Public = as.numeric(Rigidity_Public_Health)) %>% 
  mutate(Economic = as.numeric(Economic_Measures)) %>% 
  select(!c(Rigidity_Public_Health, Economic_Measures))
```

```{r, echo = TRUE}
df2_long <- df2 |>
  pivot_longer(cols = c(Public, Economic),
               names_to = "Restriction", values_to = "Rigidity") %>% 
  mutate(Date = as.Date(date))

#making a faceted plot
ggplot(df2_long, aes(x = Date, y = Rigidity, color = Restriction, group = interaction(Restriction, country))) +
    theme(
    axis.text.x = element_text(size = 6, angle = 60, hjust = 1),  # Remove extra comma here
    plot.margin = margin(t = 10, r = 10, b = 60, l = 10)
  ) +
  scale_x_date(date_labels = "%b %Y", 
               date_breaks = "3 months") +
  scale_y_continuous(limits = c(0, 1)) +
  geom_line() +
  geom_point() +
  facet_wrap(~ country) +
  labs(title = "Restrictions During COVID's Lockdown", x = "Date", y = "Restriction Index") +
  theme_minimal()


ggsave("countryrestrict.pdf", plot = last_plot(), path = "C:/Users/Joshua Puyear/Documents/csu-undergrad/ess-330-joshp-2025/github/ESS-330-FinalProject-April23/imgs", width = 12, height = 6)
```
**Figure 1.** Daily COVID lockdown restriction indices (Oxford, 2022), separated by Economic and Public.

**Public Restriction Indices**
There is a statistically significant difference (p < 0.000) in both economic and public restriction index for the top 5 biggest cumulative CO2 emitters (Figure 1). The United States has significantly lower public restrictions (p < 0.000) than every other country except China, where its public restrictions are similar (p = .3660).


```{r, echo = FALSE}
library(car)

#shapiro test for Public restrictions
#normspub <- df2 |> 
#  nest(data = -country) |>
#  mutate(
#    Shapiro = map(data, ~ shapiro.test(na.omit(.x$Public))),  # Remove NAs before applying shapiro.test
#    n = map_dbl(data, nrow),
 #   glance_shapiro = map(Shapiro, broom::glance)
#  ) |>
#  unnest(glance_shapiro)

#flextable::flextable(dplyr::select(normspub, country, n, statistic, p.value)) |>
#  flextable::set_caption("Shapiro-Wilk normality test for restrictions in each country")
#the public restriction dates of lockdown are not normally distributed

#testing economic restrictions
#normsec <- df2 |> 
#  nest(data = -country) |>
#  mutate(
#    Shapiro = map(data, ~ shapiro.test(na.omit(.x$Economic))),  # Remove NAs before applying shapiro.test
#    n = map_dbl(data, nrow),
#    glance_shapiro = map(Shapiro, broom::glance)
#  ) |>
#  unnest(glance_shapiro)

#flextable::flextable(dplyr::select(normsec, country, n, statistic, p.value)) |>
#  flextable::set_caption("Shapiro-Wilk normality test for restrictions in each country")
#economic restriction dates aren't normally distributed either
```



```{r, echo = FALSE}
#testing for statistical significance in differences between indices for how countries handled the lockdown

#kruskal.test(Public ~ country, data = df2)
#kruskal.test(Economic ~ country, data = df2)
```

post-hoc test to see which ones are different

```{r, echo = FALSE}
# using Dunn's test to see which variables are different
#library(dunn.test)
# Perform Dunn's Test for pairwise comparisons
#dunn.test(df2$Public, g = df2$country)

#dunn.test(df2$Economic, g = df2$country)
```
Results:
The United States has significantly lower public restrictions (p 
