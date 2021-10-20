---
date: "2021-10-06T10:58:08-04:00"
description: Do Steven Spielberg's and Tim Burton's movies have the same mean rating?
featured_image: ""
tags: []
title: 'Steven Spielberg and Tim Burton Movie Rating'
---

```{r load-movies-data}
movies <- read_csv(here::here("data", "movies.csv"))
glimpse(movies)
```

Your R code and analysis should go here. If you want to insert a blank chunk of R code you can just hit `Ctrl/Cmd+Alt+I` 

```{r}
# Select Tim Burton and Steven Spielberg and filter for director and rating
rating_analysis <- movies %>% filter(director == "Steven Spielberg" | director == "Tim Burton") %>% select(director, rating) %>% group_by(director)

# Calculate summary statistics
summary_stat <- rating_analysis %>%
  dplyr::summarize(
    mean_rating = mean(rating),
    sd_rating = sd(rating),
    count = n(),

    #Here calculate standard error SD/sqrt(n)
    se_rating = sd_rating/ sqrt(count),
    #Then calculate the t score
    t_critical = qt(0.95, df=4),
    #Calculate 95 prct
    lower = mean_rating - t_critical * se_rating,
    upper = mean_rating + t_critical * se_rating
  ) 
```

> Using a degree of freedom of 4 for the t test, the upper limit for Steven Spielberg is slightly higher than in the graph given. 

```{r}

# Create plot
summary_stat %>% 
  
  # Define aesthetics and reorder so that Steven Spielberh is shown on top
  ggplot(aes(x=mean_rating, y=reorder(director, mean_rating), xmin=lower,xmax=upper)) +
  
  # Add mean
  geom_point(color=c("orangered1", "turquoise4"), size=4) + 
  
  # Add error bars
  geom_errorbar(aes(xmin=lower,xmax=upper), color=c("orangered1", "turquoise4"), 
                size=1, width=0.1) +
  
  # Add labels to mean value
  geom_text(aes(label=round(mean_rating, digits=2)), size=5, vjust=-1.5)+
  
  # Add labels to error bars
  geom_text(aes(x = lower[1], y="Steven Spielberg", 
    label = round(lower[1], digits=2), vjust = -1.5)) +
  geom_text(aes(x = upper[1], y="Steven Spielberg", 
    label = round(upper[1], digits=2), vjust = -1.5)) +
  geom_text(aes(x = lower[2], y="Tim Burton", 
    label = round(lower[2], digits=2), vjust = -1.5)) +
  geom_text(aes(x = upper[2], y="Tim Burton", 
    label = round(upper[2], digits=2), vjust = -1.5)) +
  
  # Add confidence interval overlap
  geom_rect(xmin=7.27, xmax= 7.33, ymin=-Inf, ymax=+Inf, alpha=0.2, fill="grey") +
  
  # Define Theme
  theme_bw()+
  
  #Add titles
  labs(title = "Do Spielberg and Burton have the same mean IMDB ratings?", 
       subtitle = "95% confidence intervals overlap",
       x="Mean IMDB Rating", y="Director")+
  theme(text = element_text(size = 12))
```

```{r}
# Hypothesis test - H0: Tim Burton and Steven Spielberg films have the same mean 

# use the infer package to construct a 95% CI for rating
library(infer)
set.seed(1234)
n <-1000

#Generate the samples and calculate variable of interest for Spielberg
boot_meanrating_Spielberg <- rating_analysis %>%
  filter(!is.na(rating) | director =="Steven Spielberg" ) %>% 
  #Specify variable of interest
  specify(response=rating) %>% 
  #Generate bootstrap samples
  generate(reps=n,type="bootstrap") %>% 
  #Calculate mean of each sample
  calculate(stat="mean")

#Generate the samples and calculate variable of interest for Burton
boot_meanrating_Burton <- rating_analysis %>%
  filter(!is.na(rating) | director =="Tim Burton" ) %>% 
  #Specify variable of interest
  specify(response=rating) %>% 
  #Generate bootstrap samples
  generate(reps=n,type="bootstrap") %>% 
  #Calculate mean of each sample
  calculate(stat="mean")

# Calculation of standard error
 SE <- sqrt(((sd(boot_meanrating_Burton$stat)^2)/n)+((sd(boot_meanrating_Spielberg$stat)^2)/n))
 
# Calculation of t-score
 t_stat <- (mean(boot_meanrating_Burton$stat)-mean(boot_meanrating_Spielberg$stat))/SE
 print(t_stat)
 
```
