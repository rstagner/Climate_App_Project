#build the data for the Decades and Regions panel
#annual: the annual summary table
#group_by: "decade" or "region"
#value_col: the column to compare, such as "noaa_mean" or "nasa_heatwave_days"


#function 1: Prepare the group data
prepare_group_data <- function(annual, group_by, value_col, min_days = 330) {
  
  #source is the prefix of the column name: "noaa" or "nasa"
  source <- sub("_.*", "", value_col)
  days_col <- paste0(source, "_days")
  
  #keep complete years only
  complete <- annual |> 
    filter(.data[[days_col]] >= min_days)
  
  #each city's 1991 - 2020 normal
  normals <- complete |> 
    filter(year >= 1991, year <= 2020) |> 
    summarize(normal = mean(.data[[value_col]], na.rm = TRUE), .by = GEOID)
  
  #anomaly per city per decade
  decade_data <- complete |> 
    left_join(normals, by = "GEOID") |> 
    mutate(anomaly = .data[[value_col]] - normal) |> 
    summarize(value = mean(anomaly, na.rm = TRUE),
              .by = c(GEOID, city, region, decade))
  
  #return the groups the user asks for
  if (group_by == "decade") {
    decade_data |> rename(group = decade)
  } else {
    decade_data |> filter(decade == "2020s") |> rename(group = region)
  }
}

#function 2 - run whatever tests the user picks; plus assumption checks
#run the chosen test on the output of prepare_group_data()
#test: "anova" (ANOVA + Tukey), "welch" (Welch + Games-Howell),
# or "kruskal" (Kruskal-Wallis + pairwise Wilcoxon)
run_group_test <- function(data, test = "anova", alpha = 0.05) {
  
  data <- data |> mutate(group = factor(group))
  model <- aov(value ~ group, data = data)
  
  #assumption checks, run every time
  shapiro_p <- shapiro.test(residuals(model))$p.value
  levene_p <- car::leveneTest(value ~ group, data = data)$`Pr(>F)`[1]
  
  #group means, used to report differences for each test
  means <- data |> summarize(mean = mean(value), .by = group)
  
  if (test == "anova") {
    s <- summary(model)[[1]]
    statistic <- s$`F value`[1]
    p_value <- s$`Pr(>F)`[1]
    stat_label <- paste0("F(", s$Df[1], ", ", s$Df[2], ")")
    tk <- TukeyHSD(model)$group
    posthoc <- tibble(comparison = rownames(tk),
                      difference = tk[, "diff"],
                      p_adj = tk[, "p adj"])
  } else if (test == "welch") {
    w <- oneway.test(value ~ group, data = data, var.equal = FALSE)
    statistic <- unname(w$statistic)
    p_value <- w$p.value
    stat_label <- paste0("F(", w$parameter[1], ", ", round(w$parameter[2], 1), ")")
    gh <- rstatix::games_howell_test(data, value ~ group)
    posthoc <- tibble(comparison = paste(gh$group2, gh$group1, sep = "-"),
                      difference = gh$estimate,
                      p_adj = gh$p.adj)
    
  } else {
    k <- kruskal.test(value ~ group, data = data)
    statistic <- unname(k$statistic)
    p_value <- k$p.value
    stat_label <- paste0("chi-squared(", k$parameter, ")")
    pw <- pairwise.wilcox.test(data$value, data$group, p.adjust.method = "holm")
    posthoc <- as.data.frame(as.table(pw$p.value)) |> 
      filter(!is.na(Freq)) |> 
      left_join(means, by = c("Var1" = "group")) |> 
      left_join(means, by = c("Var2" = "group"), suffix = c("_1", "_2")) |> 
      transmute(comparison = paste(Var1, Var2, sep = "-"),
                difference = mean_1 - mean_2,
                p_adj = Freq)
    
  }
  
  list(
    test = test,
    statistic = statistic,
    stat_label = stat_label,
    p_value = p_value,
    n_groups = nlevels(data$group),
    means = means,
    posthoc = posthoc,
    n_sig_pairs = sum(posthoc$p_adj < alpha),
    n_pairs = nrow(posthoc),
    shapiro_p = shapiro_p,
    levene_p = levene_p
  )
}

#------- Function 3: Plot Group Means

#plot each group's mean with a 95% confidence interval
#data: the output of prepare_group_data()

plot_group_means <- function(data, y_label = "Difference from 1991-2020 normal") {
  
  summary <- data |> 
    summarize(mean = mean(value),
              se = sd(value) / sqrt(n()),
              n = n(),
              .by = group) |> 
    mutate(lower = mean - qt(0.975, n - 1) * se,
           upper = mean + qt(0.975, n- 1) * se)
  
  ggplot(summary, aes(x = group, y = mean)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.15) +
    geom_point(size = 3) +
    labs(x = NULL, y = y_label) +
    theme_minimal(base_size = 13)
}
