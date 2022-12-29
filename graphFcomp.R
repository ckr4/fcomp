library(tidyverse)

# calculate critical f-value for display
fcutoff <- function(alpha, no_trtmnts, n) {
  round(qf(1-alpha, no_trtmnts-1, (n-1)*no_trtmnts), 2)
}

# plot f distributions of null and alternative hypotheses
fplots <- function(no_trtmnts, n, means, var, alpha, show_a) {
  
  #### perform calculations
  
  ## Distribution parameters
  # Non-centrality parameter
  noncp <- (sum(means^2*n)-(sum(means*n)^2) / (no_trtmnts * n)) / var
  df_n <- no_trtmnts - 1 # DF of numerator
  df_d <- (n-1) * no_trtmnts # DF of denominator
  
  ## Plotting values
  fco <- qf(1-alpha, df_n, df_d) # Critical value for given DF and alpha
  xrange = seq(0, 2*fco, .001) # range of x-axis
  beta_range = seq(0, fco, .001) # range for mapping beta
  alpha_range = seq(fco, 2*fco, .001) # range for mapping alpha/power
  # Get height for drawing line at alpha
  co_ht <- max(df(fco, df_n, df_d, ncp=noncp), df(fco, df_n, df_d)) 
  
  ## Numerical outputs
  beta <- pf(fco, df_n, df_d, ncp=noncp) # beta value
  pwr <- 1-beta # power value
  
  ## Locations for legend items
  # Get height for top of legend items
  h0_max_ht <- max(df(xrange, df_n, df_d))
  # Midpoint of white space to locate critical f-value
  ws_mid_ht <- (max(df(xrange, df_n, df_d, ncp=noncp)) + h0_max_ht)/2
  
  # create plot
  p <- tibble(x = c(0.0, 2*fco)) %>% ggplot(aes(x = x)) 
  
  # add required elements to plot
  p +
    # add legend entry for plot of H0 distribution
    annotate("text", x=fco/2*3*.98, y=h0_max_ht*.72, size=8,
             label="H0") +
    geom_segment(x=fco/2*3*1.03, y=h0_max_ht*.72, xend=fco/2*3*1.08, 
                 yend=h0_max_ht*.72, color="black", linewidth=1.5) +
    # add legend entry for plot of Ha distribution
    annotate("text", x=fco/2*3*1.15, y=h0_max_ht*.72, size=8,
             label="Ha", color="blue") +
    geom_segment(x=fco/2*3*1.2, y=h0_max_ht*.72, xend=fco/2*3*1.25, 
                 yend=h0_max_ht*.72, color="blue", linewidth=1.5) +
    # Fill power region (1-Beta)
    annotate(
      geom = "area",
      x = alpha_range,
      y = df(alpha_range, df_n, df_d, ncp=noncp),
      linewidth = 1.5,
      fill = "#84CA72",
      alpha=.6
    ) +
    # If selected, fill critical region with white then red and add legend entry
    {if (show_a) list(annotate(
          geom = "area",
          x = alpha_range,
          y = df(alpha_range, df_n, df_d),
          linewidth = 1.5,
          fill = "white"
        ), annotate(
          geom = "area",
          x = alpha_range,
          y = df(alpha_range, df_n, df_d),
          linewidth = 1.5,
          fill = "red",
          alpha=.4
        ), geom_label(
          label=paste("Alpha =", alpha),
          x=fco/2*3*1.1, 
          y=h0_max_ht*.95,
          label.padding = unit(0.5, "lines"), # Rectangle size around label
          label.size = 0.2, 
          size=8,
          color = "black", fill="red", 
          alpha=.22
        )
      )
    } +
    # Plot density of F distribution for null hypothesis
    annotate(
      geom = "line",
      x = xrange,
      y = df(xrange, df_n, df_d),
      linewidth = 1.5,
      color = "black"
    ) +
    # Plot density of F distribution for alternative hypothesis
    annotate(
      geom = "line",
      x = xrange,
      y = df(xrange, df_n, df_d, ncp=noncp),
      linewidth = 1.5,
      color = "blue"
    ) +
    # Fill beta region (type II error)
    annotate(
      geom = "area",
      x = beta_range,
      y = df(beta_range, df_n, df_d, ncp=noncp),
      linewidth = 1.5,
      fill = "blue",
      alpha=.2
    ) +
    # draw vertical line denoting critical F-value
    geom_segment(aes(x=fco, y=0, xend=fco, yend=co_ht), linetype="dashed", 
                     color="black", linewidth=1) +
    # add legend entries for beta and power
    geom_label(label=paste("Beta =", round(beta, 4)),
               x=fco/2*3*1.1, 
               y=h0_max_ht*.87,
               label.padding = unit(0.5, "lines"), # Rectangle size around label
               label.size = 0.2, 
               size=8,
               color = "black", fill="blue", 
               alpha=.18
    ) +
    geom_label(label=paste("Power =", round(pwr, 4)),
               x=fco/2*3*1.1, 
               y=h0_max_ht*.79,
               label.padding = unit(0.5, "lines"), # Rectangle size around label
               label.size = 0.2, 
               size=8,
               color = "black", fill = "#84CA72",
               alpha=.3
    ) +
    xlab("F Value") + ylab("Density") +
    theme_classic(base_size=16)
  
}

# fplots(4, 5, c(4.2, 5.2, 6.2, 7.2), 1.7, .05, TRUE)
