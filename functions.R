# functions.R

# Load necessary libraries for these functions
library(ggplot2)

pretty_uncert <- function(x, dx){
  # A small function for displaying a value and uncertainty to appropriate number of sig. figs.  General rule, keep one uncertain digit
  # unless leading digit is 1 in which case, retain two uncertain digtis.

  pos <- -floor(log10(dx))
  u_digits <- dx * 10^pos

  if (round(u_digits < 2)){
    pos <- pos + 1
  }
  f_string <- paste0("%.", pos, "f")

  x_str <- sprintf(f_string, round(x*10^pos)/10^pos)
  u_str <- sprintf(f_string, round(dx*10^pos)/10^pos)

  return(paste0(x_str, "(", u_str,")"))
}

# Function for Iterated WLS Regression
iterated_wls <- function(df, include_constant = TRUE, tolerance = 1e-6, max_iter = 100) {
  # Initialize beta
  beta_old <- 0
  beta_new <- 1
  iter <- 0

  while(abs(beta_new - beta_old) > tolerance && iter < max_iter) {
    iter <- iter + 1
    beta_old <- beta_new

    # Compute weights
    w <- 1 / (df$sigma_y^2 + (beta_old^2 * df$sigma_x^2))

    # Fit weighted least squares model
    if(include_constant) {
      model <- lm(y ~ x, data = df, weights = w)
    } else {
      model <- lm(y ~ x - 1, data = df, weights = w)
    }

    beta_new <- coef(model)['x']
  }

  list(model = model, iterations = iter, converged = (iter < max_iter))
}


create_plot <- function(df, model, theoretical_slope, include_constant = TRUE) {
  # Create a sequence of x values for prediction

  x_max_extended <- 1.1 * max(df$x)
  x_vals <- seq(0.0, x_max_extended, length.out = 200)
  new_df <- data.frame(x = x_vals)

  # Obtain predictions and confidence intervals
  preds <- predict(model, newdata = new_df, interval = "confidence")

  # Convert preds to a data frame
  preds_df <- as.data.frame(preds)

  # Combine predictions with new_df
  pred_df <- cbind(new_df, preds_df)

  # Start building the plot
  p <- ggplot() +
    # Regression line and confidence ribbon
    geom_line(data = pred_df, aes(x = x, y = fit), color = "blue") +
    geom_ribbon(
      data = pred_df,
      aes(x = x, ymin = lwr, ymax = upr),
      alpha = 0.2,
      fill = "blue"
    ) +
    # Data points with error bars and custom cap sizes
    geom_point(data = df, aes(x = x, y = y)) +
    geom_errorbar(
      data = df,
      aes(x = x, ymin = y - sigma_y, ymax = y + sigma_y, width = sigma_x),
      color = "black"
    ) +
    geom_errorbarh(
      data = df,
      aes(y = y, xmin = x - sigma_x, xmax = x + sigma_x, height = sigma_y),
      color = "black"
    ) +
    # Ensure x-axis includes x = 0
    expand_limits(x =c(0, x_max_extended)) +
    # Theme and labels
    theme_minimal() +
    labs(x = "Reduced Mass Ratio", y = expression(Acceleration~(m/s^2)))

  # Add theoretical line if theoretical_slope is provided
  if (!is.null(theoretical_slope)) {
    # Calculate theoretical line
    theory_df <- data.frame(x = x_vals, y = theoretical_slope * x_vals)

    # Add the theoretical line to the plot
    p <- p + geom_line(
      data = theory_df,
      aes(x = x, y = y),
      color = "red",
      linetype = "dashed"
    )
  }

  return(p)
}



