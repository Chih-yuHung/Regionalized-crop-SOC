# --- Parameters (from your model) ---
a <- 1436.81
b <- -0.068
A <- function(T) a * exp(b * T)   # average annual SOC change over 0..T (kg/ha/yr)

# --- Example ages for 10 sites (years since conversion) ---
set.seed(2025)
ages <- sort(sample(3:45, size = 20, replace = FALSE))

# --- Two methods to get cumulative SOC change after T years ---
# Method 1 (consistent with your average-rate fit): C1(T) = T * A(T)
C1 <- function(T) T * A(T)

# Method 2 (inconsistent "sum of averages at each horizon"): C2(T) = sum_{t=1..T} A(t)
C2 <- function(T) sum(A(1:T))

# --- Build results table ---
res <- data.frame(
  site = paste0("Site_", seq_along(ages)),
  age_years = ages,
  avg_at_T_kg_ha_yr = A(ages),
  cumulative_M1_kg_ha = C1(ages),
  cumulative_M2_kg_ha = sapply(ages, C2)
)

# Differences
res$diff_M2_minus_M1_kg_ha <- res$cumulative_M2_kg_ha - res$cumulative_M1_kg_ha
res$percent_diff_M2_vs_M1 <- 100 * (res$cumulative_M2_kg_ha / res$cumulative_M1_kg_ha - 1)

# Round for display
res_display <- within(res, {
  avg_at_T_kg_ha_yr      <- round(avg_at_T_kg_ha_yr, 1)
  cumulative_M1_kg_ha    <- round(cumulative_M1_kg_ha, 1)
  cumulative_M2_kg_ha    <- round(cumulative_M2_kg_ha, 1)
  diff_M2_minus_M1_kg_ha <- round(diff_M2_minus_M1_kg_ha, 1)
  percent_diff_M2_vs_M1  <- round(percent_diff_M2_vs_M1, 1)
})

print(res_display, row.names = FALSE)

# --- Optional: quick visual comparison ---
# Uncomment if you want a simple plot
p <- par(mfrow = c(1,1), mar = c(5,5,2,1))
matplot(
  x = res$age_years,
  y = cbind(res$cumulative_M1_kg_ha, res$cumulative_M2_kg_ha),
  type = "b", pch = 16, lty = 1,
  xlab = "Age since conversion (years)",
  ylab = "Cumulative SOC change (kg/ha)"
)
legend("topleft", legend = c("Method 1: T × A(T)", "Method 2: sum_{t=1..T} A(t)"),
       col = 1:2, pch = 16, lty = 1, bty = "n")
