
# hierCP

Hierarchical Changepoint Detection & Forecast Reconciliation in R

## Installation

```r
# Install dependencies
install.packages(c("devtools", "roxygen2", "testthat", "forecast", "ggplot2"))

```

## Quick Start

## Example: Coherence Check

```r
library(hierCP)

# Simulate data
sim <- sim_hier_data(Tn = 100, nb = 3)
Y <- sim$Y
S <- sim$S
levels <- sim$levels

# Create hierarchy object
hier <- new_hierarchy(S, levels)

# Run pipeline (changepoint detection, forecasting, reconciliation)
res <- fit_forecast(Y, hier, h = 6, strategy = "topdown")

# Check coherence
if (!coherence_ok(res$ytilde, S)) {
	stop("Coherence check failed: Top != sum(bottoms).")
} else {
	cat("Coherence check passed: EuropeTotal == DAX+SMI+CAC+FTSE\n")
}
```

Simulate hierarchical time series data, create a hierarchy, and run the main pipeline:

```r
library(hierCP)

# Simulate data
sim <- sim_hier_data(Tn = 100, nb = 3)
Y <- sim$Y
S <- sim$S
levels <- sim$levels

# Create hierarchy object
hier <- new_hierarchy(S, levels)

# Run pipeline (changepoint detection, forecasting, reconciliation)
res <- fit_forecast(Y, hier, h = 6, strategy = "topdown")

# View results
print(res)
```

## Main Functions

### Data Simulation

- `sim_hier_data(Tn, nb, cps_bottom, noise)`
	- Simulates hierarchical time series data with changepoints.
	- Returns a list: `Y` (data matrix), `S` (summing matrix), `levels` (hierarchy levels).

### Hierarchy Construction

- `new_hierarchy(S, levels, series_names = NULL)`
	- Creates a hierarchy object from a summing matrix and level vector.

### Changepoint Detection

- `detect_cp_window(x, w, beta, cost_fun, max_cp)`
	- Detects changepoints in a single series using windowed optimal partitioning.

- `detect_cp_independent(Y, levels, betas, w, cost)`
	- Detects changepoints independently for each series.

- `detect_cp_topdown(Y, hierarchy, eta, w, soft_inherit_window, cost)`
	- Detects changepoints in a top-down hierarchical manner.

- `detect_cp_bottomup(Y, hierarchy, eta, w, soft_aggregate_window, cost)`
	- Detects changepoints in a bottom-up hierarchical manner.

### Forecasting

- `base_forecast_last_segment(x, h, n_star)`
	- Forecasts the last segment using GM(1,1) or ARIMA depending on segment length.

- `arima_lastseg_forecast(x, h)`
	- Forecasts using ARIMA for the last segment.

- `gm11_fit(x)`
	- Fits a GM(1,1) model to a segment.

- `gm11_forecast(fit, h, n_obs)`
	- Forecasts future values using a fitted GM(1,1) model.

### Reconciliation & Regularization

- `structured_fit(Y, hierarchy, h, weights, lambda, tune)`

## Main Functions

| Function                   | Description                                                        |
|----------------------------|--------------------------------------------------------------------|
| `sim_hier_data`            | Simulate hierarchical time series data                             |
| `new_hierarchy`            | Create a hierarchy object                                         |
| `fit_forecast`             | High-level pipeline: detect changepoints, forecast, reconcile      |
| `detect_cp_topdown`        | Top-down hierarchical changepoint detection                       |
| `detect_cp_bottomup`       | Bottom-up hierarchical changepoint detection                      |
| `detect_cp_independent`    | Independent changepoint detection per series                      |
| `detect_cp_window`         | Windowed changepoint detection (PELT-style)                       |
| `base_forecast_last_segment`| Choose base model for last segment (ARIMA/GM(1,1))                |
| `arima_lastseg_forecast`   | ARIMA forecast on last segment                                    |
| `gm11_fit`, `gm11_forecast`| Fit and forecast GM(1,1) model                                    |
| `plot_hierarchy`           | Plot hierarchy structure                                          |
| `plot_series_cps`          | Plot series with detected changepoints                            |
| `structured_fit`           | Structured regularization for upper levels                        |
| `reconcile_ols`            | Reconcile forecasts using OLS                                     |
| `ensure_colnames`          | Ensure column names of a matrix/data.frame                        |
| `coherence_ok`             | Check if top-level series equals sum of bottom-level series       |
| `AMSE`, `MASE`, `RMSSE`    | Forecast accuracy metrics                                         |

See the package documentation and vignettes for more details and examples.
	- Applies structured regularization to forecasts.

- `reconcile(yhatB, S)`
	- Reconciles bottom-level forecasts to ensure coherence with the hierarchy.

### Metrics

- `MASE(y, yhat, s)`
	- Mean Absolute Scaled Error.

- `RMSSE(y, yhat, s)`
	- Root Mean Squared Scaled Error.

- `AMSE(y, yhat, s)`
	- Absolute Mean Scaled Error.

### Plotting

- `plot_hierarchy(hierarchy)`
	- Visualizes the hierarchy structure (summing matrix).

- `plot_series_cps(x, tau, title)`
	- Plots a time series with detected changepoints.

## Example Workflow

```r
# Simulate data
sim <- sim_hier_data(Tn = 120, nb = 3)
hier <- new_hierarchy(sim$S, sim$levels)

# Detect changepoints
taus <- detect_cp_topdown(sim$Y, hier)

# Forecast and reconcile
res <- fit_forecast(sim$Y, hier, h = 6, strategy = "topdown")

# Plot hierarchy
plot_hierarchy(hier)

# Plot series with changepoints
plot_series_cps(sim$Y[,1], taus[[1]])
```

## Details

- The package supports independent, top-down, and bottom-up changepoint detection.
- Forecasts are made for the last segment after changepoints, using GM(1,1) or ARIMA.
- Reconciliation ensures forecasts are coherent across the hierarchy.
- Metrics help evaluate forecast accuracy.
- Plotting functions aid in visualizing results.
