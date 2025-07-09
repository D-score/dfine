# dfine 0.7.0

- Repairs bug in `plot_p_a_one_item()` to allow for both `x_var = "d"` and `x_var = "a"`
- Adds built-in color palettes and `get_palette()` function to retrieve them

# dfine 0.6.0

- Adds `plot_p_d_item()` for plotting pass curves against ability

# dfine 0.5.0

- Moves `scripts/plot_p_a.R` to the `gsedscripts` package

# dfine 0.4.0

- Adds `scripts/plot_p_a.R` to plot the passing rates by age for the -still provisional- GSED phase 1 & 2 data
- Adds functions plot_p_a_item() and plot_p_a_one_item() to plot passing rates per items by a continuous variable

# dfine 0.3.0

* Adds new function `calculate_dmodel()` which works with both long and wide data
* Adds `item_var`, `reponse_var`, `ability_var` to list returned from `rasch()`
* Makes 0/1 range checks faster in `rasch()`

# dfine 0.2.0

* Add the `rasch()` function to fit Rasch model with 1) item equating, 2) beta fixing and 3) pairs calculation

# dfine 0.1.0

* First commit
