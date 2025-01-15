# BFNPEnclosureFeedingProject

This project contains data and scripts for the manuscript "Intensive Feeding Modifies Nutrient Patterns in a Strictly Protected Area"

---

## Main Dataset

The main dataset is `bfnp_enclosure_model_data.csv` and can be found in `data/clean_data/`. The relevant columns can be described as follows:

| Column Name                | Description                                                                 |
|----------------------------|-----------------------------------------------------------------------------|
| `plot_id`                  | Unique ID of each sampling plot.                                           |
| `site_id`                  | Unique ID of each site (e.g., enclosure).                                  |
| `transect_id`              | Unique ID of each transect.                                                |
| `notes`                    | Notes made during the sampling.                                            |
| `type`                     | Whether the sample is a vegetation or soil sample.                         |
| `elevation`                | Elevation above sea level in meters.                                       |
| `n`                        | Nitrogen concentration.                                                    |
| `c`                        | Carbon concentration.                                                      |
| `k`                        | Potassium concentration.                                                   |
| `p`                        | Phosphorous concentration.                                                 |
| `mg`                       | Magnesium concentration.                                                   |
| `ca`                       | Calcium concentration.                                                     |
| `na`                       | Sodium concentration.                                                      |
| `cn`                       | C/N ratio.                                                                 |
| `np`                       | N/P ratio.                                                                 |
| `units`                    | Units of the concentrations.                                               |
| `mountain_or_enclosure`    | Whether the sample is part of the enclosure study (`enclosure`).           |
| `x`                        | Longitude.                                                                 |
| `y`                        | Latitude.                                                                  |
| `flag`                     | Indicates if a point should be excluded or not.                            |
| `distance_enclosure`       | Idealized distance to the enclosure.                                       |
| `min_distance_enclosure`   | Realized minimal distance to the enclosure.                                |
| `deer_number`              | Average deer number in the respective enclosure.                           |
| `deer_density`             | Deer density in the enclosure.                                             |

---

## Directory Structure

- **`data/`**: Contains the provided datasets. 
  - `clean_data/`: Processed datasets, including the main dataset `bfnp_enclosure_model_data.csv`.
- **`builds/`**: Contains produced outputs, such as estimates and plots.
- **`R/`**: Contains all R scripts, organized into the following subfolders:
  - **`prep/`**: Scripts for data preparation. Note that these scripts require raw species data and locations/boundaries for each reserve, which cannot be shared in this repository. As a result, scripts in this folder cannot be run.
  - **`functions/`**: Custom functions used across various analyses.
  - **`viz/`**: Scripts for generating figures and visualizations.
  - **`analysis/`**: Scripts for analysis. All scripts in this folder should only require data provided in the repository.

---
