# Worked examples

Two complete runs of the model with the parameters used in the paper:

| Folder | Farm | Crop | Season | Window |
|---|---|---|---|---|
| `NR_potato/` | NR (Tunisia) | potato, drip irrigation, 1000 m² | 12/02/2025 – 05/06/2025 | first run: 168 h past + 168 h future from 12/02/2025 |
| `AM_citrus/` | AM (Tunisia) | citrus, drip irrigation, 3500 m² | 10/03/2025 – 15/11/2025 | first run: 168 h past + 168 h future from 10/03/2025 |

Each folder contains

- `config.input.json`: farm, soil, crop and irrigation-system parameters of the paper;
- `Input_data/`: hourly climate (past and forecast), soil-moisture sensor readings (15/30/45 cm),
  irrigation volumes, and the initial model state (`kc_correction.csv` = 1, `SM_extremes_previous_run.csv` =
  maximum and minimum soil moisture of the sensor);
- `reference_output/`: the files produced by the model for this window.

The crop table (`Irrigation_model_for_paper/Input_data/crop_data/crop_table_REVIEWED.csv`) is shared with the main model.
The reference outputs are numerically identical to the first run of the simulations reported in the paper.

## Run

From the repository root (R ≥ 4.0, packages listed in the main README; internet connection needed
because the pedotransfer step queries the ROSETTA web service through `soilDB`):

```bash
Rscript examples/run_example.R NR_potato
Rscript examples/run_example.R AM_citrus
```

The script runs the model on a temporary copy, saves the new outputs in `examples/<example>/Output_data_new/`
and reports whether each output file matches `reference_output/` (tolerance 1e-6).

## Data and privacy

Climate and sensor data are those collected at the two pilot farms; farm names and farmer information are not included.
To run the model on a new farm, replace the content of `Input_data/` (same file names, one value per hour) and
adapt `config.input.json`; the WoT layer in `wot/` shows how readings from sensors and weather stations are
normalised into these inputs.
