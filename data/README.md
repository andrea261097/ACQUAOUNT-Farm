# Input data of the four pilot farms (2024 and 2025)

Hourly input time series used to run the model on the four farms of the paper, plus the
`config.input.json` of every farm and season. Files are copies of the simulation inputs, **unmodified**
(byte-identical to the originals), except for the configuration files, which have one added field,
`"kc_dynamic": false` (fixed kc, as in the paper; see the main README).

| Folder | Crop | Hourly file(s) | Coverage of the file(s) | Season in config (2024 / 2025) |
|---|---|---|---|---|
| `AM/` | citrus | `AM_SM1.csv` | 01/05/2024 00h – 29/09/2025 00h (continuous) | 01/05–15/11/2024 / 10/03–15/11/2025 |
| `JD/` | citrus | `JD_SM1.csv` | 01/05/2024 00h – 29/09/2025 00h (continuous) | 01/05–31/12/2024 / 10/03–31/12/2025 |
| `NR/` | potato | `NR_SM1.csv` (2024), `NR_SM1_2025.csv` (2025) | 05/08/2024 – 20/01/2025; 01/01/2025 – 30/06/2025 | 01/10/2024–15/01/2025 / 12/02–05/06/2025 |
| `SG/` | potato | `SG_SM1.csv` (2024), `SG_SM1_2025.csv` (2025) | 05/08/2024 – 08/01/2025; 01/01/2025 03h – 11/09/2025 | 30/09/2024–15/01/2025 / 12/02–05/06/2025 |

For AM and JD the 2024 and 2025 simulations use the same file (the simulation folder keeps two
identical copies, `*_SM1.csv` and `*_SM1_2025.csv`; only one is stored here).
The config of each season is `config.input_<FARM>_<YEAR>.json`: copy it to `Irrigation_model/config.input.json` to run that season.

## Columns of the hourly files

`data` (DD/MM/YYYY), `ora` (hour 0-23), `wind` (m/s), `temp` (°C), `RH` (%), `sw_rad` (W/m²),
`irr_given_vol` (irrigation volume given in the hour, from the plot water meter),
`moisture_sensor_raw15cm`, `moisture_sensor_raw30cm`, `moisture_sensor_raw45cm` (volumetric water content, %),
`precipitation` (mm).
Units follow `Irrigation_model/Input_output_variables_description.docx`.
The model reads these series in windows of 168 h of past data plus 168 h of forecast, one run per irrigation event.

## Known points to be aware of

* NR and SG: the 2024 and 2025 files overlap in January 2025 (NR: 457 h, SG: 166 h) and **the values in the
  overlap are different** (weather and soil moisture, not only irrigation). The overlap is outside the 2025 seasons
  (which start on 12/02/2025), so it does not affect the 2025 results.
* SG 2025: `theta_fc_user` = 15 and `soil_SAT_user` = 30 (the 2024 config has `soil_SAT_user` = 24).
