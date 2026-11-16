# WoT data-normalisation layer (templates)

This folder documents how field and weather-station data reach the ACQUAOUNT-Farm model through the
W3C [Web of Things](https://www.w3.org/WoT/) (WoT) layer of the ACQUAOUNT platform, and provides
minimal, runnable templates to reproduce the same pattern with
[node-wot](https://github.com/eclipse-thingweb/node-wot).

**Scope.** The files are *templates*: the Thing Descriptions use placeholder identifiers
(`FIELD_ID`, `STATION_ID`, `HOST`), and the property names are those used in this repository.
The code of the production platform back end (authentication, database, deployment) is not
distributed here.

## Design in brief

- One Thing Description (TD, JSON-LD) per **field** and per **weather station**
  (not per physical sensor).
- Only the **Properties** affordance is used: values are read with HTTP `GET` and written with HTTP `POST`.
  Actions and Events are not implemented.
- Independent networks (third-party weather stations, LoRaWAN sensors) do not speak WoT:
  an **adapter** converts their payload to the names and units of the TD and writes it to the Properties.
- Persistence and historical queries are handled by a separate layer (OGC SensorThings API on PostGIS)
  and are not part of the WoT layer.

## Content

| Path | Purpose |
|---|---|
| `thing_descriptions/field_TEMPLATE.td.json` | Field TD: soil moisture at 15/30/45 cm, irrigation volume |
| `thing_descriptions/weather_station_TEMPLATE.td.json` | Weather-station TD: air temperature, precipitation, solar radiation, relative humidity, wind speed |
| `adapter/adapter.js` | Normalises a payload (name, unit, timestamp) and writes it to the Properties |
| `adapter/mapping_lorawan_example.json`, `adapter/example_payload_lorawan.json` | Example mapping and payload (decoded LoRaWAN uplink) |
| `mock_server.js` | Local node-wot server exposing a TD, to test the adapter without platform access |

## Properties and units

Each Property carries an object `{ "timestamp": <ISO 8601>, "value": <number> }`, one value per hour.
Units are those expected by the model (see the main README):

| Property | Unit | Model input |
|---|---|---|
| `soilMoisture15cm`, `soilMoisture30cm`, `soilMoisture45cm` | % (volumetric) | `soil_moisture_data/SM_past.csv` |
| `irrigationVolume` | m³ per hour | `irrigation_data/water_flow_past.csv` |
| `airTemperature` | °C | `climate_data/temperature_past.csv` |
| `precipitation` | mm per hour | `climate_data/precipitation_past.csv` |
| `solarRadiation` | W m⁻² | `climate_data/solar_rad_past.csv` |
| `relativeHumidity` | % | `climate_data/humidity_past.csv` |
| `windSpeed` | m s⁻¹ | `climate_data/wind_past.csv` |

## Try it

Requires Node.js ≥ 18.

```bash
cd wot
npm install

# terminal 1: local endpoint exposing the field TD
node mock_server.js thing_descriptions/field_TEMPLATE.td.json 8080

# terminal 2: convert the example payload and write it to the Properties
node adapter/adapter.js \
  --td http://localhost:8080/mock-acquaount-field-field-id \
  --mapping adapter/mapping_lorawan_example.json \
  --payload adapter/example_payload_lorawan.json
```

The mock server prints each value written; they can be read back with
`curl http://localhost:8080/mock-acquaount-field-field-id/properties/soilMoisture15cm`.

To adapt the pipeline to another source, copy the mapping file and change `source`, `scale` and `offset`
of each property (`value = raw × scale + offset`, dotted paths into the payload). Missing values are
skipped, never filled in. On a platform that requires authentication, set the `WOT_TOKEN` environment
variable (bearer token).
