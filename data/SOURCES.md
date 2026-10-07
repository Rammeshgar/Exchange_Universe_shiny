# Data and asset sources

Prepared 7 October 2026. Provider rates and static currency geography are deliberately separate.

## Rates

[APILayer Exchange Rates Data API](https://marketplace.apilayer.com/exchangerates_data-api/tabs/api_docs), endpoints `/symbols`, `/latest`, `/timeseries`. Authenticated on the server using the user's private subscription. Current/daily observations retain their source and date; source timestamp is included for the current indicative snapshot in CSV. Provider data is subject to the user's API agreement, not the country-data license below.

## Country polygons

[Natural Earth 1:50m Admin 0 countries](https://github.com/nvkelso/natural-earth-vector/blob/master/geojson/ne_50m_admin_0_countries.geojson). Public-domain geographic data; [Natural Earth terms](https://www.naturalearthdata.com/about/terms-of-use/). Simplified to 0.025-degree tolerance, retaining topology, with Antarctica excluded. Packaged as `world.rds`; 241 features. Borders are an exploratory cartographic view, not a legal boundary authority. Attribution is visible on the map.

## Current currency geography

[mledoze/countries](https://github.com/mledoze/countries), derived from `countries.json`: names, ISO identifiers, currencies, regions and coordinates. `countries.csv` is a derived database distributed under the [Open Database License](https://opendatacommons.org/licenses/odbl/1-0/), included in `COUNTRIES-LICENSE.txt`. Currency-use metadata can change and requires periodic review.

Current-use corrections applied to the upstream dataset:

- Bulgaria uses EUR from 1 January 2026. [Council of the European Union](https://www.consilium.europa.eu/en/press/press-releases/2025/07/08/bulgaria-ready-to-use-the-euro-from-1-january-2026-council-takes-final-steps/).
- Curaçao and Sint Maarten use XCG (Caribbean guilder), introduced 31 March 2025. [Central Bank announcement](https://www.centralbank.cw/storage/app/media/press_releases_2025_1/PB2025-008%20Caribbean%20guilder%20legal%20tender%20EN.pdf). Missing provider support is shown explicitly; ANG is not silently used as a replacement.
- Zimbabwe's Zimbabwe Gold code is ZWG rather than legacy ZWL. [Zimbabwe Revenue Authority exchange-rate document](https://www.zimra.co.zw/legislation/category/77-exchange-rates-2023?download=4315%3Azimra-rates-of-exchange-for-customs-purposes-for-period-25-to-31-march-2025-zwg). Multiple-currency records are retained.

The map is current currency use even for a historical observation; it does not claim to reconstruct historical tender rules.

## Typography and icons

[Inter](https://github.com/rsms/inter), local Latin WOFF2, SIL Open Font License 1.1. License included in `www/fonts/OFL-Inter.txt`. No external font request is required. Interface icons are app-local SVG paths.
