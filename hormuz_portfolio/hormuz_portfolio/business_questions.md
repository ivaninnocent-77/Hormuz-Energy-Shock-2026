# Business questions the ten datasets can answer

Written after profiling every dataset. `Where` shows the report section (or the Shiny app).

| ID | Theme | Question | Data | Where |
|---|---|---|---|---|
| Q1 | Oil price shock | How much did oil prices rise from before the war to the peak, and when did the peak happen? | crude_oil_daily | Sec 5.1 |
| Q2 | Oil price shock | Did oil behave differently in each phase of the conflict, and are the differences real or just noise? | crude_oil_daily | Sec 5.1 |
| Q3 | Oil price shock | Was the market more jumpy (volatile) during active fighting than during the ceasefire? | crude_oil_daily | Sec 5.1 |
| Q4 | Oil price shock | Which kinds of news (military, diplomatic, economic) moved the oil price most on the day? | war_timeline + crude_oil_daily | Sec 5.1 |
| Q5 | Oil price shock | Did the gap between Brent and US WTI crude widen during the crisis? | crude_oil_daily | Sec 5.1 |
| Q6 | Oil price shock | Did prices fall as fast after the ceasefire as they rose when it collapsed? | crude_oil_daily | Sec 5.1 |
| Q7 | Historical context | How does the 2026 spike compare with earlier oil shocks such as 1973, 1979 and 1990? | fuel_prices_1970_2026 | Sec 5.2 |
| Q8 | Historical context | How unusual is a one-month jump of this size, statistically? | fuel_prices_1970_2026 | Sec 5.2 |
| Q9 | Historical context | Which decades have been the most volatile for oil prices? | fuel_prices_1970_2026 | Sec 5.2 |
| Q10 | Historical context | After past price spikes, did prices fall back within a year? | fuel_prices_1970_2026 | Sec 5.2 |
| Q11 | Pump prices | Which countries saw the biggest petrol price rises, in local currency and in dollars? | petrol_prices_comparison | Sec 5.3 |
| Q12 | Pump prices | How much of the crude oil price rise was passed on to drivers, and who was shielded? | petrol_prices_comparison + crude_oil_daily | Sec 5.3 |
| Q13 | Pump prices | Do countries that import more oil see bigger pump-price rises? | petrol_prices_comparison | Sec 5.3 |
| Q14 | Pump prices | How much extra does it cost a household to fill a 50-litre tank today? | petrol_prices_comparison | Sec 5.3 |
| Q15 | Country exposure | Which countries are the most exposed once we combine growth, markets, inflation and currency pain? | country_impact | Sec 5.4 |
| Q16 | Country exposure | Does depending more on imported oil mean a bigger hit to the economy? | country_impact | Sec 5.4 |
| Q17 | Country exposure | Do stock-market falls and GDP losses go together? | country_impact | Sec 5.4 |
| Q18 | Country exposure | How many people live in the most vulnerable countries? | country_impact | Sec 5.4 |
| Q19 | Country exposure | Can countries be grouped into natural tiers of risk? | country_impact | Sec 5.4 |
| Q20 | Country exposure | Which policy responses (subsidies, reserves, rationing, diversification) go with smaller economic damage? | country_impact | Sec 5.4 |
| Q21 | Country exposure | Are the biggest pump-price rises the same countries that suffer the biggest economic hit? | petrol + country_impact | Sec 5.7 |
| Q22 | Winners and losers | Overall, is the war's impact mostly negative, and which areas (economy, environment, consumers) carry the risk? | pros_cons_analysis | Sec 5.5 |
| Q23 | Strait of Hormuz trade | How far did crude, LNG, fertiliser and food-import traffic fall compared with normal? | four voy_intake_index files | Sec 5.6 |
| Q24 | Strait of Hormuz trade | How quickly did each flow collapse, and which fell first? | four voy_intake_index files | Sec 5.6 |
| Q25 | Strait of Hormuz trade | Did the April ceasefire bring any recovery in shipping flows? | four voy_intake_index files | Sec 5.6 |
| Q26 | Strait of Hormuz trade | Are flows still depressed as of the latest data, and by how much versus a year ago? | four voy_intake_index files | Sec 5.6 |
| Q27 | Strait of Hormuz trade | Did ship traffic move before the oil price did, or after? | voy files + crude_oil_daily | Sec 5.6 |
| Q28 | Strait of Hormuz trade | Do the fertiliser and food-import collapses move together (a food-security chain)? | fertilizer + agricultural files | Sec 5.6 |
| Q29 | Strait of Hormuz trade | How many 'days of normal trade' have been lost in total? | four voy_intake_index files | Sec 5.6 |
| Q30 | Data quality | Do the headline prices quoted in the timeline match the daily price data? | war_timeline + crude_oil_daily | Sec 4 |
| Q31 | What-if | If Brent reached a given price, what might pump prices be in each country? | petrol + crude_oil_daily | Shiny |
| Q32 | What-if | Which commodity and which month had the largest shortfall in trade flow? | four voy_intake_index files | Shiny |
