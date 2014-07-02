# DalmatinerDB Frontend

## Query langauge

The frontend query language is rather remotely related to SQL, this should make it simple to pick up. The focus is on querying a metric at a time given those queries are incredible fast, however globs can be used to query multiple metrics at a time.

The basic syntax looks like this:
```
SELECT <RANGE SECTION> FROM <SELECTION SECTION>[ <AGGREGATION SECTION>]
```

All keywords need to be uppercase!

Please keep in mind that each query can only return a single row of data at the moment.

### Range Section
There are two ways to declare ranges, while nubers here represent seconds DalmatinerDB does not care about the time unit at all:

```
BETWEEN <start> AND <end>
```

Will select all pints between the `start` and the `end`. However often the most used query is 'what happned in the past X seconds' so there is a simplified form for this:

```
LAST <count> S
```

This is equivalent to `BETWEEN now() - <count> AND now()` (this is not valid syntax).

### Selection Sections
This is where we define which metrics are slected, any string can go here, alternatively multiple metrics can be combined using either:

```
AVG OF <metric with globs>
```

or

```
SUM OF <metric with globs>
```

### Aggregation section
This is optional however it can be used to combine multiple points into one the syntax always gives the number of points to combine, chaining multiple aggregates will mean one execution is followed after another i.e. `SUM OVER 60 SUM OVER 60` would mean the same as `SUM OVER 360`

Currently supported aggregations are:

* `SUM OVER <points>` - calculates the sum over the given number of datapoints.
* `AVG OVER <points>` - calculates the average over the given number of datapoitns.
* `MAX OF <points>` - finds the maximum of the datapoints.
* `MIN OF <points>` - finds the minimum of the datapoints.
* `DERIVATE` - This is a bit of a special case, it takes no agruments but converts a column of absolute values into their deltas (the number of points gets reduced by one since the first point is removed).

## Examples

Calcuates the average hourly cpu usage over the last 24h:
```
SELECT LAST 86400 S FROM AVG OF cloud.zones.cpu.usage.* AVG OVER 3600
```
