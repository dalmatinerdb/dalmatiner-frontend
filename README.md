# DalmatinerDB Frontend

## Query langauge

The frontend query language is rather remotely related to SQL, this should make it simple to pick up. The focus is on querying a metric at a time given those queries are incredible fast, however globs can be used to query multiple metrics at a time.

The basic syntax looks like this:
```
SELECT <FIELDS> [ FROM <ALIASES>] <TIME RANGE> [IN <RESOLUTION>]
```

Please keep in mind that each query can only return a single row of data at the moment.

### Fields (`SELECT` section)
A field can either be a metric, a alias to a metric or a function:

* `cloud.zones.cpu.usage.eca485cf-bdbb-4ae5-aba9-dce767 BUCKET tachyon` - a fully qualifeid metric.
* `vm` - an alias that is defined in the `FROM` section of the queryt.
* `avg(vm, 1m)` - a aggregation function.

Fields can be aliased for output adding a `AS <alias>` directive after the field.

Multiple fields can be given seperating two fields with a `,`. The resolution of fields do not have to be the same, there is no check enforcing this!

### Aliases (`FROM` section)
When a metric is used multime times it often gets more readable to alias this metric, this is done in the `FROM` section. Multiple elements can be given seperated with a `,`. Each element takes the form: `<metric> BUCKET <bucket> AS <alias>`.

It is possible to match multiple metrics by using a mulitget aggregator and a glob to match a metric. Valid multiget aggregators are `sum` and `avg`. For example: `sum(some.metric.* BUCKET b)`.

### TIME RANGE
There are two ways to declare ranges, while nubers here represent seconds DalmatinerDB does not care about the time unit at all:

```
BETWEEN <start:reltime> AND <end:reltime>
```

Will select all pints between the `start` and the `end`. However often the most used query is 'what happned in the past X seconds' so there is a simplified form for this:

```
LAST <amount:int> [<unit:time-unit>]
```

### Resolution (`IN` section)
By default queries treat incoming data as a one second resolution, however this can be adjusted by passing a resolution section to the query, the syntax is: `IN <resolution:time>`.

## Data Types

### Time
There are two ways to declare times:

* relatively, in which case the time is a simple integer and corresponds to a number of metric points used. (i.e. `60`)
* absolute, in which case the time is a integer followed by a time unit such as `ms`, `s`, `m`, `h`, `d` and `w`, in this case the resolution of the metric is taken into acount.

### Relative times
Relative times can either be the keyword `NOW`, a absolute timestamp (a simple integer) or a relative time in the past such as `<time> AGO`.

### Metrics
Metrics are simple strings that are optionally seperated by dots and a second strring for the bucket. The two strings are seperated by the keyword `BUCKET`.

Example:
```
cloud.zones.cpu.usage.eca485cf-bdbb-4ae5-aba9-dce767 BUCKET tachyon
```

### Aggregation functions
Aggregation functions aggregate a metric over a given range of time and decrease the resolution by doing so. Aggregation functions can be nested in which case the 'higher' functiosn work with the decreased resolution of lower functions and not the raw resolution. This means the correct code to get the 1m average over 10s sums from a 1s resolution metric would be  `avg(sum(m, 10s), 1m)` not `avg(sum(m, 10s), 6s)` - however this does not apply when using the point and not the time declaration, so it would be: `avg(sum(m, 10s), 6)` not `avg(sum(m, 10s), 60)` (please note the missing `s`).

#### min/2
The minimal value over a given range of time.

#### max/2
The maximal value over a given range of time.

#### sum/2
The sum of all values of a timerange.

#### avg/2
The average of a timerange (this is the mean not the median).

#### derivate/1
Calculates the derivate of a metric, meaing N'(X)=N(X) - N(X-1)

## Examples

Calcuates the min, max and average of a metric over a hour.
```
SELECT min(vm, 10m), avg(vm, 10m), max(vm, 10m) AS max FROM cloud.zones.cpu.usage.eca485cf-bdbb-4ae5-aba9-dce767 BUCKET tachyon AS vm LAST 60m
```

## Exploring dimensions ##

Dimentions can be explored in a tree like fasion where the data gets nested in the URL path a full example would be:

```
http://localhost:8080/collections/fifo/metrics/BmFjdGlvbgVjb3VudA==/namespaces//tags/host/values
                     collection: "fifo"
                                      metric: ["action", "count"]
                                                                  namespace: ""
                                                                               tag: "host"
```

Please note the due to the fact that metrics in DDB are list not flat values the metric key must be base64 encoded. It will be retured in an object (`key`: `metric path`) when the colleciton is querried. The base64 encoded value does not need to be computed on the frontend.
