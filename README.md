# DalmatinerDB Frontend

[![Build Status](https://travis-ci.org/dalmatinerdb/dalmatiner-frontend.svg)](https://travis-ci.org/dalmatinerdb/dalmatiner-frontend)
[Official Site](https://dalmatiner.io/)
[Query langauge](https://dalmatiner.readme.io/docs/dql-specification)

## Exploring dimensions ##

Dimensions can be explored in a tree like fasion where the data gets nested in the URL path a full example would be:

```
http://localhost:8080/collections/fifo/metrics/BmFjdGlvbgVjb3VudA==/namespaces//tags/host/values
                     collection: "fifo"
                                      metric: ["action", "count"]
                                                                  namespace: ""
                                                                               tag: "host"
```

Please note the due to the fact that metrics in DDB are list not flat values the metric key must be base64 encoded. It will be retured in an object (`key`: `metric path`) when the collecton is queried. The base64 encoded value does not need to be computed on the frontend.
