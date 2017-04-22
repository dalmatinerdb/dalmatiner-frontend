# DalmatinerDB Frontend

[Query langauge](https://dalmatiner.readme.io/docs/dql-specification)


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
