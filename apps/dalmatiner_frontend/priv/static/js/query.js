var c;

$("#permalink").hide();
$("#timewrap").hide();

// If browser supports history pushStete, avoid reloading page on query change
if (typeof(history) == "object" && typeof(history.pushState) == "function") {
  $("#queryfrom").on("submit", function(e) {
    var query = $("#query").val();
    history.pushState({query: query}, document.title, "/?query=" + encodeURIComponent(query));
    e.preventDefault();
    q();
  });
  $(window).on("popstate", function(e) {
    var m = window.location.search.match(/(\?|&)query=(.*)(&|$)/);
    var query = m ? decodeURIComponent(m[2]) : "";
    if (query) {
      $("#query").val(query);
      q();
    } else {
      window.location.reload();
    }
  });
}

var QueryString = function () {
                    // This function is anonymous, is executed immediately and
                    // the return value is assigned to QueryString!
                    var query_string = {};
                    var query = window.location.search.substring(1);
                    var vars = query.split("&");
                    for (var i=0;i<vars.length;i++) {
                      var pair = vars[i].split("=");
                      // If first entry with this name
                      if (typeof query_string[pair[0]] === "undefined") {
                        query_string[pair[0]] = pair[1];
                        // If second entry with this name
                      } else if (typeof query_string[pair[0]] === "string") {
                        var arr = [ query_string[pair[0]], pair[1] ];
                        query_string[pair[0]] = arr;
                        // If third or later entry with this name
                      } else {
                        query_string[pair[0]].push(pair[1]);
                      }
                    }
                    return query_string;
                  }();

if (QueryString.metric && QueryString.bucket) {
  var metric = decodeURIComponent(QueryString.metric);
  var bucket = decodeURIComponent(QueryString.bucket);
  if (! bucket.match(/^'.*'$/)) {
    bucket = "'" + bucket + "'";
  }
  $("#query").val("SELECT " + metric + " BUCKET " + bucket + " LAST 60s");
  q();
} else if (QueryString.query) {
  var query = decodeURIComponent(QueryString.query).replace(/\+/g, ' ');
  $("#query").val(query);
  q();
}



function render_metrics(start, metrics, markers) {
  var data,
      legend = [];
  $("#results").text("");
  if (metrics.length > 0) {
    $("#results").append($("<h3></h3>").text("Metrics")).append($("<hr/>"))
    data = metrics.map(function(s) {
             var resolution = s.resolution,
                 values = s.values,
                 points = new Array(values.length);

             legend.push(s.name);
             for (var i = 0; i < values.length; i++) {
               points[i] = {
                 "date": new Date(start + (i * resolution)),
                 "value": values[i]
               };
             }
             return points;
           });

    MG.data_graphic({
      data: data,
      height: 400,
      markers: markers,
      full_width: true,
      target: '#results',
      legend: legend,
      max_data_size: 11,
      legend_target: '#legend',
      missing_is_hidden: true,
      aggregate_rollover: true
    });
  }
}

function render_event(event) {
  var div = $("<div class='events'></div>"),
      hdr = $("<h5></h5>").text(event.name),
      tbl = $("<table></table>"),
      th  = $("<thead><tr><th>Date</th><th>Event</th></tr></thead>"),
      tb  = $("<tbody></tbody>");
  event.values.map(function (e) {
    var tr = $("<tr></tr>"),
        date = new Date(e.timestamp/1000/1000);
    tr.append($("<td></td>").text(date))
    .append($("<td></td>").append($("<pre></pre>").text(JSON.stringify(e.event, null, 2))));
    tb.append(tr)
  });
  tbl.append(th)
  tbl.append(tb)
  div.append(hdr) .append(tbl)
  $("#events").append(div)
}
function render_events(start, events) {
  $("#events").text("");
  if (events.length > 0) {
    $("#events").append($("<h3></h3>").text("Events")).append($("<hr/>"))
    events.map(render_event)
  }
}

function render_graph(graph) {
  $("#graph").text("")
  if (graph) {
    $("#graph")
    .append($("<h3></h3>").text("Query graph"))
    .append($("<pre/>")
            .append(Viz(graph, {scale: 0.1})))
  }
}

function q() {
  var query = $("#query").val();
  var base = "?"
  $("#events").text("").append($("<hr/>"))
  if ($("#debug").is(':checked')) {
    base = "?graph&q=";
  } else {
    base = "?q=";
  }
  msgpack.download(base + query, {header: {accept:"application/x-msgpack"}}, function(res) {
    $("#permalink").attr("href", "/?query=" + encodeURIComponent(query));
    $("#permalink").show();
    $("#time").text((res.query_time / 1000) + "ms");
    $("#timewrap").show();

    var start = res.start * 1000,
        markers = [];

    gres = res
    metrics = res.results.filter(function(e) {
                return e.type == "metrics"
              });
    console.log(metrics[0])
    console.log("Fetched " + metrics[0].values.length * metrics.length + " elements in " +
                res.query_time / 1000 + "ms");
    events = res.results.filter(function(e) {
               return e.type == "events"
             });
    render_metrics(start, metrics, markers);
    render_events(start, events);
    render_graph(res.graph)

  });
}
