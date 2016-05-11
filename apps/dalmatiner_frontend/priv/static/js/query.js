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



function q() {
  var query = $("#query").val();
  msgpack.download("?q=" + query, {header: {accept:"application/x-msgpack"}}, function(res) {
    console.log("Fetched " + res.d[0].v.length + " elements in " + res.t / 1000 + "ms");
    $("#permalink").attr("href", "/?query=" + encodeURIComponent(query));
    $("#permalink").show();
    $("#time").text((res.t / 1000) + "ms");
    $("#timewrap").show();

    var start = res.s * 1000,
        legend = [],
        data;

    data = res.d.map(function(s) {
      var resolution = s.r,
          values = s.v,
          points = new Array(values.length);
      
      legend.push(s.n);
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
      full_width: true,
      target: '#results',
      legend: legend,
      legend_target: '#legend',
      missing_is_hidden: true,
      aggregate_rollover: true
    });
  });
}
