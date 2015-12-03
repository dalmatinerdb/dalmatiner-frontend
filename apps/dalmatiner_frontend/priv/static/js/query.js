var c;
Chart.defaults.global.responsive = true;

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
  $("#query").val("SELECT " + metric + " BUCKET " + bucket + " LAST 60s")
  q();
} else if (QueryString.query) {
  var query = decodeURIComponent(QueryString.query).replace(/\+/g, ' ');
  $("#query").val(query);
  q();
}



function q() {
  var query = $("#query").val();
  msgpack.download("?q=" + query, {header: {accept:"application/x-msgpack"}}, function(res) {
    console.log("Fetching " + res.d[0].length + " elements in " + res.t / 1000 + "ms");
    $("#time").text((res.t / 1000) + "ms");
    $("#timewrap").show();
    var idx = [];
    for (var i = 0; i < res.d[0].v.length; i++)
    {
      idx[i] = i;
    }

    colors = [
      [119, 158, 203],
      [100,  20, 100],
      [119, 221, 119],
      [255, 179,  71],
      [255, 105,  97],
      [220, 220, 220],
    ];
    points = res.d.map(function(d, i) {
      console.log(i);
      color = colors[i % colors.length];
      col = "rgba(" + color[0] + ", " + color[1] + ", " + color[2];
      return {
        fillColor: col + ", 0.1)",
        strokeColor: col + ", 1)",
        pointColor: col + ", 1)",
        pointStrokeColor: "#fff",
        data: d.v,
        label: d.n,
      };
    });
    console.log(points);
    var data = {
      labels: idx,
      datasets: points
    };
    if (c) {
      c.destroy();
    }
    var ctx = new Chart($("#myChart")[0].getContext("2d"));
    c = ctx.Line(data, {});
    $("#legend").html(c.generateLegend());
  })
}

$(function(){
  Chart.defaults.global.responsive = true;
  Chart.defaults.global.scaleBeginAtZero = true;
});
