function getParameterByName(name, url) {
  if (!url) url = window.location.href;
  name = name.replace(/[\[\]]/g, "\\$&");
  var regex = new RegExp("[?&]" + name + "(=([^&#]*)|&|#|$)"),
      results = regex.exec(url);
  if (!results) return null;
  if (!results[2]) return '';
  return decodeURIComponent(results[2].replace(/\+/g, " "));
}

$(function() {
  $("#filter").val(getParameterByName("filter", window.location));
  $("#start").val(getParameterByName("start", window.location));
  $("#end").val(getParameterByName("end", window.location));
  $("#bucket").val(getParameterByName("bucket", window.location));
  msgpack.download(
    "", {header: {accept:"application/x-msgpack"}},
    function(d) {
      $("#time").text((d.t / 1000) + "ms");
      $("#count").text(d.e.length);
      d.e.forEach(function(e) {
        var date = new Date(e.timestamp/1000/1000);
        var row = "<tr>" +
          "<td>" + date + "</td>" +
          "<td><pre>" + JSON.stringify(e.event, null, 2) + "</pre></td>" + +
          "</tr>";
        $("#events").append(row)
      })
    })
})
