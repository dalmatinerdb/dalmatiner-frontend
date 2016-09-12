$(function() {
    msgpack.download(
      "", {header: {accept:"application/x-msgpack"}},
      function(d) {
        console.log(d)
        d.forEach(function(e) {
          var date = new Date(e.timestamp/1000/1000);
          $("#events").append("<tr><td>" + date +
                              "</td><td>" + JSON.stringify(e.event) + "</td></tr>")
        })
      })
})
