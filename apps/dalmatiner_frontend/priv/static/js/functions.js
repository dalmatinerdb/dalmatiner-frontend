$(function() {
  msgpack.download("/functions", {header: {accept:"application/x-msgpack"}}, function(d) {
    d.sort(function (a, b) {
      if (a.name > b.name) {
        return 1;
      }
      if (a.name < b.name) {
        return -1;
      }
      // a must be equal to b
      return 0;
    });
    d.forEach(function(e) {
      if (e.combiner_type == "none") {
        $("#aggr_funs").append("<tr>" +
                               "<td>" + e.name +"</td>"+
                               "<td>" + e.desc +"</td>"+
                               "<td>" + e.help +"</td>"+
                               "</tr>")
      } else {
        $("#comb_funs").append("<tr>" +
                               "<td>" + e.name +"</td>"+
                               "<td>" + e.desc +"</td>"+
                               "<td>" + e.help +"</td>"+
                               "</tr>")

      }
    })
  })
})
