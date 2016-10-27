$(function() {
  function is_hist(e) {
    if (e.return == "histogram")
      return true;
    for (i=0; i < e.signature.length; i++) {
      var s = e.signature[i];
      if (s == "histogram")
        return true;
    }
    return false;
  };
  function is_aggr(e) {
    for (i=0; i < e.signature.length; i++) {
      var s = e.signature[i];
      if (s == "time")
        return true;
    }
    return false;
  };

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
      if (is_hist(e)) {
        $("#hist_funs").append("<tr>" +
                               "<td>" + e.name +"</td>"+
                               "<td>" + e.desc +"</td>"+
                               "<td>" + e.help +"</td>"+
                               "</tr>")
      } else if (is_aggr(e)) {
        $("#aggr_funs").append("<tr>" +
                               "<td>" + e.name +"</td>"+
                               "<td>" + e.desc +"</td>"+
                               "<td>" + e.help +"</td>"+
                               "</tr>")
      } else if (e.combiner_type != "none") {

        $("#comb_funs").append("<tr>" +
                               "<td>" + e.name +"</td>"+
                               "<td>" + e.desc +"</td>"+
                               "<td>" + e.help +"</td>"+
                               "</tr>")

      } else {
        $("#trans_funs").append("<tr>" +
                                "<td>" + e.name +"</td>"+
                                "<td>" + e.desc +"</td>"+
                                "<td>" + e.help +"</td>"+
                                "</tr>")
      }
    })
  })
})
