$(function() {
    msgpack.download("", {header: {accept:"application/x-msgpack"}}, function(d) {
        console.log(d)
        d.forEach(function(e) {
            $("#buckets").append("<tr>"+
                                 "<td><a href='/buckets/"+ e.name + "'>" + e.name +"</a></td>"+
                                 "<td>" + e.resolution + "ms</td>"+
                                 "<td>" + e.ppf + "</td>"+
                                 "<td>" + e.grace + "ns</td>"+
                                 "<td>" + e.ttl + "</td>"+
                                 "</tr>")
        })
    })
})
