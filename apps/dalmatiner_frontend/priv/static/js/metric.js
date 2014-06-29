$(function() {
    msgpack.download("/metrics", {}, function(d) {
        d.forEach(function(e) {
            $("#metrics").append("<tr><td>" + e +"</td></tr>")
        })
    })
})
