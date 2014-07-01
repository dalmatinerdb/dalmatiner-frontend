$(function() {
    msgpack.download("", {header: {accept:"application/x-msgpack"}}, function(d) {
        console.log(d)
        d.forEach(function(e) {
            $("#buckets").append("<tr><td><a href='/buckets/"+ e + "'>" + e +"</a></td></tr>")
        })
    })
})
