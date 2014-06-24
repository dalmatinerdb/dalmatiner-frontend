$(function() {
    $.ajax("/metrics").done(function(d) {
        d = d.sort();
        d.forEach(function(e) {
            $("#metrics").append("<tr><td>" + e +"</td></tr>")
        })

    })
})
