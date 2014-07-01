function q() {
    msgpack.download("/metrics?q=" + document.getElementById("bla").value,{}, function(res) {
        console.log("Fetching " + res.d[0].length + " elements in " + res.t / 1000 + "ms");
        var idx = [];
        for (var i = 0; i < res.d[0].length; i++)
        {
            idx[i] = i;
        }
        points = res.d.map(function(d) {
            return {
                fillColor : "rgba(220,220,220,0.5)",
                strokeColor : "rgba(220,220,220,1)",
                pointColor : "rgba(220,220,220,1)",
                pointStrokeColor : "#fff",
                data : d
            };
        });
        console.log(points);
        var data = {
            labels : idx,
            datasets : points
        };
        c.Line(data);

    })
}

$(function(){
    var ctx = document.getElementById("myChart").getContext("2d");
    c = new Chart(ctx);
});
