var c;
function q() {
    msgpack.download("?q=" + document.getElementById("bla").value, {header: {accept:"application/x-msgpack"}}, function(res) {
        console.log("Fetching " + res.d[0].length + " elements in " + res.t / 1000 + "ms");
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
        c = new Chart($("#myChart")[0].getContext("2d"));
        var L = c.Line(data, {});
        $("#legend").html(L.generateLegend());


    })
}

$(function(){
    Chart.defaults.global.responsive = true;
    Chart.defaults.global.scaleBeginAtZero = true;
});
