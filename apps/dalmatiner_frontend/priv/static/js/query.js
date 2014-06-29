function init() {

}
function q() {
    msgpack.download("/metrics?q=" + document.getElementById("bla").value,{}, function(res) {
        console.log("Fetching " + res.d.length + " elements in " + res.t / 1000 + "ms");
        var idx = [];
        for (var i = 0; i < res.d.length; i++)
        {
            idx[i] = i;
        }
        var data = {
            labels : idx,
            datasets : [
                {
                    fillColor : "rgba(220,220,220,0.5)",
                    strokeColor : "rgba(220,220,220,1)",
                    pointColor : "rgba(220,220,220,1)",
                    pointStrokeColor : "#fff",
                    data : res.d
                }
            ]
        };
        c.Line(data);

    })
}


$(function(){
    var ctx = document.getElementById("myChart").getContext("2d");
    c = new Chart(ctx);
});
