function init() {

}
function q() {
    var xmlhttp = new XMLHttpRequest();
    xmlhttp.open("GET","/metrics?q=" + document.getElementById("bla").value, false);
    xmlhttp.send();
    var res = JSON.parse(xmlhttp.responseText);
    var idx = [];
    for (var i = 0; i < res.length; i++)
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
                data : res
            }
        ]
    };
    c.Line(data);
}


$(function(){
    var ctx = document.getElementById("myChart").getContext("2d");
    c = new Chart(ctx);
});
