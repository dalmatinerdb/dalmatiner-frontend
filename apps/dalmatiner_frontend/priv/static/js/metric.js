$(function() {
  msgpack.download("", {header: {accept:"application/x-msgpack"}}, function(d) {
    console.log(d);
    var bucket = document.location.pathname.replace('/buckets/','');
    var base_url = "/?bucket=" + encodeURIComponent(bucket);
    d.forEach(function(e) {
      var url = base_url + "&metric=" + encodeURIComponent(e);
      console.log(url)
      $("#metrics").append("<tr><td><a href=\""+url+"\">" + e +"</a></td></tr>");
    })
  })
})
