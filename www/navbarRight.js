
var header = $('.navbar > .container-fluid');
header.append('\
  <ul class="nav navbar-nav navbar-right">\
    <li><a href="https://github.com/covid-19-Re/chCovidDashboard" class="fab fa-github fa-2x" data-toggle="tooltip" data-placement="left" title="Source (GitHub)"></a></li>\
  </ul>')
$(function () {
  $('[data-toggle="tooltip"]').tooltip()
})

