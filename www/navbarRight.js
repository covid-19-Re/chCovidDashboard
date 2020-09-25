
$('#tab > li:last-child').before('<li><a href="https://ibz-shiny.ethz.ch/covid-19-re/" target="_blank">\
<i class="fa fa-external-link fa-fw"></i>&nbsp;R<sub>e</sub> (Worldwide)</a></li>');
// $('.dropdown-menu').append('\
//   <li>\
//     <a href="https://github.com/covid-19-Re/chCovidDashboard" target="_blank">\
//       <i class="fa fa-github fa-fw"></i>\
//       Source\
//     </a>\
//   </li>')
$(function () {
  $('[data-toggle="tooltip"]').tooltip({ boundary: 'window' })
});
