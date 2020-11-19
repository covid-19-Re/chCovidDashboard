// initial connections
$('*[data-value="ttiq"][data-toggle="tab"]').one('shown.bs.tab', function() {
  console.log("shown.bs.tab");
  $('#parsTerTI').connections('remove');
  $('#parsSecTI').connections('remove');
  $('#parsTerTTIQ').connections({
    to: '#parsDist',
    'class': 'distParsConnection'
  });
  $('#plotsTerTTIQ').connections({
    to: '#parsTerTTIQ',
    'class': 'parsPlotConnection'
  });
});

$('*[data-value="terTTIQ"]').on('shown.bs.tab', function() {
  console.log("terTTIQ");
  $('#parsTerTI').connections('remove');
  $('#parsSecTI').connections('remove');
  $('#parsTerTTIQ').connections({
    to: '#parsDist',
    'class': 'distParsConnection'
  });
  $('#plotsTerTTIQ').connections({
    to: '#parsTerTTIQ',
    'class': 'parsPlotConnection'
  });
});

$('*[data-value="terTI"]').on('shown.bs.tab', function() {
  console.log("terTI");
  $('#parsTerTTIQ').connections('remove');
  $('#parsSecTI').connections('remove');
  $('#parsTerTI').connections({
    to: '#parsDist',
    'class': 'distParsConnection'
  });
  $('#plotsTerTI').connections({
    to: '#parsTerTI',
    'class': 'parsPlotConnection'
  });
});

$('*[data-value="secTI"]').on('shown.bs.tab', function() {
  console.log("secTI");
  $('#parsTerTTIQ').connections('remove');
  $('#parsTerTI').connections('remove');
  $('#parsSecTI').connections({
    to: '#parsDist',
    'class': 'distParsConnection'
  });
  $('#plotsSecTI').connections({
    to: '#parsSecTI',
    'class': 'parsPlotConnection'
  });
});

$(document).on('shiny:visualchange', function(event) {
  $('[id^=pars]').connections('update');
  $('[id^=plots]').connections('update');
});
