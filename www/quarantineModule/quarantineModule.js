// initial connections
$('*[data-value="quarantineDuration"][data-toggle="tab"]').one('shown.bs.tab', function() {
  console.log("shown.bs.tab");
  $('[id^=parsSC2]').connections('remove');
  $('[id^=plotsSC2]').connections('remove');
  $('#parsSC1-1').connections({
    to: '#parsDistr',
    'class': 'distParsConnection'  
  });
  $('#parsSC1-1').connections({
    to: '#parsSC1-2',
    'class': 'parsConnection'  
  });
  $('#parsSC1-2').connections({
    to: '#parsSC1-3',
    'class': 'parsConnection'  
  });
  $('#plotsSC1-1').connections({
    to: '#parsSC1-1',
    'class': 'parsPlotConnection'  
  });
  $('#plotsSC1-2').connections({
    to: '#parsSC1-2',
    'class': 'parsPlotConnection'  
  });
  $('#plotsSC1-3').connections({
    to: '#parsSC1-3',
    'class': 'parsPlotConnection'  
  });
});

$('*[data-value="sc1"]').on('shown.bs.tab', function() {
  console.log("sc1");
  $('[id^=parsSC2]').connections('remove');
  $('[id^=plotsSC2]').connections('remove');
  $('#parsSC1-1').connections({
    to: '#parsDistr',
    'class': 'distParsConnection'  
  });
  $('#parsSC1-1').connections({
    to: '#parsSC1-2',
    'class': 'parsConnection'  
  });
  $('#parsSC1-2').connections({
    to: '#parsSC1-3',
    'class': 'parsConnection'  
  });
  $('#plotsSC1-1').connections({
    to: '#parsSC1-1',
    'class': 'parsPlotConnection'  
  });
  $('#plotsSC1-2').connections({
    to: '#parsSC1-2',
    'class': 'parsPlotConnection'  
  });
  $('#plotsSC1-3').connections({
    to: '#parsSC1-3',
    'class': 'parsPlotConnection'  
  });
});

$('*[data-value="sc2"]').on('shown.bs.tab', function() {
  console.log("sc2")
  $('[id^=parsSC1]').connections('remove');
  $('#parsSC2-4').connections({
    to: '#parsDistr',
    'class': 'distParsConnection'  
  });
  $('#parsSC2-4').connections({
    to: '#parsSC2-5',
    'class': 'parsConnection'  
  });
  $('#parsSC2-5').connections({
    to: '#parsSC2-6',
    'class': 'parsConnection'  
  });
  $('#plotsSC2-4').connections({
    to: '#parsSC2-4',
    'class': 'parsPlotConnection'  
  });
  $('#plotsSC2-5').connections({
    to: '#parsSC2-5',
    'class': 'parsPlotConnection'  
  });
  $('#plotsSC2-6').connections({
    to: '#parsSC2-6',
    'class': 'parsPlotConnection'  
  });
});

$(document).on('shiny:visualchange', function(event) {
  $('[id^=pars]').connections('update');
  $('[id^=plots]').connections('update');
});
