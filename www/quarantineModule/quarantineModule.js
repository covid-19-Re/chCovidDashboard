$('*[data-value="quarantineDuration"]').on('shown.bs.tab', function() {
  $('#pars1').connections({
    to: '#parsDistr',
    'class': 'parsConnected'  
  });
  $('#pars1').connections({
    to: '#pars2',
    'class': 'parsConnected'  
  });
  $('#pars2').connections({
    to: '#pars3',
    'class': 'parsConnected'  
  });
  $('#pars3').connections({
    to: '#pars4',
    'class': 'parsConnected'  
  });
  $('#pars4').connections({
    to: '#pars5',
    'class': 'parsConnected'  
  });
  $('#pars5').connections({
    to: '#pars6',
    'class': 'parsConnected'  
  });
  $('#plots1').connections({
    to: '#pars1',
    'class': 'parsToPlot'  
  });
  $('#plots2').connections({
    to: '#pars2',
    'class': 'parsToPlot'  
  });
  $('#plots3').connections({
    to: '#pars3',
    'class': 'parsToPlot'  
  });
  $('#plots4').connections({
    to: '#pars4',
    'class': 'parsToPlot'  
  });
  $('#plots5').connections({
    to: '#pars5',
    'class': 'parsToPlot'  
  });
  $('#plots6').connections({
    to: '#pars6',
    'class': 'parsToPlot'  
  });
});

$(document).on('shiny:visualchange', function(event) {
  $('[id^=pars]').connections('update');
  $('[id^=plots]').connections('update');
});

