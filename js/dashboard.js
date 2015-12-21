(function ($) {

$(function () {
  var tzOffset = new Date().getTimezoneOffset() / 60;
  var dashboard = $('<div id="dashboard"></div>').appendTo('#body');
  var dailyDetail = $('<div id="daily-detail"></div>').appendTo(dashboard);
  var cal = $.drcal();
  cal.bind('drcal.weekRender', function (_, tr) {
    $('td', tr).each(function (_, td) {
      $(td).append('<div><div class="daynum">' + $(td).attr('day') + '</div></div>');
    });
    cal.mask('Loading...');
    $.get('/review/stats/daily?start=' + cal.find('td[date]:first').attr('date') + '&end=' + cal.find('td[date]:last').attr('date') + '&tzoffset=' + tzOffset)
     .done(function (stats) {
       $.each(stats, function (_, stat) {
         var td = cal.findCell(new Date(stat.date[0], stat.date[1] - 1, stat.date[2]));
         if (td.length > 0) {
           if (stat.reviewed) {
             td.find('> div').append($('<div class="reviews-completed"></div>').text(stat.reviewed));
           }
           if (stat.scheduled) {
             td.find('> div').append($('<div class="reviews-scheduled"></div>').text(stat.scheduled));
           }
         }
       });
       cal.unmask();
     })
     .fail(function (xhr) {cal.unmask(); toast('error', 'Failed to load weekly data.');});
  }).changeMonth(new Date());
  cal.find('.prev, .next').addClass('light');
  cal.delegate('td', 'click', function () {
    cal.find('td').removeClass('selected');
    $(this).addClass('selected');
    $.get('/review/stats/detailed?start=' + $(this).attr('date') + '&end=' + $(this).attr('date') + '&tzoffset=' + tzOffset)
     .done(function (stats) {
       var reviewedList = $('<table class="links reviewed"><thead><tr><th colspan="2">Reviewed</th></tr></thead><tbody></tbody></table>');
       var tbody = reviewedList.find('tbody');
       $.each(stats.reviewed, function (_, stat) {
         var tr = $(
           '<tr class="inline-link">'
           + '<td><a></a></td>'
           + '<td><b class="grade"></b></td>'
         + '</tr>');
         tr.find('a').attr('href', '/link/' + stat.linkNumber).text(stat.learn);
         tr.find('b').addClass('grade' + stat.grade);
         tbody.append(tr);
       });
       var scheduledList = $('<table class="links scheduled"><thead><tr><th>Scheduled</th></tr></thead><tbody></tbody></ol>');
       tbody = scheduledList.find('tbody');
       $.each(stats.scheduled, function (_, stat) {
         var tr = $('<tr class="inline-link"><td><a></a></td></tr>');
         tr.find('a').attr('href', '/link/' + stat.linkNumber).text(stat.learn);
         tbody.append(tr);
       });
       dailyDetail.empty().append(reviewedList).append(scheduledList).append('<div class="clear"></div>');
     })
     .fail(function (xhr) {toast('error', 'Failed to load daily data.');});
  });
  cal.changeMonth(new Date());
  $('.today', cal).click();
  dashboard.append(cal);
});

})(jQuery);
