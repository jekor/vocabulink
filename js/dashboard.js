// Copyright 2011, 2012 Chris Forno
//
// This file is part of Vocabulink.
//
// Vocabulink is free software: you can redistribute it and/or modify it under
// the terms of the GNU Affero General Public License as published by the Free
// Software Foundation, either version 3 of the License, or (at your option) any
// later version.
//
// Vocabulink is distributed in the hope that it will be useful, but WITHOUT ANY
// WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR
// A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
// details.
//
// You should have received a copy of the GNU Affero General Public License
// along with Vocabulink. If not, see <http://www.gnu.org/licenses/>.

(function ($) {

$(function () {
  var tzOffset = new Date().getTimezoneOffset() / 60;
  var dashboard = $('<div id="dashboard"></div>').appendTo('#body');
  var dailyDetail = $('<div id="daily-detail"></div>').appendTo(dashboard);
  var cal = $.drcal();
  cal.bind('drcal.monthRender', function () {
    cal.find('td').each(function (_, td) {
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
     .fail(function (xhr) {cal.unmask(); V.toastError(xhr.responseText, false);});
  }).bind('drcal.monthChange', function () {
    cal.find('td').removeClass('selected');
  });
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
         tr.find('a').attr('href', '/link/' + stat.linkNumber).text(stat.foreignPhrase);
         tr.find('b').addClass('grade' + stat.grade);
         tbody.append(tr);
       });
       var scheduledList = $('<table class="links scheduled"><thead><tr><th>Scheduled</th></tr></thead><tbody></tbody></ol>');
       tbody = scheduledList.find('tbody');
       $.each(stats.scheduled, function (_, stat) {
         var tr = $('<tr class="inline-link"><td><a></a></td></tr>');
         tr.find('a').attr('href', '/link/' + stat.linkNumber).text(stat.foreignPhrase);
         tbody.append(tr);
       });
       dailyDetail.empty().append(reviewedList).append(scheduledList).append('<div class="clear"></div>');
     })
     .fail(function (xhr) {V.toastError(xhr.responseText, false);});
  });
  cal.changeMonth(new Date());
  $('.today', cal).click();
  dashboard.append(cal);
});

})(jQuery);
