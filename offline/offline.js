(function ($) {

  function nowS() {return Math.round(Date.now() / 1000);}

  function disable(el) {
    el.attr('enabled', null).attr('disabled', true);
  }

  function enable(el) {
    el.attr('disabled', null).attr('enabled', true);
  }

  // Here's how we structure local storage:
  //  * 'lastSync' - a unix timestamp of when we last synced with the server
  //  * 'reviewQueue' - a list of link objects
  //  * 'gradeQueue' - a list of [link,grade,recallTime,reviewedAt] triples

  function getReviewQueue() {
    var reviewQueue = localStorage.getItem('reviewQueue');
    return reviewQueue ? JSON.parse(reviewQueue) : [];
  }

  function getGradeQueue() {
    var gradeQueue = localStorage.getItem('gradeQueue');
    return gradeQueue ? JSON.parse(gradeQueue) : [];
  }

  function removeLinksFromReviewQueue(links) {
    var linkNumbers = $.map(links, function (l) {return l['linkNumber']});
    var newQueue = $.grep(getReviewQueue(), function (l) {return $.inArray(l['linkNumber'], linkNumbers) === -1;});
    localStorage['reviewQueue'] = JSON.stringify(newQueue);
  }

  function addGrade(link, grade, recallTime, reviewedAt) {
    var newQueue = getGradeQueue();
    newQueue.push([link['linkNumber'], grade, recallTime, reviewedAt]);
    localStorage['gradeQueue'] = JSON.stringify(newQueue);
  }

  function linksDueForReview() {
    var reviewQueue = getReviewQueue();
    var now = nowS();
    var due = $.grep(reviewQueue, function (l) {return l['targetTime'] <= now;});
    // Shuffle randomly.
    due.sort(function () {return Math.random() - 0.5;});
    return due;
  }

  function sync(callback) {
    // First, POST all pending grades. Even if we had HTTP pipelining, it's
    // probably not supported for POSTs.
    var gradeQueue = getGradeQueue();
    var posted = [];
    var failed = [];
    var callback_ = function () {
      if (posted.length + failed.length === gradeQueue.length) {
        localStorage.setItem('gradeQueue', JSON.stringify(failed));
        // Now, pull all upcoming reviews.
        $.get('/review/upcoming')
         .done(function (links) {
           localStorage.setItem('reviewQueue', JSON.stringify(links));
           localStorage.setItem('lastSync', JSON.stringify(nowS()));
           callback();
         })
         .fail(function () {
           queueUpdated = true;
           callback();
         });
      }
    }
    if (gradeQueue.length) {
      for (var i in gradeQueue) {
        (function (grade) {
          $.post('/review/' + grade[0], {'grade': grade[1]
                                        ,'time':  grade[2]
                                        ,'when':  grade[3]})
           .done(function () {
             posted.push(grade);
             callback_();
           })
           .fail(function () {
             failed.push(grade)
             callback_();
           })
        })(gradeQueue[i]);
      }
    } else {
      callback_();
    }
  }

  function review(dueQueue, callback) {
    if (dueQueue.length) {
      var link = dueQueue.pop();
      $('#foreign td').text(link['foreign']);
      var startTime = Date.now();
      $('#familiar td').addClass('hidden').text('?').click(function () {
        var recallTime = Date.now() - startTime;
        var reviewedAt = nowS();
        var $familiar = $(this);
        $familiar.removeClass('hidden').text(link['familiar']).unbind('click');
        $('#gradeBar td').click(function (e) {
          $('#gradeBar td').unbind('click');
          var $grade = $(this);
          var grade = $grade.attr('grade');
          addGrade(link, grade, recallTime, reviewedAt);
          removeLinksFromReviewQueue([link]);
          review(dueQueue, callback);
        });
      });
    } else {
      callback();
    }
  }

  function updateStatus() {
    var reviewQueue = getReviewQueue();
    $('#reviewsQueued').text(reviewQueue.length);
    $('#gradesPending').text(getGradeQueue().length);
    var lastSync = localStorage.getItem('lastSync');
    $('#lastSync').text(lastSync ? new Date(JSON.parse(lastSync) * 1000).toLocaleString() : 'never');
    if (reviewQueue.length) {
      enable($('#review'));
    } else {
      disable($('#review'));
    }
  }

  $(function () {
    $(window).resize(function () {
      var min = Math.min($(window).width(), $(window).height());
      $('#reviewArea td').css('font-size', min / 10 + 'px');
    });
    $(window).resize();

    updateStatus();

    enable($('#sync'));
    $('#sync').click(function () {
      var $this = $(this);
      var text = $this.text();
      $this.text('Syncing...');
      disable($this);
      sync(function () {
        updateStatus();
        $this.text(text);
        enable($this);
      });
    });
    $('#review').click(function () {
      $('#menuArea').hide();
      $('#reviewArea').show();
      review(linksDueForReview(), getGradeQueue(), function () {
        updateStatus();
        $('#reviewArea').hide();
        $('#menuArea').show();
      });
    });
  });

})(jQuery);