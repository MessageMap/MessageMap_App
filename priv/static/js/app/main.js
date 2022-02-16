function loadScript(url) {
  var script = document.createElement('script'),
    done = false,
    head = document.getElementsByTagName("head")[0];
  script.src = url;
  script.onload = script.onreadystatechange = function () {
    if (!done && (!this.readyState ||
        this.readyState == "loaded" || this.readyState == "complete")) {
      done = true;

      // IE memory leak
      script.onload = script.onreadystatechange = null;
      head.removeChild(script);
    }
  };
  head.appendChild(script);
}

var checkpermissions = function (route = null) {
  $.ajax({
    url: '/api/me',
    type: 'get',
    success: function (result) {
      if (result && result.hasOwnProperty('name')) {
        if (route !== null) {
          loadScript('/static/js/template/' + route + '.js');
        }
        $('#username').text(result.name);
        if ($.inArray("Admin", result.roles) == 0) {} else if ($.inArray("Read/Write", result.roles) == 0) {
          $('.requireadmin').remove();
        } else {
          $('.requirewrite').prop("disabled", true);
          $('.requireadmin').remove();
        }
      } else {
        $('.page-sidebar').hide();
        $('.main-header').hide();
        $('.footer-main').hide();
        $('#app').html('');
        loadScript('/static/js/template/login.js');
      }
    },
    error: function () {
      $('.page-sidebar').hide();
      $('.main-header').hide();
      $('.footer-main').hide();
      $('#app').html('');
      loadScript('/static/js/template/' + route + '.js');
    }
  });
}

var findRoute = function () {
  var route = window.location.hash.split('#/')[1] || 'dashboard';
  if (route.indexOf('/') !== -1)
    route = route.split('/')[0];
  if (['logout'].indexOf(route) == -1) {
    checkpermissions(route);
  } else {
    $('.page-sidebar').hide();
    $('.main-header').hide();
    $('.footer-main').hide();
    $('#app').html('');
    loadScript('/static/js/template/' + route + '.js');
  }
};

$(window).bind('hashchange', function (e) {
  findRoute();
});

findRoute();

function timeConverter(UNIX_timestamp) {
  var a = new Date(UNIX_timestamp * 1000);
  var months = ['Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'];
  var year = a.getFullYear();
  var month = months[a.getMonth()];
  var date = a.getDate();
  var hour = a.getHours();
  var min = a.getMinutes();
  var sec = a.getSeconds();
  var time = date + ' ' + month + ' ' + year + ' ' + hour + ':' + min + ':' + sec;
  return time;
}

var loadVersion = function () {
  $.ajax({
    method: 'GET',
    url: '/api/version'
  }).then(function (data) {
    $('#version').text(data.version);
    $('#year').text(new Date().getFullYear());
  });
};

loadVersion();