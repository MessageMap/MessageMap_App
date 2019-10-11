function loadScript(url) {
   var script = document.createElement('script'), done = false,
       head = document.getElementsByTagName("head")[0];
   script.src = url;
   script.onload = script.onreadystatechange = function(){
     if ( !done && (!this.readyState ||
          this.readyState == "loaded" || this.readyState == "complete") ) {
       done = true;

      // IE memory leak
      script.onload = script.onreadystatechange = null;
      head.removeChild( script );
    }
  };
  head.appendChild(script);
}

var findRoute = function() {
    var route = window.location.hash.split('#/')[1] || 'dashboard';
    if(route.indexOf('/') !== -1)
      route = route.split('/')[0];
    console.log("Route: " + route);
    if(['logout'].indexOf(route) == -1){
      $.ajax({
        url: '/api/me',
        type: 'get',
        success: function(result){
          if(result && result.hasOwnProperty('name')){
            loadScript('/static/js/template/'+route+'.js');
            $('#username').text(result.name);
          } else {
              $('.page-sidebar').hide();
              $('.main-header').hide();
              $('.footer-main').hide();
              $('#app').html('');
            loadScript('/static/js/template/login.js');
          }
        },
        error: function(){
            $('.page-sidebar').hide();
            $('.main-header').hide();
            $('.footer-main').hide();
            $('#app').html('');
            loadScript('/static/js/template/'+route+'.js');
        }
      });
    } else{
        $('.page-sidebar').hide();
        $('.main-header').hide();
        $('.footer-main').hide();
        $('#app').html('');
        loadScript('/static/js/template/'+route+'.js');
    }
};

$(window).bind( 'hashchange', function(e) {
    findRoute();
});

findRoute();

var loadVersion = function() {
    $.ajax({
    	method: 'GET',
    	url: '/api/version'
  	}).then(function(data) {
        $('#version').text(data.version);
    });
};

loadVersion();
