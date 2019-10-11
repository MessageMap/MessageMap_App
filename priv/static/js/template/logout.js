var controller = {
    'template' : '',
    'script': function(){
          $.get('/api/logout');
          window.location.hash = '#/login';
    }
};

//start actions
$('#app').html(controller.template);
controller.script();
