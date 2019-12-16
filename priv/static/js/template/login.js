var controller = {
    'template' : '<div class="login-container"> \
	<div class="login-branding"> \
		<!-- Place Logo here when We have one --> \
    <img src="static/images/logos/cover.png" alt="MessageMap" title="MessageMap.io"> \
	</div> \
	<div class="login-content"> \
		<h2 class="text-center"> Login </h2> \
		<div class="error"></div> \
		<form method="post"> \
			<div class="form-group"> \
				Username: <input type="text" id="Username" placeholder="Username" class="form-control"> \
			</div> \
			<div class="form-group"> \
				Password: <input type="password" id="Password"  placeholder="Password" class="form-control"> \
			</div> \
			<div class="form-group"> \
				<button id="btn-login" class="btn btn-primary btn-block">Login</button> \
			</div> \
			<p class="text-center">Feedback to: <a href="mailto:admin@MessageMap.io">Admin@Messagemap.io</a></p> \
		</form> \
	</div>',
    'script': function(){
        $('.page-sidebar').hide();
        $('.main-header').hide();
        $('.footer-main').hide();
        $('#btn-login').click(function(e){
          e.preventDefault();
          var user = $('#Username').val();
          var pass = $('#Password').val();
          $.post('/api/auth', { 'username': user, 'password': pass }, function(result){
            if(result.request)
              window.location.hash = '#/dashboard';
            else
              $('.error').html('<h3 style="color:red">Invalid Username/Password</h3>');
          });
        });
    }
};

if(window.location.hash !== '#/login')
  window.location.hash = '#/login';

//start actions
$('#app').html(controller.template);
controller.script();
