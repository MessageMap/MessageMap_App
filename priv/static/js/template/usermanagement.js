var controller = {
	'template': '<h1 class="page-title">MessageMap.io -- User Management</h1> \
		<div class="row"> \
			<div class="col-lg-12"> \
				<div class="panel-group"> \
					<div class="panel"> \
						<div class="panel-heading no-border clearfix">  \
							<h2 class="panel-title">Listing of Users</h2> \
						</div> \
						<div class="panel-body"> \
							<div class="user-management"> \
							<div class="btn btn-success addUser">Add New User</div> \
							<div class="table-responsive"> \
								<table class="table table-hover"> \
									<thead>  \
										<tr>  \
											<th>Username</th>  \
											<th>Permission Level</th>  \
											<th>Password</th>  \
											<th>Modify</th>  \
											<th>Delete</th>  \
										</tr>  \
									</thead>  \
									<tbody id="listUsers">  \
									</tbody>  \
								</table> \
							</div> \
							</div> \
						</div> \
					</div> \
				</div> \
			</div> \
		</div>',
	'script': function () {
		var permissions = '<option value="Read">Read</option><option value="Read/Write">Read/Write</option><option value="Admin">Admin</option>';
		$('.addUser').on('click', function (e) {
			e.preventDefault();
			$('#listUsers').append("<tr><td class='email'>Email: <input type='text' class='useremail' /></td><td class='userpermissions'><select class='newuser_permissions'>" + permissions + "</select></td><td class='userpassword'>Password: <input type='password' class='userpass' /></td><td><input  type='button' class='saveuser btn btn-success' value='Save'></td><td>---------</td></tr>");
			$('.saveuser').on('click', function (e) {
				var email = $('.useremail').val();
				var password = $('.userpass').val();
				var userpermissions = $('.newuser_permissions').val();
				if (email.length > 3 && password.length > 5) {
					$.post("/api/adduser", {
						'email': email,
						'password': password,
						'permissions': userpermissions
					}, function () {
						location.reload();
					});
				} else {
					alert("Password/Email don't match requirements");
				}
			});
		});
		$.get("/api/users", function (data) {
			console.error(data);
			data.forEach(user => {
				if (user["email"] == "admin") {
					$('#listUsers').append("<tr><td>" + user["email"] + "</td><td>" + user["permissions"] + "</td><td> ------ </td><td> ------ </td><td> ------</td></tr>");
				} else {
					$('#listUsers').append("<tr><td>" + user["email"] + "</td><td class='userpermissions'>" + user["permissions"] + "</td><td class='userpassword'>**************</td><td><button class='modify btn btn-info' id='" + user["email"] + "'>Modify</button></td><td><button class='delete btn btn-alert' id='" + user["email"] + "'>Delete</button></td></tr>");
				}
			});

			$('#listUsers .modify').on('click', function (e) {
				e.preventDefault();
				console.error("Modify");
				var id = $(this).attr('id');
				var perm = $(this).closest('tr').find('.userpermissions');
				var pass = $(this).closest('tr').find('.userpassword');
				perm.html('<select id="perm_' + id + '">' + permissions + '</select>');
				pass.html('<input type="password" id="pass_' + id + '" />');
				$(this).parent().html("<input id='" + $(this).attr('id') + "' type='button' class='save btn btn-success' value='Save'>");
				$('.save').on('click', function (e) {
					var id = $(this).attr('id');
					var sendit = {
						'email': id,
						'permissions': $('#perm_' + id).val(),
						'password': $('#pass_' + id).val(),
					}
					$.post("/api/moduser", sendit, function () {
						location.reload();
					});
				});
			});
			$('#listUsers .delete').on('click', function (e) {
				e.preventDefault();
				var email = $(this).attr('id');
				$.ajax({
					url: '/api/deluser/' + email,
					method: 'DELETE',
					contentType: "application/json",
					success: function () {
						$(this).closest('tr').remove();
						location.reload();
					},
					error: function () {
						console.error("Failed to delete return to user");
					}
				});
			});
		});
	}
};

//start actions
$('#app').html(controller.template);
controller.script();
