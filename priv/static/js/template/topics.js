//  Add Application name for trail
var controller = {
  'template': '<!-- Secondary header --> \
	<div class="header-secondary row gray-bg"> \
		<div class="col-lg-12"> \
			<div class="page-heading clearfix"> \
				<h1 class="page-title pull-left">Topics</h1><button id="newTopic" data-toggle="modal" data-target="#topicModal" class="requirewrite btn btn-primary btn-sm btn-add">Add Topic</button> \
			</div> \
			<!-- Breadcrumb --> \
			<ol class="breadcrumb breadcrumb-2">  \
				<li><a href="#/"><i class="fa fa-home"></i>Home</a></li>  \
				<li class="active"><a href="#/topics"><strong>Topics</strong></a></li>  \
			</ol> \
		</div> \
	</div> \
	<!-- /secondary header --> \
  <!-- Main content --> \
    <div class="main-content"> \
		<div class="row"> \
			<div class="col-md-12"> \
        <div id="noData" class="panel-heading clearfix"> \
          <h3 class="alert alert-info"> No Data Topics have been created yet </h3> \
        </div> \
				<!-- Card list view --> \
				<div class="cards-container box-view"> \
				</div> \
				<!-- /card list view --> \
			</div> \
      <div id="topicModal" class="modal fade" tabindex="-1" role="dialog" style="display: none;"> \
      <div class="modal-dialog"> \
        <div class="modal-content"> \
          <div class="modal-header"> \
            <button type="button" class="close" data-dismiss="modal" aria-label="Close"><span aria-hidden="true">×</span></button> \
            <h4 class="modal-title">Add Topic</h4> \
          </div> \
          <div class="modal-body"> \
            <p style="display:none;" class="alert alert-danger" id="topicNameError"></p> \
            <p>Topic Name: <input type="text" id="topicName" /></p> \
          </div> \
          <div class="modal-footer"> \
            <button type="button" class="requirewrite btn btn-default" data-dismiss="modal">Close</button> \
            <button type="button" class="requirewrite btn btn-primary" id="topicNameAdd">Add Topic</button> \
          </div> \
        </div><!-- /.modal-content --> \
      </div><!-- /.modal-dialog --> \
    </div> \
		</div>',
  'script': function (id) {
    if (id) {
      $('#newTopic').hide();
      //UI for seeing One Topic
      $.get('/api/topic/' + id, function (topic) {
        $('#noData').hide();
        var Topic = '<div class="panel panel-default"> \
					<div class="panel-heading clearfix"> \
						<h3 class="panel-title">' + topic.name + '</h3> \
					</div> \
					<div class="panel-body"> \
						 <form class="form-horizontal"> \
						 	<div class="form-group">  \
								<label class="col-sm-2 control-label">Topic Id: </label>  \
								<div class="col-sm-10">  \
									<input type="text" placeholder="Placeholder" value="' + topic.id + '" class="form-control" disabled="">  \
								</div>  \
							</div> \
						 	<div class="form-group">  \
								<label class="col-sm-2 control-label">Topic Name: </label>  \
								<div class="col-sm-10">  \
									<input type="text" placeholder="Placeholder" id="topicName" value="' + topic.name + '" class="form-control">  \
								</div>  \
							</div> \
						 	<div class="form-group">  \
								<label class="col-sm-2 control-label">Created/Modified on: </label>  \
								<div class="col-sm-10">  \
									<span style="font-size:14px">' + topic.createdOn + '</span>  \
								</div>  \
							</div> \
						 	<div class="form-group">  \
								<label class="col-sm-2 control-label">Description: </label>  \
								<div class="col-sm-10">  \
                <textarea placeholder="Textarea" id="topicDescription" class="form-control">' + topic.description + '</textarea>  \
								</div>  \
							</div> \
							<div class="line-dashed"></div> \
              <div class="form-group> \
        				<div class="panel panel-default"> \
        					<div class="panel-heading clearfix"> \
        						<h3 class="panel-title">Attached Schemas</h3> <br /> <br /> \
                    <button id="newSchema" data-toggle="modal" data-target="#topicSchema" class="requirewrite btn btn-primary btn-md btn-add">Add New Schema</button> \
                    <p> Adding/Deleting A Schema from a used topic will force all Publishers to refresh tokens </p> \
        					</div> \
        					<div class="panel-body"> \
        						<div class="table-responsive"> \
        							<table class="table table-hover"> \
        								<thead>  \
        									<tr>  \
        										<th>#</th>  \
        										<th>Version</th>  \
        										<th>View Schema</th>  \
        										<th>Remove Schema</th>  \
        									</tr>  \
        								</thead>  \
        								<tbody id="schemaRows">  \
        								</tbody>  \
        							</table> \
        						</div> \
        					</div> \
        				</div> \
							<div class="line-dashed"></div> \
                                <div class="form-group"> \
                                <div class="col-sm-12 deleteWarning"> \
                                <h5> Delete All Schema And Save Before Deleting Topic </h5> \
                                </div> \
								<div class="col-sm-4"> \
									<button type="submit" class="btn btn-white" id="cancelTopic">Cancel</button> \
									<button type="submit" class="requirewrite btn btn-primary" id="saveTopic">Save changes</button> \
                                    <button type="submit" class="requirewrite btn btn-danger" id="deleteTopic">Delete Topic</button> \
								</div> \
							</div> \
						</form> \
					</div> \
          <div id="topicSchema" class="modal fade" tabindex="-1" role="dialog" style="display: none;"> \
          <div class="modal-dialog"> \
            <div class="modal-content"> \
              <div class="modal-header"> \
                <button type="button" class="close" data-dismiss="modal" aria-label="Close"><span aria-hidden="true">×</span></button> \
                <h4 class="modal-title">Add Schema to Topic</h4> \
              </div> \
              <div class="modal-body"> \
                <p>Version: <input type="text" id="schemaVersion" /></p> \
                <p><span id="schemaVersionError" class="alert alert-danger" style="display:none" /></p> \
                <p>Validation (JSON): <br /> \
                <textarea placeholder="Textarea" style="height: 200px" id="schemaValidation" class="form-control"></textarea>  \
                </p> \
              </div> \
              <div class="modal-footer"> \
                <button type="button" class="requirewrite btn btn-default" data-dismiss="modal">Close</button> \
                <button type="button" class="requirewrite btn btn-primary" id="schemaAdd">Add Schema</button> \
              </div> \
              </div> \
            </div><!-- /.modal-content --> \
          </div><!-- /.modal-dialog --> \
				</div> \
				<div id="viewSchema" class="modal fade" tabindex="-1" role="dialog" style="display: none;"> \
                          <div class="modal-dialog"> \
                            <div class="modal-content"> \
                              <div class="modal-header"> \
                                <button type="button" class="close" data-dismiss="modal" aria-label="Close"><span aria-hidden="true">×</span></button> \
                                <h4 class="modal-title">Schema:</h4> \
                              </div> \
                              <div class="modal-body"> \
                                <p>Version: <input type="text" class="schemaTitle" disabled /></p> \
                                <p>DateTime: <input type="text" class="schemaCreatedOn" disabled /></p> \
                                <p>Validation (JSON): <br /> \
                                <pre style="height: auto;overflow-x: auto;white-space: pre-wrap;white-space: -moz-pre-wrap;white-space: -pre-wrap;white-space: -o-pre-wrap;word-wrap: break-word;" class="schemaViewValidation form-control"></pre>  \
                                </p> \
                              </div> \
                              <div class="modal-footer"> \
                                <button type="button" class="requirewrite btn btn-default" data-dismiss="modal">Close</button> \
                              </div> \
                              </div> \
                            </div><!-- /.modal-content --> \
                          </div><!-- /.modal-dialog --> \
                				</div>';
        $('.cards-container').empty();
        $('.cards-container').html(Topic);
        checkpermissions();
        $('.deleteWarning').hide();
        if (topic.schemaId) {
          $('.deleteWarning').show();
          topic.schemaId.split(',').forEach(function (schemaId) {
            $.ajax({
              url: '/api/schema/' + schemaId,
              type: 'GET',
              success: function (schema) {
                var schemaHtml = '<tr class="schemaRow" id="' + schema.id + '">  \
                  <th scope="row">' + schema.id + '</th>  \
                  <td class="schemaVersions">' + schema.version + '</td>  \
                  <td><button data-toggle="modal" data-target="#viewSchema" type="submit" class="requirewrite btn btn-info" id="btnViewSchema">View schema</button></td>  \
                  <td><button type="submit" class="requirewrite btn btn-danger" id="deleteSchema">Remove schema</button></td>  \
                </tr>';
                $('#schemaRows').append(schemaHtml);
                $('#deleteTopic').prop('disabled', true);
                checkpermissions();
              }
            })
          });
          $(document).on('click', '#btnViewSchema', function (e) {
            e.preventDefault();
            var schemaId = $(this).closest('tr').attr('id');
            $.ajax({
              url: '/api/schema/' + schemaId,
              type: 'GET',
              success: function (schema) {
                var schemaObj = JSON.parse(schema.validation);
                var formattedSchema = JSON.stringify(schemaObj, null, '\t');
                $('.schemaTitle').val(schema.version);
                $('.schemaCreatedOn').val(schema.createdOn);
                $('.schemaViewValidation').html(formattedSchema);
              }
            });
          });
          $(document).on('click', '#deleteSchema', function (e) {
            e.preventDefault();
            var schemaid = $(this).closest('tr').attr('id');
            $.ajax({
              url: '/api/schema/' + schemaid,
              type: 'DELETE',
              contentType: "application/json",
              success: function (data) {
                var currentSchemaIds = $.map($(".schemaRow"), function (n, i) {
                  if (n.id !== schemaid)
                    return n.id;
                });
                $.ajax({
                  url: '/api/topic/' + id,
                  type: 'PUT',
                  contentType: "application/json",
                  data: {
                    "name": $('#topicName').val(),
                    "description": $('#topicDescription').val(),
                    "schemaid": currentSchemaIds.join(",")
                  },
                  success: function (e) {
                    $('#topicSchema').find(".close").click();
                    location.reload(true);
                  }
                });
              }
            });
          });
        }

        $('#schemaAdd').on('click', function (e) {
          e.preventDefault();
          var schemaVersion = $('#schemaVersion').val();
          var schemaAlreadyFound = false;
          // Test value is version number
          if (!/^\d*\.?\d*\.?\d*\.?\d*$/.test(schemaVersion)) {
            schemaAlreadyFound = true;
          }
          $('.schemaVersions').each(function (e) {
            if (schemaVersion == $(this).html()) {
              schemaAlreadyFound = true;
            }
          });
          if (schemaAlreadyFound) {
            $('#schemaVersionError').html('Schema Version Already Inuse/Version is not a number').show();
          } else {
            $.ajax({
              url: '/api/schema',
              type: 'POST',
              contentType: "application/json",
              data: {
                "version": $('#schemaVersion').val(),
                "validation": $('#schemaValidation').val()
              },
              success: function (data) {
                var currentSchemaIds = $.map($(".schemaRow"), function (n, i) {
                  return n.id;
                });
                currentSchemaIds.push(data.id);
                $.ajax({
                  url: '/api/topic/' + id,
                  type: 'PUT',
                  contentType: "application/json",
                  data: {
                    "name": $('#topicName').val(),
                    "description": $('#topicDescription').val(),
                    "schemaid": currentSchemaIds.join(",")
                  },
                  success: function (e) {
                    $('#topicSchema').find(".close").click();
                    location.reload(true);
                  }
                });
              }
            });
          }
        });
        $('#deleteTopic').on('click', function (e) {
          e.preventDefault();
          $.ajax({
            url: '/api/topic/' + id,
            type: 'DELETE'
          });
          window.location.hash = '#/topics';
        });
        $('#saveTopic').on('click', function (e) {
          e.preventDefault();
          var currentSchemaIds = $.map($(".schemaRow"), function (n, i) {
            return n.id;
          });
          if ((currentSchemaIds.length == 0) && ($('#topicName').val().trim().length > 0)) {
            currentSchemaIds = [];
          }
          var regex = /^[A-Za-z0-9]+$/;
          if (regex.test($('#topicName').val())) {
            $.ajax({
              url: '/api/topic/' + id,
              type: 'PUT',
              contentType: "application/json",
              data: {
                "name": $('#topicName').val(),
                "description": $('#topicDescription').val(),
                "schemaid": currentSchemaIds.join(",")
              }
            });
            window.location.hash = '#/topics';
          } else {
            $(".deleteWarning").show();
            $(".deleteWarning").addClass("alert alert-warning");
            $(".deleteWarning").html("Invalid Topic Name");
          }
        });
        $('#cancelTopic').on('click', function (e) {
          e.preventDefault();
          window.location.hash = '#/topics';
        });
      });
    } else {
      $('#newTopic').show();
      //UI for seeing All Topics
      $.get('/api/topic', function (result) {
        $('.cards-container').html('');
        $('.cards-container').empty();
        $.each(result, function (idx, topic) {
          var addTopic = '<!-- Card --> \
    					<div class="card"> \
    						<!-- Card Header --> \
    						<div class="card-header"> \
    							<!-- Card Short Description --> \
    							<div class="card-short-description"> \
    								<h5> \
                      <span class="app-name"><a class="topicNameFull" href="#/topics/' + topic.id + '">' + topic.name + '</a></span> \
                    </h5> \
        						<div class="card-content"> \
        							<p>' + topic.description + '</p> \
        						</div> \
        						<!-- /card content --> \
        					</div> \
    							</div> \
    							<!-- /card short description --> \
    						</div> \
    						<!-- /card header --> \
    						<!-- Card Content --> \
    					<!-- /card -->';
          $('#noData').hide();
          $('.cards-container').append(addTopic);
        });
      });

      $('#topicNameAdd').click(function (e) {
        e.preventDefault();
        var appN = $('#topicName').val().trim();
        var namecheck = false;
        $('.topicNameFull').each(function (e) {
          checkname = $(this).html().trim();
          if (checkname == appN) {
            namecheck = true;
          }
        });
        var regex = /^[A-Za-z0-9]+$/;
        if ((regex.test(appN)) && (namecheck == false) && (appN.length > 0)) {
          $.post('/api/topic', {
            topicName: $('#topicName').val()
          }, function (topic) {
            $('#topicModal').find(".close").click();
            var addTopic = '<!-- Card --> \
    					<div class="card"> \
    						<!-- Card Header --> \
    						<div class="card-header"> \
    							<!-- Card Short Description --> \
    							<div class="card-short-description"> \
    								<h5> \
                      <span class="app-name"><a href="#/topics/' + topic.id + '">' + topic.name + '</a></span> \
                    </h5> \
    							</div> \
    							<!-- /card short description --> \
    						</div> \
    						<!-- /card header --> \
    						<!-- Card Content --> \
    						<div class="card-content"> \
    							<p>' + topic.description + '</p> \
    						</div> \
    						<!-- /card content --> \
    					</div> \
    					<!-- /card -->';
            $('#topicName').val('');
            $('#noData').hide();
            $('.cards-container').append(addTopic);
          });
        } else {
          if (!regex.test(appN)) {
            $('#topicNameError').html("Invalid Topic Name");
          } else {
            $('#topicNameError').html("Topic Name is already in use");
          }
          $('#topicNameError').show();
        }
      });
    }
    checkpermissions();
  }

};

//Pull Application id from Route
var id = window.location.hash.split('#/')[1].split('/')[1] || false;

//start actions
$('#app').html(controller.template);
controller.script(id);