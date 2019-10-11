//TODO: Hide global Add Topic for One
//  Add Application name for trail
var controller = {
  'template': '<!-- Secondary header --> \
	<div class="header-secondary row gray-bg"> \
		<div class="col-lg-12"> \
			<div class="page-heading clearfix"> \
				<h1 class="page-title pull-left">Topics</h1><button id="newTopic" data-toggle="modal" data-target="#topicModal" class="btn btn-primary btn-sm btn-add">Add Topic</button> \
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
            <p>Topic Name: <input type="text" id="topicName" /></p> \
          </div> \
          <div class="modal-footer"> \
            <button type="button" class="btn btn-default" data-dismiss="modal">Close</button> \
            <button type="button" class="btn btn-primary" id="topicNameAdd">Add Topic</button> \
          </div> \
        </div><!-- /.modal-content --> \
      </div><!-- /.modal-dialog --> \
    </div> \
		</div>',
  'script': function(id) {
    if (id) {
      $('#newTopic').hide();
      //TODO: Build UI for looking at one topic
      console.error("Loading ID: " + id);
      //UI for seeing One Topic
      $.get('/api/topic/' + id, function(topic) {
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
                    <button id="newSchema" data-toggle="modal" data-target="#topicSchema" class="btn btn-primary btn-md btn-add">Add New Schema</button> \
        					</div> \
        					<div class="panel-body"> \
        						<div class="table-responsive"> \
        							<table class="table table-hover"> \
        								<thead>  \
        									<tr>  \
        										<th>#</th>  \
        										<th>Version</th>  \
        										<th>Number of Messages</th>  \
        										<!--th>View Schema</th-->  \
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
								<div class="col-sm-4"> \
									<button type="submit" class="btn btn-white" id="cancelTopic">Cancel</button> \
									<button type="submit" class="btn btn-primary" id="saveTopic">Save changes</button> \
                  <button type="submit" class="btn btn-danger" id="deleteTopic">Delete Topic</button> \
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
                <p>Validation (JSON): <br /> \
                <textarea placeholder="Textarea" style="height: 200px" id="schemaValidation" class="form-control"></textarea>  \
                </p> \
              </div> \
            </div> \
              <div class="modal-footer"> \
                <button type="button" class="btn btn-default" data-dismiss="modal">Close</button> \
                <button type="button" class="btn btn-primary" id="schemaAdd">Add Schema</button> \
              </div> \
            </div><!-- /.modal-content --> \
          </div><!-- /.modal-dialog --> \
				</div>';
        $('.cards-container').empty();
        $('.cards-container').html(Topic);
        if (topic.schemaId) {
          topic.schemaId.split(',').forEach(function(schemaId) {
            $.ajax({
              url: '/api/schema/' + schemaId,
              type: 'GET',
              success: function(schema) {
                var schemaHtml = '<tr class="schemaRow" id="' + schema.id + '">  \
                  <th scope="row">' + schema.id + '</th>  \
                  <td>' + schema.version + '</td>  \
                  <td style="text-align:right">0</td>  \
                  <!--td><button type="submit" class="btn btn-info" id="viewSchema">View schema</button></td-->  \
                  <td><button type="submit" class="btn btn-danger" id="deleteSchema">Remove schema</button></td>  \
                </tr>';
                $('#schemaRows').append(schemaHtml);
              }
            })
          });
          $(document).on('click', '#deleteSchema', function(e) {
            e.preventDefault();
	    if( id != '5cabed43-c268-41a4-ad1f-dc34cb98d37e'){ 
            var schemaid = $(this).closest('tr').attr('id');
            $.ajax({
              url: '/api/schema/' + schemaid,
              type: 'DELETE',
              contentType: "application/json",
              success: function(data) {
                var currentSchemaIds = $.map($(".schemaRow"), function(n, i) {
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
                  success: function(e) {
                    $('#topicSchema').find(".close").click();
                    location.reload(true);
                  }
                });
              }
            });
	   }
          });
        }
        $('#schemaAdd').on('click', function(e) {
          e.preventDefault();
          if(id != '5cabed43-c268-41a4-ad1f-dc34cb98d37e') {
          $.ajax({
            url: '/api/schema',
            type: 'POST',
            contentType: "application/json",
            data: {
              "version": $('#schemaVersion').val(),
              "validation": $('#schemaValidation').val()
            },
            success: function(data) {
              var currentSchemaIds = $.map($(".schemaRow"), function(n, i) {
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
                success: function(e) {
                  $('#topicSchema').find(".close").click();
                  location.reload(true);
                }
              });
            }
          });
	  }
        });
        $('#deleteTopic').on('click', function(e) {
          e.preventDefault();
          if(id != '5cabed43-c268-41a4-ad1f-dc34cb98d37e') {
          $.ajax({
            url: '/api/topic/' + id,
            type: 'DELETE'
          });
          window.location.hash = '#/topics';
          }
        });
        $('#saveTopic').on('click', function(e) {
          e.preventDefault();
          if(id != '5cabed43-c268-41a4-ad1f-dc34cb98d37e') {
          var currentSchemaIds = $.map($(".schemaRow"), function(n, i) {
            return n.id;
          });
          if (currentSchemaIds.length == 0)
            currentSchemaIds = [];
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
          }
        });
        $('#cancelTopic').on('click', function(e) {
          e.preventDefault();
          window.location.hash = '#/topics';
        });
      });
    } else {
      $('#newTopic').show();
      //UI for seeing All Topics
      $.get('/api/topic', function(result) {
        $('.cards-container').html('');
        $('.cards-container').empty();
        $.each(result, function(idx, topic) {
          var addTopic = '<!-- Card --> \
    					<div class="card"> \
    						<!-- Card Header --> \
    						<div class="card-header"> \
    							<!-- Card Short Description --> \
    							<div class="card-short-description"> \
    								<h5> \
                      <span class="app-name"><a href="#/topics/' + topic.id + '">' + topic.name + '</a></span> \
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

      $('#topicNameAdd').click(function(e) {
        e.preventDefault();
        $.post('/api/topic', {
          topicName: $('#topicName').val()
        }, function(topic) {
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
      });
    }
  }

};

//Pull Application id from Route
var id = window.location.hash.split('#/')[1].split('/')[1] || false;

//start actions
$('#app').html(controller.template);
controller.script(id);
