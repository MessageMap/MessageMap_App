//TODO: Hide global Add Application for One
//  Add Application name for trail
var controller = {
  'template': '<!-- Secondary header --> \
	<div class="header-secondary row gray-bg"> \
		<div class="col-lg-12"> \
			<div class="page-heading clearfix"> \
				<h1 class="page-title pull-left">Applications</h1><button id="newApp" data-toggle="modal" data-target="#appModal" class="btn btn-primary btn-sm btn-add">Add Application</button> \
			</div> \
			<!-- Breadcrumb --> \
			<ol class="breadcrumb breadcrumb-2">  \
				<li><a href="#/"><i class="fa fa-home"></i>Home</a></li>  \
				<li class="active"><a href="#/applications"><strong>Applications</strong></a></li>  \
			</ol> \
		</div> \
	</div> \
	<!-- /secondary header --> \
  <!-- Main content --> \
    <div class="main-content"> \
		<div class="row"> \
			<div class="col-md-12"> \
        <div id="noData" class="panel-heading clearfix"> \
          <h3 class="alert alert-info"> No Data Applications have been created yet </h3> \
        </div> \
				<!-- Card list view --> \
				<div class="cards-container box-view"> \
				</div> \
				<!-- /card list view --> \
			</div> \
      <div id="appModal" class="modal fade" tabindex="-1" role="dialog" style="display: none;"> \
      <div class="modal-dialog"> \
        <div class="modal-content"> \
          <div class="modal-header"> \
            <button type="button" class="close" data-dismiss="modal" aria-label="Close"><span aria-hidden="true">×</span></button> \
            <h4 class="modal-title">Add Application</h4> \
          </div> \
          <div class="modal-body"> \
            <p>Application Name: <input type="text" id="appName" /></p> \
          </div> \
          <div class="modal-footer"> \
            <button type="button" class="btn btn-default" data-dismiss="modal">Close</button> \
            <button type="button" class="btn btn-primary" id="appNameAdd">Add Application</button> \
          </div> \
        </div><!-- /.modal-content --> \
      </div><!-- /.modal-dialog --> \
    </div> \
		</div>',
  'script': function(id) {
    if (id) {
      $('#newApp').hide();
      //UI for seeing One Application
      $.get('/api/application/' + id, function(app) {
        $('#noData').hide();
        var App = '<div class="panel panel-default"> \
					<div class="panel-heading clearfix"> \
						<h3 class="panel-title">' + app.name + '</h3> \
					</div> \
					<div class="panel-body"> \
						 <form class="form-horizontal"> \
						 	<div class="form-group">  \
								<label class="col-sm-2 control-label">Application Id: </label>  \
								<div class="col-sm-10">  \
									<span class="label label-default" style="font-size:12px">' + app.id + '</span>  \
								</div>  \
							</div> \
						 	<div class="form-group">  \
								<label class="col-sm-2 control-label">Application Name: </label>  \
								<div class="col-sm-10">  \
									<input type="text" placeholder="Placeholder" id="appName" value="' + app.name + '" class="form-control">  \
								</div>  \
							</div> \
						 	<div class="form-group">  \
								<label class="col-sm-2 control-label">Description: </label>  \
								<div class="col-sm-10">  \
                <textarea placeholder="Textarea" id="appDescription" class="form-control">' + app.description + '</textarea>  \
								</div>  \
							</div> \
							<div class="line-dashed"></div> \
						 	<div class="form-group">  \
								<label class="col-sm-2 control-label">API Key: </label>  \
								<div class="col-sm-10">  \
                  <span class="label label-default" style="font-size:12px">'+ app.apiKeys + '</label>  \
								</div>  \
							</div> \
							<div class="line-dashed"></div> \
                <div class="col-sm-3">  \
                  <div class="well well-lg text-center alert alert-info"><h1>Published<br /><span id="pub_stat">0</span></h1></div> \
                </div> \
                <div class="col-sm-3">  \
                  <div class="well well-lg text-center alert alert-info"><h1>Consumed<br /><span id="con_stat">0</span></h1></div> \
                </div> \
                <div class="col-sm-3">  \
                  <div class="well well-lg text-center alert alert-info"><h1>Waiting<br /><span id="wait_stat">0</span></h1></div> \
                </div> \
                <div class="col-sm-3">  \
                  <div class="well well-lg text-center alert alert-success"><h1>Storage Percentage<br /><span id="storage_stat">0 %</span></h1></div> \
                </div> \
							<div class="line-dashed"></div> \
              <div class="form-group> \
        				<div class="panel panel-default"> \
        					<div class="panel-heading clearfix"> \
        						<h3 class="panel-title float-left">Topics Owned</h3> \
                    <button id="newTopic" data-toggle="modal" data-target="#newTopicModal" class="btn btn-primary btn-md btn-add text-right float-right">Add New Owned Topic</button> \
        					</div> \
        					<div class="panel-body"> \
        						<div class="table-responsive"> \
        							<table class="table table-hover"> \
        								<thead>  \
        									<tr>  \
        										<th>#</th>  \
        										<th>Topic Name</th>  \
        										<th>Remove Topic from Owned Listing</th>  \
        									</tr>  \
        								</thead>  \
        								<tbody id="listOwnedTopics">  \
        								</tbody>  \
        							</table> \
        						</div> \
        					</div> \
        				</div> \
							<div class="line-dashed"></div> \
              <div class="form-group> \
        				<div class="panel panel-default"> \
        					<div class="panel-heading clearfix"> \
        						<h3 class="panel-title">Topics Subscribed</h3> <br /> <br /> \
                    <button id="newSubscribe" data-toggle="modal" data-target="#newSubscribeTopicModal" class="btn btn-primary btn-md btn-add">Add New Subscrition</button> \
        					</div> \
        					<div class="panel-body"> \
        						<div class="table-responsive"> \
        							<table class="table table-hover"> \
        								<thead>  \
        									<tr>  \
        										<th>#</th>  \
        										<th>Topic Name</th>  \
        										<th>Remove Topic from Subscribe Listing</th>  \
        									</tr>  \
        								</thead>  \
        								<tbody  id="listSubscribedTopics">  \
        								</tbody>  \
        							</table> \
        						</div> \
        					</div> \
        				</div> \
							<div class="line-dashed"></div> \
              <div class="form-group"> \
								<div class="col-sm-4"> \
									<button type="submit" class="btn btn-white" id="cancelApplication">Cancel</button> \
									<button type="submit" class="btn btn-primary" id="saveApplication">Save changes</button> \
                  <button type="submit" class="btn btn-danger" id="deleteApplication">Delete Application</button> \
								</div> \
							</div> \
						</form> \
					</div> \
          <div id="newTopicModal" class="modal fade" tabindex="-1" role="dialog" style="display: none;"> \
          <div class="modal-dialog"> \
            <div class="modal-content"> \
              <div class="modal-header"> \
                <button type="button" class="close" data-dismiss="modal" aria-label="Close"><span aria-hidden="true">×</span></button> \
                <h4 class="modal-title">Add Owned Topic to Application</h4> \
              </div> \
              <div class="modal-body"> \
                <p>Topic Name: <select id="ownedTopicId" class="topicList"> \
                </select></p> \
                <!--p class="topicDescriptionSelect"> </p--> \
              </div> \
              <div class="modal-footer"> \
                <button type="button" class="btn btn-default" data-dismiss="modal">Close</button> \
                <button type="button" class="btn btn-primary" id="ownedAdd">Add Topic to Owned List</button> \
              </div> \
							</div> \
            </div><!-- /.modal-content --> \
          </div><!-- /.modal-dialog --> \
          <div id="newSubscribeTopicModal" class="modal fade" tabindex="-1" role="dialog" style="display: none;"> \
          <div class="modal-dialog"> \
            <div class="modal-content"> \
              <div class="modal-header"> \
                <button type="button" class="close" data-dismiss="modal" aria-label="Close"><span aria-hidden="true">×</span></button> \
                <h4 class="modal-title">Add Subscribed Topic to Application</h4> \
              </div> \
              <div class="modal-body"> \
                <p>Topic Name: <select id="subscribeTopicId" class="topicSubscribeList"> \
                </select></p> \
              </div> \
              <div class="modal-footer"> \
                <button type="button" class="btn btn-default" data-dismiss="modal">Close</button> \
                <button type="button" class="btn btn-primary" id="subscribedAdd">Add Topic to Subscribe List</button> \
              </div> \
							</div> \
            </div><!-- /.modal-content --> \
          </div><!-- /.modal-dialog --> \
				</div>';
        $('.cards-container').empty();
        $('.cards-container').html(App);
        //Start Subscribed Topics
        app.subscribedTopics.split(",").forEach(function(topic) {
          if(topic.length > 1){
            $.get('/api/topic/' + topic, function(result) {
              $('#listSubscribedTopics').append('<tr id="' + result.id + '" class="subscribedTopicRow">  \
                  <th scope="row">' + result.id + '</th>  \
                  <td><a href="#/topics/' + result.id + '">' + result.name + '</a></td>  \
                  <td><button  type="button" class="btn btn-danger" id="deleteSubscribed">Remove Topic</button></td>  \
                </tr>');
            });
          }
        });
        $.get('/api/stats/'+app.id, function(result){
          var wait = result.messages_waiting
          var percentFull = parseFloat((wait/20000)*100).toFixed(2);
					console.error("________________");
					console.error(percentFull);
          $('#con_stat').html(result.consumed_messages.toString().replace(/\B(?=(\d{3})+(?!\d))/g, ","));
          $('#pub_stat').html(result.published_messages.toString().replace(/\B(?=(\d{3})+(?!\d))/g, ","));
          $('#wait_stat').html(result.messages_waiting.toString().replace(/\B(?=(\d{3})+(?!\d))/g, ","));
          $('#storage_stat').html(percentFull.toString()+' %');
          if(parseInt(percentFull) > 90){
            $('#storage_stat').parent().parent().addClass('alert-danger').removeClass('alert-success');
          } else {
            $('#storage_stat').parent().parent().addClass('alert-success').removeClass('alert-danger');
          }
        });
        $(document).on('click', '#deleteSubscribed', function(e) {
            e.preventDefault();
            $(this).closest('tr').remove();
        });
        $('#newSubscribe').on('click', function(e) {
          e.preventDefault();
          $.get('/api/topic', function(result) {
            $('.topicSubscribeList').html('');
            $.each(result, function(idx, topic) {
              $('.topicSubscribeList').append('<option value="' + topic.id + '">' + topic.name + '</option>');
            });
          });
        });
        $('#subscribedAdd').on('click', function(e) {
          e.preventDefault();
          var tid = $('.topicSubscribeList option:selected').val();
          $.get('/api/topic/' + tid, function(result) {
            $('#listSubscribedTopics').append('<tr id="' + result.id + '" class="subscribedTopicRow">  \
                <th scope="row">' + result.id + '</th>  \
                <td><a href="#/topics/' + result.id + '">' + result.name + '</a></td>  \
                <td><button type="button" class="btn btn-danger" id="deleteSubscribed">Remove Topic</button></td>  \
              </tr>');
          });
          $('#newSubscribeTopicModal').find(".close").click();
        });
        //End Subscribed Topics
        //Start Owned Topics
        app.ownedTopics.split(",").forEach(function(topic) {
          if(topic.length > 1){
            $.get('/api/topic/' + topic, function(result) {
              $('#listOwnedTopics').append('<tr id="' + result.id + '" class="ownedTopicRow">  \
                  <th scope="row">' + result.id + '</th>  \
                  <td><a href="#/topics/' + result.id + '">' + result.name + '</a></td>  \
                  <td><button  type="button" class="btn btn-danger" id="deleteOwned">Remove Topic</button></td>  \
                </tr>');
            });
          }
        });
        $(document).on('click', '#deleteOwned', function(e) {
            e.preventDefault();
            $(this).closest('tr').remove();
        });
        $('#newTopic').on('click', function(e) {
          e.preventDefault();
          $.get('/api/topic', function(result) {
            $('.topicList').html('');
            $.each(result, function(idx, topic) {
              $('.topicList').append('<option value="' + topic.id + '">' + topic.name + '</option>');
            });
          });
        });
        $('#ownedAdd').on('click', function(e) {
          e.preventDefault();
          var tid = $('.topicList option:selected').val();
          $.get('/api/topic/' + tid, function(result) {
            $('#listOwnedTopics').append('<tr id="' + result.id + '" class="ownedTopicRow">  \
                <th scope="row">' + result.id + '</th>  \
                <td><a href="#/topics/' + result.id + '">' + result.name + '</a></td>  \
                <td><button type="button" class="btn btn-danger" id="deleteOwned">Remove Topic</button></td>  \
              </tr>');
          });
          $('#newTopicModal').find(".close").click();
        });
        //End Owned Topics
        $('#deleteApplication').on('click', function(e) {
          e.preventDefault();
	  if(id != 'ca8afc18-c933-4b14-bc67-61df99937df0'){
          $.ajax({
            url: '/api/application/' + id,
            type: 'DELETE'
          });
          window.location.hash = '#/applications';
	  }
        });
        $('#saveApplication').on('click', function(e) {
          e.preventDefault();
          if(id != 'ca8afc18-c933-4b14-bc67-61df99937df0'){ 
          var currentTopicIds = $.map($(".ownedTopicRow"), function(n, i) {
            return n.id;
          });
          var subscribedTopicIds = $.map($(".subscribedTopicRow"), function(n, i) {
            return n.id;
          });
          $.ajax({
            url: '/api/application/' + id,
            type: 'PUT',
            contentType: "application/json",
            data: {
              "subscribedTopics": subscribedTopicIds.join(","),
              "ownedTopics": currentTopicIds.join(","),
              "name": $('#appName').val(),
              "description": $('#appDescription').val()
            },
            success: function(){
              window.location.hash = '#/applications';
            }
          });
	  }
        });
        $('#cancelApplication').on('click', function(e) {
          e.preventDefault();
          window.location.hash = '#/applications';
        });
      });
    } else {
      $('#newApp').show();
      //UI for seeing All Applications
      $.get('/api/application', function(result) {
        $('.cards-container').html('');
        $('.cards-container').empty();
        $.each(result, function(idx, app) {
          var Pub = "";
	  var Sub = "";

          if(app.ownedTopics.length > 0)
            Pub = '<span class="badge badge-primary">Publisher</span>';
          if(app.subscribedTopics.length > 0)
            Sub = '<span class="badge badge-warning">Subscriber</span>';

          var addApp = '<!-- Card --> \
    					<div class="card"> \
    						<!-- Card Header --> \
    						<div class="card-header"> \
    							<!-- Card Short Description --> \
    							<div class="card-short-description"> \
    								<h5> \
                      <span class="app-name"><a href="#/applications/' + app.id + '">' + app.name + '</a></span>' + Pub + Sub + ' \
                    </h5> \
    								<p>API KEY: ' + app.apiKeys + '</p> \
    							</div> \
    							<!-- /card short description --> \
    						</div> \
    						<!-- /card header --> \
    						<!-- Card Content --> \
    						<div class="card-content"> \
    							<p>' + app.description + '</p> \
    						</div> \
    						<!-- /card content --> \
    					</div> \
    					<!-- /card -->';
          $('#noData').hide();
          $('.cards-container').append(addApp);
        });
      });

      $('#appNameAdd').click(function(e) {
        e.preventDefault();
        $.post('/api/application', {
          appName: $('#appName').val()
        }, function(app) {
          $('#appModal').find(".close").click();
          var Pub = '<span class="badge badge-primary">Publisher</span>';
          var Sub = '<span class="badge badge-warning">Subscriber</span>';
          var addApp = '<!-- Card --> \
    					<div class="card"> \
    						<!-- Card Header --> \
    						<div class="card-header"> \
    							<!-- Card Short Description --> \
    							<div class="card-short-description"> \
    								<h5> \
                      <span class="app-name"><a href="#/applications/' + app.id + '">' + app.name + '</a></span>' + Pub + Sub + ' \
                    </h5> \
    								<p>API KEY: ' + app.apiKeys + '</p> \
    							</div> \
    							<!-- /card short description --> \
    						</div> \
    						<!-- /card header --> \
    						<!-- Card Content --> \
    						<div class="card-content"> \
    							<p>' + app.description + '</p> \
    						</div> \
    						<!-- /card content --> \
    					</div> \
    					<!-- /card -->';
          $('#appName').val('');
          $('#noData').hide();
          $('.cards-container').append(addApp);
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
