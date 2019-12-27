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
            <p style="display:none;" class="alert alert-danger" id="appNameError"></p> \
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
                                <label class="col-sm-2 control-label">Created/Modified on: </label>  \
                                <div class="col-sm-10">  \
                                    <span style="font-size:14px">' + app.createdOn + '</span>  \
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
                        <div class=""> \
                            <div class="panel-heading clearfix"> \
                                <h2 style="padding-bottom: 5px;" class="panel-title float-left">Topics Owned</h2><div class="clearfix"></div> \
                    <button id="newTopic" data-toggle="modal" data-target="#newTopicModal" class="btn btn-primary btn-md btn-add">Add New Owned Topic</button> \
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
                        <div class=""> \
                            <div class="panel-heading clearfix"> \
                                <h2 style="padding-bottom: 5px;" class="panel-title">Topics Subscribed</h2><div class="clearfix"></div> \
                    <button id="newSubscribe" data-toggle="modal" data-target="#newSubscribeTopicModal" class="btn btn-primary btn-md btn-add">Add New Subscription</button> \
                    <button id="encryption" data-toggle="modal" data-target="#encryptionModal" class="btn btn-primary bt-md btn-add"><span class="glyphicon glyphicon-lock"></span id="encyptLabel">Enable Message Encryption</button> \
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
          <div id="encryptionModal" class="modal fade" tabindex="-1" role="dialog" style="display: none;"> \
          <div class="modal-dialog"> \
            <div class="modal-content"> \
              <div class="modal-header"> \
                <button type="button" class="close" data-dismiss="modal" aria-label="Close"><span aria-hidden="true">×</span></button> \
                <h4 class="modal-title">Setup Encryption for Messages</h4> \
              </div> \
              <div class="modal-body"> \
                <p><h2>How to Create an Certificate for Encryption</h2> \
                <ol> \
                <li>$ openssl genrsa -out private.pem 2048</li> \
                <li>$ openssl rsa -in private.pem -out public.pem -outform PEM -pubout</li> \
                <li>$ cat public.pem</li> \
                <li>paste content below </li> \
                </ol> \
                 </p> \
                 <p><h2>How to Decrypt Message using an Private Key </h2> \
                 Note: \
                  Messages will be encrypt and base64 encoded at rest.  Messages being pulled will be in the same format. <br /> \
                  You will need to unbase64 the messages and use your private key created above to read the messages <br /> \
                 Example Of Decrypting Messages with your Private Key: \
                   (File Message is the response from MessageMap Subscriber) <br /> \
                   $ base64 -d message | openssl rsautl -decrypt -out decrypted -inkey private.pem <br /> \
                  File decrypt will have the Message sent in unencrypted \
                 </p> \
                 <p id="encryptionError"> </p> \
                <p>Enter in your Certificate Value: <br /> <textarea  rows="15" class="form-control" id="EncryptionValue" /> \
                </p> \
              </div> \
              <div class="modal-footer"> \
                <button type="button" class="btn btn-default" data-dismiss="modal">Close</button> \
                <button type="button" class="btn btn-primary" id="addEncryption">Add/Update Encryption</button> \
                <button type="button" class="btn btn-primary" id="removeEncryption">Remove All Encryption</button> \
              </div> \
                            </div> \
            </div><!-- /.modal-content --> \
          </div><!-- /.modal-dialog --> \
                </div> \
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
        //Encryption Value
        if (app.encrypt.length > 1){
          $('#encryption').html('<span class="glyphicon glyphicon-lock"></span>Modify/Remove Message Encryption</button>');
          $('#EncryptionValue').val(app.encrypt);
        } else{
          $('#encryption').html('<span class="glyphicon glyphicon-unlock"></span>Enable Message Encryption</button>');
        }
        $('#removeEncryption').click(function(e){
           $('#EncryptionValue').val('');
           $('#encryptionModal').find(".close").click();
        });
        $('#addEncryption').click(function(e){
           //Validate Key Here
           $.post('/api/validateEncryption', { 'encrypt': $('#EncryptionValue').val() }, function(data, status){
              if(status == 200){
                  $('#encryptionModal').find(".close").click();
                } else {
                  $('#encryptionError').html('Invalid Value for Public Certificate Key').addClass('alert alert-danger');
                }
           }).done(function(){
             $('#encryptionModal').find(".close").click();
           }).fail(function(jqxhr, settings, ex) {
              $('#encryptionError').html('Invalid Value for Public Certificate Key').addClass('alert alert-danger');
           });
        });
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
            var owned_topics = [];
            $.get('/api/application', function(Apps){
              Apps.forEach(function(Amulti){
                Amulti.ownedTopics.toString().split(',').forEach(function(A){
                owned_topics.push(A);
                });
              });
            $('.topicList').html('');
            $.each(result, function(idx, topic) {
              if(owned_topics.indexOf(topic.id) == -1){
                $('.topicList').append('<option value="' + topic.id + '">' + topic.name + '</option>');
              }
            });
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
          $.ajax({
            url: '/api/application/' + id,
            type: 'DELETE'
          });
          window.location.hash = '#/applications';
        });
        $('#saveApplication').on('click', function(e) {
          e.preventDefault();
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
              "description": $('#appDescription').val(),
              "encryption": $('#EncryptionValue').val()
            },
            success: function(){
              window.location.hash = '#/applications';
            }
          });
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
                                    <span class="app-name"><a class="appNameFull" href="#/applications/' + app.id + '">' + app.name + '</a></span>' + Pub + Sub + ' \
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
        var appN = $('#appName').val().trim();
        var namecheck = false;
        $('.appNameFull').each(function(e){
          checkname = $(this).html().trim();
          if (checkname == appN) {
            namecheck = true;
          }
        });
        if (namecheck == false){
        $.post('/api/application', {
          appName: $('#appName').val()
        }, function(app) {
          $('#appModal').find(".close").click();
          var Pub = '<span class="badge badge-primary" style="display:none">Publisher</span>';
          var Sub = '<span class="badge badge-warning" style="display:none">Subscriber</span>';
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
        } else {
          $('#appNameError').html("Application Name is already in use");
          $('#appNameError').show();
        }
      });
    }
  }

};

//Pull Application id from Route
var id = window.location.hash.split('#/')[1].split('/')[1] || false;

//start actions
$('#app').html(controller.template);
controller.script(id);
