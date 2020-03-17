var controller = {
    'template' : '<!-- Secondary header --> \
                      <div class="header-secondary row gray-bg"> \
                          <div class="col-lg-12"> \
                              <div class="page-heading clearfix"> \
                                  <h1 class="page-title pull-left">API Documentation</h1> \
                              </div> \
                              <!-- Breadcrumb --> \
                              <ol class="breadcrumb breadcrumb-2">  \
                                  <li><a href="#/"><i class="fa fa-home"></i>Home</a></li>  \
                                  <li class="active"><a href="#/apidocs"><strong>API Documentation</strong></a></li>  \
                              </ol> \
                          </div> \
                      </div> \
                      <!-- /secondary header --> \
                    <!-- Main content --> \
                      <div class="main-content"> \
                      <div class="row"> \
                      <h1 class="pagetitle"> API Documentation</h1> \
                      </div> \
                                <hr size="2"> \
                                         <h2>Authentication</h2> \
                                          <p>Returns json data with authentication token</p> \
                                          <ul> \
                                          <li> \
                                          <strong>URL</strong> \
                                          <br> /api/auth/token?client_id=<code>APPLICATION ID</code>&amp;grant_type=authorization_code&amp;code=<code>APIKEY</code> \
                                          </li> \
                                          <li> \
                                          <strong>Method:</strong><br> \
                                          <code>POST</code></li> \
                                          <li> \
                                          <strong>Success Response:</strong> \
                                          <ul> <li><strong>Code:</strong> 200<br> <strong>Content:</strong> \
                                          <code>{ "token_type": "bearer", "scope": [LISTING_OF_APP_OPERATIONS], "refresh_token": TOKEN_REFRESH, "expires_in": NUM_SECONDS, "access_token": ACCESS_TOKEN }</code></li> \
                                           </ul> </li> \
                                           </ul> \
                                           <hr size="2"> \
                                           <h2> \
                                           <a id="Push_Message_to_Topic_12"></a> \
                                           <strong>Push Message to Topic</strong></h2> \
                                           <p>Sending a message to a Topic</p> <ul> <li><strong>URL</strong><br> /messages/<code>TOPIC</code></li> \
                                           <li><strong>Method:</strong><br> <code>POST</code></li> <li><strong>Headers:</strong><br> \
                                           <code>{ "content-type": "application/json", "authentication": "Bearer ACCESS_TOKEN" }</code></li> \
                                           <li><strong>Body:</strong><br> <code>{ "json_object":"any object you need to pass to a topic" }</code></li> \
                                           <li><strong>Success Response:</strong> \
                                          <ul> <li><strong>Code:</strong> 200<br> \
                                          <strong>Content:</strong> <code>{ "status": "good" }</code></li> </ul> \
                                          </li> </ul> <hr size="2"> \
                                          <h2><a id="Pull_Message_from_Topic_28"></a><strong>Pull Message from Topic</strong></h2> \
                                          <p>Retreive a messages from a Topic</p> <ul> <li><strong>URL</strong><br> /messages/<code>TOPIC</code></li> \
                                          <li><strong>Method:</strong><br> <code>GET</code></li> <li><strong>Headers:</strong><br> \
                                          <code>{ "content-type": "application/json", "authentication": "Bearer ACCESS_TOKEN" }</code></li> \
                                          <li><strong>Success Response:</strong> <ul> <li><strong>Code:</strong> 200<br> \
                                          <strong>Content:</strong> <code>[{ "array_of_jsons":"any object that is current on the topic" }]</code></li> </ul> \
                                          </li> </ul> \
                                              </div> \
                      </div>',
    'script': function(){
        }
};

//start actions
$('#app').html(controller.template);
controller.script();
