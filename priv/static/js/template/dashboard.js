var controller = {
	'template': '<h1 class="page-title">MessageMap.io -- Dashboard</h1> \
		<div class="row"> \
			<div class="col-lg-4 col-md-6"> \
				<div class="panel minimal panel-default"> \
					<div class="panel-heading clearfix">  \
						<div class="panel-title">Applications</div> \
					</div>  \
					<!-- panel body -->  \
					<div class="panel-body"> \
						<div class="stack-order">  \
							<h1 class="no-margins" id="app_counts">0</h1> \
						</div> \
						<div class="header-icons fa fa-3x fa-server"></div> \
					</div>  \
				</div> \
			</div> \
			<div class="col-lg-4 col-md-6"> \
				<div class="panel minimal panel-default"> \
					<div class="panel-heading clearfix">  \
						<div class="panel-title">Topics</div>  \
					</div>  \
					<!-- panel body -->  \
					<div class="panel-body">  \
						<div class="stack-order"> \
							<h1 class="no-margins" id="topic_counts">0</h1> \
						</div> \
						<div class="header-icons fa fa-3x fa-tags"></div> \
					</div>  \
				</div> \
			</div> \
			<div class="col-lg-4 col-md-6"> \
				<div class="panel minimal panel-default"> \
					<div class="panel-heading clearfix">  \
						<div class="panel-title">Schemas</div>  \
					</div>  \
					<!-- panel body -->  \
					<div class="panel-body">  \
						<div class="stack-order"> \
							<h1 class="no-margins" id="schema_counts">0</h1> \
						</div> \
						<div class="header-icons fa fa-3x fa-check-square"></div> \
					</div>  \
				</div> \
			</div> \
		</div> \
		<div class="row"> \
			<div class="col-lg-12"> \
				<div class="panel-group"> \
					<div class="panel"> \
						<div class="panel-heading no-border clearfix">  \
							<h2 class="panel-title">Global Stats</h2> \
						</div> \
						<div class="panel-body"> \
            <div class="col-md-3 well well-lg text-center alert alert-info">  \
              <h1>Publish: <span id="publishes_msgs" ></h1> \
            </div> \
            <div class="col-md-3 col-md-offset-1 well well-lg text-center alert alert-info">  \
              <h1>Consume: <span id="consume_msgs" ></h1> \
            </div> \
						<div class="col-md-3 col-md-offset-1 well well-lg text-center alert alert-success"> \
							<h1> Storage Percent Full: <span id="storage_msgs"></h1> \
						</div> \
							</div> \
						</div> \
					</div> \
				</div> \
			</div> \
		<div class="row"> \
			<div class="col-lg-12"> \
				<div class="panel-group"> \
					<div class="panel"> \
						<div class="panel-heading no-border clearfix">  \
							<h2 class="panel-title">Application Traffic</h2> \
						</div> \
						<div class="panel-body"> \
							<div class="application-stats"> \
							</div> \
						</div> \
					</div> \
				</div> \
			</div> \
		</div>',
	'script': function () {
		$('.page-sidebar').show();
		$('.main-header').show();
		$('.footer-main').show();
		var series = [];
		$.get('/api/application', function (result) {
			result.forEach(function (app) {
				$('.application-stats').append('<div class="form-group">  \
               <h1> <a href="/#/applications/'+ app.id + '">' + app.name + '</a></h1> \
              <div class="line-dashed"></div> \
                <div class="col-sm-4">  \
                  <div class="well well-lg text-center alert alert-info"><h1>Published<br /><span id="pub_stat_'+ app.id + '">0</span></h1></div> \
                </div> \
                <div class="col-sm-4">  \
                  <div class="well well-lg text-center alert alert-info"><h1>Consumed<br /><span id="con_stat_'+ app.id + '">0</span></h1></div> \
                </div> \
                <div class="col-sm-4">  \
                  <div class="well well-lg text-center alert alert-info"><h1>Waiting<br /><span id="wait_stat_'+ app.id + '">0</span></h1></div> \
                </div> \
                </div>');
			});
		});

		function wsconnect() {
			//Starting Websocket
			if (location.protocol !== 'https:') {
				ws_protocol = "ws";
			} else {
				ws_protocol = "wss";
			}
			var socket = new WebSocket(ws_protocol + '://' + window.location.hostname + ':' + window.location.port + '/ws');

			// Show a connected message when the WebSocket is opened.
			socket.onopen = function (event) {
				socket.send(true);
			};

			var datapull = {
				published: 0,
				consumed: 0
			};
			// Handle messages sent by the server.
			socket.onmessage = function (event) {
				var message = JSON.parse(event.data);
				var pub_mps = 0;
				var con_mps = 0;
				var storage = 0;
				if (datapull.published > 0) {
					pub_mps = message.totalMessagesPublished - datapull.published + 6;
				}
				if (datapull.consumed > 0) {
					con_mps = message.totalMessagesConsumed - datapull.consumed;
				}
				datapull.published = message.totalMessagesPublished;
				datapull.consumed = message.totalMessagesConsumed;
				if (parseInt(message.storage) > 9) {
					storage = ((parseInt(message.storage) / (100 - 9)) * 100);
				}
				if (storage > 100) {
					storage = 100;
				}
				$('.data-point-label').remove();
				$('#consume_msgs').html('<h1>' + message.totalMessagesConsumed.toString().replace(/\B(?=(\d{3})+(?!\d))/g, ",") + '</h1>');
				$('#publishes_msgs').html('<h1>' + message.totalMessagesPublished.toString().replace(/\B(?=(\d{3})+(?!\d))/g, ",") + '</h1>');
				if (storage > 90) {
					$('#storage_msgs').parent().parent().addClass('alert-danger').removeClass('alert-success');
				} else {
					$('#storage_msgs').parent().parent().addClass('alert-success').removeClass('alert-danger');
				}
				$('#storage_msgs').html('<h1>' + storage.toFixed(2) + ' %</h1>');
				message.appstats.forEach(function (app) {
					$('#pub_stat_' + app.id).html(app.published_messages.toString().replace(/\B(?=(\d{3})+(?!\d))/g, ","));
					$('#con_stat_' + app.id).html(app.consumed_messages.toString().replace(/\B(?=(\d{3})+(?!\d))/g, ","));
					$('#wait_stat_' + app.id).html(app.messages_waiting.toString().replace(/\B(?=(\d{3})+(?!\d))/g, ","));
				});
				$('#app_counts').html(message.applications)
				$('#topic_counts').html(message.topics)
				$('#schema_counts').html(message.schemas)
				$('#published_counts').html(message.totalMessagesPublished)
				$('#consumed_counts').html(message.totalMessagesConsumed)
			};

			// Handle any errors that occur.
			socket.onerror = function (error) {
				console.log('WebSocket Error: ');
				console.log(error);
			};

			// Show a disconnected message when the WebSocket is closed.
			socket.onclose = function (event) {
				wsconnect();
			};
		}
		wsconnect();
	}
};

//start actions
$('#app').html(controller.template);
controller.script();
