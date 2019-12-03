var controller = {
    'template' : '<h1 class="page-title">MessageMap.io -- Dashboard</h1> \
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
						<div class="bar-chart-icon"></div> \
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
						<div class="bar-chart-icon"></div> \
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
						<div class="bar-chart-icon"></div> \
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
            <div class="col-md-5 well well-lg text-center alert alert-info">  \
              <h1>Publish: <span id="publishes_msgs" ></h1> \
              <!-- span class="col-sm-12 chart rounded mh-100" id="publish_graph" / --> \
            </div> \
            <div class="col-md-5 col-md-offset-2 well well-lg text-center alert alert-info">  \
              <div class=""><h1>Consume: <span id="consume_msgs" ></h1> \
              <!-- span class="col-sm-12 chart rounded mh-100" id="consume_graph" / --> \
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
    'script': function(){
        $('.page-sidebar').show();
        $('.main-header').show();
        $('.footer-main').show();
        var series = [];
          $.get('/api/application', function(result) {
            result.forEach(function(app){
              $('.application-stats').append('<div class="form-group">  \
               <h1> <a href="/#/applications/'+app.id+'">'+app.name+'</a></h1> \
              <div class="line-dashed"></div> \
                <div class="col-sm-3">  \
                  <div class="well well-lg text-center alert alert-info"><h1>Published<br /><span id="pub_stat_'+app.id+'">0</span></h1></div> \
                </div> \
                <div class="col-sm-3">  \
                  <div class="well well-lg text-center alert alert-info"><h1>Consumed<br /><span id="con_stat_'+app.id+'">0</span></h1></div> \
                </div> \
                <div class="col-sm-3">  \
                  <div class="well well-lg text-center alert alert-info"><h1>Waiting<br /><span id="wait_stat_'+app.id+'">0</span></h1></div> \
                </div> \
                <div class="col-sm-3">  \
                  <div class="well well-lg text-center alert alert-success"><h1>Storage Percentage<br /><span id="storage_stat_'+app.id+'">0 %</span></h1></div> \
                </div> \
                </div>');
            });
          });

        //Start Graphing Data
        var pubdata = [],
          condata = [],
          data = [],
    			totalPoints = 10;

    		function getRandomData() {

    			if (data.length > 0)
    				data = data.slice(1);

    			// Do a random walk

    			while (data.length < totalPoints) {

    				var prev = data.length > 0 ? data[data.length - 1] : 50,
    					y = prev + Math.random() * 10 - 5;

    				if (y < 0) {
    					y = 0;
    				} else if (y > 100) {
    					y = 100;
    				}

    				data.push(y);
    			}

    			// Zip the generated y values with the x values

    			var res = [];
    			for (var i = 0; i < data.length; ++i) {
    				res.push([i, data[i]])
    			}

    			return res;
    		}

    		// Set up the control widget
        var stack = 0,
        			bars = true,
        			lines = false,
        			steps = false,
    		      updateInterval = 60*5;

    	//	var pub_plot = $.plot("#publish_graph", [ getRandomData() ], {
    	//		series: {
  	//				stack: stack,
  	//				lines: {
  	//					show: lines,
  	//					fill: true,
  	//					steps: steps
  	//				},
  	//				bars: {
  	//					show: bars,
  	//					barWidth: 0.6
  	//				}
    	//		},
    	//		yaxis: {
    	//			min: 0
    	//		},
    	//		xaxis: {
    	//			show: false
    	//		}
    	//	});

        //var con_plot = $.plot("#consume_graph", [ getRandomData() ], {
    	//		series: {
  	//				stack: stack,
  	//				lines: {
  	//					show: lines,
  	//					fill: true,
  	//					steps: steps
  	//				},
  	//				bars: {
  	//					show: bars,
  	//					barWidth: 0.6
  	//				}
    	//		},
    	//		yaxis: {
    	//			min: 0
    	//		},
    	//		xaxis: {
    	//			show: false
    	//		}
    	//	});

        //Starting Websocket
        var socket = new WebSocket('wss://'+window.location.hostname+':'+window.location.port+'/ws');

        // Show a connected message when the WebSocket is opened.
        socket.onopen = function(event) {
          socket.send(true);
        };

        var datapull = {
          published: 0,
          consumed: 0
        };
        // Handle messages sent by the server.
        socket.onmessage = function(event) {
          var message = JSON.parse(event.data);
          var pub_mps = 0;
          var con_mps = 0;
          if(datapull.published > 0){
            pub_mps = message.totalMessagesPublished - datapull.published  + 6;
          }
          if(datapull.consumed > 0){
            con_mps = message.totalMessagesConsumed - datapull.consumed;
          }
          datapull.published = message.totalMessagesPublished;
          datapull.consumed = message.totalMessagesConsumed;
          $('.data-point-label').remove();
          //Change this to have the numbers since last pull Def
    	//		pub_plot.setData([getRandomData()]);
          //Graph Labels
         // $.each(pub_plot.getData()[0].data, function(i, el){
         //   var o = pub_plot.pointOffset({x: el[0], y: el[1]});
         //   $('<div class="data-point-label">' + Math.round(el[1]) + '</div>').css( {
         //     position: 'absolute',
         //     left: o.left + 5,
         //     top: o.top - 20,
         //     display: 'none'
         //   }).appendTo(pub_plot.getPlaceholder()).fadeIn('slow');
         // });
    	//		pub_plot.draw();
    	//		con_plot.setData([getRandomData()]);
        //  $.each(con_plot.getData()[0].data, function(i, el){
        //    var o = con_plot.pointOffset({x: el[0], y: el[1]});
        //    $('<div class="data-point-label">' + Math.round(el[1]) + '</div>').css( {
        //      position: 'absolute',
        //      left: o.left + 5,
        //      top: o.top - 20,
        //      display: 'none'
        //    }).appendTo(con_plot.getPlaceholder()).fadeIn('slow');
        //  });
    	//		con_plot.draw();
          $('#consume_msgs').html('<h1>' + message.totalMessagesConsumed.toString().replace(/\B(?=(\d{3})+(?!\d))/g, ",") + '</h1>');
          $('#publishes_msgs').html('<h1>' + message.totalMessagesPublished.toString().replace(/\B(?=(\d{3})+(?!\d))/g, ",") + '</h1>');
          message.appstats.forEach(function(app){
            var percentFull = parseFloat((app.messages_waiting/20000)*100).toFixed(2);
            $('#pub_stat_'+app.id).html(app.published_messages.toString().replace(/\B(?=(\d{3})+(?!\d))/g, ","));
            $('#con_stat_'+app.id).html(app.consumed_messages.toString().replace(/\B(?=(\d{3})+(?!\d))/g, ","));
            $('#wait_stat_'+app.id).html(app.messages_waiting.toString().replace(/\B(?=(\d{3})+(?!\d))/g, ","));
            $('#storage_stat_'+app.id).html(percentFull.toString()+' %');
            if(parseInt(percentFull) > 90){
              $('#storage_stat_'+app.id).parent().parent().addClass('alert-danger').removeClass('alert-success');
            } else {
              $('#storage_stat_'+app.id).parent().parent().addClass('alert-success').removeClass('alert-danger');
            }
          });
          $('#app_counts').html(message.applications)
          $('#topic_counts').html(message.topics)
          $('#schema_counts').html(message.schemas)
          $('#published_counts').html(message.totalMessagesPublished)
          $('#consumed_counts').html(message.totalMessagesConsumed)
        };

        // Handle any errors that occur.
        socket.onerror = function(error) {
          console.log('WebSocket Error: ');
          console.log(error);
        };

        // Show a disconnected message when the WebSocket is closed.
        socket.onclose = function(event) {
          console.error('Disconnected from WebSocket.');
          console.error('Reconnecting');
          socket = new WebSocket('wss://'+window.location.hostname+':'+window.location.port+'/ws');
        };
    }
};

//start actions
$('#app').html(controller.template);
controller.script();
