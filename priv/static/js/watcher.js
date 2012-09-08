var PulsarAPI = (function() {
    return {
        list_hosts: function(callback) {
            $.getJSON('/site/list', callback);
        },
        add_host: function(host, callback) {
            $.ajax('/site/add', {
                data: {"host": host},
                success: callback
            });
        },
        remove_host: function(host, callback) {
            $.ajax('/site/remove', {
                data: {"host": host},
                success: callback
            });
        },
        watch_host: function(host, statistic) {
            var es = new EventSource('/site/watch?host=' + hostname);
        }
    }
})();

$(document).ready(function() {
    // Everything is stored on the graph variable
    var graph = {};

    Highcharts.setOptions({
        global: {
            useUTC: false
        }
    });

    graph.chart = new Highcharts.Chart({
        chart: {
            renderTo: 'graph',
            type: 'line',
            marginRight: 10,
        },
        title: {
            text: 'Current Traffic'
        },
        xAxis: {
            type: 'datetime',
            tickPixelInterval: 150,
        },
        yAxis: {
            title: {
                text: 'Current Visitors'
            },
            plotLines: [{
                value: 0,
                width: 1,
                color: '#808080'
            }]
        },
        tooltip: {
            formatter: function() {
                    return '<b>'+ this.series.name +'</b><br/>'+
                    Highcharts.dateFormat('%Y-%m-%d %H:%M:%S', this.x) +'<br/>'+
                    Highcharts.numberFormat(this.y, 2);
            }
        },
        legend: {
            enabled: true
        },
        exporting: {
            enabled: false
        },
        series: []
    });
    
    var EventSources = {};
    // API time!
    graph.watch_site = function watch_site(hostname) {
        // No double adding!
        if(EventSources[hostname]) return;

        // Build event source
        var es = new EventSource('/site/watch?host=' + hostname);
        EventSources[hostname] = {source: es, events: {}};

        // Add the appropriate handlers
        es.addEventListener('open', function(e) {
            graph.chart.addSeries({
                name: hostname,
                data: (function() {
                    var time = new Date().getTime();
                    var data = [];
                    for(var i = 0; i < 100; i++) {
                        data.push({x: (time - (i * 5000)), y: 0});
                    }
                    return data.reverse();
                })()
            });
            EventSources[hostname].series = graph.chart.series[graph.chart.series.length- 1];
        }, false);

        es.addEventListener("urls", function(e) {
            var data = JSON.parse(e.data);
            // Sum the points
            var total = 0;
            for(var i = 0; i < data.length; i++) {
                total += data[i][2];
            }
            var series = EventSources[hostname].series;
            if(series) {
                series.addPoint([new Date().getTime(), total], true, true);
            } else {
                alert('No series!');
            }
            // Run attached listeners
            var listeners = EventSources[hostname].events.urls;
            if(listeners !== undefined) {
                for(var i = 0; i < listeners.length; i++) {
                    listeners[i](data);
                }
            }
        }, false);

        es.addEventListener('error', function(e) {
            var series = EventSources[hostname].series;
            series.remove();
       }, false);
    }

    graph.remove_site = function remove_site(hostname) {
        var es = EventSources[hostname];
        if(!es) return;
        es.source.close();
        es.series.remove();
        delete EventSources[hostname];
    }

    graph.add_event_listener = function(host, event, callback) {
        var es = EventSources[host];
        if(!es) return;
        if(es.events[event] ===  undefined) {
            es.events[event] = [];
        }
        es.events[event].push(callback);
    }

    window.graph = graph;

});

