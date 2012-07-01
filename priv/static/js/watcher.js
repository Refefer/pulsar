$(document).ready(function() {
    Highcharts.setOptions({
        global: {
            useUTC: false
        }
    });

    window.chart = new Highcharts.Chart({
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
    window.watch_site = function watch_site(hostname) {
        // No double adding!
        if(EventSources[hostname]) return;

        // Build event source
        var es = new EventSource('/site/watch?host=' + hostname);
        EventSource[hostname] = {source: es};

        // Add the appropriate handlers
        es.addEventListener('open', function(e) {
            chart.addSeries({
                name: hostname,
                data: (function() {
                    var time = new Date().getTime();
                    var data = [];
                    for(var i = 0; i < 30; i++) {
                        data.push({x: (time - (i * 5000)), y: 0});
                    }
                    return data.reverse();
                })()
            });
            EventSource[hostname].series = chart.series[chart.series.length- 1];
        }, false);

        es.addEventListener("urls", function(e) {
            var data = JSON.parse(e.data);
            // Sum the points
            var total = 0;
            for(var i = 0; i < data.length; i++) {
                total += data[i][2];
            }
            var series = EventSource[hostname].series;
            if(series) {
                series.addPoint([new Date().getTime(), total], true, true);
            } else {
                alert('No series!');
            }
        }, false);

        es.addEventListener('error', function(e) {
            var series = EventSource[hostname].series;
            if (series !== undefined && e.eventPhase == EventSource.CLOSED) {
                series.remove();
            }
       }, false);
    }

    window.remove_site = function remove_site(hostname) {
        var es = EventSources[hostname];
        if(!es) return;
        es.source.close();
    }
});

