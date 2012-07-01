
var watchHost = (function() {
    function buildStatusHandler(host) {
        var msgs = [];
        var s = document.getElementById('status');
        return function(msg) {
            if(msgs.length > 10) {
                msgs.shift();
            }
            var date = new Date();
            msgs.push(date + ": " + host + ' - ' + msg);
            s.innerHTML = msgs.join("<br />");
        }
    }

    return function(host) {
        addStatus = buildStatusHandler(host);
        var source = new EventSource('/watch/' + host);

        source.addEventListener('urls', function(event) {
            addStatus(event.data);
        }, false);

        source.addEventListener('open', function(event) {
            addStatus('connected.')
        }, false);

        source.addEventListener('error', function(event) {
            if (event.eventPhase == EventSource.CLOSED) {
                addStatus('disconnected.')
            }
        }, false);
    }

})();


