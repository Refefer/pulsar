(function () {
    // Only one instance a piece
    if(window._pulsar_tick !== undefined) {
        return;
    }
    window._pulsar_tick = true;

    var parse_url = (function() {
        var a = document.createElement('a');
        return function(url) {
            a.href = url;
            return {hostname: a.hostname, 
                    host: a.host,
                    port: a.port,
                    path: a.pathname};
        }
    })();

    var sendRequest = (function() {
        var XMLHttpFactories = [
            function () {return new XMLHttpRequest()},
            function () {return new ActiveXObject("Msxml2.XMLHTTP")},
            function () {return new ActiveXObject("Msxml3.XMLHTTP")},
            function () {return new ActiveXObject("Microsoft.XMLHTTP")}
        ];

        function createXMLHTTPObject() {
            for(var i=0;i<XMLHttpFactories.length;i++) {
                try {
                    return XMLHttpFactories[i]();
                }
                catch (e) { }
            }
            return xmlhttp;
        }
        return function(url,success,fail) {
            var req = createXMLHTTPObject();
            if (!req) return;
            req.open("GET",url,true);
            req.onreadystatechange = function () {
                if ( req.readyState == 4 || !(req.status == 200 ||req.status === 304) ) {
                    return fail(req);
                }
                callback(req);
            }
            if (req.readyState == 4) return;
            req.send();
        }
    })();
    // Pings 
    function ping(url, state) {
        if(state.waiting) return;

        function clear_waiting() {
            state.waiting = false;
        }

        sendRequest(url, function(req) {
            clear_waiting();
            if(req.status == 401) {
                // Whoops, we are not tracking this host
            }
        },
        clear_waiting);
        state.waiting = true;
    }

    // Build pinger url
    var url  = (function build_tick_url(host) {
        var referrer_host = parse_url(document.referrer).host;
        var attrs = ['r=' + referrer_host,
                     't=' + encodeURIComponent(document.title)];

        return "/tick/" + host + '?' + attrs.join('&');
    })(document.location.host);

    var state = {url: url, last_fired: 0};

    // And ping
    state.timeout = window.setInterval(function() {
        if(window.PULSAR_HOST !== undefined) {
            ping(window.PULSAR_HOST + state.url, state);
        }
    }, 5000);
    
})();
