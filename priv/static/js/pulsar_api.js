var PulsarAPI = (function() {
    function $f(expr) {
        return new Function('return ' + expr, 'a', 'b','c','d');
    }
    var pulsar_host = '';
    return {
        set_pulsar_server: function(host) {
            var pulsar_host = host;
        },
        list_hosts: function(callback) {
            $.getJSON('/site/list', callback);
        },
        stop_watch: function(tId) {
            window.clearTimeout(tId);
        },
        watch_key: function(host, key, filters, callback) {
            var key_query;
            var url = '/stats/' + host + '/' + key;
            var qs = [];
            for(var i = 0; i < filters.length; i++) {
                var key = encodeURI(filters[i][0]);
                var value = encodeURI(filters[i][1]);
                qs.push(key + '=' + value);
            }
            url = pulsar_host + url; 
            if(qs.length) {
                url += '?' + qs.join('&');
            }
            return window.setInterval(function() {
                $.getJSON(url, callback);
            }, 5000);
        }
    }
})();

