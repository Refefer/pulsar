var PulsarAPI = (function() {
    function $f(expr) {
        return new Function('return ' + expr, 'a', 'b','c','d');
    }
    var eventsources = {};
    var pulsar_host = '';
    return {
        set_pulsar_server: function(host) {
            var pulsar_host = host;
        },
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
        watch_keys: function(host, callbacks) {
            var keys = $.map(callback, function(key) {
                return key;
            }).join(',');
        },
        watch_key: function(host, key, callback) {
            if(eventsources[host] !== undefined) {
                eventsources[host].close();
            }
            var key_query;
            if(typeof key === "string") {
                key_query = "&key=" + key;
            } else {
                key_query = "&key=" + key.key + 
                            "&value=" + key.value +
                            "&subkey=" + key.subkey;
                key = ([key.key, key.value, key.subkey]).join(':');
            }
            var url = pulsar_host + '/site/watch?host=' + host + key_query;
            var es = eventsources[host] = new EventSource(url);
            es.addEventListener(key, function(event) {
                callback(JSON.parse(event.data));
            });
        }
    }
})();

