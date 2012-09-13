var PulsarUtils = (function() {
    /*  
      Deserialiezes a url's fragment with the following convention:
        '&' denotes a separator
        '=' denotes a key/value separator
        ',' denotes a value delimiter
    */
    return {
        data_from_hash: function(hash) {
            if(hash === undefined) {
                hash = document.location.hash;
            }
            if( /^#/.test(hash) ) {
                hash = hash.substr(1);
            }
            var d = {};
            var pieces = hash.split('&');
            for(var i = 0; i < pieces.length; i++) {
                var keyvalue = pieces[i].split('=');
                var values = keyvalue[1].split(',');
                d[keyvalue[0]] = values.length === 1 ? values[0] : values;
            }
            return d;
        },

        sort_data: function(data) {
            return data.sort(function(a, b) {
                if(a[0] < b[0]) {
                    return -1;
                } else if(a[0] > b[0]) {
                    return 1
                }
                return 0;
            });           
        },

        build_key_filter: function(keys) {
            keys = (function(keys) {
                if(keys === undefined) return keys;
                if(typeof keys === "string") {
                    keys = [keys];
                }
                var f = {};
                for(var i = 0; i < keys.length; i++) {
                    f[keys[i]] = 1;
                }
                return f;
            })(keys);

            // Filter function
            return function(data) {
                // Filter out those only names we care about
                if(keys !== undefined) {
                    var new_data = [];
                    for(var i = 0; i < data.length; i++) {
                        if(keys[data[i][0]]) {
                            new_data.push(data[i]);
                        }
                    }
                    data = new_data;
                }
                return data;
            }
        },

        data_to_percent: function(data, places) {
            places = typeof places  === "integer" ? places : 2;

            // Count up everything
            var total = 0;
            for(var i = 0; i < data.length; i++) {
                total += data[i][1];
            }

            data = data.slice();

            // Normalize 
            var mag = Math.pow(10, places);
            for(var i = 0; i < data.length; i++) {
                var d = data[i][1] / total;
                data[i][1] = Math.round(mag * d) / mag;
            }

            return data;
        }

    }
})();
