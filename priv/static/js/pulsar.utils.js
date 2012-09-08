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
        }
    }
})();
