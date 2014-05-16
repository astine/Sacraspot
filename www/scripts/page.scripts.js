function format(s, yes_value, no_value) {
    yes_value = yes_value || "", no_value = no_value || "";
    var formatter = /\{(\d+)\}/g;
    function search_replace(match, ref1) {
        if (+ref1 < s.length) {
            return s[+ref1];
        } else {
            return "";
        }
    }
    if (s && s.length > 0) {
        if (! s.pop ) s = [s];
        return yes_value.replace(formatter, search_replace)
    } else {
        return no_value;
    }
}

function parse_search_string(s) {
    var search_splitter = /^(?!\s)(?!-)(.*?)(\s-.*)?$/i,
    flag_pairs = /\s*-(\w+)\s*[\s:]\s*([^-]*)/gi,
    flag_mapping = {};
    
    var temp = search_splitter.exec(s);
    var search_term = temp[1] || "", search_flags = temp[2] || "";
    search_flags.replace(flag_pairs,
                         function(match, key, value) {
                            flag_mapping[key] = parse_array(value);
                        });
    return {"term": search_term, "flags": flag_mapping};
}

/**
 * @param s {string} The string to be parsed.
 * @returns {mixed} An array if the string is comma-seperated and,
 * optionally, bracket-enclosed. Otherwise, the unaltered string.
*/
function parse_array(s) {
    var array_finder = /\[?[^,]+,\]?/i;
    if ( array_finder.test(s) ) {
        return s.replace(/(\[|\])/g,"").replace(/\s*,\s*/g,",").split(",");
    } else {
        return s;
    }
}


function normalize_json(json_data) {
    // Normalize the keys
    _.each(json_data, function(item, index, json_data) {
            _.each(item, function(val, key, item) {
                        delete item[key];
                        key = key.toLowerCase();
                        item[key] = val;
                    });
    });
    return json_data;
}


/* KO Custom Bindings and helpers below this point */

// Just an outline based on Google Maps.
function get_location() {
    if (navigator.geolocation) {
        navigator.geolocation.getCurrentPosition(function(position) {
            [position.coords.latitutde, position.coords.longitude]
        },
        function() {
            var handle_failure = true;
        })
    }
}

var unique_name = (function(){
    var i = 0;
    return function(prefix) {
        prefix = prefix || "Ps_";
        return prefix + (++i);
    }
})();

ko.bindingHandlers.map = {
    init: function(element, valueAccessor, allBindingsAccessor, viewModel) {
        var view_bindings = allBindingsAccessor();
        var zoom = view_bindings.zoom || 10,
            mapTypeId = view_bindings.mapTypeId || google.maps.MapTypeId.ROADMAP,
            location = ko.utils.unwrapObservable(valueAccessor());
        
        location = location &&
            location.length === 2 &&
            location.pop &&
            new google.maps.LatLng(location);
        
        var map = new google.maps.Map(element,
                                      {"center": location,
                                      "mapTypeId": mapTypeId,
                                      "zoom": zoom });
        element["data-map"] = map;
    },
    update: function(element, valueAccessor, allBindingsAccessor, viewModel) {
        var map = element["data-map"];
        map.setCenter(new google.maps.LatLng(valueAccessor()));
    }
};

ko.bindingHandlers.unique_id = {
    init: function(element, valueAccessor, allBindingsAccessor, viewModel) {
        var val = ko.utils.unwrapObservable(valueAccessor()),
            view_bindings = allBindingsAccessor();
        element.id = element.id || unique_name(val);
    },
    update: function(element, valueAccessor, allBindingsAccessor, viewModel) {

    }
};

/*
ko.bindingHandlers.unique_id = {
    init: function(element, valueAccessor, allBindingsAccessor, viewModel) {
        var val = ko.utils.unwrapObservable(valueAccessor()),
            view_bindings = allBindingsAccessor();

    },
    update: function(element, valueAccessor, allBindingsAccessor, viewModel) {

    }
};

JSON.parse('[{"fullname":"St. Joseph","city":"Alexandria","state":"VA","kind":"Mass","time":"Nov 11, 2010 7:45 am","details":"","language":"English","latitude":38.8123,"longitude":-77.0463,"distance":2.997086531540474,"weight":18.44026935867898},{"fullname":"St. Mary","city":"Alexandria","state":"VA","kind":"Mass","time":"Nov 11, 2010 8:00 am","details":"","language":"English","latitude":38.8021,"longitude":-77.0437,"distance":2.853866199160123,"weight":20.080947364396877},{"fullname":"St. Mary","city":"Alexandria","state":"VA","kind":"Mass","time":"Nov 11, 2010 8:00 am","details":"","language":"English","latitude":38.8021,"longitude":-77.0437,"distance":2.853866199160123,"weight":20.080947364396877},{"fullname":"St. Louis","city":"Alexandria","state":"VA","kind":"Mass","time":"Nov 11, 2010 8:45 am","details":"","language":"English","latitude":38.7652,"longitude":-77.0818,"distance":1.9069545740002332,"weight":21.899477613437536}]');
*/