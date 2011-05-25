/**
 * Sacraspot
 * Ad Majoram Dei Gloriam
 *
 * Notes: Currently needs jQuery for ajax.
 *
*/
(function($, undefined){

    var root = this, hasOwnProperty = Object.prototype.hasOwnProperty;
    var old_sacraspot = root.sacraspot, sacraspot = {};

    function no_conflict() {
        root.sacraspot = old_sacraspot;
        return sacraspot;
    }

    sacraspot.no_conflict = no_conflict;

    // Export sacraspot to the root.
    root.sacraspot = sacraspot;

    var paths = "/latitude-and-longitude" +
                ";/query-parishes" +
                ";/query-sacraments",
    base_host = "www.beggersandbuskers.com/sacraspot/api",
    protocol = window.location.protocol.indexOf("http") > -1 ?
                            window.location.protocol : "http:";

    paths = paths.split(";");
    for (var i=0; i < paths.length; i++) {
        paths[i] = protocol + "//" + base_host + paths[i];
    }

    var common_params = { // That is, parameters common to query-*
        ip: undefined,
        latitude: undefined,
        longitude: undefined,
        distance: undefined,
        maxresults: undefined
    };


    /**
     * Query
     *
     * A factory for producing <sacraspot.query> functions.
     * These functions can be chained. (i.e. sacraments().future(70000).distance(100).maxresults(25))
     *
     * @param url {string} A normalized url that is the endpoint of the REST method.
     * @param url_methods {Object} A dictionary of key-value pairs
     * @param default_values {Object}
     *
     * NOTE: Currently we are tightly coupled to jQuery.
     * I want to move out the jQuery specific stuff into the adapter.
     * The code may become slightly more verbose, but people won't *need* to
     * include jQuery when they have a perfectly viable alternative already on
     * their page.
     *
    */
    function Query(url, url_methods, default_values) {
        url = url || "";
        default_values = default_values || {};
        default_values = 
            _extend({
                data: undefined,
                dataType: "json",
                success: default_success,
                type: "POST",
                "url": url
            }, default_values, true);
        var current_values = {}, g = {}; // Global variable for state passing.

        function default_success(data, xhr_code, xhr) {
            ( _log(data), _log(xhr_code), _log(xhr) );
        }

        function query(parameters, update_defaults) {
            if (typeof parameters === "undefined") {
                // Then we are chaining
                return query;
            } else if (update_defaults && typeof parameters === 'object') {
                // Then we just want to update a default value.
                _extend(default_values, parameters, true);
                return query;
            }

            // Otherwise, we are invoking the REST method
            // and are interested in the data.
            // FIXME: This way of doing things litters the parameters with useless
            // data (data that is first in parameters and then in parameters.data)
            // and has collision potential.  Find a better way.
            parameters = _extend(parameters, default_values);
            var success = parameters.success;
            delete parameters.success;
            // `success` is the only non-data parameter that might be passed in
            // (From the `fetch` method.)

            if ( ! g.fetch ) { // Avoid double formating.
                for (param in parameters) {
                    if ( ! hasOwnProperty.call(parameters, param) ||
                         typeof query[param] !== "function" ) continue;
                    query[param](parameters[param]);
                }
            }

            // NOTE: Assumes all methods are setting data parameters
            // for the REST method.  Is this the best way to do this?
            parameters = _extend(parameters, {data: current_values, success: success});
            (_log("Parameters: "), _log(parameters));
            return sacraspot.adapter.ajax(parameters);
        }

        query.methods = [];
        for (key in url_methods) {
            if ( ! hasOwnProperty.call(url_methods, key) ) continue;
            query[key] = (function(key, method) {
                method = typeof method === "function" ? method : function(data) { return data };
                return function(value) {
                    current_values[key] = method(value);
                    return query;
                };
            })(key, url_methods[key]);
            query.methods.push(key);
        }
        query.methods.sort();

        query["fetch"] = function(callback) {
            g.fetch = true;
            // If `callback` is undefined, the default success function will be used.
            current_values.success = callback;
            var data = query(current_values);
            delete g.fetch;

            for ( key in current_values ) {
                delete current_values[key];
            }

            return data;
        };

    return query;
    }

    sacraspot.location = Query(paths[0], {"ip": undefined});
    sacraspot.parishes = Query(paths[1], common_params);
    sacraspot.sacraments = Query(paths[2], _extend({time: undefined, future: undefined, sacraments: undefined, languange: undefined}, common_params));

    /**** Adapter Code ****/

    var adapter = sacraspot.adapter = {};

    /**
     * Jquery adapter
    */
    adapter.ajax = $.ajax;


    /**** Private Utilities ****/
    sacraspot.config = {DEBUG: true, DEBUG_ALERT_MESSAGES: false};

    /**
     * Add properties of sender to receiver
     * NOTE: This operation has side effects.
     * Adds properties in place! (i.e. Does not clone receiver.)
     *
     * @argument receiver {object} optional Object receiving new attributes
     * @argument sender {object} optional Object from which attributes will be copied.
     * @argument overwrite {mixed} A boolean that answers the question, should
     *  receiver's attribute be overwritten with the value of sender's?
     *  (true: yes, false: no, only null and undefined values: "null" or null)
     *  Defaults to null.
     *
     * @returns receiver
    */
    function _extend(receiver, sender, overwrite) {
        // Defaults to null and false.
        overwrite = (overwrite === true || overwrite === false) ?
                                            overwrite : null;
        var coallesce_nulls = /null/i.test(overwrite);
        overwrite = !! overwrite;

        receiver = receiver || {};
        sender = sender || {};
        for (key in sender) {
            if (overwrite || ! hasOwnProperty.call(receiver,key)) {
                delete receiver[key];
                receiver[key] = sender[key];
            } else if ( coallesce_nulls &&
                       ( typeof receiver[key] === "undefined" || receiver[key] === null) ) {
                delete receiver[key];
                receiver[key] = sender[key];
            }
        }
        return receiver;
    }

    /**
     * Logs msg to the console if available or alerts them if desired.
     *
     * @argument msg {mixed} The string or javascript object to log.
     * @returns boolean Success
     *
    */
    function _log(msg) {
        if (sacraspot.config.DEBUG === true) {
            if (typeof console === 'object' && typeof console.log === 'function') {
                console.log(msg);
            } else if (typeof arguments[1] === 'function') {
                arguments[1](msg);
            } else if (sacraspot.config.DEBUG_ALERT_MESSAGES === true) {
                alert(msg);
            }
        }
        return true;
    }


})(jQuery);
