/**
 * @fileoverview
 * Sacraspot
 *
 * An interface to the sacraments
 * 
 *
*/
(function() {

	// Get the current value of the variable "sacraspot" in the global scope
	// to allow no_conflict to work.
	root = this;
	old_sacraspot = root.sacraspot;

	/**
	 *
	 * VARIABLE SETUP
	 *   Local variables that will not be used outside of this closure.
	 * 
	*/
	var map_container, main_form, search_bar, main_header, gm;
	
	sacraspot = {};
	
	/**
	 * Public Functions
	 * @see PUBLIC FUNCTION DEFINITIONS
	 * 
	*/
	sacraspot.init = init;
	sacraspot.search = search;
	sacraspot.noConflict = sacraspot.no_conflict = no_conflict;
	
	/**
	 * Public Attributes:
	 * 
	 * adapter: a DOM library adapter
	 * config: sacraspot configurations (Currently only debug settings)
	 * data: the returned data - both raw and formated.
	 * gm: useful google maps functions
	 * ui: the UI
	 *
	 * The adapter is set to whatever DOM library the user wants to use
	 * to wrap his DOM objects (i.e. jQuery's `$`, Prototype's `$$`,
	 * Dojo's `dojo.query`, etc.)
	 * Defaults to `document.getElementById`.
	 * 
	*/
	sacraspot.adapter = null;
	sacraspot.config = {};
	sacraspot.data = sdata = {};
	sacraspot.gm = gm = {};
	sacraspot.ui = sui = {};

	/**
	 * @namespace sacraspot.config
	 * Configuration Constants
	 * @see _log
	 * 
	*/
	sacraspot.config.DEBUG = true;
	sacraspot.config.DEBUG_ALERT_MESSAGES = null;

	/**
	 * @namespace sacraspot.data
	 * Sacraspot Data Attributes
	 *
	 * raw_data: The parsed JSON data from the last server request.
	 * parishes: A dictionary of parish objects, keyed on parish.fullname
	 * sacraments: A dictionary of sacrament arrays, keyed on sacrament name.
	 * 
	*/
	sdata.raw_data = null;
	sdata.parishes = {};
	sdata.sacraments = {};

	/**
	 * @namespace sacraspot.gm
	 * Google Maps Server Interactions
	 *
	 * geocoder: @see google.maps.Geocoder
	 * map: @see google.maps.Map
	 * mkr_mgr: dictionary
	 * 	@static start_loc google.maps.LatLong The geocoded start location
	 * 	@static markers array<google.maps.Marker> Currently active markers
	 * 	@static active_window google.maps.InfoWindow The currently open infoWindow.
	 * router: @see google.maps.DirectionsService
	 * renderer: @see google.maps.DirectionsRenderer
	 *
	 * GSD (Geocoder Status Description)
	 * @see google.maps.Geocoder.GeocoderStatus
	 * 
	 * GLTD (Geocoder Location Type Description)
	 * @see google.maps.Geocoder.GeocoderLocationType
	 *
	*/
	gm.geocoder = null;
	gm.map = null;
	gm.mkr_mgr = {
		start_loc: null,
		markers: [],
		active_window: null
	};
	gm.router = null;
	gm.renderer = null;
	
	gm.GSD = {
		"OK": "The request did not encounter any errors",
		"UNKNOWN_ERROR": "The request could not be successfully processed; the exact reason for the failure is not known",
		"OVER_QUERY_LIMIT": "The webpage has gone over Google's request limit",
		"REQUEST_DENIED": "The webpage is not allowed to use the geocoder for some reason",
		"INVALID_REQUEST": "The request was invalid",
		"ZERO_RESULTS": "The request did not encounter any errors but returns zero results",
		"ERROR": "There was a problem contacting the Google servers"
	};
	gm.GLTD = {
		"ROOFTOP": "You are here.",
		"RANGE_INTERPOLATED": "You are near here.",
		"GEOMETRIC_CENTER": "You are somewhere near here.",
		"APPROXIMATE": "You are in this general vicinity."
	};

	/**
	 * @namespace sacraspot.ui
	 * Sacraspot UI elements
	 *
	 * templates: A dictionary containing HTML rendering functions
	 * @see _.template
	 *
	 * map_container, main_form, search_bar, directions_panel:
	 *     The HTML ids of the elements that will be filled with the
	 *     corresponding UI elements.
	 *     After `sacraspot.init` is called, these will be overwritten
	 *     with the actual DOM elements.
	 * 
	*/
	sui.templates = {};
	sui.map_container = null;
	sui.main_form = null;
	sui.search_bar = null;
	sui.direction_panel = null;

	// Export Sacraspot to the root
	root.sacraspot = sacraspot;

	/**
	 * PUBLIC FUNCTION DEFINITIONS
	 * 
	*/
	
	/**
	 * @namespace sacraspot.init
	 *
	 * @argument {void}
	 * @returns {void}
	 *
	 * Sets up sacraspot.  Initializes the google.maps connector, the UI,
	 * and the adapter. This function should be called once the DOM is ready.
	 * (e.g. inside of a $(document).ready function, if you are using jQuery.)
	 *
	 * NOTE: This is a destructive operation.
	 * Once this function is run, every page UI element ID will be replaced
	 * with the actual DOM element.
	 * (e.g.
	 * 	sacraspot.ui.map_container = "my_map_container";
	 * 	sacraspot.init();
	 * 	sacraspot.ui.map_container; // now is <div id="my_map_container"></div>
	 * )
	 * 
	 * @see sacraspot.adapter
	 * @see sacraspot.gm
	 * @see sacraspot.ui
	 * 
	 * 
	*/
	function init() {
		_log("Initializing system ...");

		var $ = sacraspot.adapter = typeof sacraspot.adapter === "function" ?
												sacraspot.adapter :
												function(id) { return document.getElementById(id); };

		// Setup the page UI elements
		sui.map_container = map_container = $( sui.map_container || "map_container" );
		sui.main_form = main_form = $( sui.main_form || "search_sacraments" );
		sui.search_bar = search_bar = $( sui.search_bar || "Search" );
		sui.direction_panel = direction_panel = $( sui.direction_panel || "Directions" );
		
		// Instantiate all of the necessary google.maps objects.
		gm.geocoder = new google.maps.Geocoder();
		gm.router = new google.maps.DirectionsService();
		gm.renderer = new google.maps.DirectionsRenderer();
		
		var latlng = new google.maps.LatLng(42.75408, -75.15234);
		var myOptions = {
				zoom: 10,
				center: latlng,
				mapTypeId: google.maps.MapTypeId.ROADMAP
		};
		gm.map = new google.maps.Map(map_container, myOptions);
		gm.renderer.setMap(gm.map);
		gm.renderer.setPanel(direction_panel);

		// Clear and repopulate the search bar
		search_bar = jQuery(search_bar); // FIXME -- Uncouple from jQuery
		search_bar_value = search_bar.val();
		search_bar.bind("focus blur", function() {
			if (this.value === search_bar_value) {
				this.value = "";
			} else if (this.value === "") {
				this.value = search_bar_value;
			}
		});
		search_bar.parents().filter("form").submit(function(e) {
			e.preventDefault();
			sacraspot.search(this);
		});
		sacraspot.search(sui.main_form);
	}

	/**
	 * @namespace sacraspot.no_conflict
	 * @alias sacraspot.noConflict
	 *
	 * @argument {void}
	 * @returns {object} sacraspot.
	 *
	 * Returns the root.sacraspot object to its pre-sacraspot-loaded state.
	 * Highly unlikely that anyone else is using this namespace, but just in case.
	 *
	*/
	function no_conflict() {
		root.sacraspot = old_sacraspot;
		return this;
	}

	/**
	 * @namespace sacraspot.search
	 *
	 * Geocodes start address if requested.
	 * Searches for nearest 25 sacraments and sets up
	 * `sacraspot.data.parishes` and `sacraspot.data.sacraments` objects.
	 * 
	 * @argument form_el {DOM object}
	 * @returns {void}
	 *
	 * @see _setup_parishes
	 *
	*/
	// FIXME: Uncouple from jQuery.
	function search(form_el) {
		// Execut search functions
		_log("Beginning search request...");
		requested_address = jQuery(search_bar).val() + ""; //FIXME: Uncouple from jQuery
		requested_address = (requested_address !== search_bar_value) ? requested_address : "Manassas, VA";
		gm.geocoder.geocode({address: requested_address }, _search_success);
		jQuery.ajax({
			dataType: "json",
			url: "/query-sacraments",
			success: _setup_parishes,
		error: function(xhr_request, textStatus, errorThrown) {
			_log("An error occurred");
			_log(xhr_request);
			_log(textStatus);
			_log(errorThrown);
		},
		complete: function(xhr_obj, textStatus) {
			_log("Search request complete.");
			_log(xhr_obj);
			_log(textStatus); }
		});
		_display();
	}

	// Setup templates to use for rendering content.
	// For more on the templating system
	// see http://documentcloud.github.com/underscore/#template

	// Use Jinja2-style templates
	_.templateSettings = sui.templates.settings = {
		start: "{%", // of javascript code
		end: "%}",  // of javascript code
		interpolate: /\{\{(.+?)\}\}/g // any word inside of {{}} as a variable.
	};

	/**
	 * @argument tmplt_func_key {string} A valid key in sacraspot.ui.templates
	 * @argument prefilled_args {object} An object containing default
	 * arguments for the provided template.
	 * @see _merge
	 *
	 * @returns {function} <_.template>
	 * 
	*/
	function subclass(tmplt_func_key, prefilled_args) {
		return function(obj) {
			data = _merge(obj, prefilled_args, true);
			return this[tmplt_func_key](data);
		};
	}

	/**
	 * @namespace sacraspot.ui
	 *
	 * The javascript templates for rendering elements.
	 * @argument params {object} A dictionary with keys corresponding to
	 * the interpolated variables in the template.
	 *
	 * @returns {string} Returns the rendered string.
	 * 
	*/
	sui.templates = {
		/**
		 * @namespace sacraspot.ui.templates
		 *
		 * @argument {h: header_number (1-6), title: text for headline}
		*/
		h: _.template("<h{{h}}>{{title}}</h{{h}}>"),
		/**
		 * @namespace sacraspot.ui.templates
		 *
		 * @argument {
		 * 	items: array or object,
		 * 	formater: function to format each entry in items.
		 * }
		*/
		ul: _.template("<ul>{% _.each(items, function(list_item){ %}<li>{{ formater(list_item) }}</li>{% }); %}</ul>"),
		/**
		 * @namespace sacraspot.ui.templates
		 *
		 * @argument {
		 * 	name: name and id of the element,
		 * 	dflt: optional: two item array [value, display_value] for default option,
		 * 	values: array of values for options,
		 * 	names: array of names for options
		 * }
		*/
		dd: _.template('<select name="{{name}}" id="{{name}}">\n{% if (_.isArray(dflt) && dflt.length > 1) { %}\t<option value="{{dflt[0]}}">{{dflt[1]}}</option>\n{% } _.each(_.zip(values,names), function(item){ %}\t<option value="{{item[0]}}">{{item[1]}}</option>\n{% }); %}</select>'),
		/**
		 * @namespace sacraspot.ui.templates
		 *
		 * @argument {
		 * 	type: a valid input type (submit, button, text, etc.),
		 * 	name: name of input
		 * 	id: HTML ID of input
		 * 	value: value for input
		 * 	add_attrs: optional: {mixed} String, or dictionary.
		 * 	If a string is provided, it will be added to the end of the the input markup.
		 * 	If an object is provided each key: value pair will be added as an attribute
		 * 	in the format: key="value"
		 * }
		*/
		input: _.template('<input type="{{type}}" name="{{name}}" id="{{id}}" value="{{value}}"{% if (typeof add_attrs !== "undefined") { if (_.isString(add_attrs)) { %} {{add_attrs}}{% } else if ( ! _.isEmpty(_.keys(add_attrs)) && ! _.isNumber(_.keys(add_attrs)[0])) { _.each(add_attrs, function(value, key) { %} {{key}}="{{value}}" {% }); }} %} />'),
		/**
		 * @namespace sacraspot.ui.templates
		 *
		 * @argument Same as input, sans type
		 * @see sacraspot.ui.templates.input
		*/
		button: subclass("input",{type: 'button'}),
		/**
		 * @namespace sacraspot.ui.templates
		 *
		 * @argument Same as input, sans type
		 * @see sacraspot.ui.templates.input
		*/
		hidden: subclass("input",{type: 'hidden'}),
		/**
		 * @namespace sacraspot.ui.templates
		 *
		 * @argument Same as input, sans type
		 * @see sacraspot.ui.templates.input
		*/
		text: subclass("input", {type: 'text'}),
		/**
		 * @namespace sacraspot.ui.templates
		 *
		 * @argument {
		 * 	name: The internal name of the element.
		 * 	id: The HTML ID of the element
		 * 	value: The HTML value of hte element
		 * 	display_name: The name to display in the label.
		 * }
		 * @see sacraspot.ui.templates.input
		*/
		checkbox: _.template('<input type="checkbox" name="{{name}}" id="{{id}}" value="{{value}}" /><label for="{{id}}">{{display_name}}</label>'),
		/**
		 * @namespace sacraspot.ui.templates
		 *
		 * @argument Same as checkbox
		 * @see sacraspot.ui.templates.checkbox
		*/
		radio: _.template('<input type="radio" name="{{name}}" id="{{id}}" value="{{value}}" /><label for="{{id}}">{{display_name}}</label>'),
		/**
		 * @namespace sacraspot.ui.templates
		 *
		 * Swallows all data passed to it and returns an empty string.
		 * @argument {void}
		 * 
		*/
		blank: _.template("")
	};

	/*
	 *************************************
	 *  PRIVATE UTILITY FUNCTIONS
	 *************************************
	*/

	/**
	 * @private
	 * 
	 * @argument {object} <sacraspot.Parish> (Eventually).
	 * 
	 * @returns {string} <h4>Parish Full Name</h4>
	 * 			<h5>Sacrament Name</h5>
	 * 				<ul><li>_render_sacrament</li>...etc.
	 * 			<h5>Sacrament Name</h5>
	 * 				<ul><li>_render_sacrament</li>...etc.
	 *
	 * @side_effects Creates the parish's infoWindow HTML if it has not been created.
	 * Adds a click handler to the global event pool to route from the parish to
	 * the user's starting location.
	 * 
	 * @see _render_sacrament
	 * 
	*/
	function _render_infoWindow(parish) {
		if (parish.html === null) {
			var _html = [];
			_html.push( sui.templates.h({ h:4, title: parish.address.fullname }) );
			_.each(parish.sacraments, function(sacrament_list, sacrament_name) {
				_html.push( sui.templates.h({ h:5, title: sacrament_name }) );
				_html.push(
						   sui.templates.ul({
											items: sacrament_list,
											formater: _render_sacrament
											})
						   );
			});
			var uniqueID = _.uniqueId("ParishDirectionLink_");
			_html.push( '<p><a href="#" id="' + uniqueID + '">Get directions</a></p>' );
			$("#"+uniqueID).live("click",function(e) {
				_log("In live handler"); _log(parish);
				sacraspot.gm.router.route({
					origin: sacraspot.gm.mkr_mgr.start_loc.getPosition(),
					destination: parish.lat_long,
					travelMode: google.maps.DirectionsTravelMode.DRIVING
				}, function(results, status) {
					_log("Direction Results received"); _log(status);
					sacraspot.gm.renderer.setDirections(results);
				});
			});
			parish.html = _html.join("");
		}
		return parish.html;
	}

	/**
	 * @private
	 * 
	 * @argument sac_name {string} Valid sacraspot.data.sacraments key
	 * (i.e. "Mass")
	 * @argument end {integer} The maximum number of items to return
	 * (Defaults to the length of the sacrament list).
	 * @argument start {integer} The offset to use from 0.  (Defaults to 0)
	 * 
	 * @returns {string} <h5>Sacrament Name</h5><ul><li>_render_sacrament</li>...etc.
	 * 
	 * @see _render_sacrament
	 * 
	*/
	function _render_sacrament_list(sac_name, end, start) {
		var _html = [];
		var sacrament_list = sacraspot.data.sacraments[sac_name];
		if (sacrament_list && sacrament_list.length > 0) {
			end = _.isNumber(end) ? end : sacrament_list.length;
			start = _.isNumber(start) ? start : 0;
			_html.push( sui.templates.h({ h:5, title:sac_name }) );
			sacrament_list = sacrament_list.slice(start,end);
			_html.push (
						sui.templates.ul({
							items: sacrament_list,
							formater: _render_sacrament })
						);
		}
		return _html.join("");
	}

	/**
	 * @private
	 * 
	 * @argument sacraspot.data.sacraments.sacramentType[n]
	 * (sacraspot.Sacrament, ideally)
	 * @returns {string} time[: language],[: details]
	 * 
	*/
	function _render_sacrament(sacrament) {
		_html = [];
		_html.push(sacrament.time);
		sacrament.language ?
					_html.push( " : ", sacrament.language ) :
					_html.push( "" );
		sacrament.details ?
					_html.push( " : ", sacrament.details ) :
					_html.push( "" );
		return _html.join("");
	}

	/**
	 * @private
	 * 
	 * Called after a successful call to Google's servers to geocode an address
	 * (From the search_bar)
	 *
	 * @side_effects Creates a marker for start_loc if one does not exist.
	 * Otherwise, updates start_loc's position on the map
	 * and moves the map to be centered on the new location.
	 * 
	 * @returns {void}
	 * 
	*/
	function _search_success(results, geo_status) {
		if (gm.mkr_mgr.start_loc === null) {
			gm.mkr_mgr.start_loc = new google.maps.Marker({
				map: gm.map,
				position: results[0].geometry.location,
				title: gm.GLTD[results[0].geometry.location_type]
			});
		} else {
			gm.mkr_mgr.start_loc.setOptions({
				position: results[0].geometry.location,
				title: gm.GLTD[results[0].geometry.location_type]
			});	
		}
		var map_bounds = results[0].geometry.viewport;
		gm.map.panToBounds(map_bounds);
		gm.map.fitBounds(map_bounds);
		_log(gm.GSD[geo_status]);
	}

	/**
	 * Setup the data returned from query-sacraments
	 * to be a series of parishes objects
	 * with the following format:
	 *
	 * [parish]
	 * 		[address]
	 * 			[fullname]
	 * 			[city]
	 * 			[state]
	 * 		[loc]
	 * 			[latitude]
	 * 			[longitude]
	 * 		[lat_long] <google.maps.LatLong>
	 * 		[info_window] <google.maps.InfoWindow>
	 * 		[marker] <google.maps.Marker>
	 * 		[distance]
	 * 		[sacraments]
	 * 			[sacrament type #1]
	 * 				[sacrament #1] -> <sacraspot.data.sacraments.sacramentType[n]>
	 * 				[sacrament #2]
	 * 			[sacrament type #2]
	 * 			... etc.
	 *
	 * and sacrament objects with the following format:
	 *
	 * [sacrament]
	 * 		[time]
	 *		[details]
	 *		[language]
	 *		[parish] -> <sacraspot.data.parishes.parishFullName>
	 *		[kind]
	 *
	 * @side_effect Populates sacraspot.data.parishes
	 * and sacraspot.data.sacraments.
	 * Sets up and displays the Map (google.maps.Map)
	 * 
	 * @argument data_array {object} (Parsed JSON)
	 * @returns boolean Was there any data to work with?
	 *
	*/
	function _setup_parishes(data_array) {
		sdata.raw_data = data_array;
		var latitudes = [];
		var longitudes = [];

		// Munge the data, until it's suitable for working with in gMaps.
		_.each(data_array, function(parish) {
			// If sacrapsot.data.parishes doesn't have this parish yet
			// set it up.  FIXME: This relies on the fullname being unique.
			if ( ! sdata.parishes[parish.fullname] ) {
				// Setup the parish inside of parishes and alias it
				// to parish_data for ease of access.
				sdata.parishes[parish.fullname] = {};
				var parish_data = sdata.parishes[parish.fullname];
				
				var _latitude = parish.latitude;
				var _longitude = parish.longitude;
				parish_data.lat_long = new google.maps.LatLng(_latitude, _longitude);
				parish_data.loc = [_latitude, _longitude];
				// These will be used to determine the map bounds later.
				latitudes.push(_latitude);
				longitudes.push(_longitude);
				
				parish_data.address = {};
				parish_data.address.city = parish.city;
				parish_data.address.state = parish.state;
				parish_data.address.fullname = parish.fullname;
				parish_data.distance = parish.distance;
				parish_data.sacraments = {};
				
				// Each parish stores its google maps marker and info window (ballon)
				parish_data.info_window = null;
				parish_data.marker = new google.maps.Marker({
					title: parish_data.fullname,
					position: parish_data.lat_long,
					map: gm.map
	
				});
				
				// Marker Manager provides quick access to the markers themselves
				gm.mkr_mgr.markers.push(parish_data.marker);
				_log("Adding event listener");
				google.maps.event.addListener(parish_data.marker, "click", function() {
					if (parish_data.info_window === null) {
						// This will store the rendered html from `_render_infoWindow`.
						parish_data.html = null;
						parish_data.info_window = new google.maps.InfoWindow({
								content: _render_infoWindow(parish_data)
							});
					}
					
					// Close the current info window, if one is open.
					if (gm.mkr_mgr.active_window &&
						gm.mkr_mgr.active_window !== parish_data.info_window) {
						gm.mkr_mgr.active_window.close();
					}
					parish_data.info_window.open(gm.map, parish_data.marker);
					gm.mkr_mgr.active_window = parish_data.info_window;
				});
			}

			// Otherwise, if the parish is already set up,
			// we just need to add the current sacrament.
			var parish_sacraments = sacraspot.data.parishes[parish.fullname].sacraments;
			
			if (typeof parish_sacraments[parish.kind] !== typeof []) {
				parish_sacraments[parish.kind] = [];
			}
			if (typeof sacraspot.data.sacraments[parish.kind] !== typeof []) {
				sacraspot.data.sacraments[parish.kind] = [];
			}
			
			this_sacrament = {
					time: parish.time,
					details: parish.details,
					language: parish.language,
					parish: parish_data,
					kind: parish.kind
			};
			
			parish_sacraments[parish.kind].push(this_sacrament);
			sacraspot.data.sacraments[parish.kind].push(this_sacrament);
			
		});
		
		// Now that we have finished re-arranging the data, let's use it
		
		if (data_array.length > 0) {
			// Set the bounds for the map
			latitudes.sort();
			longitudes.sort();
			// (Google wants bounds for the south-west and north-east corners of the map. 
			var upper_bounds = new google.maps.LatLng(latitudes[latitudes.length-1], longitudes[0]);
			var lower_bounds = new google.maps.LatLng(latitudes[0], longitudes[longitudes.length-1]);
			var map_bounds = new google.maps.LatLngBounds(lower_bounds, upper_bounds);
			gm.map.panToBounds(map_bounds);
			gm.map.fitBounds(map_bounds);
			return true;
		} else {
			return false;
		}
	}

	/**
	 * @private
	 * 
	 * Run at the completion of sacraspot.search
	 * Animates the header and triggers the google.maps resize event.
	 *
	 * @argument {void}
	 * @returns {void}
	 * 
	*/
	// FIXME: Uncouple from jQuery
	// FIXME: Seperate concerns (Animation and necessary google map interactions
	//			should not be in the same function.)
	function _display() {
		main_header = jQuery("#main_header");
		main_header.animate({ paddingTop: "1em" }, 350, "linear", function(){
			$("#Results").fadeIn("slow");
			jQuery(main_form).children().
				filter("div[id]*=filters").
				each(function() { jQuery(this).fadeIn("slow"); });
			google.maps.event.trigger(gm.map, 'resize');
			});
	}

	/**
	 * Logs msg to the console if available or alerts them if desired.
	 * 
	 * @argument msg {mixed} The string or javascript object to log.
	 * @returns boolean Success
	 *
	*/
	function _log(msg) {
		//msg = "Sacraspot: " + msg;
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

	/**
	 * Add properties of sender to receiver
	 * NOTE: This operation has side effects.
	 * Adds properties in place! (i.e. Does not clone receiver.)
	 *
	 * @argument receiver {object} optional Object receiving new attributes
	 * @argument sender {object} optional Object from which attributes will be copied.
	 * @argument overwrite {boolean} If receiver already has an attribute found in sender
	 * 	should receiver's attribute be overwritten with the value of sender's?
	 * 	Defaults to false.
	 * 	
	 * @returns receiver
	*/
	// FIXME: May want to allow multiple merge objects, a la underscore.
	function _merge(receiver, sender, overwrite) {
		// overwrite defaults to false. Not using _.isBoolean to avoid coupling.
		overwrite = (arguments[arguments.length-1] === true ||
							arguments[arguments.length-1] === false) ?
							arguments[arguments.length-1] :
							false;
		receiver = receiver || {};
		sender = sender || {};
		for (key in sender) {
			if (overwrite || ! Object.prototype.hasOwnProperty.call(receiver,key)) {
				delete receiver[key];
				receiver[key] = sender[key];
			}
		}
		return receiver;
	}

})();