/*
Core JavaScript Library
$Id: core.js 73 2006-07-17 17:18:47Z mischa $

Copyright (c) 2005, Six Apart, Ltd.
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are
met:

    * Redistributions of source code must retain the above copyright
notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
copyright notice, this list of conditions and the following disclaimer
in the documentation and/or other materials provided with the
distribution.

    * Neither the name of "Six Apart" nor the names of its
contributors may be used to endorse or promote products derived from
this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

*/

/* stubs */

log = function() {};
log.error = log.warn = log.debug = log;


/* utility functions */

defined = function( x ) {
    return x === undefined ? false : true;
}


/**
 * Utility method.
 * @param x <code>any</code> Any JavaScript value, including <code>undefined</code>.
 * @return boolean <code>true</code> if the value is not <code>null</code> and is not <code>undefined</code>.
 */
exists = function( x ) {
   return (x === undefined || x === null) ? false : true;
}


finite = function( x ) {
    return isFinite( x ) ? x : 0;
}


finiteInt = function( x, base ) {
    return finite( parseInt( x, base ) );
}


finiteFloat = function( x ) {
    return finite( parseFloat( x ) );
}


max = function() {
    var a = arguments;
    var n = a[ 0 ];
    for( var i = 1; i < a.length; i++ )
        if( a[ i ] > n )
            n = a[ i ];
    return n;
}


min = function() {
    var a = arguments;
    var n = a[ 0 ];
    for( var i = 1; i < a.length; i++ )
        if( a[ i ] < n )
            n = a[ i ];
    return n;
}


/* try block */  
 
Try = {
    these: function() {
        for( var i = 0; i < arguments.length; i++ ) {
            try {
                return arguments[ i ]();
            } catch( e ) {}
        }
        return undefined;
    }
}


/* unique id generator */

Unique = {
    length: 0,
    
    id: function() {
        return ++this.length;
    }
}


/* event methods */

if( !defined( window.Event ) )
    Event = {};


Event.stop = function( event ) {
    event = event || this;
    if( event === Event )
        event = window.event;

    // w3c
    if( event.preventDefault )
        event.preventDefault();
    if( event.stopPropagation )
        event.stopPropagation();

    // ie
    try {
        event.cancelBubble = true;
        event.returnValue = false;
    } catch( e ) {}

    return false;
}


Event.prep = function( event ) {
    event = event || window.event;
    if( !defined( event.stop ) )
        event.stop = this.stop;
    if( !defined( event.target ) )
        event.target = event.srcElement;
    if( !defined( event.relatedTarget ) ) 
        event.relatedTarget = event.toElement;
    return event;
}


try { Event.prototype.stop = Event.stop; }
catch( e ) {}


/* object extensions */

Function.stub = function() {};


if( !Object.prototype.hasOwnProperty ) {
    Object.prototype.hasOwnProperty = function( p ) {
        if( !(p in this) )
            return false;
        try {
            var pr = this.constructor.prototype;
            while( pr ) {
                if( pr[ p ] === this[ p ] )
                    return false;
                if( pr === pr.constructor.prototype )
                    break;
                pr = pr.constructor.prototype;
            }
        } catch( e ) {}
        return true;
    }
}


Object.prototype.extend = function() {
    var a = arguments;
    for( var i = 0; i < a.length; i++ ) {
        var o = a[ i ];
        for( var p in o ) {
            try {
                if( !this[ p ] &&
                    (!o.hasOwnProperty || o.hasOwnProperty( p )) )
                    this[ p ] = o[ p ];
            } catch( e ) {}
        }
    }
    return this;
}


Object.prototype.override = function() {
    var a = arguments;
    for( var i = 0; i < a.length; i++ ) {
        var o = a[ i ];
        for( var p in o ) {
            try {
                if( !o.hasOwnProperty || o.hasOwnProperty( p ) )
                    this[ p ] = o[ p ];
            } catch( e ) {}
        }
    }
    return this;
}


Object.prototype.extend( {
    init: Function.stub,
    destroy: Function.stub
} );



/* function extensions */

Function.prototype.extend( {
    bind: function( object ) {
        var method = this;
        return function() {
            return method.apply( object, arguments );
        };
    },
    
    
    bindEventListener: function( object ) {
        var method = this; // Use double closure to work around IE 6 memory leak.
        return function( event ) {
            try {
                event = Event.prep( event );
            } catch( e ) {}
            return method.call( object, event );
        };
    }
} );


/* class helpers */

indirectObjects = [];


Class = function( superClass ) {

    // Set the constructor:
    var constructor = function() {
        if( arguments.length )
            this.init.apply( this, arguments );
    };    
    //   -- Accomplish static-inheritance:
    constructor.override( Class );  // inherit static methods from Class
    superClass = superClass || Object; 
    constructor.override( superClass ); // inherit static methods from the superClass 
    constructor.superClass = superClass.prototype;
    
    // Set the constructor's prototype (accomplish object-inheritance):
    constructor.prototype = new superClass();
    constructor.prototype.constructor = constructor; // rev. 0.7    
    //   -- extend prototype with Class instance methods
    constructor.prototype.extend( Class.prototype );    
    //   -- override prototype with interface methods
    for( var i = 1; i < arguments.length; i++ )
        constructor.prototype.override( arguments[ i ] );
    
    return constructor;
}


Class.extend( {
    initSingleton: function() {
        if( this.singleton )
            return this.singleton;
        this.singleton = this.singletonConstructor
            ? new this.singletonConstructor()
            : new this();
        this.singleton.init.apply( this.singleton, arguments );
        return this.singleton;
    }
} );


Class.prototype = {
    destroy: function() {
        try {
            if( this.indirectIndex )
                indirectObjects[ this.indirectIndex ] = undefined;
            delete this.indirectIndex;
        } catch( e ) {}
        
        for( var property in this ) {
            try {
                if( this.hasOwnProperty( property ) )
                    delete this[ property ];
            } catch( e ) {}
        }
    },
    
    
    getBoundMethod: function( methodName ) {
        return this[ name ].bind( this );
    },
    
    
    getEventListener: function( methodName ) {
        return this[ methodName ].bindEventListener( this );
    },
    
    
    getIndirectIndex: function() {
        if( !defined( this.indirectIndex ) ) {
            this.indirectIndex = indirectObjects.length;
            indirectObjects.push( this );
        }
        return this.indirectIndex;
    },
    
    
    getIndirectMethod: function( methodName ) {
        if( !this.indirectMethods )
            this.indirectMethods = {};
        var method = this[ methodName ];
        if( typeof method != "function" )
            return undefined;
        var indirectIndex = this.getIndirectIndex();
        if( !this.indirectMethods[ methodName ] ) {
            this.indirectMethods[ methodName ] = new Function(
                "var o = indirectObjects[" + indirectIndex + "];" +
                "return o." + methodName + ".apply( o, arguments );"
            );
        }
        return this.indirectMethods[ methodName ];
    },
    
    
    getIndirectEventListener: function( methodName ) {
        if( !this.indirectEventListeners )
            this.indirectEventListeners = {};
        var method = this[ methodName ];
        if( typeof method != "function" )
            return undefined;
        var indirectIndex = this.getIndirectIndex();
        if( !this.indirectEventListeners[ methodName ] ) {
            this.indirectEventListeners[ methodName ] = new Function( "event",
                "try { event = Event.prep( event ); } catch( e ) {}" +
                "var o = indirectObjects[" + indirectIndex + "];" +
                "return o." + methodName + ".call( o, event );"
            );
        }
        return this.indirectEventListeners[ methodName ];
    }
}


/* string extensions */

String.extend( {
    escapeJSChar: function( c ) {
        // try simple escaping
        switch( c ) {
            case "\\": return "\\\\";
            case "\"": return "\\\"";
            case "'":  return "\\'";
            case "\b": return "\\b";
            case "\f": return "\\f";
            case "\n": return "\\n";
            case "\r": return "\\r";
            case "\t": return "\\t";
        }
        
        // return raw bytes now ... should be UTF-8
        if( c >= " " )
            return c;
        
        // try \uXXXX escaping, but shouldn't make it for case 1, 2
        c = c.charCodeAt( 0 ).toString( 16 );
        switch( c.length ) {
            case 1: return "\\u000" + c;
            case 2: return "\\u00" + c;
            case 3: return "\\u0" + c;
            case 4: return "\\u" + c;
        }
        
        // should never make it here
        return "";
    },
    
    
    encodeEntity: function( c ) {
        switch( c ) {
            case "<": return "&lt;";
            case ">": return "&gt;";
            case "&": return "&amp;";
            case '"': return "&quot;";
            case "'": return "&apos;";
        }
        return c;
    },


    decodeEntity: function( c ) {
        switch( c ) {
            case "amp": return "&";
            case "quot": return '"';
            case "gt": return ">";
            case "lt": return "<";
        }
        var m = c.match( /^#(\d+)$/ );
        if( m && defined( m[ 1 ] ) )
            return String.fromCharCode( m[ 1 ] );
        m = c.match( /^#x([0-9a-f]+)$/i );
        if(  m && defined( m[ 1 ] ) )
            return String.fromCharCode( parseInt( hex, m[ 1 ] ) );
        return c;
    }
} );


String.prototype.extend( {
    escapeJS: function() {
        return this.replace( /([^ -!#-\[\]-~])/g, function( m, c ) { return String.escapeJSChar( c ); } )
    },
    
    
    escapeJS2: function() {
        return this.replace( /([\u0000-\u0031'"\\])/g, function( m, c ) { return String.escapeJSChar( c ); } )
    },
    
    
    escapeJS3: function() {
        return this.replace( /[\u0000-\u0031'"\\]/g, function( m ) { return String.escapeJSChar( m ); } )
    },
    
    
    escapeJS4: function() {
        return this.replace( /./g, function( m ) { return String.escapeJSChar( m ); } )
    },
    
    
    encodeHTML: function() {
        return this.replace( /([<>&"])/g, function( m, c ) { return String.encodeEntity( c ) } );
    },


    decodeHTML: function() {
        return this.replace( /&(.*?);/g, function( m, c ) { return String.decodeEntity( c ) } );
    },
    
    
    cssToJS: function() {
        return this.replace( /-([a-z])/g, function( m, c ) { return c.toUpperCase() } );
    },
    
    
    jsToCSS: function() {
        return this.replace( /([A-Z])/g, function( m, c ) { return "-" + c.toLowerCase() } );
    },
    
    
    firstToLowerCase: function() {
        return this.replace( /^(.)/, function( m, c ) { return c.toLowerCase() } );
    },
    
        
    rgbToHex: function() {
        var c = this.match( /(\d+)\D+(\d+)\D+(\d+)/ );
        if( !c )
            return undefined;
        return "#" +
            finiteInt( c[ 1 ] ).toString( 16 ).pad( 2, "0" ) +
            finiteInt( c[ 2 ] ).toString( 16 ).pad( 2, "0" ) +
            finiteInt( c[ 3 ] ).toString( 16 ).pad( 2, "0" );
    },
    
    
    pad: function( length, padChar ) {
        var padding = length - this.length;
        if( padding <= 0 )
            return this;
        if( !defined( padChar ) )
            padChar = " ";
        var out = [];
        for( var i = 0; i < padding; i++ )
            out.push( padChar );
        out.push( this );
        return out.join( "" );
    },


    trim: function() {
        return this.replace( /^\s+|\s+$/g, "" );
    }

} );


/* extend array object */

Array.extend( { 
    fromPseudo: function ( args ) {
        var out = [];
        for ( var i = 0; i < args.length; i++ )
            out.push( args[ i ] );
        return out;
    }
});


/* extend array object */

Array.prototype.extend( {
    copy: function() {
        var out = [];
        for( var i = 0; i < this.length; i++ )
            out[ i ] = this[ i ];
        return out;
    },


    first: function( callback, object ) {
        var length = this.length;
        for( var i = 0; i < length; i++ ) {
            var result = object
                ? callback.call( object, this[ i ], i, this )
                : callback( this[ i ], i, this );
            if( result )
                return this[ i ];
        }
        return null;
    },


    fitIndex: function( fromIndex, defaultIndex ) {
        if( !defined( fromIndex ) || fromIndex == null )
            fromIndex = defaultIndex;
        else if( fromIndex < 0 ) {
            fromIndex = this.length + fromIndex;
            if( fromIndex < 0 )
                fromIndex = 0;
        } else if( fromIndex >= this.length )
            fromIndex = this.length - 1;
        return fromIndex;
    },


    scramble: function() {
        for( var i = 0; i < this.length; i++ ) {
            var j = Math.floor( Math.random() * this.length );
            var temp = this[ i ];
            this[ i ] = this[ j ];
            this[ j ] = temp;
        }
    },
    
    
    add: function() {
        var a = arguments;
        for( var i = 0; i < a.length; i++ ) {
            var index = this.indexOf( a[ i ] );
            if( index < 0 ) 
                this.push( arguments[ i ] );
        }
        return this.length;
    },
        
    
    remove: function() {
        var a = arguments;
        for( var i = 0; i < a.length; i++ ) {
            var j = this.indexOf( a[ i ] );
            if( j >= 0 )
                this.splice( j, 1 );
        }
        return this.length;
    },


    /* javascript 1.5 array methods */
    /* http://developer-test.mozilla.org/en/docs/Core_JavaScript_1.5_Reference:Objects:Array#Methods */

    every: function( callback, object ) {
        var length = this.length;
        for( var i = 0; i < length; i++ ) {
            var result = object
                ? callback.call( object, this[ i ], i, this )
                : callback( this[ i ], i, this );
            if( !result )
                return false;
        }
        return true;
    },


    filter: function( callback, object ) {
        var out = [];
        var length = this.length;
        for( var i = 0; i < length; i++ ) {
            var result = object
                ? callback.call( object, this[ i ], i, this )
                : callback( this[ i ], i, this );
            if( result )
                out.push( this[ i ] );
        }
        return out;
    },
    
    
    forEach: function( callback, object ) {
        var length = this.length;
        for( var i = 0; i < length; i++ ) {
            object
                ? callback.call( object, this[ i ], i, this )
                : callback( this[ i ], i, this );
        }
    },
    
    
    indexOf: function( value, fromIndex ) {
        fromIndex = this.fitIndex( fromIndex, 0 );
        for( var i = 0; i < this.length; i++ ) {
            if( this[ i ] === value )
                return i; 
        }
        return -1;
    },


    lastIndexOf: function( value, fromIndex ) {
        fromIndex = this.fitIndex( fromIndex, this.length - 1 );
        for( var i = fromIndex; i >= 0; i-- ) {
            if( this[ i ] == value )
                return i;
        }
        return -1;
    },


    some: function( callback, object ) {
        var length = this.length;
        for( var i = 0; i < length; i++ ) {
            var result = object
                ? callback.call( object, this[ i ], i, this )
                : callback( this[ i ], i, this );
            if( result )
                return true;
        }
        return false;
    },


    /* javascript 1.2 array methods */

    concat: function() {
        var a = arguments;
        var out = this.copy();
        for( i = 0; i < a.length; i++ ) {
            var b = a[ i ];
            for( j = 0; j < b.length; j++ )
                out.push( b[ j ] );
        }
        return out;
    },
    

    push: function() {
        var a = arguments;
        for( var i = 0; i < a.length; i++ )
            this[ this.length ] = a[ i ];
        return this.length;     
    },


    pop: function() {
        if( this.length == 0 )
            return undefined;
        var out = this[ this.length - 1 ];
        this.length--;
        return out;
    },
    
    
    unshift: function() {
        var a = arguments;
        for( var i = 0; i < a.length; i++ ) {
            this[ i + a.length ] = this[ i ];
            this[ i ] = a[ i ];
        }
        return this.length;     
    },
    
    
    shift: function() {
        if( this.length == 0 )
            return undefined;
        var out = this[ 0 ];
        for( var i = 1; i < this.length; i++ )
            this[ i - 1 ] = this[ i ];
        this.length--;
        return out;
    }
} );


/* date extensions */

Date.extend( {
    /*  iso 8601 date format parser
        this was fun to write...
        thanks to: http://www.cl.cam.ac.uk/~mgk25/iso-time.html */

    matchISOString: new RegExp(
        "^([0-9]{4})" +                                                     // year
        "(?:-(?=0[1-9]|1[0-2])|$)(..)?" +                                   // month
        "(?:-(?=0[1-9]|[12][0-9]|3[01])|$)([0-9]{2})?" +                    // day of the month
        "(?:T(?=[01][0-9]|2[0-4])|$)T?([0-9]{2})?" +                        // hours
        "(?::(?=[0-5][0-9])|\\+|-|Z|$)([0-9]{2})?" +                        // minutes
        "(?::(?=[0-5][0-9]|60$|60[+|-|Z]|60.0+)|\\+|-|Z|$):?([0-9]{2})?" +  // seconds
        "(\.[0-9]+)?" +                                                     // fractional seconds
        "(Z|\\+[01][0-9]|\\+2[0-4]|-[01][0-9]|-2[0-4])?" +                  // timezone hours
        ":?([0-5][0-9]|60)?$"                                               // timezone minutes
    ),
    
    
    fromISOString: function( string ) {
        var t = this.matchISOString.exec( string );
        if( !t )
            return undefined;

        var year = finiteInt( t[ 1 ], 10 );
        var month = finiteInt( t[ 2 ], 10 ) - 1;
        var day = finiteInt( t[ 3 ], 10 );
        var hours = finiteInt( t[ 4 ], 10 );
        var minutes = finiteInt( t[ 5 ], 10 );
        var seconds = finiteInt( t[ 6 ], 10 );
        var milliseconds = finiteInt( Math.round( parseFloat( t[ 7 ] ) * 1000 ) );
        var tzHours = finiteInt( t[ 8 ], 10 );
        var tzMinutes = finiteInt( t[ 9 ], 10 );

        var date = new this( 0 );
        if( defined( t[ 8 ] ) ) {
            date.setUTCFullYear( year, month, day );
            date.setUTCHours( hours, minutes, seconds, milliseconds );
            var offset = (tzHours * 60 + tzMinutes) * 60000;
            if( offset )
                date = new this( date - offset );
        } else {
            date.setFullYear( year, month, day );
            date.setHours( hours, minutes, seconds, milliseconds );
        }

        return date;
    }
} );


Date.prototype.extend( {
    getISOTimezoneOffset: function() {
        var offset = -this.getTimezoneOffset();
        var negative = false;
        if( offset < 0 ) {
            negative = true;
            offset *= -1;
        }
        var offsetHours = Math.floor( offset / 60 ).toString().pad( 2, "0" );
        var offsetMinutes = Math.floor( offset % 60 ).toString().pad( 2, "0" );
        return (negative ? "-" : "+") + offsetHours + ":" + offsetMinutes;
    },


    toISODateString: function() {
        var year = this.getFullYear();
        var month = (this.getMonth() + 1).toString().pad( 2, "0" );
        var day = this.getDate().toString().pad( 2, "0" );
        return year + "-" + month + "-" + day;
    },


    toUTCISODateString: function() {
        var year = this.getUTCFullYear();
        var month = (this.getUTCMonth() + 1).toString().pad( 2, "0" );
        var day = this.getUTCDate().toString().pad( 2, "0" );
        return year + "-" + month + "-" + day;
    },


    toISOTimeString: function() {
        var hours = this.getHours().toString().pad( 2, "0" );
        var minutes = this.getMinutes().toString().pad( 2, "0" );
        var seconds = this.getSeconds().toString().pad( 2, "0" );
        var milliseconds = this.getMilliseconds().toString().pad( 3, "0" );
        var timezone = this.getISOTimezoneOffset();
        return hours + ":" + minutes + ":" + seconds + "." + milliseconds + timezone;
    },


    toUTCISOTimeString: function() {
        var hours = this.getUTCHours().toString().pad( 2, "0" );
        var minutes = this.getUTCMinutes().toString().pad( 2, "0" );
        var seconds = this.getUTCSeconds().toString().pad( 2, "0" );
        var milliseconds = this.getUTCMilliseconds().toString().pad( 3, "0" );
        return hours + ":" + minutes + ":" + seconds + "." + milliseconds + "Z";
    },


    toISOString: function() {
        return this.toISODateString() + "T" + this.toISOTimeString();
    },


    toUTCISOString: function() {
        return this.toUTCISODateString() + "T" + this.toUTCISOTimeString();
    }
} );


/* ajax */

if( !defined( window.XMLHttpRequest ) ) {
    window.XMLHttpRequest = function() {
        var types = [
            "Microsoft.XMLHTTP",
            "MSXML2.XMLHTTP.5.0",
            "MSXML2.XMLHTTP.4.0",
            "MSXML2.XMLHTTP.3.0",
            "MSXML2.XMLHTTP"
        ];
        
        for( var i = 0; i < types.length; i++ ) {
            try {
                return new ActiveXObject( types[ i ] );
            } catch( e ) {}
        }
        
        return undefined;
    }
}
/*
DOM Library - Copyright 2005 Six Apart
$Id: dom.js 83 2006-10-14 00:07:24Z mischa $

Copyright (c) 2005, Six Apart, Ltd.
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are
met:

    * Redistributions of source code must retain the above copyright
notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
copyright notice, this list of conditions and the following disclaimer
in the documentation and/or other materials provided with the
distribution.

    * Neither the name of "Six Apart" nor the names of its
contributors may be used to endorse or promote products derived from
this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

*/


/* Node class */

if( !defined( window.Node ) )
    Node = {};

try {
    Node.extend( {
        ELEMENT_NODE: 1,
        ATTRIBUTE_NODE: 2,
        TEXT_NODE: 3,
        CDATA_SECTION_NODE: 4,  
        COMMENT_NODE: 8,    
        DOCUMENT_NODE: 9,
        DOCUMENT_FRAGMENT_NODE: 11
    } );
} catch( e ) {}


/* DOM class */

if( !defined( window.DOM ) )
    DOM = {};


DOM.extend( {
    getElement: function( e ) {
        return (typeof e == "string" || typeof e == "number") ? document.getElementById( e ) : e;
    },


    addEventListener: function( e, eventName, func, useCapture ) {
        try {
            if( e.addEventListener )
                e.addEventListener( eventName, func, useCapture );
            else if( e.attachEvent )
                e.attachEvent( "on" + eventName, func );
            else
                e[ "on" + eventName ] = func;
        } catch( e ) {}
    },


    removeEventListener: function( e, eventName, func, useCapture ) {
        try {
            if( e.removeEventListener )
                e.removeEventListener( eventName, func, useCapture );
            else if( e.detachEvent )
                e.detachEvent( "on" + eventName, func );
            else
                e[ "on" + eventName ] = undefined;
        } catch( e ) {}
    },
    
    
    focus: function( e ) {
        try {
            e = DOM.getElement( e );
            e.focus();
        } catch( e ) {}
    },


    blur: function( e ) {
        try {
            e = DOM.getElement( e );
            e.blur();
        } catch( e ) {}
    },
    

    /* style */
    
    getComputedStyle: function( e ) {
        if( e.currentStyle )
            return e.currentStyle;
        var style = {};
        var owner = DOM.getOwnerDocument( e );
        if( owner && owner.defaultView && owner.defaultView.getComputedStyle ) {            
            try {
                style = owner.defaultView.getComputedStyle( e, null );
            } catch( e ) {}
        }
        return style;
    },


    getStyle: function( e, p ) {
        var s = DOM.getComputedStyle( e );
        return s[ p ];
    },


    // given a window (or defaulting to current window), returns
    // object with .x and .y of client's usable area
    getClientDimensions: function( w ) {
        if( !w )
            w = window;

        var d = {};

        // most browsers
        if( w.innerHeight ) {
            d.x = w.innerWidth;
            d.y = w.innerHeight;
            return d;
        }

        // IE6, strict
        var de = w.document.documentElement;
        if( de && de.clientHeight ) {
            d.x = de.clientWidth;
            d.y = de.clientHeight;
            return d;
        }

        // IE, misc
        if( document.body ) {
            d.x = document.body.clientWidth;
            d.y = document.body.clientHeight;
            return d;
        }
        
        return undefined;
    },


    getDimensions: function( e ) {
        if( !e )
            return undefined;

        var style = DOM.getComputedStyle( e );

        return {
            offsetLeft: e.offsetLeft,
            offsetTop: e.offsetTop,
            offsetWidth: e.offsetWidth,
            offsetHeight: e.offsetHeight,
            clientWidth: e.clientWidth,
            clientHeight: e.clientHeight,
            
            offsetRight: e.offsetLeft + e.offsetWidth,
            offsetBottom: e.offsetTop + e.offsetHeight,
            clientLeft: finiteInt( style.borderLeftWidth ) + finiteInt( style.paddingLeft ),
            clientTop: finiteInt( style.borderTopWidth ) + finiteInt( style.paddingTop ),
            clientRight: e.clientLeft + e.clientWidth,
            clientBottom: e.clientTop + e.clientHeight
        };
    },


    getAbsoluteDimensions: function( e ) {
        var d = DOM.getDimensions( e );
        if( !d )
            return d;
        d.absoluteLeft = d.offsetLeft;
        d.absoluteTop = d.offsetTop;
        d.absoluteRight = d.offsetRight;
        d.absoluteBottom = d.offsetBottom;
        var bork = 0;
        while( e ) {
            try { // IE 6 sometimes gives an unwarranted error ("htmlfile: Unspecified error").
                e = e.offsetParent;
            } catch ( err ) {
                log( "In DOM.getAbsoluteDimensions: " + err.message ); 
                if ( ++bork > 25 )
                    return null;
            }
            if( !e )
                return d;
            d.absoluteLeft += e.offsetLeft;
            d.absoluteTop += e.offsetTop;
            d.absoluteRight += e.offsetLeft;
            d.absoluteBottom += e.offsetTop;
        }
        return d;
    },
    
    
    getIframeAbsoluteDimensions: function( e ) {
        var d = DOM.getAbsoluteDimensions( e );
        if( !d )
            return d;
        var iframe = DOM.getOwnerIframe( e );
        if( !defined( iframe ) )
            return d;
        
        var d2 = DOM.getIframeAbsoluteDimensions( iframe );
        var scroll = DOM.getWindowScroll( iframe.contentWindow );
        var left = d2.absoluteLeft - scroll.left;
        var top = d2.absoluteTop - scroll.top;
        
        d.absoluteLeft += left;
        d.absoluteTop += top;
        d.absoluteRight += left;
        d.absoluteBottom += top;
        
        return d;
    },
    
    
    setLeft: function( e, v ) { e.style.left = finiteInt( v ) + "px"; },
    setTop: function( e, v ) { e.style.top = finiteInt( v ) + "px"; },
    setRight: function( e, v ) { e.style.right = finiteInt( v ) + "px"; },
    setBottom: function( e, v ) { e.style.bottom = finiteInt( v ) + "px"; },
    setWidth: function( e, v ) { e.style.width = max( 0, finiteInt( v ) ) + "px"; },
    setHeight: function( e, v ) { e.style.height = max( 0, finiteInt( v ) ) + "px"; },
    setZIndex: function( e, v ) { e.style.zIndex = finiteInt( v ); },

    
    getWindowScroll: function( w ) {
        var s = {
            left: 0,
            top: 0
        };
        
        var d = w.document;
        
        // ie
        var de = d.documentElement;
        if( de && defined( de.scrollLeft ) ) {
            s.left = de.scrollLeft;
            s.top = de.scrollTop;
        }
        
        // safari
        else if( defined( w.scrollX ) ) {
            s.left = w.scrollX;
            s.top = w.scrollY;
        }
        
        // opera
        else if( d.body && defined( d.body.scrollLeft ) ) {
            s.left = d.body.scrollLeft;
            s.top = d.body.scrollTop;
        }
        
        return s;
    },
    
    
    getAbsoluteCursorPosition: function( event ) {
        event = event || window.event;
        var s = DOM.getWindowScroll( window );
        return {
            x: s.left + event.clientX,
            y: s.top + event.clientY
        };
    },
    
    
    invisibleStyle: {
        display: "block",
        position: "absolute",
        left: 0,
        top: 0,
        width: 0,
        height: 0,
        margin: 0,
        border: 0,
        padding: 0,
        fontSize: "0.1px",
        lineHeight: 0,
        opacity: 0,
        MozOpacity: 0,
        filter: "alpha(opacity=0)"
    },
    
    
    makeInvisible: function( e ) {
        for( var p in this.invisibleStyle ) {
            if( this.invisibleStyle.hasOwnProperty( p ) )
                e.style[ p ] = this.invisibleStyle[ p ];
        }
    },


    /* text and selection related methods */

    mergeTextNodes: function( n ) {
        var c = 0;
        while( n ) {
            if( n.nodeType == Node.TEXT_NODE && n.nextSibling && n.nextSibling.nodeType == Node.TEXT_NODE ) {
                n.nodeValue += n.nextSibling.nodeValue;
                n.parentNode.removeChild( n.nextSibling );
                c++;
            } else {
                if( n.firstChild )
                    c += DOM.mergeTextNodes( n.firstChild );
                n = n.nextSibling;
            }
        }
        return c;
    },
    
    
    selectElement: function( e ) {  
        var d = e.ownerDocument;  
        
        // internet explorer  
        if( d.body.createControlRange ) {  
            var r = d.body.createControlRange();  
            r.addElement( e );  
            r.select();  
        }  
    }, 
    
    
    /* dom methods */
    
    isImmutable: function( n ) {
        try {
            if( n.getAttribute( "contenteditable" ) == "false" )
                return true;
        } catch( e ) {}
        return false;
    },
    
    
    getImmutable: function( n ) {
        var immutable = null;
        while( n ) {
            if( DOM.isImmutable( n ) )
                immutable = n;
            n = n.parentNode;
        }
        return immutable;
    },


    getOwnerDocument: function( n ) {
        if( !n )
            return document;
        if( n.ownerDocument )
            return n.ownerDocument;
        if( n.getElementById )
            return n;
        return document;
    },


    getOwnerWindow: function( n ) {
        if( !n )
            return window;
        if( n.parentWindow )
            return n.parentWindow;
        var doc = DOM.getOwnerDocument( n );
        if( doc && doc.defaultView )
            return doc.defaultView;
        return window;
    },
    
    
    getOwnerIframe: function( n ) {
        if( !n )
            return undefined;
        var nw = DOM.getOwnerWindow( n );
        var nd = DOM.getOwnerDocument( n );
        var pw = nw.parent || nw.parentWindow;
        if( !pw )
            return undefined;
        var parentDocument = pw.document;
        var es = parentDocument.getElementsByTagName( "iframe" );
        for( var i = 0; i < es.length; i++ ) {
            var e = es[ i ];
            try {
                var d = e.contentDocument || e.contentWindow.document;
                if( d === nd )
                    return e;
            }catch(err) {};
        }
        return undefined;
    },


    filterElementsByClassName: function( es, className ) {
        var filtered = [];
        for( var i = 0; i < es.length; i++ ) {
            var e = es[ i ];
            if( DOM.hasClassName( e, className ) )
                filtered[ filtered.length ] = e;
        }
        return filtered;
    },
    
    
    filterElementsByAttribute: function( es, attr ) {
        if( !es )
            return [];
        if( !defined( attr ) || attr == null || attr == "" )
            return es;
        var filtered = [];
        for( var i = 0; i < es.length; i++ ) {
            var element = es[ i ];
            if( !element )
                continue;
            if( element.getAttribute && ( element.getAttribute( attr ) ) )
                filtered[ filtered.length ] = element;
        }
        return filtered;
    },


    filterElementsByTagName: function( es, tagName ) {
        if( tagName == "*" )
            return es;
        var filtered = [];
        tagName = tagName.toLowerCase();
        for( var i = 0; i < es.length; i++ ) {
            var e = es[ i ];
            if( e.tagName && e.tagName.toLowerCase() == tagName )
                filtered[ filtered.length ] = e;
        }
        return filtered;
    },


    getElementsByTagAndAttribute: function( root, tagName, attr ) {
        if( !root )
            root = document;
        var es = root.getElementsByTagName( tagName );
        return DOM.filterElementsByAttribute( es, attr );
    },
    
    
    getElementsByAttribute: function( root, attr ) {
        return DOM.getElementsByTagAndAttribute( root, "*", attr );
    },


    getElementsByAttributeAndValue: function( root, attr, value ) {
        var es = DOM.getElementsByTagAndAttribute( root, "*", attr );
        var filtered = [];
        for ( var i = 0; i < es.length; i++ )
            if ( es[ i ].getAttribute( attr ) == value )
                filtered.push( es[ i ] );
        return filtered;
    },
    

    getElementsByTagAndClassName: function( root, tagName, className ) {
        if( !root )
            root = document;
        var elements = root.getElementsByTagName( tagName );
        return DOM.filterElementsByClassName( elements, className );
    },


    getElementsByClassName: function( root, className ) {
        return DOM.getElementsByTagAndClassName( root, "*", className );
    },


    getAncestors: function( n, includeSelf ) {
        if( !n )
            return [];
        var as = includeSelf ? [ n ] : [];
        n = n.parentNode;
        while( n ) {
            as.push( n );
            n = n.parentNode;
        }
        return as;
    },
    
    
    getAncestorsByTagName: function( n, tagName, includeSelf ) {
        var es = DOM.getAncestors( n, includeSelf );
        return DOM.filterElementsByTagName( es, tagName );
    },
    
    
    getFirstAncestorByTagName: function( n, tagName, includeSelf ) {
        return DOM.getAncestorsByTagName( n, tagName, includeSelf )[ 0 ];
    },


    getAncestorsByClassName: function( n, className, includeSelf ) {
        var es = DOM.getAncestors( n, includeSelf );
        return DOM.filterElementsByClassName( es, className );
    },


    getFirstAncestorByClassName: function( n, className, includeSelf ) {
        return DOM.getAncestorsByClassName( n, className, includeSelf )[ 0 ];
    },


    getAncestorsByTagAndClassName: function( n, tagName, className, includeSelf ) {
        var es = DOM.getAncestorsByTagName( n, tagName, includeSelf );
        return DOM.filterElementsByClassName( es, className );
    },


    getFirstAncestorByTagAndClassName: function( n, tagName, className, includeSelf ) {
        return DOM.getAncestorsByTagAndClassName( n, tagName, className, includeSelf )[ 0 ];
    },


    getPreviousElement: function( n ) {
        n = n.previousSibling;
        while( n ) {
            if( n.nodeType == Node.ELEMENT_NODE )
                return n;
            n = n.previousSibling;
        }
        return null;
    },


    getNextElement: function( n ) {
        n = n.nextSibling;
        while( n ) {
            if( n.nodeType == Node.ELEMENT_NODE )
                return n;
            n = n.nextSibling;
        }
        return null;
    },


    isInlineNode: function( n ) {
        // text nodes are inline
        if( n.nodeType == Node.TEXT_NODE )
            return n;

        // document nodes are non-inline
        if( n.nodeType == Node.DOCUMENT_NODE )
            return false;

        // all nonelement nodes are inline
        if( n.nodeType != Node.ELEMENT_NODE )
            return n;

        // br elements are not inline
        if( n.tagName && n.tagName.toLowerCase() == "br" )
            return false;

        // examine the style property of the inline n
        var display = DOM.getStyle( n, "display" ); 
        if( display && display.indexOf( "inline" ) >= 0 ) 
            return n;
    },
    
    
    isTextNode: function( n ) {
        if( n.nodeType == Node.TEXT_NODE )
            return n;
    },
    
    
    isInlineTextNode: function( n ) {
        if( n.nodeType == Node.TEXT_NODE )
            return n;
        if( !DOM.isInlineNode( n ) )
            return null;
    },


    /* this and the following classname functions honor w3c case-sensitive classnames */

    getClassNames: function( e ) {
        if( !e || !e.className )
            return [];
        return e.className.split( /\s+/g );
    },


    hasClassName: function( e, className ) {
        if( !e || !e.className )
            return false;
        var cs = DOM.getClassNames( e );
        for( var i = 0; i < cs.length; i++ ) {
            if( cs[ i ] == className )
                return true;
        }
        return false;
    },


    addClassName: function( e, className ) {
        if( !e || !className )
            return false;
        var cs = DOM.getClassNames( e );
        for( var i = 0; i < cs.length; i++ ) {
            if( cs[ i ] == className )
                return true;
        }
        cs.push( className );
        e.className = cs.join( " " );
        return false;
    },


    removeClassName: function( e, className ) {
        var r = false;
        if( !e || !e.className || !className )
            return r;
        var cs = (e.className && e.className.length)
            ? e.className.split( /\s+/g )
            : [];
        var ncs = [];
        for( var i = 0; i < cs.length; i++ ) {
            if( cs[ i ] == className ) {
                r = true;
                continue;
            }
            ncs.push( cs[ i ] );
        }
        if( r )
            e.className = ncs.join( " " );
        return r;
    },
    
    
    /* tree manipulation methods */
    
    replaceWithChildNodes: function( n ) {
        var firstChild = n.firstChild;
        var parentNode = n.parentNode;
        while( n.firstChild )
            parentNode.insertBefore( n.removeChild( n.firstChild ), n );
        parentNode.removeChild( n );
        return firstChild;
    },
    
    
    /* factory methods */
    
    createInvisibleInput: function( d ) {
        if( !d )
            d = window.document;
        var e = document.createElement( "input" );
        e.setAttribute( "autocomplete", "off" );
        e.autocomplete = "off";
        DOM.makeInvisible( e );
        return e;
    },


    getMouseEventAttribute: function( event, a ) {
        if( !a )
            return;
        var es = DOM.getAncestors( event.target, true );
        for( var i = 0; i < es.length; i++ ) {
            try {
                var e = es[ i ]
                var v = e.getAttribute ? e.getAttribute( a ) : null;
                if( v ) {
                    event.attributeElement = e;
                    event.attribute = v;
                    return v;
                }
            } catch( e ) {}
        }
    },
    

    setElementAttribute: function( e, a, v ) {
        /* safari workaround
         * safari's setAttribute assumes you want to use a namespace
         * when you have a colon in your attribute
         */
        if ( navigator.userAgent.toLowerCase().match(/webkit/) ) {
            var at = e.attributes;
            for ( var i = 0; i < at.length; i++ )
                if ( at[ i ].name == a )
                    return at[ i ].nodeValue = v;
        } else
            e.setAttribute( a, v );
    }
    
    
} );


$ = DOM.getElement;
var HTTPReq = new Object;

HTTPReq.create = function () {
    var xtr;
    var ex;

    if (typeof(XMLHttpRequest) != "undefined") {
        xtr = new XMLHttpRequest();
    } else {
        try {
            xtr = new ActiveXObject("Msxml2.XMLHTTP.4.0");
        } catch (ex) {
            try {
                xtr = new ActiveXObject("Msxml2.XMLHTTP");
            } catch (ex) {
            }
        }
    }

    // let me explain this.  Opera 8 does XMLHttpRequest, but not setRequestHeader.
    // no problem, we thought:  we'll test for setRequestHeader and if it's not present
    // then fall back to the old behavior (treat it as not working).  BUT --- IE6 won't
    // let you even test for setRequestHeader without throwing an exception (you need
    // to call .open on the .xtr first or something)
    try {
        if (xtr && ! xtr.setRequestHeader)
            xtr = null;
    } catch (ex) { }

    return xtr;
};

// opts:
// url, onError, onData, method (GET or POST), data
// url: where to get/post to
// onError: callback on error
// onData: callback on data received
// method: HTTP method, GET by default
// data: what to send to the server (urlencoded)
HTTPReq.getJSON = function (opts) {
    var req = HTTPReq.create();
    if (! req) {
        if (opts.onError) opts.onError("noxmlhttprequest");
        return;
    }

    var state_callback = function () {
        if (req.readyState != 4) return;

        if (req.status != 200) {
            if (opts.onError) opts.onError(req.status ? "status: " + req.status : "no data");
            return;
        }

        var resObj;
        var e;
        try {
            eval("resObj = " + req.responseText + ";");
        } catch (e) {
        }

        if (e || ! resObj) {
            if (opts.onError) opts.onError("evalerror: " + (e ? e.toString(): "noresObj"));
            return;
        }

        if (opts.onData)
            opts.onData(resObj);
    };

    req.onreadystatechange = state_callback;

    var method = opts.method || "GET";
    var data = opts.data || null;


    var url = opts.url;
    if (opts.method == "GET" && opts.data) {
        url += url.match(/\?/) ? "&" : "?";
        url += opts.data
    }

    url += url.match(/\?/) ? "&" : "?";
    url += "_rand=" + Math.random();

    req.open(method, url, true);

    if (method.toUpperCase() == "POST")
      req.setRequestHeader("Content-Type", "application/x-www-form-urlencoded");

    req.send(data);
};

HTTPReq.formEncoded = function (vars) {
    var enc = [];
    var e;
    for (var key in vars) {
        try {
            if (!vars.hasOwnProperty(key))
                continue;
            enc.push(encodeURIComponent(key) + "=" + encodeURIComponent(vars[key]));
        } catch( e ) {}
    }
    return enc.join("&");

};

// This file contains general-purpose LJ code
// $Id$

var LiveJournal = new Object;

// The hook mappings
LiveJournal.hooks = {};

LiveJournal.register_hook = function (hook, func) {
    if (! LiveJournal.hooks[hook])
        LiveJournal.hooks[hook] = [];

    LiveJournal.hooks[hook].push(func);
};

// args: hook, params to pass to hook
LiveJournal.run_hook = function () {
    var a = arguments;

    var hookfuncs = LiveJournal.hooks[a[0]];
    if (!hookfuncs || !hookfuncs.length) return;

    var hookargs = [];

    for (var i = 1; i < a.length; i++) {
        hookargs.push(a[i]);
    }

    var rv = null;

    hookfuncs.forEach(function (hookfunc) {
        rv = hookfunc.apply(null, hookargs);
    });

    return rv;
};

LiveJournal.pageLoaded = false;

LiveJournal.initPage = function () {
    // only run once
    if (LiveJournal.pageLoaded)
        return;
    LiveJournal.pageLoaded = 1;

    // set up various handlers for every page
    LiveJournal.initPlaceholders();
    LiveJournal.initLabels();
    LiveJournal.initInboxUpdate();

    // run other hooks
    LiveJournal.run_hook("page_load");
};

// Set up two different ways to test if the page is loaded yet.
// The proper way is using DOMContentLoaded, but only Mozilla supports it.
// So, the page_load hook will be fired when the DOM is loaded or after 1.5 seconds, whichever comes first
{
    // Others
    window.setTimeout(LiveJournal.initPage, 1500);

    // Mozilla
    DOM.addEventListener(window, "DOMContentLoaded", LiveJournal.initPage);
}

// Set up a timer to keep the inbox count updated
LiveJournal.initInboxUpdate = function () {
    // Don't run if not logged in or this is disabled
    if (! LJVAR || ! LJVAR.has_remote || ! LJVAR.inbox_update_poll) return;

    // Don't run if no inbox count
    var unread = $("LJ_Inbox_Unread_Count");
    if (! unread) return;

    // Update every five minutes
    window.setInterval(LiveJournal.updateInbox, 1000 * 60 * 5);
};

// Do AJAX request to find the number of unread items in the inbox
LiveJournal.updateInbox = function () {
    var postData = {
        "action": "get_unread_items"
    };

    var opts = {
        "data": HTTPReq.formEncoded(postData),
        "method": "POST",
        "onData": LiveJournal.gotInboxUpdate
    };

    opts.url = LJVAR.currentJournal ? "/" + LJVAR.currentJournal + "/__rpc_esn_inbox" : "/__rpc_esn_inbox";

    HTTPReq.getJSON(opts);
};

// We received the number of unread inbox items from the server
LiveJournal.gotInboxUpdate = function (resp) {
    if (! resp || resp.error) return;

    var unread = $("LJ_Inbox_Unread_Count");
    if (! unread) return;

    unread.innerHTML = resp.unread_count ? "  (" + resp.unread_count + ")" : "";
};

// Search for placeholders and initialize them
LiveJournal.initPlaceholders = function () {
    var domObjects = document.getElementsByTagName("*");
    var placeholders = DOM.filterElementsByClassName(domObjects, "LJ_Placeholder") || [];

    Array.prototype.forEach.call(placeholders, function (placeholder) {
        var parent = DOM.getFirstAncestorByClassName(placeholder, "LJ_Placeholder_Container", false);

        var containers = DOM.filterElementsByClassName(parent.getElementsByTagName("div"), "LJ_Container");
        var container = containers[0];
        if (!container) return;

        var placeholder_html = unescape(container.getAttribute("lj_placeholder_html"));

        DOM.addEventListener(placeholder, "click", function (e) {
            Event.stop(e);

            // have to wrap placeholder_html in another block, IE is weird
            container.innerHTML = "<span>" + placeholder_html + "</span>";

            DOM.makeInvisible(placeholder);
        });

        return false;
    });
};

// set up labels for Safari
LiveJournal.initLabels = function () {
    // safari doesn't know what <label> tags are, lets fix them
    if (navigator.userAgent.indexOf('Safari') == -1) return;

    // get all labels
    var labels = document.getElementsByTagName("label");

    for (var i = 0; i < labels.length; i++) {
        DOM.addEventListener(labels[i], "click", LiveJournal.labelClickHandler);
    }
};

LiveJournal.labelClickHandler = function (evt) {
    Event.prep(evt);

    var label = DOM.getAncestorsByTagName(evt.target, "label", true)[0];
    if (! label) return;

    var targetId = label.getAttribute("for");
    if (! targetId) return;

    var target = $(targetId);
    if (! target) return;

    target.click();

    return false;
};
/*
  IPPU methods:
     init([innerHTML]) -- takes innerHTML as optional argument
     show() -- shows the popup
     hide() -- hides popup
     cancel() -- hides and calls cancel callback

  Content setters:
     setContent(innerHTML) -- set innerHTML
     setContentElement(element) -- adds element as a child of the popup

   Accessors:
     getElement() -- returns popup DIV element
     visible() -- returns whether the popup is visible or not

   Titlebar:
     setTitlebar(show) -- true: show titlebar / false: no titlebar
     setTitle(title) -- sets the titlebar text
     getTitlebarElement() -- returns the titlebar element
     setTitlebarClass(className) -- set the class of the titlebar

   Styling:
     setOverflow(overflow) -- sets ele.style.overflow to overflow
     addClass(className) -- adds class to popup
     removeClass(className) -- removes class to popup

   Browser Hacks:
     setAutoHideSelects(autohide) -- when the popup is shown should it find all the selects
                                on the page and hide them (and show them again) (for IE)

   Positioning/Sizing:
     setLocation(left, top) -- set popup location: will be pixels if units not specified
     setLeft(left) -- set left location
     setTop(top)   -- set top location
     setDimensions(width, height) -- set popup dimensions: will be pixels if units not specified
     setAutoCenter(x, y) -- what dimensions to auto-center
     center() -- centers popup on screen
     centerX() -- centers popup horizontally
     centerY() -- centers popup vertically
     setFixedPosition(fixed) -- should the popup stay fixed on the page when it scrolls?
     centerOnWidget(widget) -- center popup on this widget
     setAutoCenterCallback(callback) -- calls callback with this IPPU instance as a parameter
                                        for auto-centering. Some common built-in class methods
                                        you can use as callbacks are:
                                        IPPU.center
                                        IPPU.centerX
                                        IPPU.centerY

     moveForward(amount) -- increases the zIndex by one or amount if specified
     moveBackward(amount) -- decreases the zIndex by one or amount if specified

   Modality:
     setClickToClose(clickToClose) -- if clickToClose is true, clicking outside of the popup
                                      will close it
     setModal(modality) -- If modality is true, then popup will capture all mouse events
                     and optionally gray out the rest of the page. (overrides clickToClose)
     setOverlayVisible(visible) -- If visible is true, when this popup is on the page it
                                   will gray out the rest of the page if this is modal

   Callbacks:
     setCancelledCallback(callback) -- call this when the dialog is closed through clicking
                                       outside, titlebar close button, etc...
     setHiddenCallback(callback) -- called when the dialog is closed in any fashion

   Fading:
     setFadeIn(fadeIn) -- set whether or not to automatically fade the ippu in
     setFadeOut(fadeOut) -- set whether or not to automatically fade the ippu out
     setFadeSpeed(secs) -- sets fade speed

  Class Methods:
   Handy callbacks:
     IPPU.center
     IPPU.centerX
     IPPU.centerY
   Browser testing:
     IPPU.isIE() -- is the browser internet exploder?
     IPPU.ieSafari() -- is this safari?

////////////////////


ippu.setModalDenialCallback(IPPU.cssBorderFlash);


   private:
    Properties:
     ele -- DOM node of div
     shown -- boolean; if element is in DOM
     autoCenterX -- boolean; auto-center horiz
     autoCenterY -- boolean; auto-center vertical
     fixedPosition -- boolean; stay in fixed position when browser scrolls?
     titlebar -- titlebar element
     title -- string; text to go in titlebar
     showTitlebar -- boolean; whether or not to show titlebar
     content -- DIV containing user's specified content
     clickToClose -- boolean; clicking outside popup will close it
     clickHandlerSetup -- boolean; have we set up the click handlers?
     docOverlay -- DIV that overlays the document for capturing clicks
     modal -- boolean; capture all events and prevent user from doing anything
                       until dialog is dismissed
     visibleOverlay -- boolean; make overlay slightly opaque
     clickHandlerFunc -- function; function to handle document clicks
     resizeHandlerFunc -- function; function to handle document resizing
     autoCenterCallback -- function; what callback to call for auto-centering
     cancelledCallback -- function; called when dialog is cancelled
     setAutoHideSelects -- boolean; autohide all SELECT elements on the page when popup is visible
     hiddenSelects -- array; SELECT elements that have been hidden
     hiddenCallback -- funciton; called when dialog is hidden
     fadeIn, fadeOut, fadeSpeed -- fading settings
     fadeMode -- current fading mode (in, out) if there is fading going on

    Methods:
     updateTitlebar() -- create titlebar if it doesn't exist,
                         hide it if titlebar == false,
                         update titlebar text
     updateContent() -- makes sure all currently specified properties are applied
     setupClickCapture() -- if modal, create document-sized div overlay to capture click events
                            otherwise install document onclick handler
     removeClickHandlers() -- remove overlay, event handlers
     clickHandler() -- event handler for clicks
     updateOverlay() -- if we have an overlay, make sure it's where it should be and (in)visible
                        if it should be
     autoCenter() -- centers popup on screen according to autoCenterX and autoCenterY
     hideSelects() -- hide all select element on page
     showSelects() -- show all selects
     _hide () -- actually hides everything, called by hide(), which does fading if necessary
*/

// this belongs somewhere else:
function changeOpac(id, opacity) {
    var e =  $(id);
    if (e && e.style) {
        var object = e.style;
        if (object) {
            //reduce flicker
            if (IPPU.isSafari() && opacity >= 100)
                opacity = 99.99;

            object.filter = "alpha(opacity=" + opacity * 100 + ")";
            object.opacity = opacity;
        }
    }
}

IPPU = new Class( Object, {
  setFixedPosition: function (fixed) {
    // no fixed position for IE
    if (IPPU.isIE())
      return;

    this.fixedPosition = fixed;
    this.updateContent();
  },

  clickHandler : function (evt) {
    if (!this.clickToClose) return;
    if (!this.visible()) return;

    evt = Event.prep(evt);
    var target = evt.target;
    // don't do anything if inside the popup
    if (DOM.getAncestorsByClassName(target, "ippu", true).length > 0) return;

    this.cancel();
  },

  setCancelledCallback : function (callback) {
    this.cancelledCallback = callback;
  },

  cancel : function () {
    if (this.cancelledCallback)
      this.cancelledCallback();

    this.hide();
  },

  setHiddenCallback: function (callback) {
    this.hiddenCallback = callback;
  },

  setupClickCapture : function () {
    if (!this.visible() || this.clickHandlerSetup) return;
    if (!this.clickToClose && !this.modal) return;

    this.clickHandlerFunc = this.clickHandler.bindEventListener(this);

    if (this.modal) {
      // create document-sized div to capture events
      if (this.overlay) return; // wtf? shouldn't exist yet

      this.overlay = document.createElement("div");
      this.overlay.style.position = "fixed";
      this.overlay.style.left = "0px";
      this.overlay.style.top = "0px";
      this.overlay.style.margin = "0px";
      this.overlay.style.padding = "0px";
      this.overlay.style.backgroundColor = "#000000";

      this.ele.parentNode.insertBefore(this.overlay, this.ele);
      this.updateOverlay();

      DOM.addEventListener(this.overlay, "click", this.clickHandlerFunc);
    } else {
      // simple document onclick handler
      DOM.addEventListener(document, "click", this.clickHandlerFunc);
    }

    this.clickHandlerSetup = true;
  },

  updateOverlay : function () {
    if (this.overlay) {
      var cd = DOM.getClientDimensions();
      this.overlay.style.width = (cd.x - 1) + "px";
      this.overlay.style.height = (cd.y - 1) + "px";

      if (this.visibleOverlay) {
        this.overlay.backgroundColor = "#000000";
        changeOpac(this.overlay, 0.50);
      } else {
        this.overlay.backgroundColor = "#FFFFFF";
        changeOpac(this.overlay, 0.0);
      }
    }
  },

  resizeHandler : function (evt) {
    this.updateContent();
  },

  removeClickHandlers : function () {
    if (!this.clickHandlerSetup) return;

    var myself = this;
    var handlerFunc = function (evt) {
      myself.clickHandler(evt);
    };

    DOM.removeEventListener(document, "click", this.clickHandlerFunc, false);

    if (this.overlay) {
      DOM.removeEventListener(this.overlay, "click", this.clickHandlerFunc, true);
      this.overlay.parentNode.removeChild(this.overlay);
      this.overlay = undefined;
    }

    this.clickHandlerFunc = undefined;
    this.clickHandlerSetup = false;
  },

  setClickToClose : function (clickToClose) {
    this.clickToClose = clickToClose;

    if (!this.clickHandlerSetup && clickToClose && this.visible()) {
      // popup is already visible, need to set up click handler
      var setupClickCaptureCallback = this.setupClickCapture.bind(this);
      window.setTimeout(setupClickCaptureCallback, 100);
    } else if (!clickToClose && this.clickHandlerSetup) {
      this.removeClickHandlers();
    }

    this.updateContent();
  },

  setModal : function (modal) {
    var changed = (this.modal == modal);

    // if it's modal, we don't want click-to-close
    if (modal)
      this.setClickToClose(false);

    this.modal = modal;
    if (changed) {
      this.removeClickHandlers();
      this.updateContent();
    }
  },

  setOverlayVisible : function (vis) {
    this.visibleOverlay = vis;
    this.updateContent();
  },

  updateContent : function () {
    this.autoCenter();
    this.updateTitlebar();
    this.updateOverlay();
    if (this.titlebar)
      this.setTitlebarClass(this.titlebar.className);

    var setupClickCaptureCallback = this.setupClickCapture.bind(this);
    window.setTimeout(setupClickCaptureCallback, 100);

    if (this.fixedPosition && this.ele.style.position != "fixed")
      this.ele.style.position = "fixed";
    else if (!this.fixedPosition && this.ele.style.position == "fixed")
      this.ele.style.position = "absolute";
  },

  getTitlebarElement : function () {
    return this.titlebar;
  },

  setTitlebarClass : function (className) {
    if (this.titlebar)
      this.titlebar.className = className;
  },

  setOverflow : function (overflow) {
    if (this.ele)
      this.ele.style.overflow = overflow;
  },

  visible : function () {
    return this.shown;
  },

  setTitlebar : function (show) {
    this.showTitlebar = show;

    if (show) {
      if (!this.titlebar) {
        // titlebar hasn't been created. Create it.
        var tbar = document.createElement("div");
        if (!tbar) return;
        tbar.style.width = "100%";

        if (this.title) tbar.innerHTML = this.title;
        this.ele.insertBefore(tbar, this.content);
        this.titlebar = tbar;

      }
    } else if (this.titlebar) {
      this.ele.removeChild(this.titlebar);
      this.titlebar = false;
    }
  },

  setTitle : function (title) {
    this.title = title;
    this.updateTitlebar();
  },

  updateTitlebar : function() {
    if (this.showTitlebar && this.titlebar && this.title != this.titlebar.innerHTML) {
      this.titlebar.innerHTML = this.title;
    }
  },

  addClass : function (className) {
    DOM.addClassName(this.ele, className);
  },

  removeClass : function (className) {
    DOM.removeClassName(this.ele, className);
  },

  setAutoCenterCallback : function (callback) {
    this.autoCenterCallback = callback;
  },

  autoCenter : function () {
    if (!this.visible || !this.visible()) return;

    if (this.autoCenterCallback) {
      this.autoCenterCallback(this);
      return;
    }

    if (this.autoCenterX)
      this.centerX();

    if (this.autoCenterY)
      this.centerY();
  },

  center : function () {
    this.centerX();
    this.centerY();
  },

  centerOnWidget : function (widget) {
        this.setAutoCenter(false, false);
        this.setAutoCenterCallback(null);
  var wd = DOM.getAbsoluteDimensions(widget);
    var ed = DOM.getAbsoluteDimensions(this.ele);
    var newleft = wd.absoluteRight - wd.offsetWidth / 2 - ed.offsetWidth / 2;
    var newtop = wd.absoluteBottom - wd.offsetHeight / 2 - ed.offsetHeight / 2;

        newleft = newleft < 0 ? 0 : newleft;
        newtop  = newtop  < 0 ? 0 : newtop;
    DOM.setLeft(this.ele, newleft);
    DOM.setTop(this.ele, newtop);
  },

  centerX : function () {
    if (!this.visible || !this.visible()) return;

    var cd = DOM.getClientDimensions();
    var newleft = cd.x / 2 - DOM.getDimensions(this.ele).offsetWidth / 2;
    DOM.setLeft(this.ele, newleft);
  },

  centerY : function () {
    if (!this.visible || !this.visible()) return;

    var cd = DOM.getClientDimensions();
    var newtop = cd.y / 2 - DOM.getDimensions(this.ele).offsetHeight / 2;
    DOM.setTop(this.ele, newtop);
  },

  setAutoCenter : function (autoCenterX, autoCenterY) {
    this.autoCenterX = autoCenterX || false;
    this.autoCenterY = autoCenterY || false;

    if (!autoCenterX && !autoCenterY) {
        this.setAutoCenterCallback(null);
        return;
    }

    this.autoCenter();
  },

  setDimensions : function (width, height) {
    width = width + "";
    height = height + "";
    if (width.match(/^\d+$/)) width += "px";
    if (height.match(/^\d+$/)) height += "px";

    this.ele.style.width  = width;
    this.ele.style.height = height;
  },

  moveForward : function (howMuch) {
      if (!howMuch) howMuch = 1;
      if (! this.ele) return;

      this.ele.style.zIndex += howMuch;
  },

  moveBackward : function (howMuch) {
      if (!howMuch) howMuch = 1;
      if (! this.ele) return;

      this.ele.style.zIndex -= howMuch;
  },

  setLocation : function (left, top) {
      this.setLeft(left);
      this.setTop(top);
  },

  setTop : function (top) {
    top = top + "";
    if (top.match(/^\d+$/)) top += "px";
    this.ele.style.top = top;
  },

  setLeft : function (left) {
    left = left + "";
    if (left.match(/^\d+$/)) left += "px";
    this.ele.style.left = left;
  },

  getElement : function () {
    return this.ele;
  },

  setContent : function (html) {
    this.content.innerHTML = html;
  },

  setContentElement : function (element) {
      // remove child nodes
      while (this.content.firstChild) {
          this.content.removeChild(this.content.firstChild);
      };

    this.content.appendChild(element);
  },

  setFadeIn : function (fadeIn) {
      this.fadeIn = fadeIn;
  },

  setFadeOut : function (fadeOut) {
      this.fadeOut = fadeOut;
  },

  setFadeSpeed : function (fadeSpeed) {
      this.fadeSpeed = fadeSpeed;
  },

  show : function () {
    this.shown = true;

    if (this.fadeIn) {
        var opp = 0.01;

        changeOpac(this.ele, opp);
    }

    document.body.appendChild(this.ele);
    this.ele.style.position = "absolute";
    if (this.autoCenterX || this.autoCenterY) this.center();

    this.updateContent();

    if (!this.resizeHandlerFunc) {
      this.resizeHandlerFunc = this.resizeHandler.bindEventListener(this);
      DOM.addEventListener(window, "resize", this.resizeHandlerFunc, false);
    }

    if (this.fadeIn)
        this.fade("in");

    this.hideSelects();
  },

  fade : function (mode, callback) {
      var opp;
      var delta;

      var steps = 10.0;

      if (mode == "in") {
          delta = 1 / steps;
          opp = 0.1;
      } else {
          if (this.ele.style.opacity)
          opp = finiteFloat(this.ele.style.opacity);
          else
          opp = 0.99;

          delta = -1 / steps;
      }

      var fadeSpeed = this.fadeSpeed;
      if (!fadeSpeed) fadeSpeed = 1;

      var fadeInterval = steps / fadeSpeed * 5;

      this.fadeMode = mode;

      var self = this;
      var fade = function () {
          opp += delta;

          // did someone start a fade in the other direction? if so,
          // cancel this fade
          if (self.fadeMode && self.fadeMode != mode) {
              if (callback)
                  callback.call(self, []);

              return;
          }

          if (opp <= 0.1) {
              if (callback)
                  callback.call(self, []);

              self.fadeMode = null;

              return;
          } else if (opp >= 1.0) {
              if (callback)
                  callback.call(self, []);

              self.fadeMode = null;

              return;
          } else {
              changeOpac(self.ele, opp);
              window.setTimeout(fade, fadeInterval);
          }
      };

      fade();
  },

  hide : function () {
    if (! this.visible()) return;

    if (this.fadeOut && this.ele) {
        this.fade("out", this._hide.bind(this));
    } else {
        this._hide();
    }
  },

  _hide : function () {
    if (this.hiddenCallback)
      this.hiddenCallback();

    this.shown = false;
    this.removeClickHandlers();

    if (this.ele)
    document.body.removeChild(this.ele);

    if (this.resizeHandlerFunc)
      DOM.removeEventListener(window, "resize", this.resizeHandlerFunc);

    this.showSelects();
  },

  // you probably want this for IE being dumb
  // (IE thinks select elements are cool and puts them in front of every element on the page)
  setAutoHideSelects : function (autohide) {
    this.autoHideSelects = autohide;
    this.updateContent();
  },

  hideSelects : function () {
    if (!this.autoHideSelects || !IPPU.isIE()) return;
    var sels = document.getElementsByTagName("select");
    var ele;
    for (var i = 0; i < sels.length; i++) {
      ele = sels[i];
      if (!ele) continue;

      // if this element is inside the ippu, skip it
      if (DOM.getAncestorsByClassName(ele, "ippu", true).length > 0) continue;

      if (ele.style.visibility != 'hidden') {
        ele.style.visibility = 'hidden';
        this.hiddenSelects.push(ele);
      }
    }
  },

  showSelects : function () {
    if (! this.autoHideSelects) return;
    var ele;
    while (ele = this.hiddenSelects.pop())
      ele.style.visibility = '';
  },

  init: function (html) {
    var ele = document.createElement("div");
    this.ele = ele;
    this.shown = false;
    this.autoCenterX = false;
    this.autoCenterY = false;
    this.titlebar = null;
    this.title = "";
    this.showTitlebar = false;
    this.clickToClose = false;
    this.modal = false;
    this.clickHandlerSetup = false;
    this.docOverlay = false;
    this.visibleOverlay = false;
    this.clickHandlerFunc = false;
    this.resizeHandlerFunc = false;
    this.fixedPosition = false;
    this.autoCenterCallback = null;
    this.cancelledCallback = null;
    this.autoHideSelects = false;
    this.hiddenCallback = null;
    this.fadeOut = false;
    this.fadeIn = false;
    this.hiddenSelects = [];
    this.fadeMode = null;

    ele.style.position = "absolute";
    ele.style.zIndex   = "1000";

    // plz don't remove thx
    DOM.addClassName(ele, "ippu");

    // create DIV to hold user's content
    this.content = document.createElement("div");

    this.content.innerHTML = html;

    this.ele.appendChild(this.content);
  }
});

// class methods
IPPU.center = function (obj) {
  obj.centerX();
  obj.centerY();
};

IPPU.centerX = function (obj) {
  obj.centerX();
};

IPPU.centerY = function (obj) {
  obj.centerY();
};

IPPU.isIE = function () {
    var UA = navigator.userAgent.toLowerCase();
    if (UA.indexOf('msie') != -1) return true;
    return false;
};

IPPU.isSafari = function () {
    var UA = navigator.userAgent.toLowerCase();
    if (UA.indexOf('safari') != -1) return true;
    return false;
};
LJ_IPPU = new Class ( IPPU, {
  init: function(title) {
    if (!title)
      title = "";

    LJ_IPPU.superClass.init.apply(this, []);

    this.uniqId = this.generateUniqId();
    this.cancelThisFunc = this.cancel.bind(this);

    this.setTitle(title);
    this.setTitlebar(true);
    this.setTitlebarClass("lj_ippu_titlebar");

    this.addClass("lj_ippu");

    this.setAutoCenterCallback(IPPU.center);
    this.setDimensions(400, "auto");
    this.setOverflow("hidden");

    this.setFixedPosition(true);
    this.setClickToClose(true);
    this.setAutoHideSelects(true);
  },

  setTitle: function (title) {
    var titlebarContent = "\
      <div style='float:right; padding-right: 8px'>" +
      "<img src='" + LJVAR.imgprefix + "/CloseButton.gif' width='15' height='15' id='" + this.uniqId + "_cancel' /></div>" + title;

    LJ_IPPU.superClass.setTitle.apply(this, [titlebarContent]);
  },

  generateUniqId: function() {
    var theDate = new Date();
    return "lj_ippu_" + theDate.getHours() + theDate.getMinutes() + theDate.getMilliseconds();
  },

  show: function() {
    LJ_IPPU.superClass.show.apply(this);
    var setupCallback = this.setup_lj_ippu.bind(this);
    window.setTimeout(setupCallback, 300);
  },

  setup_lj_ippu: function (evt) {
    var cancelCallback = this.cancelThisFunc;
    DOM.addEventListener($(this.uniqId + "_cancel"), "click", cancelCallback, true);
  },

  hide: function() {
    DOM.removeEventListener($(this.uniqId + "_cancel"), "click", this.cancelThisFunc, true);
    LJ_IPPU.superClass.hide.apply(this);
  }
} );

// Class method to show a popup to show a note to the user
// note = message to show
// underele = element to display the note underneath
LJ_IPPU.showNote = function (note, underele, timeout) {
    var noteElement = document.createElement("div");
    noteElement.innerHTML = note;

    return LJ_IPPU.showNoteElement(noteElement, underele, timeout);
};

LJ_IPPU.showNoteElement = function (noteEle, underele, timeout) {
    var notePopup = new IPPU();
    notePopup.init();

    var inner = document.createElement("div");
    DOM.addClassName(inner, "Inner");
    inner.appendChild(noteEle);
    notePopup.setContentElement(inner);

    notePopup.setTitlebar(false);
    notePopup.setFadeIn(true);
    notePopup.setFadeOut(true);
    notePopup.setFadeSpeed(4);
    notePopup.setDimensions("auto", "auto");
    notePopup.addClass("Note");

    var dim;
    if (underele) {
        // pop up the box right under the element
        dim = DOM.getAbsoluteDimensions(underele);
        if (!dim) return;
    }

    if (!dim) {
        notePopup.setModal(true);
        notePopup.setOverlayVisible(true);
        notePopup.setAutoCenter(true, true);
    } else {
        // default is to auto-center, don't want that
        notePopup.setAutoCenter(false, false);
        notePopup.setLocation(dim.absoluteLeft, dim.absoluteBottom + 4);
    }

    notePopup.setClickToClose(true);
    notePopup.show();
    notePopup.moveForward();

    if (! defined(timeout)) {
        timeout = 5000;
    }

    if (timeout) {
        window.setTimeout(function () {
            if (notePopup)
                notePopup.hide();
        }, timeout);
    }

    return notePopup;
};
// LiveJournal javascript standard interface routines

// create a little animated hourglass at (x,y) with a unique-ish ID
// returns the element created
Hourglass = new Class( Object, {
  init: function(widget, classname) {
    this.ele = document.createElement("img");
    if (!this.ele) return;

    var imgprefix = LJVAR ? LJVAR.imgprefix : '';

    this.ele.src = imgprefix ? imgprefix + "/hourglass.gif" : "/img/hourglass.gif";
    this.ele.style.position = "absolute";

    DOM.addClassName(this.ele, classname);

    if (widget)
      this.hourglass_at_widget(widget);
  },

  hourglass_at: function (x, y) {
    this.ele.width = 17;
    this.ele.height = 17;
    this.ele.style.top = (y - 8) + "px";
    this.ele.style.left = (x - 8) + "px";

    // unique ID
    this.ele.id = "lj_hourglass" + x + "." + y;

    document.body.appendChild(this.ele);
  },

  add_class_name: function (classname) {
      if (this.ele)
      DOM.addClassName(this.ele, classname);
  },

  hourglass_at_widget: function (widget) {
    var dim = DOM.getAbsoluteDimensions(widget);
    var x = dim.absoluteLeft;
    var y = dim.absoluteTop;
    var w = dim.absoluteRight - x;
    var h = dim.absoluteBottom - y;
    if (w && h) {
      x += w/2;
      y += h/2;
    }
    this.hourglass_at(x, y);
  },

  hide: function () {
    if (this.ele) {
      try {
        document.body.removeChild(this.ele);
      } catch (e) {}
    }
  }

} );
var ContextualPopup = new Object;

ContextualPopup.popupDelay  = 500;
ContextualPopup.hideDelay   = 250;
ContextualPopup.disableAJAX = false;
ContextualPopup.debug       = false;

ContextualPopup.cachedResults   = {};
ContextualPopup.currentRequests = {};
ContextualPopup.mouseInTimer    = null;
ContextualPopup.mouseOutTimer   = null;
ContextualPopup.currentId       = null;
ContextualPopup.hourglass       = null;
ContextualPopup.elements        = {};

ContextualPopup.setup = function (e) {
    // don't do anything if no remote
    if (!LJVAR || !LJVAR.ctx_popup) return;

    // attach to all ljuser head icons
    var domObjects = document.getElementsByTagName("*");
    var ljusers = DOM.filterElementsByClassName(domObjects, "ljuser") || [];

    var userElements = [];
    ljusers.forEach(function (ljuser) {
        var nodes = ljuser.getElementsByTagName("img");
        for (var i=0; i < nodes.length; i++) {
            var node = nodes.item(i);

            // if the parent (a tag with link to userinfo) has userid in its URL, then
            // this is an openid user icon and we should use the userid
            var parent = node.parentNode;
            var userid;
            if (parent && (userid = parent.href.match(/\?userid=(\d+)/i)))
                node.userid = userid[1];
            else
                node.username = ljuser.getAttribute("lj:user");

            if (!node.username && !node.userid) continue;

            userElements.push(node);
            DOM.addClassName(node, "ContextualPopup");
        }
    });

    // attach to all userpics
    var images = DOM.filterElementsByTagName(domObjects, "img") || [];
    images.forEach(function (image) {
        // if the image url matches a regex for userpic urls then attach to it
        if (image.src.match(/userpic\..+\/\d+\/\d+/) ||
            image.src.match(/\/userpic\/\d+\/\d+/)) {
            image.up_url = image.src;
            DOM.addClassName(image, "ContextualPopup");
            userElements.push(image);
        }
    });

    var ctxPopupId = 1;
    userElements.forEach(function (userElement) {
        ContextualPopup.elements[ctxPopupId + ""] = userElement;
        userElement.ctxPopupId = ctxPopupId++;
    });

    DOM.addEventListener(document.body, "mousemove", ContextualPopup.mouseOver.bindEventListener());
}

ContextualPopup.isCtxPopElement = function (ele) {
    return (ele && DOM.getAncestorsByClassName(ele, "ContextualPopup", true).length);
}

ContextualPopup.mouseOver = function (e) {
    var target = e.target;
    var ctxPopupId = target.ctxPopupId;

    // if the ctxpopup class isn't fully loaded and set up yet,
    // skip the event handling for now
    if (!ContextualPopup || !ContextualPopup.isCtxPopElement) return;

    // did the mouse move out?
    if (!target || !ContextualPopup.isCtxPopElement(target)) {
        if (ContextualPopup.mouseInTimer) {
            window.clearTimeout(ContextualPopup.mouseInTimer);
            ContextualPopup.mouseInTimer = null;
        };

        if (ContextualPopup.ippu) {
            if (ContextualPopup.mouseInTimer || ContextualPopup.mouseOutTimer) return;

            ContextualPopup.mouseOutTimer = window.setTimeout(function () {
                ContextualPopup.mouseOut(e);
            }, ContextualPopup.hideDelay);
            return;
        }
    }

    // we're inside a ctxPopElement, cancel the mouseout timer
    if (ContextualPopup.mouseOutTimer) {
        window.clearTimeout(ContextualPopup.mouseOutTimer);
        ContextualPopup.mouseOutTimer = null;
    }

    if (!ctxPopupId)
    return;

    var cached = ContextualPopup.cachedResults[ctxPopupId + ""];

    // if we don't have cached data background request it
    if (!cached) {
        ContextualPopup.getInfo(target);
    }

    // start timer if it's not running
    if (! ContextualPopup.mouseInTimer && (! ContextualPopup.ippu || (
                                                                      ContextualPopup.currentId &&
                                                                      ContextualPopup.currentId != ctxPopupId))) {
        ContextualPopup.mouseInTimer = window.setTimeout(function () {
            ContextualPopup.showPopup(ctxPopupId);
        }, ContextualPopup.popupDelay);
    }
}

// if the popup was not closed by us catch it and handle it
ContextualPopup.popupClosed = function () {
    ContextualPopup.mouseOut();
}

ContextualPopup.mouseOut = function (e) {
    if (ContextualPopup.mouseInTimer)
        window.clearTimeout(ContextualPopup.mouseInTimer);
    if (ContextualPopup.mouseOutTimer)
        window.clearTimeout(ContextualPopup.mouseOutTimer);

    ContextualPopup.mouseInTimer = null;
    ContextualPopup.mouseOutTimer = null;
    ContextualPopup.currentId = null;

    ContextualPopup.hidePopup();
}

ContextualPopup.showPopup = function (ctxPopupId) {
    if (ContextualPopup.mouseInTimer) {
        window.clearTimeout(ContextualPopup.mouseInTimer);
    }
    ContextualPopup.mouseInTimer = null;

    if (ContextualPopup.ippu && (ContextualPopup.currentId && ContextualPopup.currentId == ctxPopupId)) {
        return;
    }

    ContextualPopup.currentId = ctxPopupId;

    ContextualPopup.constructIPPU(ctxPopupId);

    var ele = ContextualPopup.elements[ctxPopupId + ""];
    var data = ContextualPopup.cachedResults[ctxPopupId + ""];

    if (! ele || (data && data.noshow)) {
        return;
    }

    if (ContextualPopup.ippu) {
        // default is to auto-center, don't want that
        ContextualPopup.ippu.setAutoCenter(false, false);

        // pop up the box right under the element
        var dim = DOM.getAbsoluteDimensions(ele);
        if (!dim) return;

        ContextualPopup.ippu.setLocation(dim.absoluteLeft, dim.absoluteBottom);
        ContextualPopup.ippu.show();
    }
}

ContextualPopup.constructIPPU = function (ctxPopupId) {
    if (ContextualPopup.ippu) {
        ContextualPopup.ippu.hide();
        ContextualPopup.ippu = null;
    }

    var ippu = new IPPU();
    ippu.init();
    ippu.setTitlebar(false);
    ippu.setFadeOut(true);
    ippu.setFadeIn(true);
    ippu.setFadeSpeed(15);
    ippu.setDimensions("auto", "auto");
    ippu.addClass("ContextualPopup");
    ippu.setCancelledCallback(ContextualPopup.popupClosed);
    ContextualPopup.ippu = ippu;

    ContextualPopup.renderPopup(ctxPopupId);
}

ContextualPopup.renderPopup = function (ctxPopupId) {
    var ippu = ContextualPopup.ippu;

    if (!ippu)
    return;

    if (ctxPopupId) {
        var data = ContextualPopup.cachedResults[ctxPopupId];

        if (!data) {
            ippu.setContent("<div class='Inner'>Loading...</div>");
            return;
        } else if (!data.username || !data.success || data.noshow) {
            ippu.hide();
            return;
        }

        var username = data.display_username;

        var inner = document.createElement("div");
        DOM.addClassName(inner, "Inner");

        var content = document.createElement("div");
        DOM.addClassName(content, "Content");

        var bar = document.createElement("span");
        bar.innerHTML = " | ";

        // userpic
        if (data.url_userpic && data.url_userpic != ContextualPopup.elements[ctxPopupId].src) {
            var userpicContainer = document.createElement("div");
            var userpicLink = document.createElement("a");
            userpicLink.href = data.url_allpics;
            var userpic = document.createElement("img");
            userpic.src = data.url_userpic;
            userpic.width = data.userpic_w;
            userpic.height = data.userpic_h;

            userpicContainer.appendChild(userpicLink);
            userpicLink.appendChild(userpic);
            DOM.addClassName(userpicContainer, "Userpic");

            inner.appendChild(userpicContainer);
        }

        inner.appendChild(content);

        // relation
        var relation = document.createElement("div");
        if (data.is_comm) {
            if (data.is_member)
                relation.innerHTML = "You are a member of " + username;
            else if (data.is_friend)
                relation.innerHTML = "You are watching " + username;
            else
                relation.innerHTML = username;
        } else if (data.is_syndicated) {
            if (data.is_friend)
                relation.innerHTML = "You are subscribed to " + username;
            else
                relation.innerHTML = username;
        } else {
            if (data.is_requester) {
                relation.innerHTML = "This is you";
            } else {
                var label = username + " ";

                if (data.is_friend_of) {
                    if (data.is_friend)
                        label += "is your mutual friend";
                    else
                        label += "lists you as a friend";
                } else {
                    if (data.is_friend)
                        label += "is your friend";
                }

                relation.innerHTML = label;
            }
        }
        DOM.addClassName(relation, "Relation");
        content.appendChild(relation);

        // add site-specific content here
        var extraContent = LiveJournal.run_hook("ctxpopup_extrainfo", data);
        if (extraContent) {
            content.appendChild(extraContent);
        }

        // member of community
        if (data.is_logged_in && data.is_comm) {
            var membership      = document.createElement("span");
            var membershipLink  = document.createElement("a");

            var membership_action = data.is_member ? "leave" : "join";

            if (data.is_member) {
                membershipLink.href = data.url_leavecomm;
                membershipLink.innerHTML = "Leave";
            } else {
                membershipLink.href = data.url_joincomm;
                membershipLink.innerHTML = "Join community";
            }

            if (!ContextualPopup.disableAJAX) {
                DOM.addEventListener(membershipLink, "click", function (e) {
                    Event.prep(e);
                    Event.stop(e);
                    return ContextualPopup.changeRelation(data, ctxPopupId, membership_action, e); });
            }

            membership.appendChild(membershipLink);
            content.appendChild(membership);
        }

        // friend
        var friend;
        if (data.is_logged_in && ! data.is_requester) {
            friend = document.createElement("span");

            if (! data.is_friend) {
                // add friend link
                var addFriend = document.createElement("span");
                var addFriendLink = document.createElement("a");
                addFriendLink.href = data.url_addfriend;

                if (data.is_comm)
                    addFriendLink.innerHTML = "Watch community";
                else if (data.is_syndicated)
                    addFriendLink.innerHTML = "Subscribe to feed";
                else
                    addFriendLink.innerHTML = "Add friend";

                addFriend.appendChild(addFriendLink);
                DOM.addClassName(addFriend, "AddFriend");

                if (!ContextualPopup.disableAJAX) {
                    DOM.addEventListener(addFriendLink, "click", function (e) {
                        Event.prep(e);
                        Event.stop(e);
                        return ContextualPopup.changeRelation(data, ctxPopupId, "addFriend", e); });
                }

                friend.appendChild(addFriend);
            } else {
                // remove friend link (omg!)
                var removeFriend = document.createElement("span");
                var removeFriendLink = document.createElement("a");
                removeFriendLink.href = data.url_addfriend;

                if (data.is_comm)
                    removeFriendLink.innerHTML = "Stop watching";
                else if (data.is_syndicated)
                    removeFriendLink.innerHTML = "Unsubscribe";
                else
                    removeFriendLink.innerHTML = "Remove friend";

                removeFriend.appendChild(removeFriendLink);
                DOM.addClassName(removeFriend, "RemoveFriend");

                if (!ContextualPopup.disableAJAX) {
                    DOM.addEventListener(removeFriendLink, "click", function (e) {
                        Event.stop(e);
                        return ContextualPopup.changeRelation(data, ctxPopupId, "removeFriend", e); });
                }

                friend.appendChild(removeFriend);
            }

            DOM.addClassName(relation, "FriendStatus");
        }

        // add a bar between stuff if we have community actions
        if (data.is_logged_in && data.is_comm)
            content.appendChild(bar.cloneNode(true));

        if (friend)
            content.appendChild(friend);

        // break
        if (data.is_logged_in && !data.is_requester) content.appendChild(document.createElement("br"));

        // view label
        var viewLabel = document.createElement("span");
        viewLabel.innerHTML = "View: ";
        content.appendChild(viewLabel);

        // journal
        if (data.is_person || data.is_comm || data.is_syndicated) {
            var journalLink = document.createElement("a");
            journalLink.href = data.url_journal;

            if (data.is_person)
                journalLink.innerHTML = "Journal";
            else if (data.is_comm)
                journalLink.innerHTML = "Community";
            else if (data.is_syndicated)
                journalLink.innerHTML = "Feed";

            content.appendChild(journalLink);
            content.appendChild(bar.cloneNode(true));
        }

        // profile
        var profileLink = document.createElement("a");
        profileLink.href = data.url_profile;
        profileLink.innerHTML = "Profile";
        content.appendChild(profileLink);

        // clearing div
        var clearingDiv = document.createElement("div");
        DOM.addClassName(clearingDiv, "ljclear");
        clearingDiv.innerHTML = "&nbsp;";
        content.appendChild(clearingDiv);

        ippu.setContentElement(inner);
    }
}

// ajax request to change relation
ContextualPopup.changeRelation = function (info, ctxPopupId, action, evt) {
    if (!info) return true;

    var postData = {
        "target": info.username,
        "action": action
    };

    // get the authtoken
    var authtoken = info[action + "_authtoken"];
    if (!authtoken) log("no auth token for action" + action);
    postData.auth_token = authtoken;

    // needed on journal subdomains
    var url = LJVAR.currentJournal ? "/" + LJVAR.currentJournal + "/__rpc_changerelation" : "/__rpc_changerelation";

    // callback from changing relation request
    var changedRelation = function (data) {
        if (ContextualPopup.hourglass) ContextualPopup.hideHourglass();

        if (data.error) {
            ContextualPopup.showNote(data.error, ctxPopupId);
            return;
        }

        if (data.note)
        ContextualPopup.showNote(data.note, ctxPopupId);

        if (!data.success) return;

        if (ContextualPopup.cachedResults[ctxPopupId + ""]) {
            var updatedProps = ["is_friend", "is_member"];
            updatedProps.forEach(function (prop) {
                ContextualPopup.cachedResults[ctxPopupId + ""][prop] = data[prop];
            });
        }

        // if the popup is up, reload it
        ContextualPopup.renderPopup(ctxPopupId);
    };

    var opts = {
        "data": HTTPReq.formEncoded(postData),
        "method": "POST",
        "url": url,
        "onError": ContextualPopup.gotError,
        "onData": changedRelation
    };

    // do hourglass at mouse coords
    var mouseCoords = DOM.getAbsoluteCursorPosition(evt);
    if (!ContextualPopup.hourglass && mouseCoords) {
        ContextualPopup.hourglass = new Hourglass();
        ContextualPopup.hourglass.init(null, "lj_hourglass");
        ContextualPopup.hourglass.add_class_name("ContextualPopup"); // so mousing over hourglass doesn't make ctxpopup think mouse is outside
        ContextualPopup.hourglass.hourglass_at(mouseCoords.x, mouseCoords.y);
    }

    HTTPReq.getJSON(opts);

    return false;
}

// create a little popup to notify the user of something
ContextualPopup.showNote = function (note, ctxPopupId) {
    var ele;

    if (ContextualPopup.ippu) {
        // pop up the box right under the element
        ele = ContextualPopup.ippu.getElement();
    } else {
        if (ctxPopupId) {
            var ele = ContextualPopup.elements[ctxPopupId + ""];
        }
    }

    LJ_IPPU.showNote(note, ele);
}

ContextualPopup.hidePopup = function (ctxPopupId) {
    if (ContextualPopup.hourglass) ContextualPopup.hideHourglass();

    // destroy popup for now
    if (ContextualPopup.ippu) {
        ContextualPopup.ippu.hide();
        ContextualPopup.ippu = null;
    }
}

// do ajax request of user info
ContextualPopup.getInfo = function (target) {
    var ctxPopupId = target.ctxPopupId;
    var username = target.username;
    var userid = target.userid;
    var up_url = target.up_url;

    if (!ctxPopupId)
    return;

    if (ContextualPopup.currentRequests[ctxPopupId + ""]) {
        return;
    }

    ContextualPopup.currentRequests[ctxPopupId] = 1;

    if (!username) username = "";
    if (!userid) userid = 0;
    if (!up_url) up_url = "";

    var params = HTTPReq.formEncoded ({
        "user": username,
            "userid": userid,
            "userpic_url": up_url,
            "mode": "getinfo"
    });

    // needed on journal subdomains
    var url = LJVAR.currentJournal ? "/" + LJVAR.currentJournal + "/__rpc_ctxpopup" : "/__rpc_ctxpopup";

    // got data callback
    var gotInfo = function (data) {
        if (ContextualPopup.hourglass) ContextualPopup.hideHourglass();

        ContextualPopup.cachedResults[ctxPopupId] = data;

        if (data.error) {
            if (data.noshow) return;

            ContextualPopup.showNote(data.error, ctxPopupId);
            return;
        }

        if (data.note)
        ContextualPopup.showNote(data.note, data.ctxPopupId);

        ContextualPopup.currentRequests[ctxPopupId] = null;

        ContextualPopup.renderPopup(ctxPopupId);

        // expire cache after 5 minutes
        setTimeout(function () {
            ContextualPopup.cachedResults[ctxPopupId] = null;
        }, 60 * 1000);
    };

    HTTPReq.getJSON({
        "url": url,
            "method" : "GET",
            "data": params,
            "onData": gotInfo,
            "onError": ContextualPopup.gotError
            });
}

ContextualPopup.hideHourglass = function () {
    if (ContextualPopup.hourglass) {
        ContextualPopup.hourglass.hide();
        ContextualPopup.hourglass = null;
    }
}

ContextualPopup.gotError = function (err) {
    if (ContextualPopup.hourglass) ContextualPopup.hideHourglass();

    if (ContextualPopup.debug)
        ContextualPopup.showNote("Error: " + err);
}

// when page loads, set up contextual popups
LiveJournal.register_hook("page_load", ContextualPopup.setup);

Horizon = new Object;

// Holds style info for the top level menu
Horizon.toplevel_hover_style = null;
// Holds the last menu opened
Horizon.opened_menu = null;
// Holds Select elements tht have been hidden
Horizon.hiddenSelects = [];
// Is IE version 6 or lower flag
Horizon.isIE6down = false;

Horizon.scheme_init = function () {
    Horizon.toplevel_hover_style = DOM.getComputedStyle($("Alpha"));
    Horizon.setIsIE6down();
}

Horizon.open_menu = function (m) {
    /* Needed to nicely turn on top level menu items for IE */
    if (Horizon.toplevel_hover_style) {
        m.style.backgroundImage = Horizon.toplevel_hover_style.backgroundImage;
        m.style.borderColor = Horizon.toplevel_hover_style.borderColor;
        m.style.borderWidth = Horizon.toplevel_hover_style.borderWidth;
        m.style.borderStyle = Horizon.toplevel_hover_style.borderStyle;
        var links = m.getElementsByTagName('a');
        links[0].style.color = Horizon.toplevel_hover_style.color;
    }

    Horizon.hideSelects();
    var menus = m.getElementsByTagName('ul');
    for (var i = 0; i <= menus.length; i++) {
        if (!menus[i]) return;
        menus[i].style.display = "block";
    }

    Event.stop(e);
    return false;
}

Horizon.close_menu = function (m) {
    if (Horizon.toplevel_hover_style) {
        m.style.backgroundImage = '';
        m.style.borderColor = '';
        m.style.borderWidth = '';
        m.style.borderStyle = '';
        var links = m.getElementsByTagName('a');
        links[0].style.color = '';
    }

    Horizon.showSelects();
    var menus = m.getElementsByTagName('ul');
    for (var i = 0; i <= menus.length; i++) {
        if (!menus[i]) return;
        menus[i].style.display = "none";
    }

    Event.stop(e);
    return false;
}

Horizon.mouseMove = function (evt) {
    Event.prep(evt);
    if (!evt.target) return;

    var ancestors = DOM.getAncestorsByClassName(evt.target, "NavMenuItem", true);
    if (ancestors.length) {
        if (!Horizon.opened_menu || Horizon.opened_menu != ancestors[0])
            Horizon.open_menu(ancestors[0]);
        if (Horizon.opened_menu && Horizon.opened_menu != ancestors[0])
            Horizon.close_menu(Horizon.opened_menu);
        Horizon.opened_menu = ancestors[0];
    } else {
        if (Horizon.opened_menu) Horizon.close_menu(Horizon.opened_menu);
        Horizon.opened_menu = null;
    }
}

Horizon.hideSelects = function () {
    if (!Horizon.isIE6down) return;
    var sels = document.getElementsByTagName("select");
    var ele;
    for (var i = 0; i < sels.length; i++) {
        ele = sels[i];
        if (!ele) continue;

        if (ele.style.visibility != 'hidden' && ele.className == 'hideable') {
            ele.style.visibility = 'hidden';
            Horizon.hiddenSelects.push(ele);
        }
    }
}

Horizon.showSelects = function () {
    var ele;
    while (ele = Horizon.hiddenSelects.pop())
        ele.style.visibility = '';
}

Horizon.setIsIE6down = function () {
    var UA = navigator.userAgent.toLowerCase();
    if ((UA.indexOf('msie 5') != -1) || (UA.indexOf('msie 6') != -1))
        Horizon.isIE6down = true;
}

/* Try to speed up page load in Mozilla */
if (window.controllers) {
    document.addEventListener("DOMContentLoaded", Horizon.scheme_init, null);
    document.addEventListener("mousemove", Horizon.mouseMove, document);
} else {
    DOM.addEventListener(window, "load", Horizon.scheme_init);
    DOM.addEventListener(document, "mousemove", Horizon.mouseMove);
}

// ljtalk for ctxpopup
LiveJournal.register_hook("ctxpopup_extrainfo", function (userdata) {
    var content = document.createElement("div");

    if (userdata.is_person) {
        if (userdata.is_online !== '') {
            // online status
            var onlineStatusLabel = document.createElement("span");
            var jabberTitle = userdata.jabber_title;
            onlineStatusLabel.innerHTML = jabberTitle + ": ";
            DOM.addClassName(onlineStatusLabel, "OnlineStatus");
            content.appendChild(onlineStatusLabel);

            // build status
            var onlineStatus = document.createElement("span");


            var onlineStatusText = document.createElement("span");
            onlineStatus.appendChild(onlineStatusText);

            content.appendChild(onlineStatus);

            if (userdata.is_online) {
                var imLink = document.createElement("a");
                imLink.href = "gizmoljtalk://sendMessage?id=" + userdata.username;

                var imIcon = document.createElement("img");
                imIcon.src = LJVAR.imgprefix + "/ljtalk_im.gif";
                onlineStatusText.innerHTML = "Online";
                imIcon.title = imIcon.alt = "IM this user";

                imLink.appendChild(imIcon);
                onlineStatus.appendChild(imLink);

                var callLink = document.createElement("a");
                callLink.href = "gizmoljtalk://call?id=" + userdata.username;

                var callIcon = document.createElement("img");
                callIcon.src = LJVAR.imgprefix + "/ljtalk_call.gif";
                callIcon.title = callIcon.alt = "Call this user";

                callLink.appendChild(callIcon);
                onlineStatus.appendChild(callLink);
            } else if (userdata.is_online == '0') {
                onlineStatusText.innerHTML = "Offline";
            }
        }

        // gizmo download link
        if (userdata.jabber_show_download) {
            var gizmoBlurb = document.createElement("div");
            var dl_link = userdata.jabber_link;
            var download_title = userdata.jabber_download_title;
            gizmoBlurb.innerHTML = "(Download <a href='" + dl_link + "'>" + download_title + "</a>)";
            DOM.addClassName(gizmoBlurb, "GizmoBlurb");
            content.appendChild(gizmoBlurb);
        }
    }

    return content;
});
