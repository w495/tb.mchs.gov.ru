

qx.Class.define("bsk.util.utils",
{
    type : "static",

    statics : {
        
        coordLenLimit : 4,
        addrLenLimit : 55,

        mkSpeedStr : function(sp) {
            var speed = "неизвестно";
            try {
                if(sp != "" && sp != "null")
                    speed = (sp*1).toFixed(1);
                }
            catch(err) {
            }
            return speed;                                                                               
        },

        parseCoord : function(str) {
            if(!str || str == "")
                return '';
            var c = null;
            try {
                c = qx.util.Json.parse(str);
            }
            catch(er) {
                var V = str.match("POINT\\((.*)\\)");
                var point = V[1].split(" ");
                point[0] = point[0] * 1;
                point[1] = point[1] * 1;
                c = {coordinates : point};
            }
            return c;                                                                                                                                                    
        },

        formatDTItem : function(i) {
            if(String(i).length == 1)
                i = "0" + i;
            return i;
        },

        formatTime : function(hour, min, sec) {
            var ret = "" + this.formatDTItem(hour) +
                ":" +
                this.formatDTItem(min);
            if(sec != undefined)
                return ret + ":" + this.formatDTItem(sec);
            return ret;
        },

        formatDate : function(year, month, day) {
            return "" + year + "." +
                this.formatDTItem(month) +
                "."
                + this.formatDTItem(day);
        },

        formatJsDateTime : function(dt) {
            return this.formatDate(dt.getFullYear(),
                dt.getMonth() + 1,
                dt.getDate()) +
                " " +
                this.formatTime(dt.getHours(),
                                  dt.getMinutes(),
                                  dt.getSeconds());
        },

        formatJsTime : function(dt) {
            return this.formatTime(dt.getHours(),
                                   dt.getMinutes(),
                                   dt.getSeconds());
        },

        getDateLocal : function(str, offset) {
            var arr = str.split(" ");
            var ld = new Date();
            if(arr.length >= offset + 5) {
                ld.setFullYear(arr[offset]);
                ld.setMonth(arr[offset+1]-1);
                ld.setDate(arr[offset+2]);
                ld.setHours(arr[offset+3]);
                ld.setMinutes(arr[offset+4]);
                ld.setSeconds(arr[offset+5]);
            }
            return ld;
        },

 
        getDate : function(str, offset) {
            var arr = str.split(" ");
            var ld = new Date();
            if(arr.length >= offset + 5) {
                ld.setUTCFullYear(arr[offset]);
                ld.setUTCMonth(arr[offset+1]-1);
                ld.setUTCDate(arr[offset+2]);
                ld.setUTCHours(arr[offset+3]);
                ld.setUTCMinutes(arr[offset+4]);
                ld.setUTCSeconds(arr[offset+5]);
            }
            return ld;
        },

        chopCoord : function(c, len) {
            var fc = c*1;
            var toLen = len || this.coordLenLimit;
            return fc.toFixed(toLen);
            //if(fc < 0)
            //    fLen = fLen - 1;
            //return fc.toPrecision(toLen + fLen);

        },

        chopAddrName : function(a) {
            if(a.length > this.addrLenLimit) {
                return a.slice(0, this.addrLenLimit) + "...";
            }
            else {
                return a;
            }
        },

        /**
            Создает полную копию объекта
        **/
        clone : function(obj){
            var Clone = function(){};
            Clone.prototype = obj;
            return new Clone();
        },

        parseBoolean : function (str) {
            return /^true$/i.test(str);
        },
        
        /** *******************************************************************
                Строковые операции
            *******************************************************************
        **/
        
        trim: function (input) {
            if(input.trim)
                return input.trim();
            var output = qx.lang.String.trim(input);
            if(output == input){
                output = output.replace(/^\s*([\S\s]*)\b\s*$/, '$1');
            }
            return output;
        },
        
        capitalize: function (input) {
             return input.charAt(0).toUpperCase() + input.slice(1);
        },

        kill_tags: function (input) {
             input = input.replace(/&/g, '&amp;');
             input = input.replace(/</g, '&lt;');
             input = input.replace(/>/g, '&gt;');
             return input;
        },

        unkill_tags: function (input) {
             input = input.replace(/&lt;/g, '<');
             input = input.replace(/&gt;/g, '>');
             input = input.replace(/&amp;/g, '&');
             return input;
        },
        
        /**
            Нормировка строк.
            Строка в номальных языках начинается
                с заглавной буквы и не с пробела.
            Символы < и > преобразуем.
         **/
        normalize_string: function (input) {
             var output = bsk.util.utils.trim(input);
             output = bsk.util.utils.capitalize(output);
             output = bsk.util.utils.kill_tags(output);
             return output;
        },

        /**
            Обратная нормировка строк.
            Символы < и > преобразуем обратно.
         **/
        unnormalize_string: function (input) {
             input = bsk.util.utils.unkill_tags(input);
             return input;
        },

        normalize_date: function (input) {
             if(input.getTime)
                return input.getTime()
            return "62167219200" // {{1970,1,1},{0,0,0}}
        },

        unnormalize_date: function (input) {
            return new Date(input)
        },
        
        /**
            Нормировка для различных типов данных
         **/
        normalize: function (input) {
            console.log(input, "normalize " + typeof(input))
            
            if(typeof(input)=='string')
                return bsk.util.utils.normalize_string(input);
            if(typeof(input)=='object'){
                if(input instanceof Date)
                    return bsk.util.utils.normalize_date(input);
            }
            //... 
            return input;
        },
        
        /**
            Обратная нормировка для различных типов данных
         **/
        unnormalize: function (input) {
            console.log(input, "unnormalize " + typeof(input))
            if(typeof(input)=='string')
                return bsk.util.utils.unnormalize_string(input);
            if(typeof(input)=='object'){
                if(input instanceof Date)
                    return bsk.util.utils.unnormalize_date(input);
            }
            //... 
            return input;
        },

        
        _keyStr : "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/=",

        // public method for encoding
        encode : function (input) {
            var output = "";
            var chr1, chr2, chr3, enc1, enc2, enc3, enc4;
            var i = 0;

            input = bsk.util.utils._utf8_encode(input);

            while (i < input.length) {

                chr1 = input.charCodeAt(i++);
                chr2 = input.charCodeAt(i++);
                chr3 = input.charCodeAt(i++);

                enc1 = chr1 >> 2;
                enc2 = ((chr1 & 3) << 4) | (chr2 >> 4);
                enc3 = ((chr2 & 15) << 2) | (chr3 >> 6);
                enc4 = chr3 & 63;

                if (isNaN(chr2)) {
                    enc3 = enc4 = 64;
                } else if (isNaN(chr3)) {
                    enc4 = 64;
                }

                output = output +
                bsk.util.utils._keyStr.charAt(enc1) + bsk.util.utils._keyStr.charAt(enc2) +
                bsk.util.utils._keyStr.charAt(enc3) + bsk.util.utils._keyStr.charAt(enc4);

            }

            return output;
        },

        // public method for decoding
        decode : function (input) {
            var output = "";
            var chr1, chr2, chr3;
            var enc1, enc2, enc3, enc4;
            var i = 0;

            input = input.replace(/[^A-Za-z0-9\+\/\=]/g, "");

            while (i < input.length) {

                enc1 = bsk.util.utils._keyStr.indexOf(input.charAt(i++));
                enc2 = bsk.util.utils._keyStr.indexOf(input.charAt(i++));
                enc3 = bsk.util.utils._keyStr.indexOf(input.charAt(i++));
                enc4 = bsk.util.utils._keyStr.indexOf(input.charAt(i++));

                chr1 = (enc1 << 2) | (enc2 >> 4);
                chr2 = ((enc2 & 15) << 4) | (enc3 >> 2);
                chr3 = ((enc3 & 3) << 6) | enc4;

                output = output + String.fromCharCode(chr1);

                if (enc3 != 64) {
                    output = output + String.fromCharCode(chr2);
                }
                if (enc4 != 64) {
                    output = output + String.fromCharCode(chr3);
                }

            }

            output = bsk.util.utils._utf8_decode(output);

            return output;

        },

        // private method for UTF-8 encoding
        _utf8_encode : function (string) {
            string = string.replace(/\r\n/g,"\n");
            var utftext = "";

            for (var n = 0; n < string.length; n++) {

                var c = string.charCodeAt(n);

                if (c < 128) {
                    utftext += String.fromCharCode(c);
                }
                else if((c > 127) && (c < 2048)) {
                    utftext += String.fromCharCode((c >> 6) | 192);
                    utftext += String.fromCharCode((c & 63) | 128);
                }
                else {
                    utftext += String.fromCharCode((c >> 12) | 224);
                    utftext += String.fromCharCode(((c >> 6) & 63) | 128);
                    utftext += String.fromCharCode((c & 63) | 128);
                }

            }

            return utftext;
        },

        // private method for UTF-8 decoding
        _utf8_decode : function (utftext) {
            var string = "";
            var i = 0;
            var c = 0;
            var c1 = 0;
            var c2 = 0;
            var c3 = 0;

            while ( i < utftext.length ) {

                c = utftext.charCodeAt(i);

                if (c < 128) {
                    string += String.fromCharCode(c);
                    i++;
                }
                else if((c > 191) && (c < 224)) {
                    c2 = utftext.charCodeAt(i+1);
                    string += String.fromCharCode(((c & 31) << 6) | (c2 & 63));
                    i += 2;
                }
                else {
                    c2 = utftext.charCodeAt(i+1);
                    c3 = utftext.charCodeAt(i+2);
                    string += String.fromCharCode(((c & 15) << 12) | ((c2 & 63) << 6) | (c3 & 63));
                    i += 3;
                }

            }

            return string;
        }
    }
});
