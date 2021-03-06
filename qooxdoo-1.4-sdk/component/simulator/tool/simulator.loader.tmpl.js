(function(){

if(typeof print == "function") {
  print("Loading Simulator...");
}

if (!this.window) window = this;

if (!window.navigator) window.navigator = {
  userAgent: "Mozilla/5.0 (Macintosh; U; Intel Mac OS X 10_6_4; de-de) AppleWebKit/533.17.8 (KHTML, like Gecko) Version/5.0.1 Safari/533.17.8", 
  product: "", 
  cpuClass: "",
  language: "en-US"
};

if (!navigator.platform) navigator.platform = environment["os.name"];

if (!window.qx) window.qx = {};

if (!window.qxvariants) qxvariants = {};

if (!qx.$$environment) qx.$$environment = {};
var envinfo = %{EnvSettings};
for (var k in envinfo) qx.$$environment[k] = envinfo[k];

qx.$$packageData = {};
qx.$$loader = {};
})();

%{BootPart}

if (typeof exports != "undefined") {for (var key in qx) {exports[key] = qx[key];}}

simulator.Init.ready();