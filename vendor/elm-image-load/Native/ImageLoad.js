Elm.Native.ImageLoad = {};
Elm.Native.ImageLoad.make = function(elm) {

  elm.Native = elm.Native || {};
  elm.Native.ImageLoad = elm.Native.ImageLoad || {};
  if (elm.Native.ImageLoad.values)
  {
    return elm.Native.ImageLoad.values;
  }

  var Task = Elm.Native.Task.make(elm);
  var Json = Elm.Native.Json.make(elm);

  function load(url, decoder)
  {
    return Task.asyncFunction(function (callback) {
      var img = new Image();
      img.onload = function () {
        var value = A2(Json.runDecoderValue, decoder, img);
        if (value.ctor === 'Ok')
        {
          callback(Task.succeed(value._0));
        } else {
          callback(Task.fail({ctor: 'DecodeError'}));
        }
      };
      img.onerror = img.onabort = function () {
        callback(Task.fail({ctor: 'LoadError'}));
      };
      img.src = url;
    });
  }

  elm.Native.ImageLoad.values = {
    load: F2(load)
  };

  return elm.Native.ImageLoad.values;
};
