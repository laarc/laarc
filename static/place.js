// function placeConnect() {
//   if (typeof placeSrc !== "undefined") {
//     placeSrc.removeEventListener('put', onPut);
//     placeSrc.close();
//   }
//   placeSrc = new EventSource('/place.events');
//   placeSrc.addEventListener('put', placeOnPut);
//   return placeSrc;
// }

// function placeOnPut(evt) {
//   const { data = {} } = evt;
//   const json = JSON.parse(data);
//   const { path, data: d } = json;
//   placePut(path, d.style)
// }

//window.addEventListener('load', placeConnect);

function placePut(id, styles) {
  const item = byId(id).firstElementChild.firstElementChild;
  Object.keys(styles).forEach((k) => {
    item.style[k] = styles[k];
  });
  return item;
}

function placeSet(id, value, key = {style: "backgroundColor"}) {
  const item = byId(id).firstElementChild.firstElementChild;
  var i = item;
  while (typeof key === "object") {
    for (let k in key) {
      i = i[k];
      key = key[k];
      break;
    }
  }
  i[key] = value;
  return item;
}

function placeGet(id, key = {style: "backgroundColor"}) {
  const item = byId(id).firstElementChild.firstElementChild;
  var i = item;
  while (typeof key === "object") {
    for (let k in key) {
      i = i[k];
      key = key[k];
      break;
    }
  }
  return key ? i[key] : i;
}

function theArgs() {
    var vars = {};
    var parts = window.location.href.replace(/[?&]+([^=&]+)=([^&]*)/gi, function(m,key,value) {
        vars[key] = value;
    });
    return vars;
}

function theArg(key, ifUnset) {
  var x = theArgs()[key];
  return (x != null) ? x : ifUnset;
}

function placeFrom() {
  placeGet(theArg("from"))
}

function placeSubmit(node) {
  const el = node.parentElement;
  const to = el.id;
  const from = theArg("from");
  if (from) {
    if (to !== from && to && to.indexOf(",0") < 0) {
      placeSet(to, placeGet(from));

      // ping server
      var ping = new Image();
      ping.src = "/placeset?from="+from+"&to="+to;
      return false;
    }
  }
}

