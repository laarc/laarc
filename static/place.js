placeKilled = false;

function placeConnect() {
  if (typeof placeSrc === "undefined") {
    placeSrc = new EventSource('/place.events');
    placeSrc.addEventListener('kill', placeOnKill);
    placeSrc.addEventListener('put', placeOnPut);
  }
  return placeSrc;
}

placeSeen = Object.create(null);

function placeEvent(id, evt) {
  if (id && !placeSeen[id]) {
    placeSeen[id] = evt;
    return true;
  }
  return false;
}

function placeOnKill(evt) {
  const { data = {} } = evt;
  const json = JSON.parse(data);
  const { id, path, data: d } = json;
  if (placeEvent(id, evt)) {
    console.log("Received kill event")
    console.dir(placeKillEvt = evt);
    placeKilled = true;
    if (typeof placeSrc !== "undefined") {
      placeSrc.removeEventListener('put', placeOnPut);
      placeSrc.removeEventListener('kill', placeOnKill);
      placeSrc.close();
      delete placeSrc;
    }
  }
}

function placeOnPut(evt) {
  const { data = {} } = evt;
  const json = JSON.parse(data);
  const { id, path, data: d } = json;
  if (placeEvent(id, evt)) {
    console.log("Received put event");
    console.dir(placePutEvt = evt);
    placePut(path, d.style)
  }
}

window.addEventListener('load', placeConnect);

function placePut(id, styles) {
  const item = byId(id).firstElementChild.firstElementChild;
  Object.keys(styles).forEach((k) => {
    item.style[k] = styles[k];
  });
  return item;
}

function placeSet(id, value, key = {style: "backgroundColor"}) {
  const el = byId(id);
  if (el) {
    const item = el.firstElementChild.firstElementChild;
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
}

function placeGet(id, key = {style: "backgroundColor"}) {
  const el = byId(id);
  if (el) {
    const item = el.firstElementChild.firstElementChild;
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


function placeFromDefault() {
  if (typeof placeAnchor === "undefined") {
    placeSet(theArg("from"), null, "value");
  }
  placeAnchor = ((typeof placeAnchor === "undefined") ? undefined : placeAnchor);
  return placeAnchor;
}

function placeFrom(set = placeFromDefault()) {
  placeSet(placeAnchor, null, "value")
  placeAnchor = set;
  placeSet(placeAnchor, "x", "value");
  return placeAnchor;
}

function placeSubmit(node) {
  const el = node.parentElement;
  const to = el.id;
  const from = placeFrom();
  if (from) {
    if (to === from) {
      placeFrom("");
      return false;
    } else if (to && to.indexOf(",0") >= 0) {
      placeFrom(to);
      return false;
    } else {
      placeSet(to, placeGet(from));

      // ping server
      var ping = new Image();
      ping.src = "/placeset?from="+from+"&to="+to;
      if (!placeKilled) {
        placeConnect();
      }
      return false;
    }
  } if (to) {
    placeFrom(to)
    return false;
  }
}

