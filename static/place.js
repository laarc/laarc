(function() {
  let t;
  let source;

  function makeSource() {
    if (source) {
      source.removeEventListener('put', onPut);
      source.close();
    }
    source = new EventSource('/place.events');
    source.addEventListener('put', onPut);
  }

  function onPut(evt) {
    const { data = {} } = evt;
    const json = JSON.parse(data);
    const { path, data: d } = json;
    const item = byId(path).firstElementChild.firstElementChild;
    Object.keys(d.style).forEach((k) => {
      item.style[k] = d.style[k];
    });
  }

  function start() {
    makeSource();
    t = setInterval(() => {
      makeSource();
    }, 10000);
  }

  //window.addEventListener('load', start);
})();
