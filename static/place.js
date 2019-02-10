(function() {
  function start() {
    const source = new EventSource('/place.events');

    source.addEventListener('put', function (evt) {
      const { data = {} } = evt;
      const json = JSON.parse(data);
      const { path, data: d } = json;

      const item = byId(path).firstElementChild.firstElementChild;

      Object.keys(d.style).forEach((k) => {
        item.style[k] = d.style[k];
      });
    });
  }

  window.addEventListener('load', start);
})();
