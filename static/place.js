(function() {
  const markValue = String.fromCharCode(215);
  let placeQuery;

  function sendRequest(url, options = {}, data) {
    const fetchOptions = Object.assign({}, options);

    if (data) fetchOptions.body = JSON.stringify(data);

    fetch(url, fetchOptions)
      .catch(err => console.log(err));
  }

  const makeFromQuery = (fromId) => `from=${fromId}`;
  const makePathString = (fromStr, toId) => `/placeop?${fromStr}&to=${toId}`;
  const url = (pathString) => {
    const { host, protocol } = location;
    return new URL(pathString, `${protocol}//${host}`);
  };

  function handleClick(evt) {
    const cell = evt.target.parentNode;

    if (placeQuery) {
      const inputVal = evt.target.getAttribute('value');

      if (inputVal === markValue) {
        placeQuery = undefined;
        evt.target.setAttribute('value', '');
      } else {
        const urlPath = makePathString(placeQuery, cell.id);
        const urlObj = url(urlPath);
        sendRequest(urlObj.href, {
          method: 'POST',
        });
      }
    } else {
      placeQuery = makeFromQuery(cell.id);
      evt.target.setAttribute('value', markValue);
    }
  }

  function listenForSubmit() {
    const buttons = document.querySelectorAll('#place tr td input');
    buttons.forEach((b) => {
      b.addEventListener('click', handleClick);
    });
  }

  const handlePutEvent = function (evt) {
    const { data = {} } = evt;
    const json = JSON.parse(data);
    const { path, data: d } = json;

    const item = byId(path).firstElementChild;

    Object.keys(d.style).forEach((k) => {
      item.style[k] = d.style[k];
    });
  };

  function startEvents() {
    const source = new EventSource('/place.events');

    source.addEventListener('put', handlePutEvent);

    source.addEventListener('kill', function () {
      source.removeEventListener('put', handlePutEvent);
    });
  }

  function init() {
    const { pathname } = location;
    if (pathname === '/l/place' || pathname === '/place') {
      listenForSubmit();
      startEvents();
    }
  }

  window.addEventListener('load', init);
})();
