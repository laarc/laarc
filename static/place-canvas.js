const place_url = '/place.json';

let place_colors = {};
let place_board = {};
let place_inf;
let place_canvas;
let place_ctx;

const place_size = 12;

// Prepare some variables for the dragging gestures logic
let place_mouseIsDown = false;
let place_isPanning = false;
let place_previousMousePosition;

function place_windowKeydown(evt) {
  if (evt.which === 68) {
    place_isPanning = !place_isPanning;
  }
}

function place_canvasMousedown(event) {
  if (event.which === 1) {
    place_mouseIsDown = true;
  } else {
    place_isPanning = true;
  }
}

function place_windowMouseup(event) {
  if (event.which === 1) {
    place_mouseIsDown = false;
    place_inf.updateChunks();
  } else {
    place_isPanning = false;
  }
}

function place_windowMousemove(event) {
  var newMousePosition = { x: event.offsetX, y: event.offsetY };
  if (place_mouseIsDown && place_isPanning) {
    // pan the canvas whenever dragging with the middle or right mouse button
    var dx = place_previousMousePosition.x - newMousePosition.x;
    var dy = place_previousMousePosition.y - newMousePosition.y;
    // Canvas gets really messy if you do not clear it up :)
    place_ctx.clearRect(0, 0, place_canvas.width, place_canvas.height);
    place_inf.moveBy(dx, dy);
  } else if (place_mouseIsDown) {
    // draw lines when dragging with the left mouse button
    if (place_previousMousePosition) {
      place_ctx.beginPath();
      place_ctx.moveTo(place_previousMousePosition.x, place_previousMousePosition.y);
      place_ctx.lineTo(newMousePosition.x, newMousePosition.y);
      place_ctx.stroke();
    }
  }
  place_previousMousePosition = newMousePosition;
}

function place_addListeners() {
  window.addEventListener('keydown', place_windowKeydown);
  place_canvas.addEventListener('mousedown', place_canvasMousedown);
  window.addEventListener("mouseup", place_windowMouseup);
  window.addEventListener("mousemove", place_windowMousemove);
}

function place_replaceTable() {
  const hnMain = document.getElementById('hnmain');
  const mainRect = hnMain.getBoundingClientRect();
  const { width } = mainRect;
  const oldPlace = document.getElementById('place');
  const parent = oldPlace.parentElement;
  parent.removeChild(oldPlace);
  const place = document.createElement('div');
  place.id = 'place';
  parent.append(place);
  place_canvas = document.createElement('canvas');
  place_canvas.id = 'canvas';
  place_canvas.width = width.toString();
  place_canvas.height = window.innerHeight.toString();
  place.appendChild(place_canvas);
  place_ctx = place_canvas.getContext('2d');
  place_inf = infiniteCanvas.initialize(place_ctx);
  place_addListeners();
}

function place_drawBoard() {
  place_board.forEach((row, rowIndex) => {
    row.forEach((pixel, pixIndex) => {
      var color = place_colors[pixel] || { hex: 'black' };
      place_ctx.fillStyle = color.hex;
      place_ctx.fillRect(
        place_size * pixIndex,
        place_size * rowIndex,
        place_size,
        place_size,
      );
    });
  });
}

function place_getBoard() {
  fetch(place_url).then(resp => resp.json()).then(json => {
    place_colors = json.colors;
    place_board = json.board;
    place_drawBoard();
  });
}

function place_start() {
  place_replaceTable();
  place_getBoard();
}

window.addEventListener('load', place_start);
