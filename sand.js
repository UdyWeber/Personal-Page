(function () {
  "use strict";

  var COLS = 300;
  var ROWS = 200;
  var grid = new Uint16Array(COLS * ROWS);
  var hue = 1;
  var mouseDown = false;
  var mouseX = 0;
  var mouseY = 0;
  var canvas = null;
  var ctx = null;
  var imageData = null;
  var pixels = null;
  var running = false;

  function idx(x, y) {
    return y * COLS + x;
  }

  function getCell(x, y) {
    if (x < 0 || x >= COLS || y < 0 || y >= ROWS) return 65535;
    return grid[idx(x, y)];
  }

  function setCell(x, y, val) {
    if (x >= 0 && x < COLS && y >= 0 && y < ROWS) {
      grid[idx(x, y)] = val;
    }
  }

  function updatePhysics() {
    for (var y = ROWS - 2; y >= 0; y--) {
      var startX, endX, stepX;
      if (y & 1) {
        startX = 0; endX = COLS; stepX = 1;
      } else {
        startX = COLS - 1; endX = -1; stepX = -1;
      }
      for (var x = startX; x !== endX; x += stepX) {
        var cell = grid[idx(x, y)];
        if (cell === 0) continue;

        var below = getCell(x, y + 1);
        if (below === 0) {
          setCell(x, y, 0);
          setCell(x, y + 1, cell);
          continue;
        }

        var dir = ((x + y) & 1) ? 1 : -1;
        var bl = getCell(x + dir, y + 1);
        var br = getCell(x - dir, y + 1);

        if (bl === 0) {
          setCell(x, y, 0);
          setCell(x + dir, y + 1, cell);
        } else if (br === 0) {
          setCell(x, y, 0);
          setCell(x - dir, y + 1, cell);
        }
      }
    }
  }

  function spawnSand() {
    if (!mouseDown || !canvas) return;

    var rect = canvas.getBoundingClientRect();
    var scaleX = COLS / rect.width;
    var scaleY = ROWS / rect.height;
    var cx = Math.floor(mouseX * scaleX);
    var cy = Math.floor(mouseY * scaleY);
    var radius = 8;

    for (var dy = -radius; dy <= radius; dy++) {
      for (var dx = -radius; dx <= radius; dx++) {
        if (dx * dx + dy * dy > radius * radius) continue;
        var gx = cx + dx;
        var gy = cy + dy;
        if (gx >= 0 && gx < COLS && gy >= 0 && gy < ROWS && grid[idx(gx, gy)] === 0) {
          grid[idx(gx, gy)] = hue;
        }
      }
    }
  }

  function hsvToRgb(h) {
    var hf = h / 60.0;
    var hi = Math.floor(hf);
    var f = hf - hi;
    var q = 1.0 - f;
    var r, g, b;
    switch (hi) {
      case 0: r = 1; g = f; b = 0; break;
      case 1: r = q; g = 1; b = 0; break;
      case 2: r = 0; g = 1; b = f; break;
      case 3: r = 0; g = q; b = 1; break;
      case 4: r = f; g = 0; b = 1; break;
      default: r = 1; g = 0; b = q; break;
    }
    return [(r * 255) | 0, (g * 255) | 0, (b * 255) | 0];
  }

  var colorLUT = new Uint8Array(361 * 3);
  for (var h = 0; h <= 360; h++) {
    var rgb = hsvToRgb(h);
    colorLUT[h * 3] = rgb[0];
    colorLUT[h * 3 + 1] = rgb[1];
    colorLUT[h * 3 + 2] = rgb[2];
  }

  function render() {
    if (!ctx || !imageData) return;

    var data = pixels;
    var i4 = 0;
    for (var i = 0; i < COLS * ROWS; i++) {
      var val = grid[i];
      if (val > 0) {
        var ci = val * 3;
        data[i4] = colorLUT[ci];
        data[i4 + 1] = colorLUT[ci + 1];
        data[i4 + 2] = colorLUT[ci + 2];
        data[i4 + 3] = 255;
      } else {
        data[i4] = 0;
        data[i4 + 1] = 0;
        data[i4 + 2] = 0;
        data[i4 + 3] = 255;
      }
      i4 += 4;
    }
    ctx.putImageData(imageData, 0, 0);
  }

  function frame() {
    if (!running) return;
    updatePhysics();
    spawnSand();
    hue = hue >= 360 ? 1 : hue + 1;
    render();
    requestAnimationFrame(frame);
  }

  function setupCanvas(container) {
    canvas = document.createElement("canvas");
    canvas.width = COLS;
    canvas.height = ROWS;
    canvas.style.display = "block";
    canvas.style.width = "100%";
    canvas.style.height = "100%";
    canvas.style.imageRendering = "pixelated";
    ctx = canvas.getContext("2d");
    imageData = ctx.createImageData(COLS, ROWS);
    pixels = imageData.data;
    container.appendChild(canvas);

    // mouse events directly on the canvas
    canvas.addEventListener("mousedown", function (e) {
      mouseDown = true;
      mouseX = e.offsetX;
      mouseY = e.offsetY;
      focused = true;
      hideOverlay();
    });
    canvas.addEventListener("mouseup", function () {
      mouseDown = false;
    });
    canvas.addEventListener("mousemove", function (e) {
      if (e.buttons & 1) {
        mouseDown = true;
      }
      mouseX = e.offsetX;
      mouseY = e.offsetY;
    });
    canvas.addEventListener("mouseleave", function () {
      mouseDown = false;
    });

    // touch events for mobile
    canvas.addEventListener("touchstart", function (e) {
      e.preventDefault();
      var touch = e.touches[0];
      var rect = canvas.getBoundingClientRect();
      mouseDown = true;
      mouseX = touch.clientX - rect.left;
      mouseY = touch.clientY - rect.top;
      focused = true;
      hideOverlay();
    }, { passive: false });
    canvas.addEventListener("touchmove", function (e) {
      e.preventDefault();
      var touch = e.touches[0];
      var rect = canvas.getBoundingClientRect();
      mouseX = touch.clientX - rect.left;
      mouseY = touch.clientY - rect.top;
    }, { passive: false });
    canvas.addEventListener("touchend", function () {
      mouseDown = false;
    });
  }

  // "click me and become god" overlay
  var overlay = null;

  function createOverlay(container) {
    overlay = document.createElement("div");
    overlay.style.cssText = [
      "position:absolute", "top:0", "left:0", "width:100%", "height:100%",
      "display:flex", "align-items:center", "justify-content:center",
      "flex-direction:column", "gap:12px",
      "background:rgba(0,0,0,0.7)", "z-index:10", "cursor:pointer",
      "transition:opacity 0.5s ease"
    ].join(";");

    var label = document.createElement("div");
    label.textContent = "click me and become god";
    label.style.cssText = [
      "font-family:'Space Mono',monospace", "font-size:14px",
      "color:rgba(168,85,247,0.6)", "letter-spacing:0.15em",
      "text-transform:uppercase", "animation:blink 2s steps(1) infinite"
    ].join(";");

    var sub = document.createElement("div");
    sub.textContent = "// falling sand simulator";
    sub.style.cssText = [
      "font-family:'Space Mono',monospace", "font-size:10px",
      "color:rgba(168,85,247,0.25)", "letter-spacing:0.1em"
    ].join(";");

    overlay.appendChild(label);
    overlay.appendChild(sub);
    container.appendChild(overlay);

    overlay.addEventListener("click", function () {
      hideOverlay();
    });
  }

  function hideOverlay() {
    if (!overlay) return;
    overlay.style.opacity = "0";
    overlay.style.pointerEvents = "none";
  }

  var focused = false;

  // keyboard reset — only when sand panel is focused
  document.addEventListener("keyup", function (e) {
    if (e.key === "r" && focused) {
      grid.fill(0);
      hue = 1;
    }
  });

  // clear focus when clicking outside the sand container
  document.addEventListener("click", function (e) {
    var container = document.getElementById("sand-container");
    if (container && !container.contains(e.target)) {
      focused = false;
    }
  }, true);

  function tryStart() {
    if (running) return;
    var container = document.getElementById("sand-container");
    if (!container) {
      setTimeout(tryStart, 100);
      return;
    }
    setupCanvas(container);
    createOverlay(container);
    running = true;
    frame();
  }

  window.SandSim = { start: tryStart };
})();
