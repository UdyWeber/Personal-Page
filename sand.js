(function () {
  "use strict";

  var CONFIG = {
    COLS: 300,
    ROWS: 200,
    SPAWN_RADIUS: 8,
    CONTAINER_ID: "sand-container",
    BOTTOM_BAR_ID: "sand-bottom-bar"
  };

  var grid = new Uint16Array(CONFIG.COLS * CONFIG.ROWS);
  var hue = 1;
  var mouseDown = false;
  var mouseX = 0;
  var mouseY = 0;
  var canvas = null;
  var ctx = null;
  var imageData = null;
  var pixels = null;
  var running = false;

  // CRT WebGL state
  var glCanvas = null;
  var gl = null;
  var shaderProgram = null;
  var sandTexture = null;
  var uSand = null;
  var uPhase = null;
  var uTime = null;

  // Reset animation state
  var resetPhase = "idle"; // 'idle' | 'turning_off' | 'turning_on'
  var resetStartTime = 0;
  var TURN_OFF_MS = 600;
  var TURN_ON_MS = 500;

  function resetGrid() {
    grid.fill(0);
    hue = 1;
  }

  function triggerReset() {
    if (resetPhase !== "idle") return;
    if (!gl) {
      // No WebGL — instant reset
      resetGrid();
      return;
    }
    resetPhase = "turning_off";
    resetStartTime = performance.now();
  }

  function purpleRgba(alpha) {
    return "rgba(168,85,247," + alpha + ")";
  }

  function idx(x, y) {
    return y * CONFIG.COLS + x;
  }

  function getCell(x, y) {
    if (x < 0 || x >= CONFIG.COLS || y < 0 || y >= CONFIG.ROWS) return 65535;
    return grid[idx(x, y)];
  }

  function setCell(x, y, val) {
    if (x >= 0 && x < CONFIG.COLS && y >= 0 && y < CONFIG.ROWS) {
      grid[idx(x, y)] = val;
    }
  }

  function updatePhysics() {
    for (var y = CONFIG.ROWS - 2; y >= 0; y--) {
      var startX, endX, stepX;
      if (y & 1) { // alternate scan direction per row
        startX = 0; endX = CONFIG.COLS; stepX = 1;
      } else {
        startX = CONFIG.COLS - 1; endX = -1; stepX = -1;
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

        var dir = ((x + y) & 1) ? 1 : -1; // alternate diagonal direction for natural spread
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
    var scaleX = CONFIG.COLS / rect.width;
    var scaleY = CONFIG.ROWS / rect.height;
    var cx = Math.floor(mouseX * scaleX);
    var cy = Math.floor(mouseY * scaleY);
    var radius = CONFIG.SPAWN_RADIUS;

    for (var dy = -radius; dy <= radius; dy++) {
      for (var dx = -radius; dx <= radius; dx++) {
        if (dx * dx + dy * dy > radius * radius) continue;
        var gx = cx + dx;
        var gy = cy + dy;
        if (gx >= 0 && gx < CONFIG.COLS && gy >= 0 && gy < CONFIG.ROWS && grid[idx(gx, gy)] === 0) {
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
    return [(r * 255) | 0, (g * 255) | 0, (b * 255) | 0]; // truncate to integer
  }

  var colorLUT = new Uint8Array(361 * 3);
  for (var lutHue = 0; lutHue <= 360; lutHue++) {
    var rgb = hsvToRgb(lutHue);
    colorLUT[lutHue * 3] = rgb[0];
    colorLUT[lutHue * 3 + 1] = rgb[1];
    colorLUT[lutHue * 3 + 2] = rgb[2];
  }

  function render() {
    if (!ctx || !imageData) return;

    var data = pixels;
    var pixelOffset = 0;
    for (var i = 0; i < CONFIG.COLS * CONFIG.ROWS; i++) {
      var hueValue = grid[i];
      if (hueValue > 0) {
        var colorIndex = hueValue * 3;
        data[pixelOffset] = colorLUT[colorIndex];
        data[pixelOffset + 1] = colorLUT[colorIndex + 1];
        data[pixelOffset + 2] = colorLUT[colorIndex + 2];
        data[pixelOffset + 3] = 255;
      } else {
        data[pixelOffset] = 0;
        data[pixelOffset + 1] = 0;
        data[pixelOffset + 2] = 0;
        data[pixelOffset + 3] = 255;
      }
      pixelOffset += 4;
    }
    ctx.putImageData(imageData, 0, 0);
  }

  function renderWebGL(phaseValue, timeValue) {
    if (!gl || !shaderProgram) return;

    // Upload 2D canvas as texture
    gl.bindTexture(gl.TEXTURE_2D, sandTexture);
    gl.texImage2D(gl.TEXTURE_2D, 0, gl.RGBA, gl.RGBA, gl.UNSIGNED_BYTE, canvas);

    gl.uniform1f(uPhase, phaseValue);
    gl.uniform1f(uTime, timeValue);

    gl.drawArrays(gl.TRIANGLE_STRIP, 0, 4);
  }

  function frame() {
    if (!running) return;

    var now = performance.now();

    if (resetPhase === "turning_off") {
      // Freeze physics, render frozen frame
      var elapsed = now - resetStartTime;
      var t = Math.min(elapsed / TURN_OFF_MS, 1.0);
      render();
      renderWebGL(-t, now * 0.001);
      if (elapsed >= TURN_OFF_MS) {
        resetGrid();
        render();
        resetPhase = "turning_on";
        resetStartTime = now;
      }
      requestAnimationFrame(frame);
      return;
    }

    if (resetPhase === "turning_on") {
      var elapsed2 = now - resetStartTime;
      var t2 = 1.0 - Math.min(elapsed2 / TURN_ON_MS, 1.0);
      render();
      renderWebGL(t2, now * 0.001);
      if (elapsed2 >= TURN_ON_MS) {
        resetPhase = "idle";
      }
      requestAnimationFrame(frame);
      return;
    }

    // Normal idle operation
    updatePhysics();
    spawnSand();
    hue = hue >= 360 ? 1 : hue + 1;
    render();
    renderWebGL(0.0, now * 0.001);
    requestAnimationFrame(frame);
  }

  // ---- WebGL CRT setup ----

  var VERT_SRC = [
    "attribute vec2 a_pos;",
    "varying vec2 v_uv;",
    "void main() {",
    "  v_uv = a_pos * 0.5 + 0.5;",
    "  gl_Position = vec4(a_pos, 0.0, 1.0);",
    "}"
  ].join("\n");

  var FRAG_SRC = [
    "precision mediump float;",
    "varying vec2 v_uv;",
    "uniform sampler2D u_sand;",
    "uniform float u_phase;",
    "uniform float u_time;",
    "",
    "vec2 barrelDistort(vec2 uv) {",
    "  vec2 c = uv - 0.5;",
    "  float r2 = dot(c, c);",
    "  return c * (1.0 + r2 * 0.06) + 0.5;",
    "}",
    "",
    "void main() {",
    "  vec2 uv = v_uv;",
    "  float scaleY = 1.0;",
    "  float brightnessBoost = 0.0;",
    "  float fadeToBlack = 1.0;",
    "",
    "  // Turn-off: u_phase goes 0 -> -1",
    "  if (u_phase < 0.0) {",
    "    float t = -u_phase;",
    "    brightnessBoost = smoothstep(0.0, 0.15, t) * 1.5 * smoothstep(0.3, 0.15, t);",
    "    if (t > 0.15) {",
    "      float ct = clamp((t - 0.15) / 0.55, 0.0, 1.0);",
    "      scaleY = mix(1.0, 0.005, ct * ct);",
    "    }",
    "    if (t > 0.7) fadeToBlack = 1.0 - (t - 0.7) / 0.3;",
    "  }",
    "  // Turn-on: u_phase goes 1 -> 0",
    "  else if (u_phase > 0.0) {",
    "    float t = u_phase;",
    "    if (t > 0.3) {",
    "      float et = (t - 0.3) / 0.7;",
    "      scaleY = mix(1.0, 0.005, et * et);",
    "    } else {",
    "      scaleY = 1.0 + sin(t / 0.3 * 3.14159) * 0.03;",
    "    }",
    "    if (t > 0.5) brightnessBoost = (t - 0.5) * 2.0 * 0.8;",
    "  }",
    "",
    "  // Apply vertical squeeze",
    "  uv.y = 0.5 + (uv.y - 0.5) / max(scaleY, 0.001);",
    "  if (uv.y < 0.0 || uv.y > 1.0) { gl_FragColor = vec4(0,0,0,1); return; }",
    "",
    "  vec2 d = barrelDistort(uv);",
    "  if (d.x < 0.0 || d.x > 1.0 || d.y < 0.0 || d.y > 1.0) { gl_FragColor = vec4(0,0,0,1); return; }",
    "",
    "  vec4 color = texture2D(u_sand, d);",
    "",
    "  // Scanlines",
    "  color.rgb *= 0.92 + 0.08 * sin(d.y * 200.0 * 6.28318);",
    "  // RGB sub-pixel offset",
    "  color.r = mix(color.r, texture2D(u_sand, d + vec2(0.001, 0)).r, 0.3);",
    "  color.b = mix(color.b, texture2D(u_sand, d - vec2(0.001, 0)).b, 0.3);",
    "  // Vignette",
    "  vec2 vig = d - 0.5;",
    "  color.rgb *= clamp(1.0 - dot(vig, vig) * 1.2, 0.0, 1.0);",
    "  // Brightness + fade",
    "  color.rgb = color.rgb * fadeToBlack + brightnessBoost;",
    "  // Phosphor glow on turn-on",
    "  if (u_phase > 0.0 && u_phase < 0.5) color.g += u_phase * 0.2;",
    "",
    "  color.a = 1.0;",
    "  gl_FragColor = color;",
    "}"
  ].join("\n");

  function compileShader(type, src) {
    var s = gl.createShader(type);
    gl.shaderSource(s, src);
    gl.compileShader(s);
    if (!gl.getShaderParameter(s, gl.COMPILE_STATUS)) {
      console.error("CRT shader error:", gl.getShaderInfoLog(s));
      gl.deleteShader(s);
      return null;
    }
    return s;
  }

  function setupWebGL(container) {
    glCanvas = document.createElement("canvas");
    glCanvas.style.cssText = "position:absolute;top:0;left:0;width:100%;height:100%;pointer-events:none;z-index:1";
    container.appendChild(glCanvas);

    gl = glCanvas.getContext("webgl", { alpha: false, premultipliedAlpha: false });
    if (!gl) {
      // Fallback: no WebGL, show 2D canvas directly
      canvas.style.opacity = "1";
      glCanvas.parentNode.removeChild(glCanvas);
      glCanvas = null;
      return;
    }

    // Match resolution to 2D canvas for crisp texture
    glCanvas.width = CONFIG.COLS;
    glCanvas.height = CONFIG.ROWS;

    var vs = compileShader(gl.VERTEX_SHADER, VERT_SRC);
    var fs = compileShader(gl.FRAGMENT_SHADER, FRAG_SRC);
    if (!vs || !fs) {
      canvas.style.opacity = "1";
      gl = null;
      return;
    }

    shaderProgram = gl.createProgram();
    gl.attachShader(shaderProgram, vs);
    gl.attachShader(shaderProgram, fs);
    gl.linkProgram(shaderProgram);
    if (!gl.getProgramParameter(shaderProgram, gl.LINK_STATUS)) {
      console.error("CRT program link error:", gl.getProgramInfoLog(shaderProgram));
      canvas.style.opacity = "1";
      gl = null;
      return;
    }
    gl.useProgram(shaderProgram);

    // Fullscreen quad
    var quadVerts = new Float32Array([-1, -1, 1, -1, -1, 1, 1, 1]);
    var buf = gl.createBuffer();
    gl.bindBuffer(gl.ARRAY_BUFFER, buf);
    gl.bufferData(gl.ARRAY_BUFFER, quadVerts, gl.STATIC_DRAW);
    var aPos = gl.getAttribLocation(shaderProgram, "a_pos");
    gl.enableVertexAttribArray(aPos);
    gl.vertexAttribPointer(aPos, 2, gl.FLOAT, false, 0, 0);

    // Texture for 2D canvas
    sandTexture = gl.createTexture();
    gl.bindTexture(gl.TEXTURE_2D, sandTexture);
    gl.pixelStorei(gl.UNPACK_FLIP_Y_WEBGL, true);
    gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_S, gl.CLAMP_TO_EDGE);
    gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_T, gl.CLAMP_TO_EDGE);
    gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MIN_FILTER, gl.NEAREST);
    gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MAG_FILTER, gl.NEAREST);

    // Uniform locations
    uSand = gl.getUniformLocation(shaderProgram, "u_sand");
    uPhase = gl.getUniformLocation(shaderProgram, "u_phase");
    uTime = gl.getUniformLocation(shaderProgram, "u_time");
    gl.uniform1i(uSand, 0);

    gl.viewport(0, 0, glCanvas.width, glCanvas.height);
  }

  function setupCanvas(container) {
    canvas = document.createElement("canvas");
    canvas.width = CONFIG.COLS;
    canvas.height = CONFIG.ROWS;
    canvas.style.display = "block";
    canvas.style.width = "100%";
    canvas.style.height = "100%";
    canvas.style.imageRendering = "pixelated";
    canvas.style.position = "absolute";
    canvas.style.top = "0";
    canvas.style.left = "0";
    ctx = canvas.getContext("2d");
    imageData = ctx.createImageData(CONFIG.COLS, CONFIG.ROWS);
    pixels = imageData.data;
    container.appendChild(canvas);

    // Try WebGL overlay
    setupWebGL(container);

    // If WebGL succeeded, hide 2D canvas visually (still renders offscreen)
    if (gl) {
      canvas.style.opacity = "0";
    }

    // mouse events directly on the canvas
    function handleMouseDown(e) {
      mouseDown = true;
      mouseX = e.offsetX;
      mouseY = e.offsetY;
      focused = true;
      hideOverlay();
    }
    function handleMouseUp() {
      mouseDown = false;
    }
    function handleMouseMove(e) {
      if (e.buttons & 1) {
        mouseDown = true;
      }
      mouseX = e.offsetX;
      mouseY = e.offsetY;
    }
    function handleMouseLeave() {
      mouseDown = false;
    }

    canvas.addEventListener("mousedown", handleMouseDown);
    canvas.addEventListener("mouseup", handleMouseUp);
    canvas.addEventListener("mousemove", handleMouseMove);
    canvas.addEventListener("mouseleave", handleMouseLeave);

    // touch events for mobile
    function handleTouchStart(e) {
      e.preventDefault();
      var touch = e.touches[0];
      var rect = canvas.getBoundingClientRect();
      mouseDown = true;
      mouseX = touch.clientX - rect.left;
      mouseY = touch.clientY - rect.top;
      focused = true;
      hideOverlay();
    }
    function handleTouchMove(e) {
      e.preventDefault();
      var touch = e.touches[0];
      var rect = canvas.getBoundingClientRect();
      mouseX = touch.clientX - rect.left;
      mouseY = touch.clientY - rect.top;
    }
    function handleTouchEnd() {
      mouseDown = false;
    }

    canvas.addEventListener("touchstart", handleTouchStart, { passive: false });
    canvas.addEventListener("touchmove", handleTouchMove, { passive: false });
    canvas.addEventListener("touchend", handleTouchEnd);
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
      "color:" + purpleRgba(0.6), "letter-spacing:0.15em",
      "text-transform:uppercase", "animation:blink 2s steps(1) infinite"
    ].join(";");

    var sub = document.createElement("div");
    sub.textContent = "// falling sand simulator";
    sub.style.cssText = [
      "font-family:'Space Mono',monospace", "font-size:10px",
      "color:" + purpleRgba(0.25), "letter-spacing:0.1em"
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
  function handleKeyReset(e) {
    if (e.key === "r" && focused) {
      triggerReset();
    }
  }
  document.addEventListener("keyup", handleKeyReset);

  // clear focus when clicking outside the sand container
  function handleClickOutside(e) {
    var container = document.getElementById(CONFIG.CONTAINER_ID);
    if (container && !container.contains(e.target)) {
      focused = false;
    }
  }
  document.addEventListener("click", handleClickOutside, true);

  function tryStart() {
    if (running) return;
    var container = document.getElementById(CONFIG.CONTAINER_ID);
    if (!container) {
      setTimeout(tryStart, 100);
      return;
    }
    setupCanvas(container);
    createOverlay(container);
    createResetButton();
    running = true;
    frame();
  }

  function createResetButton() {
    var bar = document.getElementById(CONFIG.BOTTOM_BAR_ID);
    if (!bar) return;
    var btn = document.createElement("button");
    btn.textContent = "RESET";
    btn.style.cssText = [
      "font-family:'Space Mono',monospace", "font-size:9px",
      "color:" + purpleRgba(0.5), "background:" + purpleRgba(0.06),
      "border:1px solid " + purpleRgba(0.15), "padding:4px 10px",
      "cursor:pointer", "letter-spacing:0.1em", "text-transform:uppercase"
    ].join(";");
    btn.addEventListener("click", function (e) {
      e.stopPropagation();
      triggerReset();
    });
    bar.appendChild(btn);
  }

  window.SandSim = {
    start: tryStart,
    reset: function () { triggerReset(); }
  };
})();
