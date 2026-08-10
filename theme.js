/* CS 770 (Fall 2026) — light / dark / auto theme toggle.
   Cycles Auto -> Light -> Dark; persists in localStorage 'cs770-theme'.
   Applies via data-theme on <html> (auto = attribute removed). */
(function () {
  var KEY = "cs770-theme";
  var ORDER = ["auto", "light", "dark"];
  var LABELS = { auto: "◐ Auto", light: "☀ Light", dark: "☾ Dark" };

  function current() {
    var v = null;
    try { v = localStorage.getItem(KEY); } catch (e) {}
    return ORDER.indexOf(v) === -1 ? "auto" : v;
  }

  function apply(mode) {
    var root = document.documentElement;
    if (mode === "auto") {
      root.removeAttribute("data-theme");
    } else {
      root.setAttribute("data-theme", mode);
    }
    updateButtons(mode);
  }

  function updateButtons(mode) {
    var btns = document.querySelectorAll(".theme-toggle");
    for (var i = 0; i < btns.length; i++) {
      btns[i].textContent = LABELS[mode];
      btns[i].setAttribute("aria-label", "Theme: " + mode + " (click to change)");
    }
  }

  function cycle() {
    var next = ORDER[(ORDER.indexOf(current()) + 1) % ORDER.length];
    try { localStorage.setItem(KEY, next); } catch (e) {}
    apply(next);
  }

  window.__cs770cycle = cycle;

  // Sync the button label once the DOM is ready (theme itself is set pre-paint).
  if (document.readyState === "loading") {
    document.addEventListener("DOMContentLoaded", function () { updateButtons(current()); });
  } else {
    updateButtons(current());
  }
})();
