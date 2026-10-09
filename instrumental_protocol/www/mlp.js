// MLP-Design (Option A: Stufenkarten)
// - setzt body.mlp, solange eine MLP-Seite angezeigt wird (CSS greift nur dann)
// - nummeriert die Antwortkarten auf 5-stufigen Item-Seiten
// - markiert die angetippte Karte kurz und blättert dann weiter
(function () {
  // Wird im Standalone über test_options() und in Batterien über die
  // htmlDependency der Seiten geladen -> nur einmal initialisieren.
  if (window.mlpDesignLoaded) return;
  window.mlpDesignLoaded = true;

  var SELECT_DELAY_MS = 300;

  // Schlüsselwort der Stufe fett setzen. psychTestR maskiert HTML in den
  // Antwort-Labels (das Dictionary liefert <strong>...</strong>), daher nur
  // genau diese Tags wieder zurückwandeln; "**...**" dient als Fallback.
  function boldMarkup(el) {
    var html = el.innerHTML;
    if (html.indexOf("strong") < 0 && html.indexOf("**") < 0) return;
    el.innerHTML = html
      .replace(/&lt;(\/?)strong&gt;/g, "<$1strong>")
      .replace(/\*\*(.+?)\*\*/g, "<strong>$1</strong>");
  }

  function decorate() {
    var page = document.querySelector(".mlp-page");
    document.body.classList.toggle("mlp", !!page || window.mlpStandalone === true);
    if (!page) return;

    // psychTestR legt die Antwort-Buttons neben (nicht in) den Prompt-Container
    var ui = document.getElementById("response_ui");
    if (!ui || ui.dataset.mlpDone) return;
    ui.dataset.mlpDone = "1";

    var numbered = page.classList.contains("mlp-numbered");
    var buttons = ui.querySelectorAll(".btn");
    buttons.forEach(function (btn, i) {
      var label = document.createElement("span");
      label.className = "mlp-label";
      while (btn.firstChild) label.appendChild(btn.firstChild);
      boldMarkup(label);
      if (numbered) {
        btn.classList.add("mlp-lv" + (i + 1));
        var num = document.createElement("span");
        num.className = "mlp-num";
        num.setAttribute("aria-hidden", "true");
        num.textContent = String(i + 1);
        btn.appendChild(num);
      }
      btn.appendChild(label);
    });
  }

  // Klick abfangen, bevor psychTestRs onclick="trigger_button(...)" feuert
  document.addEventListener("click", function (e) {
    var btn = e.target.closest && e.target.closest("body.mlp #response_ui .btn");
    if (!btn) return;
    e.preventDefault();
    e.stopPropagation();
    var ui = btn.closest("#response_ui");
    if (ui.classList.contains("mlp-locked")) return;
    ui.classList.add("mlp-locked");
    btn.classList.add("mlp-selected");
    setTimeout(function () { trigger_button(btn.id); }, SELECT_DELAY_MS);
  }, true);

  function start() {
    decorate();
    new MutationObserver(decorate).observe(document.body, { childList: true, subtree: true });
  }
  if (document.readyState === "loading") document.addEventListener("DOMContentLoaded", start);
  else start();
})();
