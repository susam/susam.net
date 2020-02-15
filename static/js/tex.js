;(function () {
  function loadjs(url) {
      var script = window.document.createElement('script')
      script.src = url
      window.document.head.appendChild(script)
  }

  loadjs('https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js')
})()
