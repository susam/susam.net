// MathJax configuration.
window.MathJax = {
  tex2jax: {
    // Enable $...$ as delimiter for inline math.
    inlineMath: [['$', '$'], ['\\(', '\\)']],
    processEscapes: true
  },

  TeX: {
    // Enable equation numbering.
    equationNumbers: {
      autoNumber: 'AMS'
    },

    Macros: {
      qed: '\\tag*{\\( \\blacksquare \\)}',
      bitxor: '\\operatorname{\\hat{}}',
      ord: '\\operatorname{ord}',
    }
  }
}

;(function () {
  function loadjs(url) {
      var script = window.document.createElement('script')
      script.src = url
      window.document.head.appendChild(script)
  }

  loadjs('https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.4/' +
         'MathJax.js?config=TeX-AMS_CHTML')
})()
