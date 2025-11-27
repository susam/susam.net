  <link rel="stylesheet" href="{{ root }}js/katex/katex.min.css">
  <style>
    .katex {font-size: 1.1em }
    .katex a:link {text-decoration: none}
    .katex-display {
      overflow-x: auto; /* Allow horizontal scroll for wide equations. */
      overflow-y: hidden; /* Hide vertical scroll due to horizontal scroll. */
    }
  </style>
  <script defer src="{{ root }}js/katex/katex.min.js"></script>
  <script defer src="{{ root }}js/katex/contrib/auto-render.min.js"></script>
  <script>
    document.addEventListener('DOMContentLoaded', function () {
      renderMathInElement(document.body, {
        delimiters: [
          {left: '\\(', right: '\\)', display: false},
          {left: '\\[', right: '\\]', display: true},
          {left: '\\begin{equation}', right: '\\end{equation}', display: true},
          {left: '\\begin{align}', right: '\\end{align}', display: true},
          {left: '\\begin{align*}', right: '\\end{align*}', display: true},
          {left: '\\begin{alignat}', right: '\\end{alignat}', display: true},
          {left: '\\begin{alignat*}', right: '\\end{alignat*}', display: true},
          {left: '\\begin{array}', right: '\\end{array}', display: true},
        ],
        macros: {
          '\\eqnref': '\\href{###1}{(\\text{#2})}',
          '\\label': '\\htmlId{#1}{}'
        },
        throwOnError: false,
        trust: function (context) {
          return ['\\htmlId', '\\href'].includes(context.command)
        }
      })
    })
  </script>
