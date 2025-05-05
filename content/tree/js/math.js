(function () {
  'use strict'

  function loadCSS (path, callback) {
    const link = document.createElement('link')
    link.rel = 'stylesheet'
    link.href = BASE_URL + path
    link.onload = callback
    document.head.appendChild(link)
  }

  function loadJS (path, callback) {
    const script = document.createElement('script')
    script.src = BASE_URL + path
    script.onload = callback
    document.head.appendChild(script)
  }

  let loaded = 0

  function ready () {
    if (++loaded !== 2) return
    const style = document.createElement('style')
    style.textContent = '.katex {font-size: 1.1em} .katex a:link {text-decoration: none}'
    document.head.appendChild(style)
    log('Document ready state:', document.readyState)
    if (document.readyState === 'loading') {
      log('Setting event listener for render')
      document.addEventListener('DOMContentLoaded', render)
    } else {
      log('Rendering immediately')
      render()
    }
  }

  function render () {
    /* global renderMathInElement */
    log('Rendering started')
    renderMathInElement(document.body, {
      delimiters: [
        { left: '\\(', right: '\\)', display: false },
        { left: '\\[', right: '\\]', display: true },
        { left: '\\begin{equation}', right: '\\end{equation}', display: true },
        { left: '\\begin{align}', right: '\\end{align}', display: true },
        { left: '\\begin{align*}', right: '\\end{align*}', display: true }
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
    log('Rendering complete')
  }

  function log () {
    if (LOGGING) {
      console.log.apply(null, arguments)
    }
  }

  function baseURL () {
    const src = document.currentScript.src
    return src.substring(0, src.lastIndexOf('/') + 1)
  }

  const LOGGING = location.search.indexOf('logging') !== -1
  const BASE_URL = baseURL()

  function main () {
    loadCSS('katex/katex.min.css', ready)
    loadJS('katex/katex.min.js', function () {
      // Load katex.js before auto-render.js to avoid
      // "external_katex_default() is undefined" error.
      loadJS('katex/contrib/auto-render.min.js', ready)
    })
  }

  main()
})()
