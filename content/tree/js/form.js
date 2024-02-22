window.onload = function () {
  const form = document.getElementsByTagName('form')[0]
  const input = document.createElement('input')
  const a = 403 + Math.floor((1000 - 403) * Math.random())
  const b = a % 97
  const c = a % 71
  const x = 1000000 * a + 1000 * b + c
  input.setAttribute('type', 'hidden')
  input.setAttribute('name', 'token')
  input.setAttribute('value', x.toString())
  form.appendChild(input)
}
