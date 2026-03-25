window.wander = {
  // Other Wander consoles in my neighbourhood.
  consoles: [
    'https://susam.net/wander/',
    'https://dahlstrand.net/wander/',
    'https://www.siddharthagolu.com/wander/',
    'https://wander.liontask.dev/',
    'https://juleskourelakos.com/wander/',
    'https://bitzero.cloud/wander/',
  ],
  // My recommendations.
  pages: [
    'http://www.antonis.de/qbebooks/gwbasman/',
    'https://aartaka.me/',
    'https://int10h.org/',
    'https://int10h.org/oldschool-pc-fonts/fontlist/',
    'https://maxwellito.com/',
    'https://midnight.pub/',
    'https://sachachua.com/',
    'https://susam.net/',
    'https://thejeshgn.com/',
    'https://www.bitsavers.org/',
    'https://www.norvig.com/',
    'https://chrismorgan.info/',
    'https://quarters.captaintouch.com/blog/',
    'https://www.evalapply.org/',
    'https://jgc.org/',
    'https://xnacly.me/',
    'https://tonsky.me/',
  ],
  // My console will never load these URLs.
  ignore: [
    // These pages fail to load in the console due to frame restrictions.
    'https://cari.institute/',
    'https://dbushell.com/',
    'https://drkhsh.at/',
    'https://hyperdoc.khinsen.net/',
    'https://levels.io/',
    'https://wdl.mcdaniel.edu/',
    'https://www.honest-broker.com/',
    'https://www.joshuamckiddy.com',
    'https://www.strangeloopcanon.com/',

    // Out of scope.  These are commercial platforms, not personal websites.
    'https://medium.com/',
    'https://substack.com/',

    // These pages present distracting cookie banners.
    'https://rickyyean.com/',

    // SSL_ERROR_BAD_CERT_DOMAIN
    'https://wander.liontask.dev/',

    // Recommends too many AI-generated and startup-focused pages.
    // Content focus differs from the small web emphasis of this project.
    'https://www.davidtran.me/wander/',
  ]
}
