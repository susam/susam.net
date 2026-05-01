const wander = {
  // Other Wander consoles in my neighbourhood.
  consoles: [
    'https://aartaka.me/wander/',
    'https://bitzero.cloud/wander/',
    'https://blog.gridranger.dev/wander/',
    'https://dahlstrand.net/wander/',
    'https://juleskourelakos.com/wander/',
    'https://susam.net/wander/',
    'https://www.siddharthagolu.com/wander/',
    'https://heckmeck.de/wander/',
  ],
  // My recommendations.
  pages: [
    'https://aartaka.me/',
    'https://adelfaure.net/artware/ascii_might_fly/',
    'https://chrismorgan.info/',
    'https://int10h.org/',
    'https://int10h.org/oldschool-pc-fonts/fontlist/',
    'https://jgc.org/',
    'https://maxwellito.com/',
    'https://midnight.pub/',
    'https://quarters.captaintouch.com/blog/',
    'https://sachachua.com/blog/',
    'https://susam.net/',
    'https://susam.net/invaders.html',
    'https://susam.net/jokes.html',
    'https://susam.net/myrgb.html',
    'https://thejeshgn.com/',
    'https://tonsky.me/',
    'https://www.bitsavers.org/',
    'https://www.evalapply.org/',
    'https://www.norvig.com/',
    'https://xnacly.me/',
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
    'https://www.math.colostate.edu/',
    'https://www.strangeloopcanon.com/',

    // Out of scope.  These are commercial platforms, not personal websites.
    'https://medium.com/',
    'https://*.substack.com/',
    'https://*.wordpress.com/',

    // These pages present distracting cookie banners.
    'https://rickyyean.com/',

    // SSL_ERROR_BAD_CERT_DOMAIN
    'https://wander.liontask.dev/',

    // Recommends too many AI-generated and startup-focused pages.
    // Content focus differs from the small web emphasis of this project.
    'https://www.davidtran.me/wander/',

    // Some people have incorrectly added this UI wrapper URL instead
    // of the actual Wander console URL which is located at /wander/.
    'https://blog.gridranger.dev/navigator/',

    // Interesting website recommending websites belonging to people
    // named Josh.  However, this is a directory, not a personal website.
    'https://joshing.you/wander/',
  ]
}
