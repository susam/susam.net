Layout Placeholders
===================

There are a few important and confusing placeholders used in the
layout files. They are documented here:

  - `{{ maze }}`:
    - Points to the Maze website (home).
    - Has the value `/maze/` while running `make site`.
    - Has the value `https://susam.net/maze/` while running `make dist`.
    - On `https://susam.net/` it points to `https://susam.net/maze/`.
    - On `https://susam.github.io/maze/` it points `https://susam.github.io/maze/`.
    - On local file system, it points to `https://susam.net/maze/`.
    - Helps to choose between a short path without domain vs. a full
      URL with domain based on whether the files are deployed on a
      live web server or on a local filesystem.
    - Used to link to files on Maze from the main website.
  - `{{ site-url }}`:
    - Points to the canonical URL (primary URL) of the main website.
    - Always has the value `https://susam.net/`.
    - All links to comment/subscribe app must use this because this
      app only runs on the web-server for `susam.net`. This app does
      not run on the mirror on GitHub.
    - Links in Feed must also use this URL, so that feed users reach
      the canonical website.
