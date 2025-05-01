Layout Placeholders
===================

There are a few important and confusing placeholders used in the
layout files. They are documented here:

- `{{ site-url }}`:
    - Points to the canonical URL (primary URL) of the website.
    - Always has the value `https://susam.net/`.
    - Links in Feed must also use this URL, so that feed users reach
      the canonical website.
