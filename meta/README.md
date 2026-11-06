Developer Notes
===============

Content Headers
---------------

For `.aux.html`, `.page.html` and `.post.html` documents, put the
required headers first in this order:

  - `date`
  - `title`
  - `tag`
  - `uuid`

For all document types, put optional headers after the required
headers and use them in this order:

  - `subtitle`
  - `zone`
  - `import`
  - `tex`
  - `toc`
  - `hide`
  - `draft`
  - `update`
  - `cslug` or `ccslug`

The `.talk.html` documents may use the same headers except `date`.
Their date is provided by the first comment block, not by a document
header.


Comment Blocks
--------------

In `.disc.html` and `.talk.html` documents, put comment block headers
in this order:

  - `date`
  - `name`
  - `url`
  - `source`

Omit `url` or `source` when they do not apply.
