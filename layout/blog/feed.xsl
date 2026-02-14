<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
  <xsl:output method="html" indent="yes"/>
  <xsl:template match="/rss/channel">
    <html lang="en">
      <head>
        <meta charset="UTF-8"/>
        <meta name="viewport" content="width=device-width, initial-scale=1"/>
        <title>
          <xsl:value-of select="description"/>
        </title>
        <style>{{ css }}        </style>
        <style>details { margin: 1em 0 }</style>
      </head>
      <body>
        <h1><xsl:value-of select="description"/></h1>
        <div>(<xsl:value-of select="count(item)"/> items)</div>
        <p>
          This is an RSS news feed.  It is meant to be read using a
          feed reader application rather than viewed directly in a web
          browser.  To use it, copy this document's URL from your
          browser's address bar and add it to your feed reader.
        </p>
        <xsl:for-each select="item">
          <details>
            <summary>
              <a>
                <xsl:attribute name="href">
                  <xsl:value-of select="link"/>
                </xsl:attribute>
                <xsl:value-of select="title"/>
              </a>
              (<xsl:value-of select="substring(pubDate, 6, 11)"/>)
            </summary>
            <xsl:value-of select="description" disable-output-escaping="yes"/>
          </details>
        </xsl:for-each>
        <footer>
          <hr/>
          <nav>
            <a href="{{ root }}{{ index }}">Home</a>
            <a href="{{ root }}links.html">Links</a>
            <a href="{{ root }}about.html">About</a>
            <a href="https://github.com/susam">GitHub</a>
            <a href="https://mastodon.social/@susam">Mastodon</a>
          </nav>
          <p>
            &#xa9; 2001&#x2013;{{ current-year }} {{ author }}
          </p>
        </footer>
      </body>
    </html>
  </xsl:template>
</xsl:stylesheet>
