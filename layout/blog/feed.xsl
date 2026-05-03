<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="1.0"
                xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                xmlns:atom="http://www.w3.org/2005/Atom"
                exclude-result-prefixes="atom">
  <xsl:output method="html" indent="yes"/>
  <xsl:template match="/atom:feed">
    <html lang="en">
      <head>
        <meta charset="UTF-8"/>
        <meta name="viewport" content="width=device-width, initial-scale=1"/>
        <title>
          <xsl:value-of select="atom:subtitle"/>
        </title>
        <style>{{ css }}        </style>
        <style>details { margin: 1em 0 }</style>
      </head>
      <body>
        <h1><xsl:value-of select="atom:subtitle"/></h1>
        <div>(<xsl:value-of select="count(atom:entry)"/> items)</div>
        <p>
          This is an Atom news feed.  It is meant to be read using a
          feed reader application rather than viewed directly in a web
          browser.  To use it, copy this document's URL from your
          browser's address bar and add it to your feed reader.
        </p>
        <xsl:for-each select="atom:entry">
          <details>
            <summary>
              <a>
                <xsl:attribute name="href">
                  <xsl:value-of select="atom:link/@href"/>
                </xsl:attribute>
                <xsl:value-of select="atom:title"/>
              </a>
              (<xsl:value-of select="substring(atom:updated, 1, 10)"/>)
            </summary>
            <xsl:value-of select="atom:content" disable-output-escaping="yes"/>
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
