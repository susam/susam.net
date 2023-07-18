<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
  <xsl:output method="html" indent="yes"/>
  <xsl:template match="/rss/channel">
    <html xmlns="http://www.w3.org/1999/xhtml" lang="en">
      <head>
        <title>
          <xsl:value-of select="description"/>
        </title>
        <link rel="stylesheet" href="../css/main.css"/>
      </head>
      <body>
        <h1>
          <xsl:value-of select="description"/>
        </h1>
        <xsl:for-each select="item">
          <h2>
            <a>
              <xsl:attribute name="href">
                <xsl:value-of select="link"/>
              </xsl:attribute>
              <xsl:value-of select="title"/>
            </a>
          </h2>
          <div>
            Published on
            <xsl:value-of select="substring(pubDate, 6, 11)"/>
          </div>
          <p>
            <xsl:value-of
                select="substring-before(substring-after(description, '&lt;p&gt;'), '&lt;/p&gt;')"
                disable-output-escaping="yes"/>
            ...
          </p>
        </xsl:for-each>
      </body>
    </html>
  </xsl:template>
</xsl:stylesheet>
