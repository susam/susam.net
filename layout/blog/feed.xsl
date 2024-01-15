<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
  <xsl:output method="html" indent="yes"/>
  <xsl:template match="/rss/channel">
    <html xmlns="http://www.w3.org/1999/xhtml" lang="en">
      <head>
        <title>
          <xsl:value-of select="description"/>
        </title>
        <style>{{ css }}        </style>
      </head>
      <body>
        <h1>
          <xsl:value-of select="description"/>
        </h1>
        <xsl:for-each select="item">
          <h2>
            <xsl:attribute name="id">
              <xsl:value-of select="translate(guid, ':/.', '---')"/>
            </xsl:attribute>
            <a>
              <xsl:attribute name="href">
                <xsl:value-of select="link"/>
              </xsl:attribute>
              <xsl:value-of select="title"/>
            </a>
            <a>
              <xsl:attribute name="href">
                <xsl:value-of select="concat('#', translate(guid, ':/.', '---'))"/>
              </xsl:attribute>
            </a>
          </h2>
          <div>
            Published on
            <xsl:value-of select="substring(pubDate, 6, 11)"/>
          </div>
          <p>
            <xsl:value-of
                select="substring-after(substring-before(description, '&lt;/p&gt;'), '&lt;p&gt;')"
                disable-output-escaping="yes"/> ...

          </p>
          <p>
            <xsl:value-of
                select="substring-after(description, '&lt;!-- ### --&gt;')"
                disable-output-escaping="yes"/>

          </p>
        </xsl:for-each>
      </body>
    </html>
  </xsl:template>
</xsl:stylesheet>
