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
      </head>
      <body>
        <h1>
          <xsl:value-of select="description"/>
        </h1>
        <xsl:for-each select="item">
          <!-- <h2 id="[guid]">title<a href="#[guid]"></a></h2> -->
          <h2>
            <xsl:attribute name="id">
              <xsl:value-of select="translate(guid, ':/.?', '---')"/>
            </xsl:attribute>
            <a>
              <xsl:attribute name="href">
                <xsl:value-of select="link"/>
              </xsl:attribute>
              <xsl:value-of select="title"/>
            </a>
            <a>
              <xsl:attribute name="href">
                <xsl:value-of select="concat('#', translate(guid, ':/.?', '---'))"/>
              </xsl:attribute>
            </a>
          </h2>
          <!-- <div>Published on [date]</div> -->
          <div>
            <xsl:text>Published on </xsl:text>
            <xsl:value-of select="substring(pubDate, 6, 11)"/>
          </div>
          <!-- <p>[first paragraph]</p> -->
          <p>
            <xsl:value-of
                select="substring-after(substring-before(description, '&lt;/p&gt;'), '&lt;p&gt;')"
                disable-output-escaping="yes"/> ...
          </p>
          <!-- <p><a href="[link]">Read on website</a> | <a href="[first-tag]">#[first-tag]</a></p>-->
          <p>
            <a>
              <xsl:attribute name="href">
                <xsl:value-of select="link"/>
              </xsl:attribute>
              <xsl:text>Read on website</xsl:text>
            </a>
            <xsl:text> | </xsl:text>
            <a>
              <xsl:attribute name="href">
                <xsl:value-of select="substring-before(substring-after(substring-after(substring-after(description, '&lt;!-- ### --&gt;'), '|'), '&lt;a href=&quot;'), '&quot;')"/>
              </xsl:attribute>
              <xsl:value-of select="substring-before(substring-after(substring-after(substring-after(description, '&lt;!-- ### --&gt;'), '|'), '&gt;'), '&lt;')"/>
            </a>
          </p>
        </xsl:for-each>
      </body>
    </html>
  </xsl:template>
</xsl:stylesheet>
