<?xml version="1.0" encoding="utf-8"?>

<!DOCTYPE xsl:stylesheet [ <!ENTITY nbsp "&#160;"> ]>

<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">

<xsl:import href="../shared/master/screen/list-page.xsl"/>

<xsl:import href="../shared/widgets/dirs-list.xsl"/>
<xsl:import href="../shared/widgets/doc-body.xsl"/>

<xsl:template name="docs-container-header">
    <xsl:text>Нормативно-правовые документы</xsl:text>
</xsl:template>

<xsl:template name="docs-content">
    <div id="search-body">
        <div>

            
        </div>
    </div>
</xsl:template>


<xsl:template match="/">
  <xsl:apply-imports/>
</xsl:template>

</xsl:stylesheet>

