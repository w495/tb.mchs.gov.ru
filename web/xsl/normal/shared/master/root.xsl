<?xml version="1.0" encoding="utf-8"?>
<!--
    Это основной шаблон для вывода на экран монитора.
-->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">

<xsl:output
    omit-xml-declaration="no"
    method="html"
    indent="yes"
    encoding="utf-8"
/>

<!--
    Включаем сторонние статичные элементы.
    Они предельно конкретны и меняться от шаблона к шаблону не будут.
    
    Все include могут быть только в этом документе
-->

<!-- Заголовок с DOCTYPE html и определениями для IE -->
<xsl:include href="../includes/_html5header.xsl" />

<!-- Верхнее меню -->
<xsl:include href="../includes/menu-container.xsl" />

<!-- Подол страницы -->
<xsl:include href="../includes/footer-container.xsl" />

<!-- Виджет игры -->
<xsl:include href="../includes/game-container.xsl" />

<!-- Виджет нижнего баннера -->
<xsl:include href="../includes/bottom-banner-container.xsl" />

<!-- Список видов транспорта -->
<xsl:include href="../includes/transport-container.xsl" />

<!-- Авторизация -->
<xsl:include href="../includes/athorise-container.xsl" />

<!-- ====================================================================  -->
<!-- <HTML></HTML> -->
<!-- ====================================================================  -->

<xsl:template match="/" name="root">
<xsl:call-template name="html5header-min" /> 
<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="ru" >
    <head xmlns="http://www.w3.org/1999/xhtml" xml:lang="ru">
        <xsl:call-template name="head" />
    </head>
    <body xmlns="http://www.w3.org/1999/xhtml" xml:lang="ru">
        <xsl:call-template name="body" />
        <script type="text/javascript">
          var _gaq = _gaq || [];
          _gaq.push(['_setAccount', 'UA-15347927-4']);
          _gaq.push(['_trackPageview']);
            (function() {
                var ga = document.createElement('script'); ga.type = 'text/javascript'; ga.async = true;
                ga.src = ('https:' == document.location.protocol ? 'https://ssl' : 'http://www') + '.google-analytics.com/ga.js';
                var s = document.getElementsByTagName('script')[0]; s.parentNode.insertBefore(ga, s);
            })();
        </script>
    </body>
    <xsl:call-template name="foot-scripts" />
</html>
</xsl:template>

<!-- ====================================================================  -->
<!-- ГОЛОВА -->
<!-- ====================================================================  -->

<xsl:template name="head">
    <xsl:call-template name="meta" />    
    <xsl:call-template name="viewport" />
    <title>
        <xsl:call-template name="title" />
    </title>
    
    <xsl:call-template name="links" />
    <xsl:call-template name="head-scripts" />
</xsl:template>

<xsl:template name="head-scripts">
</xsl:template>

<!-- МETA -->
<!-- ====================================================================  -->
<xsl:template name="meta">
    <xsl:call-template name="meta-http-equiv" />
    <xsl:call-template name="meta-oth" />
</xsl:template>

<xsl:template name="meta-http-equiv">
    <meta http-equiv="Content-Type" content="text/html;charset=UTF-8" charset="UTF-8" />
    <meta http-equiv="X-UA-Compatible" content="IE=edge,chrome=1" />
    <meta name="google-site-verification" content="GSCMBiIDlYUZv4HCOJCgY8FETmJngUZeg2qh2jmO5io" />
    <!-- К сожалению, яндекс не умеет парсить xml стандартными средствами -->
    <xsl:text disable-output-escaping="yes"><![CDATA[<meta name='yandex-verification' content='56d788f4cfcf2db5' />]]></xsl:text>
    <!-- потому мы так извращаемся -->
</xsl:template>

<xsl:template name="meta-oth">
    <meta name="author" content="TvZavr Team" />
    <meta name="description" content="" />
</xsl:template>

<!-- viewport -->
<!-- ====================================================================  -->
<xsl:template name="viewport">
    <meta name="viewport" content="width=device-width, initial-scale=1.0" />
</xsl:template>

<!-- links -->
<!-- ====================================================================  -->
<xsl:template name="links">
    <link rel="icon" href="/favicon.ico" />
    <link rel="shortcut icon" href="/favicon.ico" />
    <link rel="shortcut icon" href="/static/site-media/favicon.ico" />
        
    <!-- <link rel="apple-touch-icon" href="" />-->
    <xsl:call-template name="link-css" />
</xsl:template>

<xsl:template name="link-css">
    <link rel="stylesheet" type="text/css" media="all" href="/static/*.css" />
</xsl:template>

<!-- ====================================================================  -->
<!-- ТЕЛО -->
<!-- ====================================================================  -->

<xsl:template name="body">
    <div id="background">
        <div id="main">
            <xsl:call-template name="main" />
        </div>
    </div>
</xsl:template>

<xsl:template name="main">
    <xsl:text> </xsl:text>
</xsl:template>

<!-- ====================================================================  -->
<!-- ХВОСТ -->
<!-- ====================================================================  -->

<xsl:template name="foot-scripts">

</xsl:template>


</xsl:stylesheet>
