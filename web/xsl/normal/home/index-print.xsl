<?xml version="1.0" encoding="utf-8"?>

<!DOCTYPE xsl:stylesheet [ <!ENTITY nbsp "&#160;"> ]>

<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">

<xsl:import href="../shared/master/screen/title-page.xsl"/>
<xsl:include href="../shared/utils/tipograf.xsl" />

<xsl:template name="title">МЧС</xsl:template>

<xsl:include href="./includes/news-list.xsl" />

<xsl:template name="news">
    <div id="news-list">
        <xsl:call-template name="news-list">
            <xsl:with-param name="Item" select="/data/news/item" />
            <xsl:with-param name="NewsPath" select="'/News'" />
            <xsl:with-param name="Spec" select="/data/meta/is-spec" />
        </xsl:call-template>
    </div>
</xsl:template>

<xsl:template name="article">
<div style="font-size:130%">
  <p>
  <xsl:text>
  Россия располагает одной из самых мощных транспортных систем в мире. В эту систему входят наземный (автомобильный, железнодорожный и метро), воздушный, водный (морской и речной) транспорт. Каждый вид транспорта представляет собой совокупность средств, путей сообщений, технических устройств и сооружений, относящихся к транспортной инфраструктуре.
  </xsl:text>
  </p>
  <p>
  <xsl:text>﻿
  Именно транспортная система обеспечивает эффективную работу всех отраслей народного хозяйства, жизнеспособность экономики страны, а также нормальную жизнедеятельность населения.
  </xsl:text>
  </p>
  <p>
  <xsl:text>
  Вот почему задачи надежности и безопасности транспортной системы – это первоочередные задачи любого промышленно-развитого государства.
  </xsl:text>
  </p>
  <p>
  <xsl:text>
  Под транспортной безопасностью понимают совокупность мер, призванных защитить интересы государства и общества, а также жизнь и здоровье пассажиров от внутренних и внешних угроз в транспортной сфере. Эти меры направлены на:
  </xsl:text>
  </p>
  <ul>
      <li>обеспечение безопасных для здоровья и жизни людей условий проезда</li>
      <li>обеспечение безопасности функционирования и эксплуатации собственно транспортных средств и других объектов транспортной инфраструктуры</li>
      <li>обеспечение безопасности перевозок грузов и багажа</li>
      <li>обеспечение экономической, экологической, информационной, пожарной, санитарной, химической, радиационной безопасности.</li>
  </ul>
  <p>
  <xsl:text>
  Несмотря на огромное внимание, которое уделяется проблемам безопасности на транспорте, в транспортной системе, как и в любой другой системе, время от времени происходят чрезвычайные ситуации. Поэтому, наряду с мерами безопасности, важнейшее значение имеет комплекс мер, направленных на эффективную организацию аварийно-спасательных работ по ликвидации последствий аварий и катастроф на транспорте. Из важнейших моментов, относящихся к комплексу таких мер, можно выделить следующие:
  </xsl:text>
  </p>
  <ul>
      <li>обеспечение своевременной и достоверной информации о случившемся</li>
      <li>обеспечение скорой медицинской помощи и оперативной эвакуации пострадавших</li>
      <li>обеспечение оперативного развертывания специальной спасательной техники и средств тушения пожаров</li>
      <li>обеспечение в кратчайшие сроки возобновления движения по транспортной коммуникации</li>
  </ul>
</div>
</xsl:template>


<xsl:template match="/">
    <xsl:call-template name="root"/>
</xsl:template>

</xsl:stylesheet>