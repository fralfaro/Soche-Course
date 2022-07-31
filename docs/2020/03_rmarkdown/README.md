# Rmarkdown_SOCHE

Estos archivos corresponden al Minicurso 1: “Cómo diseñar un informe de manera rápida y sencilla con R Markdown”.

A cargo de la Dra. Teresa Boca (FAUBA - rtboca@agro.uba.ar), a dictarse en el marco de:

Ciclo de Minicursos 2020 Sociedad Chilena de Estadística - SOCHE

Los días 5 y 7 de enero

<p>Para realizar las actividades&nbsp; del minicurso debera tener instalados en su PC:&nbsp;</p>
<p><a href="https://www.r-project.org/">R</a> y <a href="https://rstudio.com/">Rstudio</a></p>
<p>Y los paquetes que figuran en el archivo "paquetes a instalar.r"</p>
<p>Este video tutorial lo guiar&aacute; como hacerlo&nbsp;</p>
<ul>
<li><a href="https://www.youtube.com/embed/QaKCirYknS8">V&iacute;deo de Instalaci&oacute;n de los paquetes para usar Rmarkdown</a></li>
</ul>
<p>&nbsp;</p>

<div><span style="color: black; font-family: Calibri,Helvetica,sans-serif,serif,EmojiFont; font-size: small;"><span style="color: #495057; font-size: small;"><strong>&iquest;Qu&eacute; es RMarkdown?</strong></span><span style="color: #495057; font-size: small;"><br /></span></span></div>
<div><span style="color: black; font-family: Calibri,Helvetica,sans-serif,serif,EmojiFont; font-size: small;"><span style="color: #495057; font-size: small;">Un documento&nbsp;</span><span style="color: #e83e8c; font-family: SFMono-Regular,Menlo,Monaco,Consolas,Liberation Mono,Courier New,monospace,serif,EmojiFont; font-size: xx-small;">RMarkdown</span><span style="color: #495057; font-size: small;">&nbsp;tiene la propiedad de combinar texto, c&oacute;digos y gr&aacute;fico con la intenci&oacute;n de generar art&iacute;culos cient&iacute;ficos de alta calidad, que pueden verse en formatos como pdf, doc o HTML.&nbsp; Algo que los hace muy &uacute;tiles y llamativos es que dentro de ellos podemos tener la ejecuci&oacute;n de c&oacute;digos y con ello ver de una manera directa resultados estad&iacute;sticos que nos ayudan a sacar conclusiones.&nbsp;Estos documentos est&aacute;n basado en el lenguaje&nbsp;</span><span style="color: #495057; font-size: small;"><strong>Markdown</strong></span><span style="color: #495057; font-size: small;">,&nbsp;la forma que&nbsp;</span><span style="color: #e83e8c; font-family: SFMono-Regular,Menlo,Monaco,Consolas,Liberation Mono,Courier New,monospace,serif,EmojiFont; font-size: xx-small;">RStudio</span><span style="color: #495057; font-size: small;">&nbsp;utilizo para adaptar documentos&nbsp;</span><span style="color: #495057; font-size: small;"><strong>Markdown</strong></span><span style="color: #495057; font-size: small;">&nbsp;a formatos especifico (pdf, HTML, Doc) fue a traves del&nbsp;convertidor&nbsp;</span><span style="color: #495057; font-size: small;"><strong>Pandoc.</strong></span><span style="color: #495057; font-size: small;">&nbsp;Para que pueda ocurrir la magia, de pasar del texto marcado al documento final,&nbsp;</span><span style="color: #e83e8c; font-family: SFMono-Regular,Menlo,Monaco,Consolas,Liberation Mono,Courier New,monospace,serif,EmojiFont; font-size: xx-small;">RStudio</span><span style="color: #495057; font-size: small;">&nbsp;utiliza el paquete&nbsp;</span><span style="color: #495057; font-size: small;"><strong>knitr</strong></span><span style="color: #495057; font-size: small;">, este paquete hace la codificaci&oacute;n del texto marcado, recordemos que puede tener c&oacute;digos, interpretando los caracteres para generar el documento final con los resultados esperados.&nbsp;</span></span></div>
<div><span style="color: black; font-family: Calibri,Helvetica,sans-serif,serif,EmojiFont; font-size: small;"><a id="LPlnk881118" href="https://rmarkdown.rstudio.com/" target="_blank" rel="noopener noreferrer"><span style="font-size: small;"><span id="LPlnk881118">https://rmarkdown.rstudio.com/</span></span></a><span style="color: #495057; font-size: small;"><br /></span><span style="color: #495057; font-size: small;"><br /></span></span></div>
<div><span style="color: black; font-family: Calibri,Helvetica,sans-serif,serif,EmojiFont; font-size: small;"><span style="color: #495057; font-size: small;"><strong>&iquest;Cu&aacute;les</strong></span><span style="color: #495057; font-size: small;"><strong>&nbsp;son requisitos del taller?</strong></span><span style="color: #495057; font-size: small;"><br /></span></span></div>
<div><span style="color: black; font-family: Calibri,Helvetica,sans-serif,serif,EmojiFont; font-size: small;"><span style="color: #495057; font-size: small;">Asumimos conocimiento y manejo minímo de entorno R  m&iacute;nimo&nbsp;, de lo contrario&nbsp;recomendamos estos videos:</span><br /></span></div>
<div><span style="color: black; font-family: Calibri,Helvetica,sans-serif,serif,EmojiFont; font-size: small;">&nbsp;</span></div>
<ul>
<li><a href="https://www.youtube.com/watch?v=OQLeFQNJUTs&amp;list=PLeasdnnD5qYaM52mMJ3-Lx8NAD6S1i8et&amp;index=7&amp;ab_channel=BocaRosaTeresa" target="_blank" rel="noopener noreferrer"><span style="font-size: small;">Instalar R</span></a></li>
<li><a href="https://www.youtube.com/watch?v=qP_0qZcKhCw&amp;list=PLeasdnnD5qYaM52mMJ3-Lx8NAD6S1i8et&amp;index=6&amp;ab_channel=BocaRosaTeresa" target="_blank" rel="noopener noreferrer"><span style="font-family: Calibri,Helvetica,sans-serif,EmojiFont,Apple Color Emoji,Segoe UI Emoji,NotoColorEmoji,Segoe UI Symbol,Android Emoji,EmojiSymbols; font-size: small;">Primeros pasos R</span></a></li>
<li><a href="https://www.youtube.com/watch?v=qP_0qZcKhCw&amp;list=PLeasdnnD5qYaM52mMJ3-Lx8NAD6S1i8et&amp;index=3&amp;ab_channel=BocaRosaTeresa" target="_blank" rel="noopener noreferrer">Uso de R</a></li>
</ul>
<div><span style="color: black; font-family: Calibri,Helvetica,sans-serif,serif,EmojiFont; font-size: small;">&nbsp;</span></div>
