
<!-- This is the project specific website template -->
<!-- It can be changed as liked or replaced by other content -->

<?php
$domain=ereg_replace('[^\.]*\.(.*)$','\1',$_SERVER['HTTP_HOST']);
$group_name=ereg_replace('([^\.]*)\..*$','\1',$_SERVER['HTTP_HOST']);
$themeroot='http://r-forge.r-project.org/themes/rforge/';
echo '<?xml version="1.0" encoding="UTF-8"?>

<!DOCTYPE html
	PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN"
	"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en" lang="en   ">

  <head>
	<meta http-equiv="Content-Type" content="text/html; charset=UTF-8" />
	<title><?php echo $group_name; ?></title>
	<link href="<?php echo $themeroot; ?>styles/estilo1.css" rel="stylesheet" type="text/css" />
  </head>

<body>

<!-- R-Forge Logo -->
<table border="0" width="100%" cellspacing="0" cellpadding="0">
<tr><td>
<a href="/"><img src="images/Rlogo.jpg" border="0" alt="R-Forge Logo" /> </a> </td> </tr>
<tr><td align="right">
<img src="images/gamesNWS.jpg" border="0" alt="R-gamesNWS Logo" /></td> </tr>
</table>


<!-- get project title  -->
<!-- own website starts here, the following may be changed as you like 

<?php if ($handle=fopen('http://'.$domain.'/export/projtitl.php?group_name='.$group_name,'r')){
$contents = '';
while (!feof($handle)) {
	$contents .= fread($handle, 8192);
}
fclose($handle);
echo $contents; } ?>

 end of project description -->
 
<h1>Welcome to Play games with R using the NWS Server technology!</h1>
<p>The R package 'gamesNWS' provides an infrastructure to play different 
card games (e.g. uno, poker) in a network. You can play the games with your friends 
in the whole world or against several computer players at your local machine. 
For the communication a NetWorkSpace Server and the R package NWS will be used. 
Just install a NWS Server, send the login data to your friends and start the game. 
</p>

<h2>How to play a game?</h2>
<p>Everyone has to install the 'gamesNWS' and 'NWS' package. 
<ul>
  <li>install.packages("gamesNws",repos="http://R-Forge.R-project.org")</li>
  <li>install.packages("nws")</li>
</ul>
Than a master user (one of the players) is required:
<ul>
  <li>First of all you need a running NWS Server. The installation is quite simple: http://nws-r.sourceforge.net/</li>
  <li>A master player has to create the game(e.g. a Uno game): startUnoGame('MyGame', serverHost='localhost') </li>
</ul>
Than all players have to connect to the same server(e.g. a Uno game): playUno('MyGame', serverHost='localhost', user='username')<br>
The rest is quite simple and will be explained during the game.
</p>

<h3>Example(UNO):</h3>
  <p>Start three R sessions<br>
  For all:<ul>
  	<li>install.packages("gamesNws",repos="http://R-Forge.R-project.org")</li>
  	<li>library(gamesNws)</li></ul>
  Console 1:<ul>
  <li>startUnoGame('exampleWorkSpace', serverHost='localhost',config=c(T,T,T,T))</li></ul>
  Console 2:<ul>
  <li>playUnoGame('exampleWorkSpace', serverHost='localhost', user='exampleplayer1')</li></ul>
  Console 3:<ul>
  <li>playUnoGame('exampleWorkSpace', serverHost='localhost', user='exampleplayer2')</li></ul>
  Console 1:<ul>
  <li>s</li></ul>
  Console 2 or 3:<ul>
  Depending on the played card,you can play one of the following commands:
  <li>get-info (shows some game-relevant information)</li>
  <li>NO (if you don't want or can't play a card)</li>
  <li>color-value (plays one of your hand cards)</li></ul>
</ul>
</p>

<h2>Implemented Games:</h2>
<ul>
<li><a href="uno.html">UNO</a></li>
</ul>

<h2>ToDo's</h2>
<ul>
  <li>Provide NWS-Game Server for everyone</li>
  <li>Implement second game: POKER</li>
  <li>Chat</li>
</ul>

<h2>How to add further games to the 'gamesNWS' package?</h2>
<p>ToDo</p>

<p> The <strong>project summary page</strong> you can find <a href="http://<?php echo $domain; ?>/projects/<?php echo $group_name; ?>/"><strong>here</strong></a>. </p>

</body>
</html>
