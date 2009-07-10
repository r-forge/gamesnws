
<!-- This is the project specific website template -->
<!-- It can be changed as liked or replaced by other content -->

<?php

$domain=ereg_replace('[^\.]*\.(.*)$','\1',$_SERVER['HTTP_HOST']);
$group_name=ereg_replace('([^\.]*)\..*$','\1',$_SERVER['HTTP_HOST']);
$themeroot='http://r-forge.r-project.org/themes/rforge/';

echo '<?xml version="1.0" encoding="UTF-8"?>';
?>
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
<a href="/"><img src="<?php echo $themeroot; ?>/images/logo.png" border="0" alt="R-Forge Logo" /> </a> </td> </tr>
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
 
<h1>Welcome to Playing games with R using a NWS Server project!</h1>
<p>The R package 'gamesNWS' provides an infrastructure to play different 
games (e.g. uno, poker) in a network. You can play the games with your friends 
in the whole world or against several computer players at your local machine. 
For the communication a NetWorkSpace Server and the R package NWS will be used. 
Just install a NWS Server, send the login data to your friends and start the game. 
</p>

<h2>How to play a game?</h2>
<p>Everyone has to install the 'gamesNWS' and 'NWS' package. 
<ul>
  <li>install.packages("gamesNWS",repos="http://R-Forge.R-project.org")</li>
  <li>install.packages("NWS")</li>
</ul>
Than a master user (one of the players) is required:
<ul>
  <li>First of all you need a running NWS Server. The installation is quite simple: http://nws-r.sourceforge.net/</li>
  <li>A master player has to create the game: ws <- createUnoGame('MyGame', serverHost='localhost')</li>
  <li>The master user has to start the game: startUnoGame(ws) </li>
</ul>
Than all players have to connect to the same server: playUno('MyGame', serverHost='localhost')<br>
The rest is quite simple and will be explained during the game.
</p>

<h2>ToDo's</h2>
<ul>
  <li>UNO: calculate Points for winner</li>
  <li>UNO: improve logging for more statistcs</li>
  <li>Implement second game: POKER</li>
  <li>Write documentation</li>
  <li>Provide NWS-Game Server for everyone</li>
</ul>

<h2>How to add further games to the 'gamesNWS' package?</h2>
<p>ToDo</p>

<p> The <strong>project summary page</strong> you can find <a href="http://<?php echo $domain; ?>/projects/<?php echo $group_name; ?>/"><strong>here</strong></a>. </p>

</body>
</html>
