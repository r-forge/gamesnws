
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
  <li>install.packages("gamesNws",repos="http://R-Forge.R-project.org")</li>
  <li>install.packages("nws")</li>
</ul>
Than a master user (one of the players) is required:
<ul>
  <li>First of all you need a running NWS Server. The installation is quite simple: http://nws-r.sourceforge.net/</li>
  <li>A master player has to create the game: ws <- createUnoGame('MyGame', serverHost='localhost')</li>
  <li>The master user has to start the game: startUnoGame(ws) </li>
</ul>
Than all players have to connect to the same server: playUno('MyGame', serverHost='localhost', user='username')<br>
The rest is quite simple and will be explained during the game.
</p>

<h3>Example:</h3>
  <p>Run 3 Unix-consoles.<br>
  For all:<ul>
  	<li>install.packages("gamesNws",repos="http://R-Forge.R-project.org")</li>
  	<li>install.packages("nws")</li>
  	<li>library(nws)</li>
  	<li>library(gamesNws)</li></ul>
  Console 1:<ul>
  <li>ws <- createUnoGame('exampleWorkSpace', serverHost='138.245.80.17')</li>
  <li>startUnoGame(ws)</li></ul>
  Console 2:<ul>
  <li>playUnoGame('exampleWorkSpace', serverHost='138.245.80.17', user='exampleplayer1')</li></ul>
  Console 3:<ul>
  <li>playUnoGame('exampleWorkSpace', serverHost='138.245.80.17', user='exampleplayer2')</li></ul>
  Console 1:<ul>
  <li>s</li>
  <li>y</li>
  <li>y</li>
  <li>y</li>
  <li>y</li></ul>
  Console 2 or 3:<ul>
  <li>get-info</li>
  <li>NO</li>
  <li>red-5</li></ul>
</ul>
</p>

<h2>ToDo's</h2>
<ul>
  <li>logfile-directory</li>
  <li>GUI</li>
  <li>Further logging-modes for more statistical information</li>
  <li>Further computer player with different skills:<ul>
  	<li>Try to concatenate penalties</li>
  	<li>Randomly forget to say "UNO"</li>
  	<li>Try not to have rybg-cards at the end</li>
  	<li>Try to combine BREAK and/or BACK cards in 2-Player-game</li>
  	<li>Try to play maximum-scored card</li>
  	<li>Try to play minimum-scored card</li>
  	<li>...</li></ul></li>
  <li>Provide NWS-Game Server for everyone</li>
  <li>Implement second game: POKER</li>
  <li>Chat</li>
</ul>

<h2>UNO-Rules</h2>
<p>We used the rules discribed in <a href="http://en.wikipedia.org/wiki/Uno_(game)">wikipedia(engl)</a>.<br>
And we started to implement some of the additional rules described in <a href="http://de.wikipedia.org/wiki/Uno_(Kartenspiel)">German wikipedia</a>.</p>

<h2>UNO-Commands</h2>
<table border=1>
	<tr>
		<th>Command</th>
		<th>Function</th>		
	</tr>
	<tr>
		<td>get-info</td>
		<td>shows some game-relevant information</td>
	</tr>
	<tr>
		<td>say-uno</td>
		<td>this command has to be issued, before the second-last card is played</td>
	</tr>
	<tr>
		<td>NO</td>
		<td>if you don't want or can't play a card</td>
	</tr>
	<tr>
		<td>(color)-(value)</td>
		<td>color = {red, yellow, blue, green, rybg}<br>value = {0:9, 2+, 4+, BREAK, BACK}</td>
	</tr>
</table>


<h2>How to add further games to the 'gamesNWS' package?</h2>
<p>ToDo</p>

<p> The <strong>project summary page</strong> you can find <a href="http://<?php echo $domain; ?>/projects/<?php echo $group_name; ?>/"><strong>here</strong></a>. </p>

</body>
</html>
