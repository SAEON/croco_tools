<?php

require 'templates/header.tpl';

if (!isset($_POST['confirm']))
{

	require 'templates/form.tpl';

}
else
{
	include('config.php');

	$message="Name: ".$_POST['name']."\n\rInstitution: ".$_POST['institution']."\n\rCountry: ".$_POST['country']."\n\rMail: ".$_POST['mail']."\n\rScientific Project: ".$_POST['project']."\n\rComment: ".$_POST['comment'];
	if ( mail($destinataire, $objet, $message, $entetes) ) 
	{ 
		require 'templates/download.tpl';
	}
	else
	{ 
		 echo "<h2>Echec de l'envoi du mail, contactez l'administrateur</h2>"; 
	}

}
require 'templates/footer.tpl';

?>
