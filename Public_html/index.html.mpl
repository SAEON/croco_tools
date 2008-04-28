<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN"
    "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">

<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="fr" lang="fr">

  <head>
    <title>ROMS</title>
    <meta http-equiv="Content-Type" content=
    "text/html; charset=iso-8859-1" />
    <link type="text/css" href="style.css" media="all" rel="stylesheet" />
  </head>


  <body>
	
    <div id="logo"><a href="http://www.brest.ird.fr"></a></div>	
    <h1>ROMS_AGRIF / ROMSTOOLS</h1>
    <h2>A Regional Oceanic Modeling System with mesh refinement, 
        pre- and post-processing tools</h2>

      <ul id="menu">
        <li><a href="doc/index.html">USER'S GUIDE</a></li>
        <li><a href="download.php">DOWNLOAD</a></li>
        <li><a href="changelog.html">CHANGE LOG</a></li>
	<li><a href="documentation.html">DOCUMENTATION</a></li>
        <li><a href="https://gforge.inria.fr/forum/?group_id=735">FORUM</a></li>
        <li><a href="references.html">REFERENCES</a></li>
        <li><a href="links.html">LINKS</a></li>
      </ul>

      <p class="desc">Over the last several years, we have developed at UCLA, Rutgers University and 
                      IRD a three-dimensional numerical oceanic model intended for simulating currents, 
		      ecosystems, biogeochemical cycles, and sediment movement in various coastal 
		      regions. It is called the Regional Oceanic Modeling System (ROMS). This IRD 
		      version of the code (ROMS_AGRIF) makes use of the AGRIF grid refinement procedure 
		      developed at the LMC-IMAG and is accompanied by a powerful toolbox for ROMS pre- 
		      and post-processing: ROMSTOOLS.</p>

      <h3> Update and News :</h3>
      <ul>
	<li> 04/28/2008 : <a href="changelog.html">ROMSTOOLS Update</a>.</li>

      </ul>	

      <hr class="spacer" />
      <div class="images"><img src="images/sst2_beng.jpg" alt="sst2_beng" />
      <img src="images/vort_peru.jpg" alt="vort_peru" /></div>

      
      <h3>Acknowledgements:</h3>

      <p>Roms_Agrif is the fruit of a collaborative work between IRD (Institut de Recherche pour le 
         Developpement; P. Marchesiello, P. Penven), INRIA (Institut National de Recherche en Informatique 
	 et en Automatique, LMC-IMAG; Laurent Debreu), and UCLA (Coastal Center of the Institute of the 
	 Environment; J.C. McWilliams, A. Shchepetkin, X. Capet, M. Blaas, H. Frenzel).</p>

      <p>ROMSTOOLS benefits from several Matlab packages: 
         <ul>
	 <li><a href="http://woodshole.er.usgs.gov/operations/sea-mat/air_sea-html/index.html"> 
	     Air-Sea Matlab Toolbox</a> developed by B. Beardlsley and R. Pawlowicz.</li>
	 <li><a href="http://www.whoi.edu/science/PO/people/ashcherbina/">
	     Matlab Editmask Toolbox</a> developed by A. Y. Shcherbina.</li>
	 <li><a href="http://mexcdf.sourceforge.net/">Mexnc Toolbox</a> developed by J. Evans.</li>
	 <li><a href="http://www.eos.ubc.ca/~rich/map.html"> Matlab Mapping Toolbox</a> 
	     developed by R. Pawlowicz.</li>
	 <li><a href="http://mexcdf.sourceforge.net/netcdf_toolbox/netcdf_toolbox.html">
	     Matlab NetCDF Toolbox</a> developed by C. R. Denham.</li>
	 <li><a href="http://www.opendap.org/download/ml-structs.html">
	     Matlab OpenDAP Toolbox</a> developed by G. Flierl and J. Gallagher.</li>
         </ul>
      </p>

      <p>ROMSTOOLS uses also several dataset: 
         <ul>
	 <li><a href="http://iridl.ldeo.columbia.edu/SOURCES/.DASILVA/.SMD94"> 
	     DASILVA Atlas of Surface Marine Data</a>.</li>
	 <li><a href="http://airsea-www.jpl.nasa.gov/seaflux/">
	     QuikSCAT winds</a>.</li>
	 <li><a href="http://oceancolor.gsfc.nasa.gov/cgi/climatologies.pl">
	     SeaWIFS Ocean Color Climatologies</a>.</li>
 	 <li><a href="http://podaac-www.jpl.nasa.gov/sst/">Pathfinder Sea Surface 
	     Temperature Climatology</a>.</li>
	 <li><a href="http://topex.ucsd.edu/cgi-bin/get_data.cgi">ETOPO2 Global
	     Topography</a>.</li>
	 <li><a href="http://www.coas.oregonstate.edu/research/po/research/tide/global.html">
	     TPXO global model of ocean tides</a>.</li>
	 <li><a href="http://www.nodc.noaa.gov/OCL/indprod.html">
	     World Ocean Atlas</a>.</li>
        </ul>
      </p>


      <h3>Contacts:</h3>

      <address><strong>
              <a href="http://stockage.univ-brest.fr/~cambon/">
	      Gildas Cambon</a></strong><br />
	      Institut de Recherche pour le Developpement<br />
	      UR065 LEGOS<br />
	      <strong>FRANCE</strong><br />
	      Gildas.Cambon*AT*ird.fr<br />
      </address>

      <address><strong>
              <a href="http://www-ljk.imag.fr/membres/Laurent.Debreu">
	      Laurent Debreu</a></strong><br />
	      INRIA<br />
	      51 rue des Mathematiques<br />
	      38041 Grenoble Cedex 9 <br />
	      <strong>FRANCE</strong><br />
	      Laurent.Debreu*AT*imag.fr<br /><br />
      </address>

      <address><strong>
              <a href="http://www.ird.nc/UR65/Marchesiello/">
	      Patrick Marchesiello</a></strong><br />
	      Institut de Recherche pour le Developpement<br />
	      UR065 LEGOS<br />
	      Centre IRD de Noumea<br /> 
              98848 Noumea Cedex <br />
	      <strong>NOUVELLE CALEDONIE</strong><br />
	      <img src="images/marchesiello_mail.jpg" height=15 /><br />
      </address>

      <address><strong>
              <a href="http://www.brest.ird.fr/personnel/ppenven/">
	      Pierrick Penven</a></strong><br />
	      Institut de Recherche pour le Developpement<br />
	      UR097 ECO-UP<br />
	      Centre IRD de Bretagne<br />
	      B.P. 70 - 29280 Plouzane<br />
	      <strong>FRANCE</strong><br />
	      <img src="images/penven_mail.jpg" height=15  /><br />
      </address>

      <hr class="spacer" />



  </body>
</html>
