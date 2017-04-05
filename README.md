instaR: Access to Instagram API via R
---------

This package provides a series of functions that allow R users to access Instagram's API to search for media that mention specific hashtags or were sent from a certain location, to download recent pictures from a given users, and to capture lists of followers.

__IMPORTANT__: according to the [updated platform policy](https://www.instagram.com/developer/changelog/), access to most endpoints of the Instagram API (including those that return public data) requires previous approval by Instagram. More information about the permission review process can be found [here](https://www.instagram.com/developer/review/). 

## Getting Started from GitHub
To install from github, first install the devtools package and and then run ``devtools::install_github("pablobarbera/instaR/instaR")``

The httpuv package is needed to trigger the web request when authorizing against Instragram using instaOAuth. When configuring the client on https://instagram.com/developer/clients/ to receive your `app_id` and `app_secret` ensure the REDIRECT URI includes the trailing / e.g. `http://localhost:1410/`

Functionalities include:
* Downloading pictures from users / using hashtags
* Getting comments for individual posts
* Getting likes for individual posts
* Getting the most popular posts
* Getting basic user information
* Getting a hash tag count

For examples please see examples.R

<script type="text/javascript">

  var _gaq = _gaq || [];
  _gaq.push(['_setAccount', 'UA-1191254-10']);
  _gaq.push(['_trackPageview']);

  (function() {
    var ga = document.createElement('script'); ga.type = 'text/javascript'; ga.async = true;
    ga.src = ('https:' == document.location.protocol ? 'https://ssl' : 'http://www') + '.google-analytics.com/ga.js';
    var s = document.getElementsByTagName('script')[0]; s.parentNode.insertBefore(ga, s);
  })();

</script>

