/* -*- C++ -*- */

/**
 * From Netflix, Completely re-written, stripped down and optimized.
 * Plus I made all new images.
 *
 */

var bar_type_empty   = 0;
var bar_type_predict = 1;
var bar_type_rate    = 2;
var bar_type_vote    = 3;
var bar_type_max    = 3;

var bar_height       = 10;
var bar_star_width   = 10;

var bar_gap_width    = 1;
var bar_width        = 50
var bar_row_padding  = 3;

var bar_img_root = 'http://content.ytmnd.com/assets/images/stars/';
var bar_set_page = 'http://www.ytmnd.com/vote/'

var barimages;
var bartooltip = new Array();

bartooltip[1] = 'lame';
bartooltip[2] = 'uh ok';
bartooltip[3] = 'neat';
bartooltip[4] = 'cool';
bartooltip[5] = 'hot';

var barcount = 0;
var user_id = new Array();
var baritemids = new Array();
var bartypes = new Array();
var barnumstars = new Array();
var bartimers = new Array();
var barpredictions = new Array();
var barspecialvalues = new Array();
var barlastentered = -1;
var barenabled = false;

var wsctimers = new Array();
var wscpicks = new Array();
var wsclastentered = -1;

/**
 * Get the image filename for a vote bar by color and number.
 *
 */

function barimagename(startype, n) {

  if (startype == 1) { star_color = 'red';  }
  if (startype == 2) { star_color = 'gold'; }
  if (startype == 3) { star_color = 'blue'; }
  if (startype == 4) { star_color = 'grey'; }

  if (n == 'NaN') {
    n = 0;
  }

  return bar_img_root + "stars_" + star_color + "_" + n + ".png";
}


function barimage(startype, numstars) {
  var n = Math.min(Math.max(Math.round(numstars * 10), 0), 10*5);
  if (!barimages[startype][n]) {
    barimages[startype][n] = new Image(bar_width, bar_height);
    barimages[startype][n].src = barimagename(startype, n);
  }
  return barimages[startype][n].src;
}


function barpreload() {
  if (!barimages) {
    barimages = new Array();
    for (var j = 0; j <= bar_type_max; j++) {
      barimages[j] = new Array();
    }
    for (var j = bar_type_rate; j <= bar_type_rate; j++) {
      for (var i = 1; i <= 5; i++) {
	barimages[j][i] = new Image(bar_width, bar_height);
	barimages[j][i].src = barimagename(j, 10*i);
      }
    }
    barenabled = true;
  }
}

function wscmouseover (imgnum, which, tooltip) {


  if (wsctimers[imgnum] != 0) {
    window.clearTimeout(wsctimers[imgnum]);
    wsctimers[imgnum] = 0;
  }


  if (wsclastentered >= 0 && wsclastentered != imgnum) {
    wscrestore(wsclastentered);
  }  

  if (wsclastentered >= 0 && wsclastentered == imgnum && which != '') {
    wscrestore(wsclastentered);
  }
  wsclastentered = imgnum;


  if (which != '') {
    document.images["wsc"+imgnum].src = bar_img_root+"nsw_"+which+".png";
  }

  window.status = tooltip;
}

function barmouseover(imgnum, whichstar) {
    if (bartimers[imgnum] != 0) {
        window.clearTimeout(bartimers[imgnum]);
        bartimers[imgnum] = 0;
    }

    if (barlastentered >= 0 && barlastentered != imgnum) {
      barrestore(barlastentered);
    }

    if (barlastentered >= 0
            && barlastentered == imgnum
            && whichstar < 0) {
        barrestore(barlastentered);
    }
    barlastentered = imgnum;


    if (whichstar > 0) {
        document.images["bar"+imgnum].src = barimage(bar_type_rate, whichstar);
    } else {
        //
    }

    window.status = bartooltip[whichstar];
    return false;
}

function wscrestore (imgnum) {
  document.images["wsc"+imgnum].src = bar_img_root+"nsw_"+wscpicks[imgnum]+".png";
  
  wsctimers[imgnum] = 0;

  if (wsclastentered == imgnum) {
    wsclastentered = -1;
  }

  window.status = "";
}

function barrestore(imgnum) {

    document.images["bar"+imgnum].src = barimage(bartypes[imgnum], barnumstars[imgnum]);

    bartimers[imgnum] = 0;

    if (barlastentered == imgnum) {
        barlastentered = -1;
    }

    window.status = "";
}


function wscmouseout(imgnum, which) {

  if (! wsctimers[imgnum]) {
    wsctimers[imgnum] = window.setTimeout("wscrestore("+imgnum+")", 100);
  }        
  window.status = "";

}

function barmouseout(imgnum, whichstar) {
    // restore the image to the saved state, once a little time has elapsed.

    if (! bartimers[imgnum]) {
        bartimers[imgnum] = window.setTimeout("barrestore("+imgnum+")", 100);
    }
    window.status = "";

}

function barvote(imgnum, whichstar) {
  document.getElementById('bar'+imgnum).src = barimage(3, whichstar);
}

function wscclick (imgnum, which) {

  var itemid;
  var ratehref;
  var ratewindow;

  if (!barenabled) {
    return void(0);
  }

  itemid = baritemids[imgnum];

  ratehref = "?site_id="+itemid+"&wsc="+which+"&user_id="+user_id[itemid];
  wscpicks[imgnum] = which;
  ratehref = bar_set_page + ratehref;
  window.setTimeout("wscrestore("+imgnum+")", 10);
  
  if (document.getElementById('voter').src != false) {
    document.getElementById('voter').src = ratehref + '&type=img';
    barenabled = true;
    return void(0);
  }

  // try to use an iframe.
  if (window.vote_frame) {
    window.vote_frame.location.href = ratehref+"&type=iframe";
    barenabled = true;
    return void(0);
  }

  // if that failed, try using a pop-up window.
  ratewindow = window.open("", "nf_gauge_set", "resizable=no,dependent=yes,width=1,height=1,screenx="
			   +window.screenx+",screeny="+window.screeny
			   +",top="+window.screenx+",left="+window.screeny);

  if (ratewindow && ! ratewindow.closed) {
    //ratewindow.blur();
    ratewindow.location.href = ratehref+"&type=js";
    //self.focus();
    barenabled = true;
    return void(0);
  }

  // if that failed, run in this window.
  window.location.href = ratehref + '&type=force';
  barenabled = true;
  return void(0);
}

function barclick (imgnum, whichstar) {

    var itemid;
    var ratehref;
    var ratewindow;

    if (!barenabled) {
        return void(0);
    }


    barenabled = false;
    itemid = baritemids[imgnum];
    ratehref = "?site_id="+itemid+"&vote="+whichstar+"&user_id="+user_id[itemid];
    bartypes[imgnum] = bar_type_vote;
    barnumstars[imgnum] = whichstar;
    barspecialvalues[imgnum] = 0;

    ratehref = bar_set_page + ratehref;
    window.setTimeout("barrestore("+imgnum+")", 10);

    // use the transparent gif to tally votes
    
    if (document.getElementById('voter').src != false) {
      document.getElementById('voter').src = ratehref + '&type=img';
      barenabled = true;
      return void(0);
    }

    // try to use an iframe.
    if (window.vote_frame) {
      window.vote_frame.location.href = ratehref+"&type=iframe";
      barenabled = true;
      return void(0);
    }

    // if that failed, try using a pop-up window.
    ratewindow = window.open("", "nf_gauge_set", "resizable=no,dependent=yes,width=1,height=1,screenx="
			     +window.screenx+",screeny="+window.screeny
			     +",top="+window.screenx+",left="+window.screeny);

    if (ratewindow && ! ratewindow.closed) {
        //ratewindow.blur();
        ratewindow.location.href = ratehref+"&type=js";
        //self.focus();
        barenabled = true;
        return void(0);
    }

    // if that failed, run in this window.
    window.location.href = ratehref + '&type=force';
    barenabled = true;
    return void(0);
}


function barinsert1 (imgnum, itemid, startype, numstars) {
    var alttext;
    var imgattrs;
    barpreload();
    if (numstars == 1) {
        alttext = numstars+" star";
    } else {
        alttext = numstars+" stars";
    }
    imgattrs = " src='" + barimage(startype, numstars) + "'"
               + " alt='" + alttext + "'"
               + " width=" + bar_width
               + " height=" + bar_height
               + " border=0";

    with (document) {
        if (itemid < 0) {
            write("<img"+imgattrs+">");
        } else {
            // associate the gap between stars with the star just to the left of the gap.
	    write("<map name='wscm"+imgnum+"'>");
	    write("<area href='#"+'vote_'+itemid+"' onclick='javascript:wscclick("+imgnum+",\"red\");' shape='rect' coords='0,0,23,10' alt='not work-safe' onmouseover='wscmouseover("+imgnum+",\"red\", \"not work-safe\");' onmouseout='wscmouseout("+imgnum+", \"red\");'>");
	    write("<area href='#"+'vote_'+itemid+"' onclick='javascript:wscclick("+imgnum+",\"green\");' shape='rect' coords='23,0,40,10' alt='work-safe' onmouseover='wscmouseover("+imgnum+",\"green\", \"work-safe\");' onmouseout='wscmouseout("+imgnum+", \"green\");'>");
	    write("</map>");
            write("<map name='bar"+imgnum+"'>");
            for (var i = 1; i <= 5; i++) {
                var left = (i-1) * (bar_star_width + bar_gap_width);
                var right = Math.min(left + bar_star_width +bar_gap_width - 1, bar_width);
                var top = 0;
                var bottom = bar_height - 1;
		var randName = 'vote_' + itemid;
                write("<area href='#"+randName+"' name='"+randName+"' onclick='javascript:barclick("+imgnum+","+i+");' alt='"+bartooltip[i]+"'");
                write(" onmouseover='barmouseover("+imgnum+","+i+");'");
                write(" onmouseout='barmouseout("+imgnum+","+i+");'");
                write(" shape='rect' coords='"+left+","+top+","+right+","+bottom+"'>");
            }
            write("</map>");
            write("<img"+imgattrs+" name='bar"+imgnum+"' id='bar"+imgnum+"'  usemap='#bar"+imgnum+"'>");
	    write("&nbsp;<img border='0' src='"+bar_img_root+"nsw_"+wscpicks[imgnum]+".png' name='wsc"+imgnum+"' id='wsc"+imgnum+"' usemap='#wscm"+imgnum+"'>");
        }
    }
}





function barinsert (userid, itemid, startype, numstars, wsc) {
  
    var imgnum = barcount++;


    if (itemid >= 0) {
        baritemids[imgnum] = itemid;
        bartypes[imgnum] = startype;
        barnumstars[imgnum] = numstars;
	wscpicks[imgnum] = wsc;

        bartimers[imgnum] = 0;
        wsctimers[imgnum] = 0;

	barspecialvalues[imgnum] = 0;
	user_id[itemid] = userid;
    }

    var totalwidth = bar_width;
    with (document) {
      barinsert1(imgnum, itemid, startype, numstars);

    }
}
