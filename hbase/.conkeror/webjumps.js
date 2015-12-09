define_webjump("duckduckgo", "https://duckduckgo.com/?q=%s&kp=-1&kl=us-en&kh=1&kc=0");
define_webjump("d", "https://duckduckgo.com/?q=%s");
//define_webjump("g", "https://duckduckgo.com/?q=%s"); // use "goo" for Google

// pinboard
define_webjump("pb", "javascript:q=location.href;if(document.getSelection){d=document.getSelection();}else{d='';};p=document.title;void(open('https://pinboard.in/add?url='+encodeURIComponent(q)+'&description='+encodeURIComponent(d)+'&title='+encodeURIComponent(p),'Pinboard','toolbar=no,width=700,height=350'));");

// instapaper
define_webjump("ip", "javascript:function%20iprl5(){var%20d=document,z=d.createElement('scr'+'ipt'),b=d.body,l=d.location;try{if(!b)throw(0);d.title='(Saving...)%20'+d.title;z.setAttribute('src',l.protocol+'//www.instapaper.com/j/r69NBgXqtoWG?u='+encodeURIComponent(l.href)+'&t='+(new%20Date().getTime()));b.appendChild(z);}catch(e){alert('Please%20wait%20until%20the%20page%20has%20loaded.');}}iprl5();void(0)");

// readability
define_webjump("read", "javascript:(%28function%28%29%7Bwindow.baseUrl%3D%27http%3A//www.readability.com%27%3Bwindow.readabilityToken%3D%27%27%3Bvar%20s%3Ddocument.createElement%28%27script%27%29%3Bs.setAttribute%28%27type%27%2C%27text/javascript%27%29%3Bs.setAttribute%28%27charset%27%2C%27UTF-8%27%29%3Bs.setAttribute%28%27src%27%2CbaseUrl%2B%27/bookmarklet/read.js%27%29%3Bdocument.documentElement.appendChild%28s%29%3B%7D%29%28%29)");

// Internet Archive Wayback Machine
define_webjump("wb", "javascript:location.href='http://web.archive.org/web/*/'+document.location.href;");

// neuric stuff
define_webjump("rt","https://bugs.neuric.internal/Dashboards/31/Micah%27s%20Tickets");
