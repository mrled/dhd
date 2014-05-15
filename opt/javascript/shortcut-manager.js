// ==UserScript==
// @ShortcutManager
// @name Pinboard++
// @namespace JjSqFvexWTlV
// @key Ctrl+Shift+p
// @include *
// ==/UserScript==
javascript:q=location.href;if(document.getSelection){d=document.getSelection();}else{d='';};p=document.title;void(open('https://pinboard.in/add?url='+encodeURIComponent(q)+'&description='+encodeURIComponent(d)+'&title='+encodeURIComponent(p),'Pinboard',%20'toolbar=no,width=700,height=350'));


// ==UserScript==
// @ShortcutManager
// @name Instapaper++
// @namespace JjSqFvexWTlV
// @key Ctrl+Shift+i
// @include *
// ==/UserScript==
javascript:function%20iprl5()%7Bvar%20d=document,z=d.createElement('scr'+'ipt'),b=d.body;try%7Bif(!b)throw(0);d.title='(Saving...)%20'+d.title;z.setAttribute('src','https://www.instapaper.com/j/r69NBgXqtoWG?u='+encodeURIComponent(d.location.href)+'&t='+(new%20Date().getTime()));b.appendChild(z);%7Dcatch(e)%7Balert('Please%20wait%20until%20the%20page%20has%20loaded.');%7D%7Diprl5();void(0)


// ==UserScript==
// @ShortcutManager
// @name SearchFocus
// @namespace JjSqFvexWTlV
// @key Ctrl+Shift+s
// @include *
// ==/UserScript==
javascript:var%20inputs=document.getElementsByTagName('input'),firstSearch=false,textinputs=[],i,t;for(i=0;i<inputs.length;i++)if(((inputs[i].type==='text')||(inputs[i].type==='search'))&&inputs.disabled!==true)textinputs.push(inputs[i]);for(t=0;t<textinputs.length;t++)if((/search/i).test(textinputs[t].className)||(/(^[sq]$|search|query)/i).test(textinputs[t].id)||(/^(q(uery)?|s|.*?search.*)$/).test(textinputs[t].name)){firstSearch=textinputs[t];break;}if(!firstSearch)textinputs[0].focus();else%20firstSearch.focus();


// ==UserScript==
// @ShortcutManager
// @name DDG->Google
// @namespace JjSqFvexWTlV
// @key Ctrl+Shift+g
// @include http://duckduckgo.com/*
// @include https://duckduckgo.com/*
// ==/UserScript==
javascript:var%20nl='https://google.com/search?q='+document.getElementById('search_form_input').value;window.location.href=nl;


// ==UserScript==
// @ShortcutManager
// @name DDG->Yandex
// @namespace JjSqFvexWTlV
// @key Ctrl+Shift+r
// @include http://duckduckgo.com/*
// @include https://duckduckgo.com/*
// ==/UserScript==
javascript:var%20nl='http://www.yandex.com/yandsearch?text='+document.getElementById('search_form_input').value;window.location.href=nl;




// ==UserScript==
// @ShortcutManager
// @name contentEditable = true;
// @namespace JjSqFvexWTlV
// @key Ctrl+Shift+e
// @include *
// ==/UserScript==
javascript:document.body.contentEditable = true;

