---
author: Saiful B. Khan
tags:
  - GNOME
  - WebKit
  - gobject-introspection
title: "Wrapping Webkit's Javascript call API"
date: "2017-09-30T21:47:28+05:30"
draft: false
---

WebKitGTK+ allows embedding a fully-featured web engine in GTK+ applications,
and you probably would not find a better option if you want to support any kind
of browser-like capabilities in the app. It even allows running javascript
in context of the web page being rendered by a [WebKit WebView](https://webkitgtk.org/reference/webkit2gtk/stable/WebKitWebView.html).
This post is about some trouble I ran into while trying to call javascript from
a webview and how I went about solving it.

TL;DR - I wrote an introspectable API.


#### The Problem

The operation of executing and returning the result is asynchronous in WebKit
(and rightly so!). Extracting the result out of the `WebKitJavascriptResult`
object involves fetching references to other objects, namely `JavaScriptCore
.GlobalContext` and `JavaScriptCore.Value`. Since the JavaScriptCore library,
which is written in C++, exposes an interface to call upon and interact with
these objects, the whole process of running and reading results becomes quite
easy through GObject C, as [illustrated in the WebKitGTK+ docs](https://webkitgtk.org/reference/webkit2gtk/stable/WebKitWebView.html#webkit-web-view-run-javascript-finish).

However, I wanted to make these execution calls using PyGObject (for an app
I'm writing) and found out it was not possible because the module
`gi.repository.JavaScriptCore` contains only opaque definitions for the
`GlobalContext` and `Value` types, both of which are needed to extract any form
of meaningful data from the result of an async call. Try fetching one of these
from a WebKitJavascriptResult and you get an exception:

```text
TypeError: Couldn't find foreign struct converter for 'JavaScriptCore.GlobalContext'
```

Even so, I do not think this was an oversight. PyGObject does cover as much of
the API as it was intended to. Supporting types for a library (JavaScriptCore)
external to WebKitGTK+ just for the convenience of one method seems unfair.
Though I would have really liked if we did not have to deal with these
'external' objects altogether. But, I suppose things are the way they are for a
reason, even though I fail to get the big picture. Nonetheless, a solution was
needed.


#### The Solution

I tried looking around the web for a quick fix, but I found none. I came after
a [library](https://github.com/sumary/pyjavascriptcore) written for an outdated
version of WebKit and was huge enough to prevent me from attempting to bring it
up to date. I tried using everything from `ctypes` module to Cython to directly
call C code or at least wrap it but could not get it to work. I do think its
still possible to do this using Cython but I conceded defeat after a few 
MemoryErrors (bad hacking on my part). [This answer](https://stackoverflow.com/a/38041497/6547160)
on SO finally inspired me to write my own introspectable C API.

I wrote libwkjscore which provides just about enough utility to retrieve most
basic types of data from `WebKitJavascriptResult` objects. Its written in C so
that GIR and typelib files can easily be generated for use in other languages.
It allows declaring a `WkJsCoreResult` object which acts as a more handy proxy
for the particular `WebKitJavascriptResult` object that is passed to its
constructor. From this object one can ask for its result type and issue one of
the result processing functions to get its value either as a boolean, a number
or a string. For Javascript objects that do not fall in these categories we can
stringify them if possible.

Once library is built, along with GIR and typelib files, and installed (using
[Meson](mesonbuild.com) for all this), we can import it in a language that has
GObject introspection bindings for it. I have only used it in Python for now
but it should work in GJS and Vala in a similar manner. Here's a toy demo
(hope I'm not being too pedantic):

```python
import gi
gi.require_version('Gtk', '3.0')
gi.require_version('WebKit2', '4.0')
gi.require_version('WkJsCore', '0.1')

from gi.repository import Gtk, WebKit2 as WebKit, WkJsCore
```

The above boilerplate is to make sure that the correct version of each of those
libraries are imported. Next we write a function in which we create a scrolled
window within a `GtkWindow` and connect some signals.

```python
def create_window():
    window = Gtk.Window()
    window.set_default_size(250, 100)
    window.connect('delete-event', Gtk.main_quit)

    # Create webview and connect callback to
    # any kind content load change signal
    webview = WebKit.WebView()
    webview.connect("load_changed", load_change_cb)
    webview.load_html('<p id="para">Lorem ipsum</p>')

    # Need a scrollable container for a proper webview	
    scrolled = Gtk.ScrolledWindow(hadjustment=None, vadjustment=None)
    scrolled.add(webview)

    window.add(scrolled)
    return window

def load_change_cb(webview, load_event):
    # This is the event when any kind of content loading
    # has just finished. That's HTML in our case.
    if load_event == WebKit.LoadEvent.FINISHED:
        # Get HTML inside element with id = 'para'
        script = 'document.getElementById("para").innerHTML'
        webview.run_javascript(script, None, js_finished_cb, None)
```

Anybody who has written an application using WebKit, the above code may seem
familiar. We are setting up a GtkWindow with webview, loading some HTML (raw or
from a URI), and then doing something once the content has loaded. In this case
we are calling Javascript on the loaded content. When webview has finished
executing our JS code, it calls the callback function `js_finished_cb` and
passing along the `GAsyncResult` from the completed operation. Running
`webview.run_javascript_finish()` function on this result will give us a
`WebKitJavascriptResult` object. But we won't be able to proceed any further
in obtaining the _actual_ result from our JS code, at least not from
PyGObject. This is where `WkJsCoreResult` can help:

```python
def js_finished_cb(webview, result, user_data):
    js_result = webview.run_javascript_finish(result)

    # Create a WkJsCore.Result proxy for js_result
    result_proxy = WkJsCore.Result(jsresult=js_result)

    # The result is supposed to be a string...
    assert result_proxy,get_result_type() == WkJsCore.Type.STRING

    # Fetch the result using appropriate process method
    # for the type you want i.e., string in this case
    html_string = result_proxy.process_result_as_string()
    print(html_string)
```

That was simple enough right? You pass a `WebKitJavascriptResult` to construct
a `WkJsCoreResult` object from which you can inquire its type and then ask it
for the actual value stored in it through whatever method you feel would be
appropriate to prcoess it. Now, all we need is to start the cascade:

```python
if __name__ == '__main__':
    window = create_window
    window.show_all()
    Gtk.main()
```

This should spawn a tiny window with the string `Lorem ipsum` and also print
it to the standard output as expected from `js_finished_cb` callback. Close
the window and our demonstration is complete. For a proper description of the
methods available for `WkJsCoreResult`, take a look at the [interface file](https://github.com/saifulbkhan/wkjscore-result/blob/master/src/wkjscoreresult.h).

Now I can write my Python app which _can_ run Javascript through a WebView,
although I did add another dependency. But its fast and compact so thats ok.
Feel free to use it in your own projects. You can find [wkjscore-result](https://github.com/saifulbkhan/wkjscore-result)
on Github, along with instructions to build and install it. Please report any
issues that you encounter.
