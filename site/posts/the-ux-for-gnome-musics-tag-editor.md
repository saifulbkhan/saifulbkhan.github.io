---
author: Saiful B. Khan
comments: true
date: 2016-06-08 16:17:34+00:00
title: The UX for GNOME Music's tag editor
wordpress_id: 125
categories:
  - GNOME
tags:
  - gnome-music
  - GSoC
---

Its the second week of GSoC 2016.  The development of a functional UI editor dialog is in progress. The editor at first should be able to allow the user to edit common tags ('title', 'album', 'artist', etc.) for a single song. If done properly it would pave a way to implement automatic tag suggestions and extensions for editing multiple songs (related or not) at once through the dialog.

<!-- more -->Here's a demonstration of how the user would go about from one of the Views in GNOME Music to edit a single song:


### Selecting a song


A song needs to be first selected if it is to be edited. This can be done through the _Songs_ view or the _AlbumWidget_ for a particular album. When the user selects a (single) song from one of these places, the _actionbar_ reveals itself which now offers an '**Edit Details'** button along with one of the playlist management buttons.

![GNOME Music: selecting songs in 'Songs' view](/images/posts/scr11.png) Selecting songs from 'Songs' view

![GNOME Music: selecting songs from an album](/images/posts/scr21.png) Selecting songs from an album

If the user chooses to _Edit Details_ of the selected song, the editor dialog is brought up.


### The tag editor dialog


The editor responsible managing details of a single song offers common and relevant data that the average user would care about. The provided fields, namely _Title_, _Album_, _Artist_, _Composer_, _Genre_, _Track_, _Disc_ and _Year_, are all editable. Apart from this the media-art associated with the song is also displayed. Clicking this cover will open up another dialog that allows file selection (only image types) that will now be used as the cover art for that song, replacing the old one.

![GNOME Music: tag editor dialog](/images/posts/scr31.png) The tag editor dialog allows common tags to be edited

![GNOME Music: tag editoy dialog fields](/images/posts/scr41.png) Edited entries are written as soon as 'Enter' is pressed or when entry loses focus

Each edit is physically stored into the music file as well as updated in the database. This part is taken care of by the tracker-writeback daemon. Once the user is satisfied with all the changes he/she can close the dialog.


### Player accommodates to changes


Any changes made to a song or any music entity, should be reflected in the database as well as the current graphical user interface. Following the close of editor dialog, the player and the different _views_ adjust their contents to accommodate the changes made. The edited song is inserted into the _Songs_ list and the old item is removed. The song is also added to the container provided for the song's album in the _Albums_ view.

![the edited song gets placed where it should](/images/posts/scr51.png) New song entry is inserted into the _Songs_ list

![edited song placed in the album container](/images/posts/scr71.png) The albums view as well as the album container reflect the changes written

Its worth noting that the above UI behavior is subject to change and it will quite possibly if I can find better ways to do things. The manual selection of cover art and an _Undo_ option for immediately edited songs are two other planned features. They're next in my cross-hairs.


### Long road ahead...


There are still a few bugs and defects lurking about, that keep the UI from behaving as it should ideally. I'll be working on them pretty soon, once I implement a functional dialog for manual edits to its full extent (like storing cover-art and and the _undo_ option, both features I didn't cover in this post). In fact my 'bugs-to-solve' list keeps growing  by the day. But I suppose that's unavoidable when you're developing software.

Thanks for reading and your thoughts are welcome!  :)



