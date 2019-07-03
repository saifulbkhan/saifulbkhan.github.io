---
author: Saiful B. Khan
comments: true
date: 2016-07-28 10:21:41+00:00
title: Automatic metadata download in Music
wordpress_id: 369
categories:
  - GNOME
tags:
  - gnome-music
  - GSoC
---

Its been quite some time since I last blogged. My semester exams prevented me from working on the project. I've had a month after that to get automatic metadata fetching available for the tag editor.<!-- more -->

My original plan was to use metadata already being presented by the solid AcoustID + chromaprint plugin combination through grilo, only to realise later that the AcoustID webservice does not provide all the metadata that I would need for Music's tag editor. It does, however, provide the Musicbrainz recording ID for a music piece that can be used to fetch a lot of metadata related to that recording. Here's how we're doing it...



### Chromaprint plugin generates a fingerprint



The chromaprint plugin in grilo uses gstreamer-chromaprint to generate a unique fingerprint for each music file. The resolve function, as any other resolve function in grilo, takes in a list of metadata keys and a GrlMedia. In this case the GrlMedia must be of audio type and contain a URL for the song's physical location. The supported metadata keys are 'duration' and 'chromaprint'.

```
def get_chromaprint(self, track_media):
    source = grilo.registry.lookup_source('grl-chromaprint')
    assert source is not None
    fingerprint_key = grilo.registry.lookup_metadata_key('chromaprint')
    assert fingerprint_key != Grl.METADATA_KEY_INVALID

    options = grilo.options
    keys = [fingerprint_key, Grl.METADATA_KEY_DURATION]

    source.resolve(track_media, keys, options, self.resolve_acoustid, None)
```

The fingerprint generated is stored in the GrlMedia that will be passed on to the callback function `self.resolve_acoustid`. Read [here](https://oxygene.sk/lukas/2011/01/how-does-chromaprint-work/) about how the chromaprint is actually generated.



### AcoustID plugin fetches Musicbrainz IDs for the fingerprint



Grilo's AcoustID plugin again takes in an audio type GrlMedia with chromaprint and duration data. This combination (fingerprint + duration) is used to lookup a particular music piece and if available return very accurate data identified with the song. The AcoustID webservice returns a list of recordings that may be possible matches; but currently only the first result is being returned as the most probable match. The supported keys for this plugin _'title', 'mb-recording-ID', 'album',  'mb-release-ID', 'artist', 'mb-artist-ID'._

```
def resolve_acoustid(self, source, op_id, media, data=None, error=None):
    audio = Grl.Media.audio_new()
    fingerprint_key = grilo.registry.lookup_metadata_key('chromaprint')
    assert fingerprint_key != Grl.METADATA_KEY_INVALID

    chromaprint = media.get_string(fingerprint_key)
    assert chromaprint is not None
    audio.set_string(fingerprint_key, chromaprint)
    audio.set_duration(media.get_duration())
    keys = [
        Grl.METADATA_KEY_MB_ARTIST_ID,
        Grl.METADATA_KEY_ARTIST,
        Grl.METADATA_KEY_MB_ALBUM_ID,
        Grl.METADATA_KEY_ALBUM,
        Grl.METADATA_KEY_MB_RECORDING_ID,
        Grl.METADATA_KEY_TITLE
    ]
    options = Grl.OperationOptions.new()
    options.set_resolution_flags(Grl.ResolutionFlags.NORMAL)

    plugin_source = grilo.registry.lookup_source('grl-acoustid')
    plugin_source.resolve(audio, keys, options, self.resolve_mb, None)
```

The 'audio' GrlMedia is passed on to another callback, `self.resolve_mb`, which receives it with the metadata fetched and stored in the media itself. Read in details about the AcoustID webservice [here](https://acoustid.org/webservice).



### Musicbrainz plugin gets song tags for the mb recording ID



The Musicbrainz plugin would finally be able to fetch desired data for a song (recording) with the help of its mb-recording-ID provided through acoustID. I'll write about this plugin and the Musicbrainz webservice in another post when it gets reviewed & pushed to grilo-plugins upstream. For now, it takes in a GrlMedia with a valid musicbrainz recording ID and stores the media with fetched data from Musicbrainz database.

```
def resolve_mb(self, plugin_source, op_id, audio, data=None, error=None):
    mb_source = grilo.registry.lookup_source('grl-musicbrainz')
    keys = self.metadata_keys
    keys.remove(Grl.METADATA_KEY_CREATION_DATE)
    keys.append(Grl.METADATA_KEY_PUBLICATION_DATE)
    options = Grl.OperationOptions.new()
    options.set_resolution_flags(Grl.ResolutionFlags.NORMAL)

    def _mb_callback(mb_source, operation, audio, data=None, error=None):
        if error:
            logger.error(error)
        else:
            if audio.get_title():
                self._title_entry.set_text(audio.get_title())
            if audio.get_album():
                self._album_entry.set_text(audio.get_album())
            if audio.get_artist():
                self._artist_entry.set_text(audio.get_artist())
            if audio.get_composer():
                self._composer_entry.set_text(audio.get_composer())
            if audio.get_genre():
                self._genre_entry.set_text(audio.get_genre())
            if audio.get_track_number():
                self._track_entry.set_text(str(audio.get_track_number()))
            if audio.get_album_disc_number:
                self._disc_entry.set_text(
                    str(audio.get_album_disc_number()))
            if audio.get_publication_date():
                self._year_entry.set_text(
                    str(audio.get_publication_date().get_year()))

    mb_source.resolve(audio, keys, options, _mb_callback, None)
```

Finally the modified media is passed on to another callback responsible for filling up the proper entries in tag editor GUI. I'm still undecided about how to allow the user to selectively write only the tags they want. But more on this later.

Thanks to [Victor](https://wiki.gnome.org/VictorToso) for helping with his work on all these grilo plugins and also helping me understand. Any helpful comments and suggestions are welcome.
