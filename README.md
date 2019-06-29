# Script and resources for building my blog

If someone wants to reuse this, simply replace materials in the _site_ directory
of the repo with your own data (posts, templates, static assets, etc.) for
building a static website. Once executed, the generator will create a _dist_
folder that contains your site, ready for hosting.

## Build Instructions
Assuming you have [haskell-stack](http://haskellstack.org/) installed, all you
need to do (after cloning) is:

```
stack build
```

This will build the site generator executable using the main script (located in
[Main.hs](app/Main.hs)). Once you have the executable built successfully, run:

```
stack exec blog-exe site
```

If the command successfully buids your website, you should see a _dist_ folder
with all the files for your website, including the _index.html_ file. You can
see what it looks like by hosting it locally using your favorite hosting 
mechanism. For instance, if you have [Serve](https://www.npmjs.com/package/serve),
all you need to do is:

```
serve dist
```

That's it.