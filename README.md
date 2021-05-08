# Script and resources for building my blog

If someone wants to reuse this, simply replace materials in the _site_ directory of the repo with your own data (posts, templates, static assets, etc.) for building a static website. Once executed, the generator will create a _dist_ folder that contains your site, ready for hosting.

**Note to self**: You do not need to setup anything below. Just create posts using the Python script, write your post and push it to this repo. Travis will take care of generating and publishing the site. Use a markdown extension to locally preview your posts.

## Build Instructions

Assuming you have [haskell-stack](http://haskellstack.org/) installed, all you need to do (after cloning) is:

```bash
stack build
```

This will build the site generator executable using the main script (located in [Main.hs](app/Main.hs)).

A simple Python script is present (no dependencies) that allows easy creation of posts with a title and tags. To create a new post, run:

```bash
./createpost.py --title "Name of your post" --tags first-tag second-tag
```

This should create a new .md file in the `site/posts` directory. The name of the file is just the title that you added in a lowercased and hyphentated form.

Once you have edited and saved your markdown. Run the site generating executable:

```bash
stack exec blog site
```

If the command successfully buids your website, you should see a _dist_ folder with all the files for your website, including the _index.html_ file. You can see what it looks like by hosting it locally using your favorite hosting mechanism. For instance, if you have [Serve](https://www.npmjs.com/package/serve), all you need to do is:

```bash
serve dist
```

That's it.
