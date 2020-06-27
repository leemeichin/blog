---
title: Blogging in Haskell
date: 2020-06-27
status: published
category: programming
---

It's taken me quite a while to settle on a particular look and feel for this blog. Rather than just having an outlet for writing, I wanted the creation of it to be a learning experience too. Hugo[^1], Gatsby[^2] and Zola[^3], with Netlify CMS[^4] as a fancy interface for writing posts on top of it all. Each attempt left me feeling less inspired than the last.

Eventually I stumbled across Hakyll[^5] and, after finding a CSS 'framework' that gave the appearance of a terminal UI[^6], I felt like I had enough to get things off the ground.

The major appeal so far has been the immense ease of customisation. Hakyll itself isn't a static site generator in the same sense that others are, and as a result it offers a layer of customisation that other generators generally defer to templating languages for.

The main difference is that you don't pull down a `hakyll` binary and then throw a `yaml` file together in order to configure a few pre-defined properties; you're instead given a basic implementation of a generator, using hakyll's own library, and thus have complete control over routing, page generation, templating, and so on. This generally lives in a `site.hs` file and it's not difficult to follow even for relative newbies to Haskell. The structure of everything else is entirely up to you.

Once you compile this file, you end up with a nice binary, e.g. `site`, and _that_ is what you use to generate your site. It is beautiful in its elegance and I'm eager to see what I can add to this site while also learning some more Haskell at the same time.

As an example, on the home page, there is a `git log` output section. It's fairly primitive, although I intend to build out the functionality a bit more. Writing the functionality was fairly effortless, with the help of some other authors on the net:

```haskell
data GitLog = Hash | Commit | Full
  deriving (Eq, Read)

instance Show GitLog where
  show content = case content of
    Hash   -> "%h"
    Commit -> "%h: %s"
    Full   -> "%h: %s (%ai)"

getGitLog :: GitLog -> Integer -> FilePath -> IO [String]
getGitLog content limit path = do
  (status, stdout, _) <- readProcessWithExitCode
    "git"
    [ "log"
    , "--format=" ++ show content
    , "--max-count=" ++ show limit
    , "--"
    , path
    ]
    ""

  return $ case status of
    ExitSuccess -> splitOn "\n" (trim stdout)
    _           -> [""]
  where trim = dropWhileEnd isSpace

logListField
  :: String -> String -> GitLog -> Integer -> String -> Context String
logListField pluralName singularName style limit path =
  listField pluralName ctx $ unsafeCompiler $ do
    logs <- getGitLog style limit path
    return $ map logItem logs
 where
  ctx = field singularName (return . show . itemBody)
  logItem log = Item (fromString $ path ++ "/log/" ++ log) log
```

The result of adding this code, and then inserting it into the template context, is that I have a new template variable that I can loop over, for each log item. The practical use is fairly limited, but I like it because it adds a certain flavour to the site. Later on I will try to use a parser combinator library to be able to present the different parts of the log with more control.

In any case, I've enjoyed playing around with Haskell in order to deploy this site, and I'm looking forward to seeing what else I can build with the language. It's truly fascinating.


[^1]: <https://gohugo.io/>
[^2]: <https://www.gatsbyjs.org/>
[^3]: <https://www.getzola.org/>
[^4]: <https://www.netlifycms.org/>
[^5]: <https://jaspervdj.be/hakyll/>
[^6]: <https://terminalcss.xyz/>