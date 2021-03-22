---
title: Blog hacking
date: 20201-03-18
category: programming
status: draft
---

One of the reasons I chose Hakyll for this blog, aside from messing around with Haskell, was because it was designed as a library. You build your own static site generator from it, adding in whatever bits and pieces you want.

Ever since I first started with it, I liked the idea of integrating with git. The front page of this site has a commit log at the bottom of the page and I only recently updated it to actually link each commit to the source. This week I improved it so each post would show its own commit history too.

This information probably isn't interesting to many people who read whatever I post, but for me it adds a bit more character to the site.

What struck me is just how insanely abstracted Haskell can get, for better or worse. Take a look at the code I ended up with:

```haskell
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

data GitLog = GitLog { commitHash :: String, commitMsg :: String, commitDate :: String }
  deriving (Show)

getGitLog :: Integer -> String -> IO [GitLog]
getGitLog limit path = do
  (status, stdout, _) <- readProcessWithExitCode
    "git"
    [ "log"
    , "--format=" ++ logfmt
    , "--max-count=" ++ show limit
    , "--"
    , path
    ]
    ""
  return $ case status of
    ExitSuccess -> map parseGitLog $ splitOn "\n" (trim stdout)
    _           -> []
  where trim = dropWhileEnd isSpace
        logfmt = "%h;%s;%ai"

parseGitLog :: String -> GitLog
parseGitLog log = GitLog {..}
  where [commitHash, commitMsg, commitDate] = splitOn ";" log

gitLogCtx :: Context GitLog
gitLogCtx = field "commit" (return . commitHash . itemBody)
    <> field "message" (return . commitMsg . itemBody)
    <> field "date" (return . commitDate . itemBody)

logItem :: GitLog ->  Item GitLog
logItem log = Item (fromString $ "log/" ++ commitHash log) log

logListFieldWith fieldName limit =
  listFieldWith fieldName gitLogCtx $ \item -> unsafeCompiler $ do
    logs <- getGitLog limit $ show (itemIdentifier item)
    return $ map logItem logs

logListField
  :: String -> Integer -> String -> Context String
logListField fieldName limit path =
  listField fieldName gitLogCtx $ unsafeCompiler $ do
    logs <- getGitLog limit path
    return $ map logItem logs
```

## Language pragmas and records

Haskell has language pragmas for practically _everything_. `RecordWildcards`, for example, is how I can make a `GitLog` more easily. These two snippets of code achieve more or less the same thing (the other one being in Ruby).

```haskell
parseGitLog log = GitLog {..}
  where [commitHash, commitMsg, commitDate] = splitOn ";" log
```

```ruby
GitLog = Struct.new(:commit_hash, :commit_msg, :commit_date)

def parse_git_log(log)
  GitLog.new(*log.split(';'))
end
```

Records are a bit odd in Haskell because of how you access a field, which in this case is like this: `commitHash gitLog`. You can't have two records with the same field names as a result, because that would introduce a naming conflict. Newer language extensions resolve that problem.

As for `OverloadedStrings`, string handling is also a bit odd in Haskell-land. A string can be a `[Char]` (as in a list of bytes), or it can be a `Text`, or it can be something similar. I find myself importing `Data.Text` quite often.

## Where what?

I'm quite fond of how you can define a function in Haskell as if it was a mathematical equation. I think that `let` and `where` solve a similar problem that variables do, in a language that does not have variables, and so you can use these to assign more descriptive names to things.

In this example, I think `where trim = dropWhileEnd isSpace` makes my intention clearer than something like `splitOn "\n" (dropWhileEnd isSpace $ stdout)`. The interesting thing is that you're filling in these terms after, not before, like you would with a variable.

```haskell
getGitLog limit path = do
  -- ...
  return $ case status of
    ExitSuccess -> map parseGitLog $ splitOn "\n" (trim stdout)
    _           -> []
  where trim = dropWhileEnd isSpace
```

## (.)

```haskell
field "commit" (return . commitHash . itemBody)
field "message" (return . commitMsg . itemBody)
field "date" (return . commitDate . itemBody)
```

This would be familiar to any JS dev doing React or Redux these days. Haskell does function composition with `.`, and it would be similar to this:

```javascript
field("commit", compose(_return, commitHash, itemBody));
```

`return` in Haskell isn't the same as `return` in most other languages, though.
