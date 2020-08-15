---
title: My favourite thing about programming
date: 2020-08-15
category: programming
status: published
---

In a word: languages.

A programming language is a way for man to communicate with machine just as spoken languages are a way for one person to verbally communicate with another. Of course, this is not the only way in either case: we have sign language, body language, written language, and ways of communicating that use all of the senses. In that sense, a programming language is just one way to talk to a computer, and it's usually the way we write programs for it to execute for us. Human Interface Devices can also tell a computer what to do: a mouse controls your cursor, a keyboard controls typed instructions, and in reverse a monitor and speaker provide visual and audible feedback on your input.

I don't know why I explained all of that, it's a bit wanky, but I suppose the key word is communication.

I'm a fan of language in general, if such a fan can exist. I picked up a passable amount of Spanish during my time in Barcelona, learned how to read the Russian alphabet in about a weekend, and over a few years have slowly but surely learned how to properly read, write and speak the language a bit better. I find it fascinating.

It's not so different with programming languages and, honestly, I think the sheer variety of ways to write a program is what keeps me interested in the field, and keeps my mind open to different ways of thinking.

Currently I'm playing with Haskell and also taking it a bit more seriously, to see if I can build a couple of things I can later integrate into this site. If you don't know Haskell but have a background in PHP, Javascript, Java, or a C-like language, you might be in for a rude surprise when faced with a terse but insanely expressive syntax.

For example, this is the function I have for generating the estimated reading time you see at the top of every post here:

```haskell
ertField :: String -> Snapshot -> Context String
ertField name snapshot = field name $ \item -> do
  body <- itemBody <$> loadSnapshot (itemIdentifier item) snapshot
  let words = length (T.words . T.pack $ body)
  return $ show $ round $ words // 250
```

That's a lot of symbols doing a lot of legwork! And while this is difficult to understand for an untrained eye, it would become more familiar after understanding some of the basic 'rules' of Haskell and the 'styles' of programming you can use. Of course, you can always take it too far:

```haskell
(👏) = ($)

ertField name snapshot = field name 👏 \item -> do
  body <- itemBody <$> loadSnapshot (itemIdentifier item) snapshot
  let words = length (T.words . T.pack 👏 body)
  return 👏 show 👏 round 👏 words // 250
```

That should go down well with the Twitter crowd.

The reason I love Haskell is because it's purely functional and that means you have to think about your problems and potential solutions in a different way. It's a lot harder to procedurally hack things together, and it really takes you back to the fundamental concept of a function being something that takes input, acts on it, and provides output. It's incredibly elegant.

Moving on, there's Lisp. My familiarity with Lisp comes from customising my emacs setup over the past several years, and it remains my favourite way to script an editor purely because of how powerful it is. The beauty of Lisp is the simplicity of its execution: everything is a list, and working with the language is hardly any different to directly modifying an abstract syntax tree (AST). This lends itself to a different kind of expressivity because there aren't really that many rules around the syntax, and oftentimes your lisp application can be built entirely in a REPL due to the ability to change anything at runtime.

With emacs in particular, it made it trivially easy for me to launch a Rails console inside a deployed Kubernetes pod.

```emacs-lisp
(defun inf-ruby-console-k8s (env)
  (interactive (list (completing-read "Environment: "
                                      '("dev" "staging" "preprod") nil t)))
  (shell-command (concat "kubectl config use-context " env))
  (inf-ruby-console-run "kubectl exec -it ruby-app -- rails c" (concat "k8s-ruby-" env)))
```

I mapped it to a certain keybinding and a panel would open to the side within a second, ready for me to use. I don't think I'd have the patience to try and reproduce that in, say, VS Code, without using a task runner. Emacs itself is entirely programmable so you don't need to worry about setting up extension boilerplate to make minor modifications.

I should round this post off with an even three examples, so my final two are Smalltalk and Prolog. I haven't managed to build anything in it yet, as the learning curve is quite unlike any other. However, aspects of Smalltalk live on in languages like Ruby, where everything is an object and everything is defined in terms of message passing. I think Objective-C can be counted there too, although both languages diverge from some of Smalltalk's ideals in the name of pragmatism. My short term goal with Smalltalk is to (attempt to) implement a raytracer as described in The Ray Tracer Challenge by James Buck[^0], so I can better understand the language and apply what I've learned elsewhere. 

Prolog is on my list, and I keep coming back to it every couple of months to see what I'm inspired to do. I find it, and logic programming, intriguing, and I wonder how many problems would be solved a bit more easily that way compared to how we usually smush things together in procedural or OOP languages. Watch this space for more insight, I guess.

One thing I've noticed while writing this is that each language selected has little in common with the other, and they're quite unique in terms of how they look and how they're understood by the computer. It all ends up as machine code at the end of the day, but the solution to a single problem would be drastically different in each one. It's not so different with more common languages like Go, PHP, Javascript, Rust, or C++, really; you might be able to pick up the syntax fairly easily if you've used one of those languages but you'll still have a fun time understanding the different rules they enforce, the different constraints you have work around.

For as long as that remains true I think I'll always have new toys to play with.

[0]: <http://raytracerchallenge.com/>