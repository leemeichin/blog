---
title: Setting up with Hakyll
date: 2020-06-21
category: programming
status: draft
---

I've spent more time than I would care to admit trying out different static site builders. I eventually landed on Hakyll after giving various pre-built things a go with Netlify CMS. Admittedly there was quite a bit of appeal in being able to learn a bit more practical Haskell by virtue of it powering the site generator, and I've already spent a few days learning how to add some custom functionality[^1]^,^[^2]. It's been good fun as I've always had a fondness for the language.

I suppose it all started with my [CV](https://cv.mrlee.dev), which is nothing more than a hand-written HTML file and a custom CSS 'framework' called [Terminal CSS](https://terminalcss.xyz). Oh, and a sprinkling of javascript with a `noscript` fallback for good measure. For the first time in quite a while I finally felt I'd found a design I actually liked, and that was easy enough for me to customise to suit my needs; namely adding support for `prefers-color-scheme`[^3] so dark mode and light mode afficionados alike could enjoy their visit.

[^1]: [Jekyll Style URLs with Hakyll](http://aherrmann.github.io/programming/2016/01/31/jekyll-style-urls-with-hakyll/index.html) 
[^2]: [How this page is generated - Part 02](https://blog.ysndr.de/posts/internals/2020-03-22-built-with-hakyll-part-2.html)
[^3]: [MDN: `prefers-color-scheme`](https://developer.mozilla.org/en-US/docs/Web/CSS/@media/prefers-color-scheme)