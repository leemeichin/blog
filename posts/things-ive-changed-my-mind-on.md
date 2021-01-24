---
title: Things I've learned after 10 years in the industry
date: 2021-01-24
category: programming
status: published
---

Earlier today I read a blog post titled _Software development topics I've changed my mind on after 6 years in the industry_[^0] and it made me reflect on how my own thinking has (hopefully) evolved over my decade long career. I'm not going to discuss the content of the linked post, except to say that as much as I empathise with the author and have been an angry programmer myself, the overly aggressive tone that occasionally slips out isn't really my cup of tea.

A lot of things can happen in ten years, and if nothing else my 30-something year old self feels substantially less enlightened than his more youthful counterpart. I look forward to reading this again in future and seeing what I think about it then.

This will probably come across as a bunch of hot takes that work better on Twitter, but that's one dumpster fire I'd prefer not to climb into. So here we go, in no particular order...

- I feel happier in myself when I don't pretend to have an answer to everything.

  _Corrollary: Having an answer to everything (or being overconfident) is a hard habit to break._

- A boring tech stack is nice, but you have to allow some scope for innovation if you don't want to stagnate.

  _One idea is to set up an innovation budget[^1], but it's not the only one._

- PHP is a legitimate--if not _superior_--option to start a new project with and it deserves another chance.

  _The language has matured spectacularly since 7.1 onwards._

- It's up to me as a software engineer to research ideas and proposals and judge them on merit, not anecdata.

  _I used to find things like Domain Driven Design (DDD) silly and enterprisey, because I used a poor implementation of it to judge the process itself._

- The cloud is not a foregone conclusion.

  _You can still get a lot more bang for your buck with a VPS, and you'll learn something about systems administration too._

- QA is only a bottleneck when you treat the team as part of a deployment pipeline.

  _So many issues can be fixed at the design phase, before a line of code is even written, if you involve your QA engineers._

- Not every people problem can be solved with technology.

  _Automate everything away and you strip away the humanity._

- My greatest learning moments came from maintenance programming and working with legacy code, rather than building new features.

  _Debugging is a skill, fixing bugs is a skill, and the depth of your domain knowledge will be limited if you don't know how to do it well._

- When talking about User Experience, your users aren't just your paying customers, they're your support team and your developers.

  _Dedicating some resources to internal productivity can do wonders for your team, which eventually leads to increased user satisfaction._

- Technology isn't invalidated by its age, it is strengthened by it.

  _I occasionally here people saying things like people don't use Rails any more because it's old and serverless or some shit is the hype. Don't use hype as the foundation for your business, use stability._

- You can write much better code if you don't micro-manage the structure of it; be judicious with your linters and formatters.

  _Setting limits on things like function length or class length sounds ideal, but these kind of linters are ultimately shaping your code through blunt force and, possibly more often than not, making it worse by enforcing needless indirection._

- If it's in a pull request (PR) it might already be too late to change course.

  _The sort of feedback you can give in a pull request is not of the same quality you can get with a conversation held before committing code._

- It's better when I'm not sentimental about the code I write or the ideas I present.
  _Rejection isn't always so easy to handle, but there's always another time and a 'no' is still better than not being acknowledged at all. It's a great opportunity for constructive feedback and learning._

It feels a bit weird to write out a series of arbitrary maxims the way I just have, so I'll aim to explore each one in more depth throughout this year. With that said, it was an interesting exercise if ultimately a rather shallow one. Watch this space for more details.

[^0]: <https://chriskiehl.com/article/thoughts-after-6-years>
[^1]: <https://mcfunley.com/choose-boring-technology>
