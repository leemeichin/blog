---
title: Devops
date: 2020-09-18
status: published
category: programming
---

I recently left a healthcare company called Babylon a few weeks back. Of all the things I enjoyed there, and the things that made it unique, one has to be how it has managed to dance across the line between startup and enterprise. I don't mean to say 'enterprise' in a perjorative sense; it's more that you can't really avoid that when you're working in a heavily regulated and audited sector. Mistakes can be literally life or death, and the data collected over time includes people's medical information and health records. There really isn't any room for fucking about, but that doesn't mean that other business functions have to be so serious.

Enterprise might not be the best word, but it's the only one I have right now, and nevertheless I took a few valuable lessons away from the experience that I now find it quite strange to be without.

Let's talk about bugs and production servers. Before Bablyon I had not worked in a single place that restricted access to production. As a developer working primarily with Ruby on Rails applications, getting prod access on Heroku or AWS was practically an onboarding step, and that meant I could easily boot up a console and modify the application runtime on the fly. This is an amazingly powerful tool in development and testing environments, it's basically just a boostrapped REPL, but expose that in production and a malicious actor could wreak all sorts of havoc without leaving a trace. This is even worse if your rails app is running under `root` for some reason (e.g. through a poor Docker setup), as you can quite easily jump into a shell from there.

You couldn't do any of this at Bablyon because production and preprod were locked down _tight_, and even seeing production logs required a background check. This didn't really make debugging worse, because instead there was a huge investment in tooling (internal and external) and developer experience to balance it out. One of my favourite outcomes of this is the creation of an open source tool for managing a Kubernetes cluster, called `shipcat`[^1]. You know it's good when it has its own cute logo.

What I've since realised is that this particular crutch (live debugging in production) prevents the business from properly investing in safer and more compliant tools for engineers to investigate issues. You want proper structured logging, good alerting on error conditions, and a whole slew of observability (o11y) tools that can help you diagnose the system from the outside-in without compromising it. You want to have a team of impassioned engineers who enjoy working on internal productivity/efficacy, creating new tools to address pain-points in the organisation's development and support lifecycle.

The ELK stack seems to be the go-to solution for structured logging, and knowing how to work your way around Kibana (and Elasticsearch by extension) is invaluable. Not only can you trace logs to identify an issue, you can visualise them to get an idea of how pervasive that problem is. You know, you missed an N+1 query but a simple log visualisation showing thousands of similar requests in aggregate can help you identify that.

Sentry is a fantastic tool for logging exceptions in more detail, as with the correct setup you can get a decent stacktrace and combine it with other information to learn more about a problem. Again, this aggregates errors for you so you can assess the scope and severity of the issue just by counting the number of events over time.

Application Performance Management (APM) tools like New Relic and Datadog are similarly great, especially when dealing with microservices or a distributed codebase, as you can get a much bigger picture of what is happening across network boundaries, and not just in a single application. This would apply the same for your logs provided you pass around a correlation ID that allows you to group the various hops for a single inbound request.

It's not just about the code though, what about the infrastructure and the hardware? Grafana will sort you out there, so you can understand what sort of load your servers and databases are under.

I feel like the devops culture is one that ultimately democratises your technology stack and the way it's operated. It offers transparency into the system, allowing software engineers and platform/infrastructure engineers to make decisions based on the same information. It breaks down the silos you might typically build around the programming language you use, or how you deploy software, and who exactly should own that. Everyone can own it and bring their experience to the table to help iterate and improve.

Before I proselytise too much, I think this is really important because you can only go so far before the luxury of directly debugging in production is stripped away. GDPR, HIPAA, PCI, etc. will surely see to that as your startup grows and expands. In place of that, you have a fantastic set of tools that can give you everything you need to kick off an investigation if something goes wrong, if you take the time to learn how to use them. 

And if I had one suggestion for any budding project finding itself in the hands of real life users in production, consider what you want your devops culture to be like and, if you can, see how early you can encourage your team without depending on offering wide-scale production access. And maybe even consider what kind of internal tooling you can build to improve the productivity and efficacy of your engineers.

[^1]: <https://github.com/babylonhealth/shipcat>
