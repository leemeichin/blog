---
title: Do you really need those microservices?
date: 2020-07-21
category: programming
status: published
---

I've been through half a dozen rounds of interviewing over the past couple of months, with different companies. Naturally, when you say you've had experience working with microservices, you're practically begging the question. What do you think about them?

I'm not really for-or-against them, and in fact I find it a little strange and frustrating when it's expected that you take a side in favour of having a nuanced opinion. The idea of being pro-microservice and anti-monolith, for example, is utterly absurd. It's never really voiced that way, but it's often easy to detect the absolutist stance masquerading as a balanced opinion. Even the title of this post falls victim to that, as the question presented is quite clearly loaded.

Anyway, I love being asked this question because after a good five years of working with distributed systems orchestrated by Kubernetes, almost entirely in the form of 'migrating away from the monolith', I've had plenty of time to formulate and adapt my thinking around it.

First and foremost, I believe the main benefit of a business switching to microservices is the manifestation of Conway's Law[^1] in practice. Prior to making the switch in architecture, the business most likely decided on an organisational structure that exchanges large, difficult to manage teams for a distributed collection of smaller, self-sufficient, self-empowered teams. More often than not these follow a squad and chapter model, otherwise reduced to 'the Spotify model', and a hierarchy of cross-functional teams is established. Once that structure is put in place and people are shuffled around a bit, the question of ownership in a mixed-responsibility, mixed-domain codebase becomes apparent. Microservices are thus the solution to a perceived conflict between squads and they shift a lot of that conflict from the teams themselves, to the channels in-between them.

Under no circumstance is the technical implication of such a change considered, particularly in older codebases for which this change would introduce a significant level of disruption. The organisational benefits of distributing teams and workload are substantial, but the drawbacks of distributing _code_ are similarly worthy of consideration too, and it often becomes a gateway to extra complexity as once-simple tasks find themselves spread apart over various network calls and machines.

I don't consider this a dealbreaker, but in my experience I've always felt like there's a step missing between the singular, monolithic codebase and the highly distributed microservice architecture. There's a hell of a lot you can do in that singular codebase, in terms of taking smaller steps towards a service-oriented architecture, or a domain-driven one. 

In those situations, you are investing primarily in the work required to understand the different domains in your codebase and how they speak to each other, with much lower risk than fundamentally changing your infrastructure as well as your architecture. In Ruby, you can abstract code into gems and provide solid, public APIs that other parts of the code can use. In Java you have modules and packages. Every language you care to use has the concept of packaging code into bundles or libraries that can be shared as a dependency.

Adopting this workflow introduces much lower risk because, in the event of failure, you can easily adjust your expectations around the domains and how they communicate and fix them in a singular release. It may not be perfect, but if you refactor enough of your code in SOA[^2]/DDD[^3] style then you'll have a much easier time turning those domains into proper microservices further down the line.

This is where a microservice architecture truly shines. If you have clear, well bounded domains, and you've developed solid API contracts as well as standards for versioning, logging, etc to allow for centralised aggregation of useful resources (for debugging or auditing, for example), and if the team in charge of the domain can essentially treat that service as a full-blown product with documentation, support, and its own priorities and backlog, then that is where the power of that infrastructure comes into play.

Another amazing usecase for this architecture is when you have strong regulatory requirements to sequester and protect personally identifying information. This could involve PCI compliance as a financial business, HIPAA as an American medical firm, or GDPR for a whole host of privacy concerns across the EU and the UK. Network access can be controlled much more easily than different modules or functions in your singular codebase, and the risk of a low-importance feature being compromised to access the critical resources in that case is far too high.

I believe that's a lot easier to do when you start early, but if you come to it late then you fall into an easy trap: how do you migrate your stuff into new services while also maintaining your existing codebase, along with all the features that you're still expected to deliver? You can't stop the world to rewrite all of the code, so there's a possibility that you enter a refactoring spiral as your service plays cat and mouse with the monolith. This is infinitely more difficult when migrating a codebase written in a dynamic language, as automated refactoring is nowhere near as easy without a solid type-system to back it up.

If I was to offer anybody advice about how to make all of this happen successfully, I'd say to stop thinking in terms of the existing monolith, and instead look at what individual products you could separate or extract, or even spin-off into their own businesses if the idea was unique enough to sell individually. And don't jump to solutions like Kubernetes until you're dealing with enough of these services that your existing deployment setup is too hard to manage.

[^1]: <https://en.wikipedia.org/wiki/Conway%27s_law>
[^2]: <https://en.wikipedia.org/wiki/Service-oriented_architecture>
[^3]: <https://en.wikipedia.org/wiki/Domain-driven_design>