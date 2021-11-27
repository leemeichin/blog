* Ruby Sorcery Part 2: Ractor, Chapter 1
:PROPERTIES:
:CREATED: [2021-09-28]
:PUBLISHED: f
:CATEGORY: ruby
:END:

This is part two of a series of posts about Ruby and its more experimental features. The first part is about pattern matching.◊^[1]

Ractor is a new addition to Ruby's core library, and it is essentially an implementation of the Actor model. More importantly, it offers a more lightweight approach to concurrency that might feel more at home to those familiar with Go's channels, or have perhaps worked with Elixir. Note that this isn't a wholesale replacement of Ruby's existing multithreading implementations, namely ◊code{Thread}◊^[2] and ◊code{Fiber}◊^[3], and is still highly experimental. As such, there is no guarantee that it would remain stable across future Ruby versions.

◊aside{Speaking of ◊code{Fiber}s, they've received some upgrades in Ruby 3 too. You can now create non-blocking fibers and provide your own scheduler to run them automatically.}

◊h1{The actor model}

Before looking at Ruby's implementation of it, it's useful to understand what the actor model is, at least in simple enough terms that you can then use to explore the topic in more depth.

In a nutshell, it's a way of handling concurrent communication. If, in OOP, everything is computed in terms of passing messages between objects, then the singular rule of the actor model is that everything is an actor. So, what's an actor?

An actor is something that has one or more addresses, and receives messages; but for anything useful to happen, it also has to do something with those messages.

◊aside{Imagine a ◊code{no-reply} email inbox, where every cry for help sent to it is routinely ignored. That is a version of an actor model that has an address, can receive messages, but can do nothing else.}

In addition to receiving messages, then, an actor can also send messages to another actor. On top of that, it can also create new actors as children of itself.

The final missing piece is that an actor can hold internal state, or in other words, store data. This is inaccessible to any other actor and therefore the ◊em{only} way to manipulate data inside an actor is to send it a message that it understands.

How does it fit together? An actor can have multiple addresses, and many actors can share the same address. This means that you can achieve scaling by having more actors listen on an address, effectively load-balancing incoming messages, and you can create more powerful actors that perform different tasks based on those same messages.


◊h2{Actor languages}

If you've used Erlang◊^[4] or Elixir◊^[5] to any degree, you have already built something around this model and, perhaps, were not aware of it. This is because the actor model is an intuitive aspect of those languages that makes them what they are, and therefore everything is implemented in terms of it.

◊aside{Pony◊^[6] is a another more recent language that is designed around the actor model.}

In Erlang/Elixir, one key feature is that programs are designed to be fault-tolerant by being able to crash. If a crash happens, a new actor will be spun up in its place and processing will continue as normal. This is handled by another actor, known as a supervisor, which has the sole responsibility of doing something when other actors crash.

◊aside{WhatsApp, Discord use Erlang/Elixir in their operations. It also powers telephone exchanges and 3G networks across the world.}

◊h2{Actors in the wild}

You can consider technologies like Kubernetes◊^[7], Kafka◊^[8], and Web Workers◊^[9] in the browser to each be an application of the actor model in some form. This doesn't necessarily mean they were built with that in mind, just that you can find that they share many characteristics in common.

For example, in Kubernetes (K8S), the orchestrator is an actor that behaves as a supervisor. It is responsible for monitoring all of the other services that are deployed in the cluster and ensuring that they're kept alive. If a service goes down, it will attempt to reboot it. The service is also an actor, as it can talk to other services. It can change its own state (e.g. in-memory or with a database), but it cannot (or should not) reach into other services to do the same.

Kafka offers three concepts of an actor: a producer, which is an actor that can only send messages; a consumer, which is an actor that can only receive messages ◊em{but also} create more producers to send messages; and a broker, which acts as a storage layer as well as a supervisor of sorts.

Similarly, an email inbox is another application of the pattern. Your email inbox is attached to one or more email addresses (or aliases), and messages that are meant for you are sent to your address (or many at once). Eventually, they will arrive in your inbox and you can then read the email and decide to archive it, delete it, report it as spam, and so on. This is also the case for a mailing list, where messages sent to the mailing list's address will eventually be distributed to every subscriber's address.

◊h1{Ractor}

Ruby's implementation of the actor model is called Ractor. There's a little bit of history here: it was originally called Guilds, and it's been in the making for a good few years now.

Ruby 3.0 introduces Ractor to the general community, however it is still marked as experimental. This means that the API may change in later versions, or behaviours may change based on feedback, and while you might be fine to run Ractors in production...well... ◊em{caveat emptor}◊^[10].

First things first, quick recap on Actors:

◊ul{
  ◊li{An actor can send messages to another actor}
  ◊li{An actor can create more actors}
  ◊li{An actor can mutate its own state but not another actor's state}
}

In order to guarantee thread-safety, some aspects of the language have had to change. Most objects in Ruby are unshareable by default, which is different to how a ◊code{Thread} behaves, and this means that code inside a ractor essentially cannot read ◊em{anything} outside of its own scope, which includes global variables and constants.

Rather than rewording the Ruby manual on Ractors◊^[11], let's dig into a practical example and build a basic echo server over TCP.

◊aside{If you're following along, make sure you're using Ruby 3!}


◊codeblock['ruby]{
  require 'socket'

  tcp_server = Ractor.new do
    server = TCPServer.new(1337)

    loop do
      Ractor.new(server.accept) do |client|
        loop do
          input = client.gets
          client.puts(input.upcase)
        end
      end
    end
  end
}

This example demonstrates how one Ractor can create more Ractors: whenever a new connection is established to the TCP server, a new Ractor is spawned and a TCP client is moved into it. This new Ractor listens on the connection and when input is received, it echoes it back but in uppercase.

Try it for yourself by running that code in an IRB console, and then open up ◊code{telnet} in another session.

◊script[#:id "asciicast-438705" #:src "https://asciinema.org/a/438705.js" #:async "true" #:data-cols "190"]{}

◊noscript{
  ◊codeblock['bash]{
    telnet localhost 1337
      Trying 127.0.0.1...
      Connected to localhost.
      Escape character is '^]'.
      hello
      HELLO
      world
      WORLD
      look, it works!
      LOOK, IT WORKS!
      ^]
      telnet> Connection closed.
  }
}

The problem with this code is that it's too simple: it shows how one Ractor can spawn other ractors, but it's not taking advantage of the communication channels they have and how objects are shared between Ractors. Keep an eye out for the next part of Ruby Sorcery, where there'll be a much deeper dive into Ractor's capabilities.

◊footnotes{
  ◊^[1]{◊<>["https://www.kamelasa.dev/posts/ruby-sorcery.html"]}
  ◊^[2]{◊<>["https://ruby-doc.org/core-3.0.2/Thread.html"]}
  ◊^[3]{◊<>["https://ruby-doc.org/core-3.0.2/Fiber.html"]}
  ◊^[4]{◊<>["https://www.erlang.org"]}
  ◊^[5]{◊<>["https://elixir-lang.org"]}
  ◊^[6]{◊<>["https://www.ponylang.io"]}
  ◊^[7]{◊<>["https://kubernetes.io"]}
  ◊^[8]{◊<>["https://kafka.apache.org"]}
  ◊^[9]{◊<>["https://developer.mozilla.org/en-US/docs/Web/API/Web_Workers_API/Using_web_workers"]}
  ◊^[10]{Let the buyer beware.}
  ◊^[11]{◊<>["https://docs.ruby-lang.org/en/master/doc/ractor_md.html"]}
}