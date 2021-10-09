#lang pollen

◊define-meta[title]{Ruby Sorcery Part 2: Ractor, Chapter 2}
◊define-meta[date]{2021-10-09}
◊define-meta[published #f]
◊define-meta[category]{ruby}

In the previous chapter of this excurusion into Ractor◊^[1], the concept of the actor model was introduced and a toy TCP server was created. It was a naive implementation that created new Ractors for every TCP connection made to the server, and it looked like this:

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

While simple, this isn't something you'd call 'production ready'. There is at least one notable shortcoming: see if you can guess what it is, it will be addressed in a later chapter. Meanwhile, it's a good time to turn this toy example into something more serious.

◊aside{Hint: what does or does not happen when the client's connection is lost or terminated?}

◊h2{Building an HTTP server with Ractor}

In the beginning, the HyperText Transfer Protocol was (and still is!) a purely text-based specification layered on top of TCP. It has grown a hell of a lot of complexity over the years, in order to power more complex interactions over the web, but it is still easier to build a server that understands HTTP (even just HTTP 1.1) than it is to build the browser that handles all of the rendering.

So, it sounds like a good time to turn this toy TCP server into a basic HTTP server, using Ractors, and by the end of this chapter it should be possible for the the server to understand a simple request:

◊codeblock['text]{
  GET /hello-world HTTP/1.1
  Host: localhost:1337

}

There is, of course, a lot more to serving HTTP than this. For a start, there's no HTTP response! That will be covered in the following chapter.

◊h2{Parsing the request}

The anatomy of an HTTP request is divided, essentially, into three parts:

◊codeblock['text']{
  GET /hello-world HTTP/1.1        | 1. Method, location/target, HTTP version
  Host: localhost:1337             | 2. Headers (host, content-type, accept,
  Content-Type: application/json       caching, user-agent, etc.)
                                   | Empty line to separate headers and body
  { data: "Hello, world!" }        | 3. Body
                                   | Final empty line to denote end of request
}

The first line states the request method (i.e. ◊code{GET}, ◊code{POST}, etc.) and the target of the request, which will often be a relative path on the server but can also be a full URL.

◊aside{Note that there is also a concept of HTTP 'trailers', which are headers that appear ◊em{after} the body◊^[2]. These are only used for certain kinds of chunked requests and are way out of scope for this post.}

What follows is a list of headers, which are key/value pairs used to provide extra information about the request. For the sake of simplicity, most of these will be ignored, and there are ◊em{many} of them.

Finally, there is a place for the body of the request. This is optional, but it must have an empty line both before and afterwards if it is present. A typical request body may contain URLEncoded form data, which is how your typical HTML forms work, but there is not much of a restriction provided that the ◊code{Content Type} header describes the format of the payload, e.g if it's JSON, XML, or perhaps even something like an image or a video.

Since the purpose of the post is to demonstrate Ractor, and because in the Actor model, everything is an actor... the thing that parses the request will be an Actor too.

◊aside{This is also a good time to use some of that pattern matching knowledge from the first part of this series.◊^[3]}

Something like this should do the trick. Keep in mind that the goal is to mess around with Ractors and Ruby, for the sake of example.

◊codeblock['ruby]{
  require 'strscan'

  HttpRequest = Struct.new(
    :method, :location, :version, :host, :content_type, :headers, :body,
    keyword_init: true
  )
  
  HttpRequestParser = Ractor.new do
    while raw_req = StringScanner.new(receive)
      http_req = HttpRequest.new

      case raw_req.scan_until(/\n/).strip.split(" ")
      in "GET" => method, location, "HTTP/1.1" => http_version
        http_req.method = method
        http_req.location = location
        http_req.version = http_version
      end

      http_req.headers = raw_req.scan_until(/^$/).split("\n").map(&:strip)
      http_req.headers.each do |header|
        case header.split(": ")
        in "Content-Type", content_type
          http_req.content_type = content_type
        in "Host", host
          http_req.host = host
        else
          next
        end
      end

      http_req.body = raw_req.rest.strip

      # `move` the object as this Ractor no longer needs ownership
      # the Ractor that calls `take` will... take... ownership
      Ractor.yield(http_req, move: true)
    end
  ensure
    raw_req.terminate
  end
}

◊aside{Why all the ◊code{loop}s and ◊code{while} loops? Ractors behave a bit like Enumerators◊^[5], which means that if they stop yielding values or actually return a value, the Ractors close and can no longer be used.}

What we have here is a Ractor that waits for incoming HTTP request messages, and then parses them into something that the server can more easily work with by pulling out important info like the request location, the HTTP method, the content type, and the body. In this example, Ruby's pattern matching features are liberally employed to handle the parsing in some places; this is more for the sake of demonstration to show that it ◊em{can} be done, not necessarily that it always ◊em{should} be.

In any case, once the ◊code{HttpRequest} object is constructed, it is yielded so that another Ractor can use the object, and therefore it will sit in a queue (or a mailbox, in actor model parlance) until it is taken from it. As a final housekeeping step, the string scanner instance used to parse the request is terminated. It's always a good idea to clean up after yourself if the language provides you the mechanism to do so.

Going back to the functionality at hand; this basically shunts the parsing of HTTP requests into another thread, which means that the Ractors responsible for managing the TCP layer can stay responsible for that, and hand over the application-layer responsibilities to other actors/processes/Ractors.

It's a good time to integrate this parser with the TCP server, then.

◊codeblock['ruby]{
  require 'socket'
  require 'strscan'

  HttpRequest = Struct.new(
    :method, :location, :version, :host, :content_type, :headers, :body,
    keyword_init: true
  )

  HttpRequestParser = -> do
    Ractor.new do
      while raw_req = StringScanner.new(receive)
        http_req = HttpRequest.new

        case raw_req.scan_until(/\n/).strip.split(" ")
        in "GET" => method, location, "HTTP/1.1" => http_version
          http_req.method = method
          http_req.location = location
          http_req.version = http_version
        end

        http_req.headers = raw_req.scan_until(/^$/).split("\n").map(&:strip)
        http_req.headers.each do |header|
          case header.split(": ")
          in "Content-Type", content_type
            http_req.content_type = content_type
          in "Host", host
            http_req.host = host
          else
            next
          end
        end

        http_req.body = raw_req.rest.strip

        # `move` the object as this Ractor no longer needs ownership
        # the Ractor that calls `take` will... take... ownership
        Ractor.yield(http_req, move: true)
      end
    ensure
      raw_req.terminate
    end
  end
  
  Ractor.new do
    tcp_server = TCPServer.new(1337)

    loop do
      Ractor.new(tcp_server.accept) do |client|
        loop do
          HttpRequestParser.send(client.gets)
          req = HttpRequestParser.take
          client.puts(req.body.upcase)
        end
      end
    end
  end
}

◊footnotes{
  ◊^[1]{◊<>["https://www.kamelasa.dev/posts/ruby-sorcery-ractor.html"]}
  ◊^[2]{◊<>["https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Trailer"]}
  ◊^[3]{◊<>["https://www.kamelasa.dev/posts/ruby-sorcery.html"]}
  ◊^[4]{◊<>["https://ruby-doc.com/core-3.0.0/Enumerator.html"]}
}