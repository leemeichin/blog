#lang pollen

* Ruby Sorcery Part 2: Ractor, Chapter 3
◊define-meta[date]{2021-10-17}
◊define-meta[published #f]
◊define-meta[category]{ruby}

In chapter 2 of this exploration, a basic Ractor-based TCP server was refactored into a partly-functional HTTP server.◊^[1] It can handle super-basic requests, but it doesn't send back a valid HTTP response. That means that it's difficult to use tools like ◊code{curl} to interact with the server and, thanks to that, previous demonstrations have depended on hand-crafting requests inside a ◊code{telnet} session.

This chapter is therefore going to focus on constructing a valid HTTP response. As with the previous chapter, it won't cover 100% of the spec, but it will lay down some decent foundations.

First, a quick recap of the server so far:

◊codeblock['ruby]{
  Ractor.new do
    tcp_server = TCPServer.new(1337)

    loop do
      Ractor.new(tcp_server.accept) do |client|
        HttpRequestParser.send(client.gets("\r\n\r\n"))
        request = HttpRequestParser.take
        client.puts("requested: #{request.location}")
        client.close
      end
    end
  end
}

What we have so far is this:

◊ul{
  ◊li{A ractor that spawns a TCP server at port 1337}
  ◊li{A ractor that handles incoming TCP connections}
  ◊li{A ractor that parses HTTP requests sent over the connection}
}

And this bit of code in particular is what needs some attention:

◊codeblock['ruby]{
  # ...
  client.puts("requested: #{request.location}")
  client.close
  # ...
}

◊h2{HTTP responses in a nutshell}

A response has a similar structure to a request, comprising a status line, an arbitrary number of headers, and then optionally the response body.

◊codeblock['text]{
  HTTP/1.1 200 OK                       | 1. Status line
  Content-Type: text/html               | 2. Headers (content-type, CORS, caching,
  Content-Length: 21                    |      connection, custom X-* headers, etc.)
                                        | Empty CRLF line to separate headers and body
  <h1>Hello world!</h1>                 | 3. Body
}

◊aside{Most responses will contain a body, but there are certain status codes where it wouldn't make sense to provide one, e.g. with ◊code{3xx} codes for temporary and permanent redirects, and the ◊code{201 No Content} response.}

Most responses, and in particular the ones being handled by this basic Ractor server, will be sent all at once. But how does the client know when it's received all of the response from the server? This is important to know, because it won't be possile to render HTML or parse JSON until the client knows that it has all of the data. It can't rely on two consecutive carriage-returns, as with a request, because the response itself may legitimately contain those characters. Neither can it rely on the server closing the TCP connection, because that can happen for many other reasons.

The ◊code{Content-Length} header is therefore required for such responses, as it informs the client that a response body will be present and it will be of a certain size in bytes. Given that information, the client can read in the same number of bytes and expect to have received the whole response.

This is not the only way to define a response, as it can also be split up into chunks and delivered in parts: this is generally what happens when downloading large files. The same method can also be used for streaming data where the total length is unknown, but the client is expected to process the response on-the-fly instead of waiting for a completed payload.

For the sake of simplicity, the server will handle only the simple case for now: given a request to the server, it will respond with ◊code{200 OK} and a string of HTML.

◊h2{Building an HTTP Response in Ractor}

This is the example HTTP request from the previous chapter◊^[1].

◊codeblock['text]{
  GET /hello-world HTTP/1.1
  Host: localhost:1337
}

It doesn't do very much, and the server can respond with whatever it likes. At a higher level of abstraction, this is typically the part where you define your 'routes' and then add controllers or handlers to perform some action, and then the underlying framework will convert all of that into something that can be sent back over the wire to the browser. In Ruby, Rack◊^[2] is a popular library that does achieves the same thing.

For our sake, the server will just take the location provided in the request and then echo it back in an HTML document, perhaps something like this:

◊codeblock['text]{
  HTTP/1.1 200 OK
  Content-Length: {size-of-body-in-bytes}
  Content-Type: text/html; charset=utf-8

  <html><head><title>Echo!</title><body><h1>{the-request-location}</h1></body></html>
}

Note that the HTML body is all on one line, and not prettified. Maintaining newlines and indentation in the response is a waste of bytes because they don't have significant meaning except for human readability, and it's a good idea to keep responses as lightweight as possible.

It's also unclear what the length of the content will be, exactly, until the request has been made. That means there are two pieces of information in the response that have to be computed, while the rest can be hard-coded.

Of course, this will be done in another Ractor, but we'll also address the problem introduced by the Ractor that parses requests: if a single Ractor is used to globally process requests, and another is used to globally process responses, then how does the server know which response belongs to which request? When that's done, the fix for the request parser will also become apparent.

◊aside{This is important to keep in mind, especially when dealing with sensitive data and user accounts, as users may start receiving account information relating to other people.}


◊footnotes{
  ◊^[1]{◊<>["https://www.kamelasa.dev/posts/ruby-sorcery-ractor-2.html"]}
  ◊^[2]{◊<>["https://github.com/rack/rack"]}
}