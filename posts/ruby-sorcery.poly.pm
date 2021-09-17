#lang pollen

◊define-meta[title]{Ruby Sorcery Part 1: Pattern Matching}
◊define-meta[date]{2021-09-17}
◊define-meta[published #t]
◊define-meta[category]{ruby}


I recently realised that despite many new additions to the Ruby language and ecosystem, I've never really had an opportunity to take advantage of many of them. Of course, some new language features are more useful than others, particularly when it comes to maintaining code as a team, but what is also interesting is that they also support less conventional, or immediately-apparent, use-cases.

The first part of this series of posts is all about ◊em{Pattern Matching}.

◊h2{Pattern matching}

Ruby's pattern matching support, introduced experimentally in 2.7, is a lot more powerful than you may expect. All you need is to replace ◊code{when} with ◊code{in} and your ◊code{case} statements become capable of matching against ◊em{anything}.

◊codeblock['ruby]{
  require 'base64'
  
  class ParsedJson; end;
  
  def handle_response(response)
    case response
    in { code: (300..399) }
      redirect_to response.headers[:location]
    in { status: :unauthorized | :forbidden => status }
      raise NotAllowedError.new(status)
    in { code: 200, body: ParsedJson => payload }
      Service.call(payload)
    in { code: 200, body: String => text, content_type: 'application/base64' }
      Base64.decode64(text)
    end
  end
    
      
  handle_response({ code: 200, body: 'aGVsbG8gd29ybGQ=', content_type: 'application/base64' })
  #=> 'hello world'

  handle_response({ code: 301 })
  # => redirect

  handle_response({ status: :forbidden, code: 403 })
  # NotAllowedError (forbidden)
}

◊h3{Custom destructuring}

You can deeply match any object in Ruby so long as you define a method to represent it as a hash, or a method to represent it as an array. Or both.

◊codeblock['ruby]{
  class PlayingCard
    attr_reader :value, :colour, :suit
    
    def initialize(value:, colour:, suit:)
      @value = value
      @colour = colour
      @suit = suit
    end

    def deconstruct
      [value, colour, suit]
    end

    def deconstruct_keys(*)
      {
        value: value,
        colour: colour,
        suit: suit
      }
    end
  end
}

This ◊code{PlayingCard} class is now capable of pattern matching.

◊codeblock['ruby]{
  def face_card?(playing_card)
    case playing_card
    in { value: 'K' | 'Q' | 'J' } then true
    else false
    end
  end

  face_card?(PlayingCard.new(value: 3, colour: :red, suit: :spades))
  #=> false
}

◊h3{Pinning}

That's fairly basic, what about pattern matching poker? Matching one card is easy, but suppose you want to match a hand.

◊codeblock['ruby]{
  class PokerHand
    attr_reader :cards
    
    def initialize(cards: [])
      @cards = cards
    end

    def deconstruct
      cards
    end
  end
}

Now that a hand of cards is represented, it should be possible to use pattern matching to find a winning play, say... a Royal Flush. For this to work, ◊em{variable pinning} is required, because a Royal Flush requires the colour and suit to be the same for each card.

This particular solution depends on the hand being ordered, but that's fine, a lot of computational problems become simpler if you sort them first. For the sake of example, assume that has already happened.

◊codeblock['ruby]{
  def royal_flush?(hand)
    case hand
    in [[1, c, s], [10, ^c, ^s], ['J', ^c, ^s], ['Q', ^c, ^s], ['K', ^c, ^s]]
      true
    else false
    end
  end

  # alternatively, if golfing in Ruby 3:
  # def royal_flush?(hand) = !!(hand in [[1, c, s], [10, ^c, ^s], ['J', ^c, ^s], ['Q', ^c, ^s], ['K', ^c, ^s]] rescue false)


  my_hand = PokerHand.new(cards: [
    PlayingCard.new(value: 1, colour: :black, suit: :hearts),
    PlayingCard.new(value: 10, colour: :black, suit: :hearts),
    PlayingCard.new(value: 'J', colour: :black, suit: :hearts),
    PlayingCard.new(value: 'Q', colour: :black, suit: :hearts),
    PlayingCard.new(value: 'K', colour: :black, suit: :hearts),
  ])

  royal_flush?(my_hand)
  # => true
}

The clever bit here is that the first part of the match (◊code{[1, c, s]}) is used to constrain the rest of the pattern. So if ◊code{c} is ◊code{:red}, then ◊code{^c} also has to be ◊code{:red} in order to match.

◊h2{Pattern guards}

You'll see this a lot if you're familiar with Elixir or other languages that do pattern matching well. Essentially, you can add conditional logic to your patterns so that a match is only possible if a separate condition is met.

Building on the poker example, maybe it's valid to play the Joker, but only if the dealer has allowed it?

◊codeblock['ruby]{
  def joker_allowed?
    true
  end
    
  def valid_call?(card)
    case card
    in [:Joker, *] if joker_allowed?
      puts 'joker allowed'
      true
    else true
    end
  end

  valid_call?(PlayingCard.new(value: :Joker, colour: nil, suit: nil))
  # => joker allowed
  # => true
}

◊h2{Destructuring assignment without ◊code{case}}

One of the odd side-effects of this pattern matching functionality is that you get a new kind of assingment. In fact, in Ruby 3 this gets a syntax of its own with the rightward assignment operator, but you can still use something similar in 2.7.

In fact, this method also allows you to use pattern matching while destructuring. It's not so easy on the eyes, however, as the variable bindings are actually inside the pattern, and not the expression on the left-hand side.

You also have to be absolutely sure you're matching the right thing.

◊codeblock['ruby]{
  card = PlayingCard.new(value: 7, suit: :diamonds, colour: :red)

  card in { value: (1..10) => v, suit: :diamonds  => s}

  # v => 7
  # s: :diamonds

  begin
    card in { value: String, suit: Symbol }
  rescue NoMatchingPatternError
    puts 'son, I am disappoint'
  end
}

◊h2{Optimisations}

If you recall earlier examples, I defined ◊code{destructure_keys(*)}, which meant that I was explicitly ignoring the arguments normally passed to the method. This is useful in simple cases, but when dealing with complex objects you might want to be a bit more thoughtful about how you return a value. For example, converting the entire structure of the object into a hash might not be appropriate.

◊codeblock['ruby]{
  # When used in pattern matching, this class will only destructure into the provided keys
  
  class PokerHand
    def deconstruct_keys(keys)
      cards.map { |card| card.slice(keys) }
    end
  end
}

◊hr{}

Well, this doesn't cover the entirety of Ruby's pattern matching fun, but it should at least show you the various things you're now able to do with the feature. If in doubt, RTFM◊^[1]; Ruby's documentation is absolutely fantastic.

◊aside{Specifying 'rubydoc' in your Google searches should reveal Ruby's official documentation and not the SEO spam that is ApiDock.}

Check in soon to see another deep-dive into Ruby Sorcery.

◊footnotes{
  ◊^[1]{◊<>["https://docs.ruby-lang.org/en/3.0.0/doc/syntax/pattern_matching_rdoc.html"]}
}