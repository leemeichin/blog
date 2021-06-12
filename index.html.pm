#lang pollen

◊(define-meta title "Home")

◊h1[#:class "prompt"]{
  > ◊span[#:class "type-in"]{
    ls -al /var/www/kamelasa.dev/archive
  }
}

◊table[#:class "borderless"]{
  ◊tbody{
    ◊for/published-posts[#:as p]{
      ◊tr{
        ◊td[#:class "file-permission"]{-rw-r--r--}
        ◊td[#:class "user"]{mrlee}
        ◊td[#:class "group"]{www}
        ◊td[#:class "size"]{◊(post->size p)}
        ◊td[#:class "date"]{◊(post->date p)}
        ◊td{
          ◊a[#:href (page-url p) #:title (post->title p)]{
            ◊post->title[p]
          }
        }
      }
    }
  }
}

◊h1[#:class "prompt"]{
  > ◊span[#:class "type-in"]{
    git log --format=pretty | head -n 10
  }
}

◊ul{
  ◊for/splice[([log (in-list (post->history))])]{
    ◊li[#:class "git-log"]{
      ◊a[#:href (log->giturl log)]{
        ◊log->commit[log]
      }
      ◊span[#:class "commit-msg"]{
        ◊log->message[log]
      }
      ◊tag-time[#:class "commit-time" #:datetime (log->date log)]{
        (◊log->date[log])
      }
    }
  }
}