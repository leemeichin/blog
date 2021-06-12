<!DOCTYPE html>
<html lang="en">
  <head>
    <meta charset="utf-8" />
    <meta name="viewport" content="width=device-width, initial-scale=1" />

    <title>kamelåså - ◊post->title[here]</title>

    <link rel="alternate" type="application/rss+xml" href="/rss.xml" />
    <link rel="alternate" type="application/atom+xml" href="/atom.xml" />
    <link rel="stylesheet" href="/css/terminal.min.css" />
    <link rel="stylesheet" href="/css/main.css" />
    <link rel="stylesheet" href="/css/syntax.css" />

    <script
      async
      defer
      data-domain="kamelasa.dev"
      src="https://plausible.io/js/plausible.js"
    ></script>
  </head>

  <body class="terminal">
    <div class="container">
      <div class="terminal-nav">
        <header class="terminal-logo">
          <div class="logo terminal-prompt">
            <a class="no-style" href="/">kamelåså</a>
            <span>special topics in calamity something or other</span>
          </div>
        </header>
        <nav class="terminal-menu">
          <ul vocab="https://schema.org/" typeof="BreadcrumbList">
            <li>
              <a
                href="https://sr.ht/~mrlee/"
                class="icon-link"
                title="Code @ Sourcehut"
              >
                <svg
                  xmlns="http://www.w3.org/2000/svg"
                  width="24"
                  height="24"
                  viewBox="0 0 24 24"
                  fill="none"
                  stroke="currentColor"
                  stroke-width="2"
                  stroke-linecap="round"
                  stroke-linejoin="round"
                  class="icon-img"
                >
                  <polyline points="16 18 22 12 16 6"></polyline>
                  <polyline points="8 6 2 12 8 18"></polyline>
                </svg>
              </a>
            </li>
            <li>
              <a
                href="https://plausible.io/kamelasa.dev"
                class="icon-link"
                title="Site stats"
              >
                <svg
                  xmlns="http://www.w3.org/2000/svg"
                  width="24"
                  height="24"
                  viewBox="0 0 24 24"
                  fill="none"
                  stroke="currentColor"
                  stroke-width="2"
                  stroke-linecap="round"
                  stroke-linejoin="round"
                  class="icon-img"
                >
                  <path d="M21.21 15.89A10 10 0 1 1 8 2.83"></path>
                  <path d="M22 12A10 10 0 0 0 12 2v10z"></path>
                </svg>
              </a>
            </li>
          </ul>
        </nav>
      </div>
    </div>

    <main class="container">
◊when/splice[(equal? 'posts (parent here))]{
  <article>
    <header class="header inverse-video">
      <h2 class="title">◊post->title[here]</h2>
      <span class="ert">~◊post->ert[here] min.read</span>
      <span class="post-date">◊post->date[here]</span>
    </header>

    <section>
      ◊(->html doc)
    </section>

    <hr />

    <section>
      <h3>Changelog</h3>
      <ul>
        ◊for/splice[([log (in-list (post->history here))])]{
          <li class="git-log">
            <a href="◊log->giturl[log]">◊log->commit[log]</a>
            <span class="commit-msg">◊log->message[log]</span>
            <time class="commit-time" datetime="◊log->date[log]">(◊log->date[log])</time>
          </li>
        }
      </ul>
    </section>
  </article>
}

◊when/splice[(not (equal? 'posts (parent here)))]{
  ◊(->html doc)
}
    </main>

    <footer class="container">
      <header>
        <p class="modeline inverse-video">
          -UUU:----F1&nbsp;&nbsp;◊|page-title|&nbsp;&nbsp;&nbsp;&nbsp;Bot
          L100%&nbsp;&nbsp;Git:main&nbsp;&nbsp;(HTML+) ----------
        </p>
      </header>
    </footer>
  </body>
</html>
