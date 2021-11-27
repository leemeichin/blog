* FLoC Off
:PROPERTIES:
◊define-meta[date]{2021-04-16}
◊define-meta[published #t]
◊define-meta[category]{programming}

FLoC (Federated Learning of Cohorts) is Google's answer to the diminishing utility of third party cookies.

Browsing this site will not opt you into this latest experiment in large-scale privacy violation.

You might notice that the site does gather analytics using plausible.io◊^[1], who themselves go into some more detail about this and how to opt-out◊^[2].

You can see the analytics for yourself, as I have made them public - you and I see the same thing on that page. It's a glorified hit-counter that lets me see what posts land better than others and it is very easily adblockable. In fact, go ahead and block Javascript on this site - if there's any feature I ever add that depends on it, there will always be an accessible ◊code{<noscript>}◊^[3] fallback if it actually matters to me.

I don't have any issue with that kind of technology, for what it's worth. You're only seeing how people use your site so you can figure out how you might tweak things, or understand what you need to do less of if you're scaring people away. It has practically nothing in common with the invasive tracking and advertising that follows you all across the internet, the likes that Google and Facebook involve themselves with at a scale beyond human comprehension.

Anyway, every page here is served with the ◊code{Permissions-Policy: interest-cohort=()} header set. There is a valid argument that this still presents a datapoint that can be tracked, but since the change is happening server-side, it is less useful than if you sent the same thing from your browser in every request, adding to your unique fingerprint (as with ◊code{Do-Not-Track}, an abject failure of a standard◊^[4]).

If you're curious, you can also check out the Security Headers report for this site◊^[5].

◊footnotes{
  ◊^[1]{◊<>["https://plausible.io/kamelasa.dev"]}
  ◊^[2]{◊<>["https://plausible.io/blog/google-floc"]}
  ◊^[3]{◊<>["https://developer.mozilla.org/en-US/docs/Web/HTML/Element/noscript"]}
  ◊^[4]{◊<>["https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/DNT"]}
  ◊^[5]{◊<>["https://securityheaders.com/?q=www.kamelasa.dev&followRedirects=on"]}
}