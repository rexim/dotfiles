(rc/require 'elfeed)
(setq elfeed-feeds
      '(("http://githubengineering.com/atom.xml")
        ("https://fornever.me/rss.xml")
        ("http://dsavenko.com/atom.xml")
        ("http://feeds.feedburner.com/xoredhq")
        ("http://blog.4geo.ru/feeds/posts/default")))
(global-set-key (kbd "C-x w") 'elfeed)
