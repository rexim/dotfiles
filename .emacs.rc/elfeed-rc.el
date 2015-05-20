(rc/require 'elfeed)
(setq elfeed-feeds
      '(("http://githubengineering.com/atom.xml")
        ("https://fornever.me/rss.xml")))
(global-set-key (kbd "C-x w") 'elfeed)
