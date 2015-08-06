(rc/require 'elfeed)
(setq elfeed-feeds
      '(
        ("https://fornever.me/rss.xml")
        ("http://dsavenko.com/atom.xml")
        ("http://feeds.feedburner.com/xoredhq")
        ("http://blog.4geo.ru/feeds/posts/default")
        ("http://eev.ee/feeds/atom.xml")
        ("https://blog.debiania.in.ua/feeds/all.rss"))
      )
(global-set-key (kbd "C-x w") 'elfeed)
