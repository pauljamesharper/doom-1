;;; ~/.doom.d/+lookup.el -*- lexical-binding: t; -*-

(setq counsel-search-engines-alist
      '((ddg "https://duckduckgo.com/ac/" "https://duckduckgo.com/?q=" counsel--search-request-data-ddg)
        (google "http://suggestqueries.google.com/complete/search" "https://www.google.com/search?q=" counsel--search-request-data-google)
        (bib "https://fu-berlin.hosted.exlibrisgroup.com/primo-explore/search?query=any,contains,%s&tab=fub&search_scope=FUB_ALL&vid=FUB&lang=de_DE&offset=0")))
