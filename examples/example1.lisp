"<!DOCTYPE html>"
(:|html| nil ""
  (:|head| nil ""
    (:|meta| (:|charset| "UTF-8"))
    (:|title| nil "Quick Notes"))
  (:|body| nil ""
    (:|div| (:id "main") ""
      (:|div| (:class "note") ""
        (:|h2| nil "Quick Notes")
        (:|ul| nil ""
          (:|li| nil ""
            "Haml is usually indented with two spaces,"
            "although more than two is allowed."
            "You have to be consistent, though.")
          (:|li| nil ""
            "The first character of any line is called"
            "the \"control character\" - it says \"make a tag\""
            "or \"run Common Lisp code\" or all sorts of things.")
          (:|li| nil ""
            "Haml takes care of nicely indenting your HTML.")
          (:|li| nil ""
            "Haml allows Common Lisp code and body."
            "But not in this example."
            "We turned it off for security.")))
      ""
      (:|div| (:|class| "note") ""
        "You can get more information by reading the"
        (:|a| (:|href| "readme.html") "Official CL-HAML Reference."))
      ""
      (:|div| nil ""
        (:|p| nil ""
          "This example doesn't allow Common Lisp to be executed,"
          "but real CL-HAML does.")
        (:|p| nil ""
          "Common Lisp code is included by using = at the"
          "beginning of a line.")
        (:|p| nil ""
          "Read the tutarial for more information.")))))
