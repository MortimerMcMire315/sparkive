Sparkive
========

Simple, elegant digital library management
------------------------------------------

**Sparkive**, currently in early development, will be a platform for managing and
describing large collections of information. The software will allow users to
easily manage digital libraries and place them online for public or protected
browsing through a customizable interface.

By tagging library items with arbitrary attributes, library managers can allow
their library to be browsed with respect to any attribute.


Technical details
-----------------

Sparkive's interface is web-based. The site runs on Happstack with Shakespearean
templates. On the backend, users may configure Sparkive to use either Acid-State
(no setup, recommended for small or medium-sized collections) or PostgreSQL
(more-involved setup, recommended for large collections due to speed).


Hacking on Sparkive
-------------------

Feel free to hack on this project. I will inevitably always put off frontend
work in favor of backend work, so any frontend developers are welcome (and
encouraged) to work on HTML/CSS/JS (Hamlet/Lucius/Julius) templates.

Goals
---------------

* Library manager interface vs. normal user interface
* Items may or may not include a file (for example, one item may be an MP3 file
  of an interview, while another item is simply a reference to a book
  representing a physical or non-free book).
* Cross-references
* User documentation
* Batch-adding/modifying attributes for multiple items
* Looks good (i.e. someone besides me does some design + CSS work)
* Simple conversion between Acid-State and PostgreSQL databases so that users
  may easily switch between the two
* Users can modify CSS/HTML and make their own templates

Install & Run
-------------
* Install [stack](https://docs.haskellstack.org/en/stable/README/)
* In the `conf` directory, edit `sparkive_example.conf` and move to `sparkive.conf`.
* run `stack build`

License
-------

[GPL-3](https://www.gnu.org/licenses/gpl-3.0.en.html)
