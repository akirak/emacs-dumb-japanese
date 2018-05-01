# katawa.el

katawa.el is yet another way to writing Japanese in Emacs. It is not a real
input method for Emacs, but it provides a simple API which lets you produce
Japanese text in Emacs.

katawa.el may become extensible in the future, but it currently supports
only one backend: a wrapper around [Google CGI API for Japanese Input][google_cgiapi]. 
It doesn't require an offline dictionary on your computer, but it requires
an internet connection.

[google_cgiapi]: https://www.google.co.jp/ime/cgiapi.html

This package also ships with an Ivy interface for writing Japanese as well as
a company backend. The company backend is experimental.

## Prerequisites

- Emacs 25.1
- dash.el
- request

The following dependencies are optional:

- Ivy, required by the Ivy interface (katawa-ivy.el)
- Company, required by the Company backend (company-katawa.el)

## Installation

FIXME

## Usage

### `katawa-insert`

`katawa-insert` interactive command, which is included in katawa.el, lets you
insert Japanese text into the current buffer. If you enter *romaji*, i.e.
Japanese text represented in alphabets, after the prompt, then the result
is entered into the buffer. However, I suggest that you should use `katawa-ivy`
command for this purpose, which is an Ivy equivalent supporting dynamic
candidates.

### Ivy

#### Inserting Japanese into the buffer

`katawa-ivy` command lets you write Japanese by typing text in an Ivy interface.

The following configuration lets you start writing Japanese by typing "jj"
key chords in a `text-mode` buffer:

``` emacs-lisp
(require 'key-chord)
(key-chord-mode 1)
(define-key text-mode-map (vector 'key-chord ?j ?j) #'katawa-ivy)
```

In case you forget to invoke the command and insert alphabets literally,
there is `katawa-ivy-fix` command. This command, without a region, transliterates
single-byte characters before the point. 

#### Editing Japanese

`katawa-ivy.el` provides the following two commands for re-transliterating
existing text. I recommend that you bind keys to these commands:

- With an active region, `katawa-ivy-fix` command re-transliterate text
  in the region.

- `katawa-ivy-fix-at-point` command re-transliterates a segment under
  the point. Segmentation is currently done by the Google CGI API, and
  its results look unnatural, but this interface allows you to rewrite
  a word.

#### Displaying candidates at your cursor position

[ivy-posframe](https://github.com/tumashu/ivy-posframe) may be what you want.
It requires Emacs 26.

### Company

The package also ships with a [company](https://github.com/company-mode/company-mode)
backend for writing Japanese. It is experimental, and I personally recommend
the Ivy interface over this solution, but it looks more like an input method.

To use the backend, load `company-katawa.el` and add `company-katawa-backend`
to `company-backends`.

``` emacs-lisp
(add-hook 'company-backends 'company-katawa-backend)
```

You also need to turn on `company-katawa-mode` minor mode in buffers in which
you want to activate the transliteration. 

## Limitations and known issues

As katawa depends on the Google API for transliteration and segmentation,
its capability is limited by the backend:

- The Google API may not provide a dictionary entry for a word you want to
  enter. katawa is totally helpless in this situation. Try other words and/or
  another Japanese input method to get the kanji you need.

- It seems that the API sometimes fails to provide a candidate in *katakana*
  (カタカナ). In that case, you can switch the input method (`C-u C-\`) to
  `japanese-katakana` to write katakana literally. You can also confirm
  the text temporarily in *hiragana* (ひらがな) but convert it into katakana using
  `japanese-katakana-region` command.
  
`katawa-ivy` and its family sometimes suffers from an error returned by the
Google CGI API and stops updating their candidates. In such a situation, you
can abort the completion but resume it using `ivy-resume` command.

## Why it depends on Google CGI API rather than mozc

[mozc](https://github.com/google/mozc) has been around, and it is smarter than
the dumb CGI API, but I personally don't want to use mozc. The distribution size
of mozc is pretty large, and it takes indeed a lot of time to build the package. 
I hated downloading and building mozc, so I stopped using it.

The Google CGI API backend of katawa only depends on the online service.
It requires neither a huge binary installed onto the local machine nor offline
dictionaries to look up words. As I don't write much Japanese, I don't care
about the transliteration quality. Ease of deployment is of greater importance.

## What is meant by *katawa*?

It is a Japanese word which means *being disabled*. This software was named
as such, since it is not fully capable as a Japanese input method.
Rather, it is an incomplete effort to solve the problem of writing Japanese
on computers. 
It will not become complete, because Japanese is not going to be saved. 
Nonetheless, for now, I sometimes have to deal with a problem of communicating
in the language, and katawa is intended to be a solution for that.

## Inspirations

katawa was originally inspired by the following solutions:

- [Google IME SKK サーバー 作った](http://blog.sushi.money/entry/20110421/1303274561)
- [ac-mozc](https://github.com/igjit/ac-mozc)

## License

GPL v3
