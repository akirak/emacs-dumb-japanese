# katawa.el [![Build Status](https://travis-ci.org/akirak/katawa.el.svg?branch=master)](https://travis-ci.org/akirak/katawa.el)

Katawa.el is a method to write Japanese in Emacs. It is not a real
input method for Emacs, but it provides a simple API that allows you to produce some
Japanese text in Emacs/EXWM via a completion interface. It is not suitable for writing a document in the language, but it can be used to insert a few words into a document in another language (e.g. English) or perform a search.

Katawa.el is designed to be backend-agnostic, i.e. you can use any function to generate candidates, but it currently ships with only [Google CGI API for Japanese Input][google_cgiapi]. This backend doesn't require an offline dictionary on your computer, but it requires an internet connection.

[google_cgiapi]: https://www.google.co.jp/ime/cgiapi.html

Katawa.el can be integrated with various completion interfaces. The following frontend integrations are included in this package:

- Simple `completion-read` interface
- Ivy interface which inserts the result into the current buffer
- Ivy interface which sends the result into an X window in EXWM
- Company backend

## Prerequisites

- Emacs 25.1
- dash.el
- request

The following dependencies are optional:

- Ivy, required by the Ivy interface (katawa-ivy.el)
- Company, required by the Company backend (company-katawa.el)

## Installation

Use the following recipe:

    (katawa :host github :repo "akirak/katawa.el")

## Configuration

The following configuration is an example:

``` emacs-lisp
(use-package katawa
  :commands (katawa-ivy katawa-ivy-fix katawa-ivy-fix-at-point)
  :init
  (bind-keys :map text-mode-map
             ("C-c j" . katawa-ivy)
             ("C-c J" . katawa-ivy-fix)))

```

## Usage

### Simple (`katawa.el`)

#### `katawa-insert`

`katawa-insert` interactive command, which is included in katawa.el, lets you
insert Japanese text into the current buffer. If you enter *romaji*, i.e.
Japanese text represented in alphabets, after the prompt, then the result
is entered into the buffer. However, I suggest that you should use `katawa-ivy`
command for this purpose, which dynamically updates candidates as you supply input.

### Ivy (`katawa-ivy.el`)

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

### Ivy with EXWM support (`katawa-ivy-exwm.el`)

`katawa-ivy-exwm` command is almost the same as `katawa-ivy`, but if the current buffer is an [EXWM](https://github.com/ch11ng/exwm/) buffer, it pastes the result into the X window. This is convenient for performing a search with a few Japanese words inside a browser.

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
  
`katawa-ivy` and its family sometimes suffer from an error returned by the
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
