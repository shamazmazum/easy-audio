Easy audio
==========
[![Build Status](https://api.cirrus-ci.com/github/shamazmazum/easy-audio.svg)](https://cirrus-ci.com/github/shamazmazum/easy-audio)
![CI](https://github.com/shamazmazum/easy-audio/workflows/CI/badge.svg)

Overview
-------
**NB**: 25.04.2022: `easy-audio/utils` system was merged to
`easy-audio/core`. Global nicknames were removed from all packages loaded with
`(asdf:load-system :easy-audio)`. Use full names (like `easy-audio.flac` instead
of `flac`) or local nicknames. The version was bumped to `1.1`.

Easy audio is my small but slowly growing pack of audio decoders. It can
help you decode audio files and also provides easy access to metadata.

It has:
 * FLAC format support. Can decode anything, supports almost all metadata
   blocks.
 * Partial support for WavPack format. Can read and decode non-hybrid
   lossless WavPack data which is the most used, anyway. Support many
   metadata blocks (though they are not as useful as in FLAC).
 * Partial wav container support, can read uncompressed, a-law compressed
   and mu-law compressed audio data.
 * OGG container support, but, unfortunately, without Vorbis decoder.
   Can read FLAC compressed data inside OGG container.
 * APEv2 tags support (currently only in wavpack files).
 * Partial APE support. Only the most recent version (3.99) is supported, also
   mono audio is not supported. Also there is no integrity checks.

It has minimum dependencies (only `flexi-streams` for reading UTF-8 coded
values from metadata) and written entirely in Common Lisp. It contains
unsafe code to achieve maximal performance and assumes that fixnums are
more than 32-bit wide, so I am not sure if it works on some 32-bit
implementations or not.

Documentation
------------
Documentation for `easy-audio` is automatically generated by `codex`. Just
run `(codex:document :easy-audio)`. You can also visit a
[project page](http://shamazmazum.github.io/easy-audio/).
