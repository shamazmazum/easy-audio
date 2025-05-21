# Changelog

## Version 1.2

This version is a step towards removing CLOS and unsafe code.

* API change: MIXCHANNELS is renamed to INTERLEAVE-CHANNELS and creates now a
  new array.
* API change: MAKE-OUTPUT-BUFFERS is gone. It's cheap to create an array
  implicitly when needed.
* API change: Readers for vorbis comment metadata in flac are renamed to
  VORBIS-COMMENT-USER and VORBIS-COMMENT-VENDOR
* API change: FLAC:FRAME-DECODE is renamed to FLAC:DECODE-FRAME.
* Improvement: flac2wav example can decode 24bps files.
* Improvement: flac audio frames are now not modified while decoding.
* Optimization INTERLEAVE-CHANNELS with number of channels > 2 now works faster.
* Bug fix: Wavpack decoder correctly handles 32 bps audio files
* Bug fix: Wavpack decoder correctly handles pseudo-stereo data blocks.

## Version 1.1

`easy-audio/utils` system was merged to `easy-audio/core`. Global nicknames were
removed from all packages loaded with `(asdf:load-system :easy-audio)`. Use full
names (like `easy-audio.flac` instead of `flac`) or local nicknames.
