# haze

[![MIT license](https://img.shields.io/badge/license-MIT-blue.svg)](https://github.com/cronokirby/haze/blob/master/LICENSE)

Small bittorrent client, written in Haskell.

The main goal of this project is to provide a clear implementation of the Bittorrent protocol
in Haskell. Providing good performance is also something that would be good to achieve, but reaching
the completeness of something like `libtorrent` or `transmission` is out of the scope of this project.

## Examples
![Here's a gif of the progress display](https://cdn.discordapp.com/attachments/251783968515555330/569967958118498395/Peek_2019-04-22_21-28.gif)

## Usage

```
Usage: haze TORRENT_FILE [-o|--output-dir DIRECTORY] [-l|--log-file LOG_FILE]
            [-p|--port PORT] [-v|--version]
  Download the torrent in TORRENT_FILE

Available options:
  TORRENT_FILE             The torrent file to download
  -o,--output-dir DIRECTORY
                           The directory to download the torrent to
  -l,--log-file LOG_FILE   Logging will happen to this file if set
  -p,--port PORT           The port to listen for incoming connections on
  -h,--help                Show this help text
  -v,--version             Display what version the program is using
```

After launching **haze** with a torrent, it will show a progress display with
ongoing information on the status of the torrent. Once the torrent has finished downloading,
**haze** will continue to seed it until the program is cancelled (using CTRL-C is fine).

By default, **haze** will output the torrent files to the current directory, although
if a torrent has multiple files, it will usually define a relative root, and files will
end up there instead. Using a folder for the output is recommended for single file torrents,
since many small files are generated while downloading the torrent, before they get glued
together to make up the large file. These files might otherwise temporarily clutter up
a folder you want to keep clean, so be wary of that.

At the moment, **haze** only supports downloading a single torrent file at once.

**haze** can perform logging to a file if specified, otherwise no logging is done.
The logging exists mainly for debug purposes, although it might be interesting to look
at to gleam some more about how the program works :)

## Resources
The main resource I used for learning about the specification is here:
https://wiki.theory.org/index.php/BitTorrentSpecification

The above page is enough to understand the spec, but it takes a few reads
to grok all the information.
