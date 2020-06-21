use "fpic.sml";
processTeXfile "tryit";
use "fpicpics.sml";
OS.Process.system "latex tryit";
OS.Process.system "dvipdf tryit";
OS.Process.system "convert tryit.pdf tryit.png";
OS.Process.system "display tryit.png";

