package org.docopt;

import java.util.Map;

public class NavalFate {
  public static final String USAGE = "Naval Fate.\n"
                                     + "\n"
                                     + "Usage:\n"
                                     + "  naval_fate ship new <name>...\n"
                                     + "  naval_fate ship <name> move <x> <y> [--speed=<kn>]\n"
                                     + "  naval_fate ship shoot <x> <y>\n"
                                     + "  naval_fate mine (set|remove) <x> <y> [--moored|--drifting]\n"
                                     + "  naval_fate -h | --help\n"
                                     + "  naval_fate --version\n"
                                     + "\n"
                                     + "Options:\n"
                                     + "  -h --help     Show this screen.\n"
                                     + "  --version     Show version.\n"
                                     + "  --speed=<kn>  Speed in knots [default: 10].\n"
                                     + "  --moored      Moored (anchored) mine.\n"
                                     + "  --drifting    Drifting mine.";
  public static final String VERSION = "1.0";

  public static void main(String[] args) {
    Map<String, Object> collected = org.docopt.javaapi.Docopt.apply(USAGE, args, false, VERSION, false);
    System.out.println(collected);
  }
}
