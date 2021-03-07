(* Command line text file statistics program *)

try
    begin match Sys.argv with
        [|_; filename|] ->
            let stats = Textstats.stats_from_file filename in
                Printf.printf "Words: %d\nCharacters: %d\nSentences: %d\nLines: %d\n"
                (Textstats.words stats) 
                (Textstats.characters stats) 
                (Textstats.sentences stats) 
                (Textstats.lines stats)
    |   _   -> 
            Printf.printf "Usage: stats <filename>\n";
    end
with
    e ->
        Printf.printf "An error occured: %s\n" (Printexc.to_string e);
        exit 1

