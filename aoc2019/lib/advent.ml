let read_lines file x =
  In_channel.with_open_text file In_channel.input_all |> Str.(split (regexp x))
;;
