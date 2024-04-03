type t = {
  mutable pos : int; (* in bytes *)
  buf : Bytes.t; (* 8 byte buffer for words *)
  oc : out_channel; (* output *)
}

let create oc = { pos = 0; buf = Bytes.make 8 '\000'; oc }
let close t = close_out t.oc

module Word = struct
  exception Int_too_big

  let[@inline] bits (start : int) (endb : int) ~(v : int) =
    let bits = endb + 1 - start in
    if v >= Int.shift_left 1 (bits + 1) then raise Int_too_big;
    Int64.of_int (Int.shift_left v start)

  let ( -- ) a b = Int64.logor a b
end

let write (bw : t) (b : bytes) ~(off : int) ~(len : int) =
  output bw.oc b off len;
  bw.pos <- bw.pos + len

let write_bytes (bw : t) (b : bytes) = write bw b ~off:0 ~len:(Bytes.length b)

let write_string (bw : t) (s : string) =
  output_string bw.oc s;
  bw.pos <- bw.pos + String.length s

let zeros = Bytes.make 8 '\000'

let pad_to_word (bw : t) =
  match bw.pos mod 8 with
  | 0 -> ()
  | overflow -> write bw zeros ~off:0 ~len:(8 - overflow)

let write_int64 (bw : t) (w : Int64.t) =
  Bytes.set_int64_le bw.buf 0 w;
  write_bytes bw bw.buf
