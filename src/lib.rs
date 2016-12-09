#[macro_use]
extern crate nom;

use nom::{be_u8, be_u16, be_u32, IResult, ErrorKind};


// Main Parser Entry Point /////////////////////////////////////////////////////

pub fn parse_midi<'a>(input: &'a [u8]) -> Result<Midi<'a>, ErrorKind> {
    match parse_file(input) {
        IResult::Done(_, midi) => Ok(midi),
        IResult::Error(e) => Err(e),
        IResult::Incomplete(_) => unreachable!(),
    }
}


// Midi Data Structures ////////////////////////////////////////////////////////

#[derive(Debug)]
pub struct Midi<'a> {
    header: Header,
    chunks: Vec<Chunk<'a>>,
}

#[derive(Debug)]
pub struct Header {
    len: u32,
    format: u16,
    tracks: u16,
    division: u16,
}

#[derive(Debug)]
pub enum Event<'a> {
    Midi(u32, MidiEvent),
    Meta(u32, MetaEvent<'a>),
    Sysex(u32, SysexEvent<'a>),
}

#[derive(Debug)]
pub struct TrackChunk<'a> {
    events: Vec<Event<'a>>,
}

#[derive(Debug)]
pub enum Chunk<'a> {
    Track(TrackChunk<'a>),
}


// Midi Container Parsers //////////////////////////////////////////////////////

named!(parse_file<&[u8], Midi>,
  do_parse!(
    header: header >>
    chunks: many0!(chunk) >>
    eof!() >>
    (Midi {
        header: header,
        chunks: chunks.into_iter().filter_map(|x| x).collect(),
    })
  )
);

named!(header<&[u8], Header>,
  do_parse!(
    tag!(b"MThd") >>
    len: be_u32 >>
    format: be_u16 >>
    tracks: be_u16 >>
    division: be_u16 >>
    (Header {
        len: len,
        format: format,
        tracks: tracks,
        division: division,
    })
  )
);

fn chunk(input: &[u8]) -> IResult<&[u8], Option<Chunk>> {
    let (_, check) = try_parse!(input, opt!(tag!(b"MTrk")));
    if check.is_some() {
        map!(input, track, |x| Some(Chunk::Track(x)))
    } else {
        ignore(input)
    }
}

fn track(input: &[u8]) -> IResult<&[u8], TrackChunk> {
    let (rest, data) = try_parse!(input, do_parse!(
      tag!(b"MTrk") >>
      len: be_u32 >>
      data: take!(len) >>
      (data)
    ));
    let (_, events) = try_parse!(data, terminated!(many0!(event), eof!()));
    IResult::Done(rest, TrackChunk {
        events: events,
    })
}

named!(ignore<&[u8], Option<Chunk> >,
  do_parse!(
    take!(4) >>
    len: be_u32 >>
    take!(len) >>
    (None)
  )
);

named!(event<&[u8], Event>,
  do_parse!(
    dt: var_length >>
    event: alt!(switch!(be_u8,
      0xFF => map!(meta_event, |x| Event::Meta(dt, x)) |
      0xF0 => map!(sysex_event, |x| Event::Sysex(dt, x)) |
      0xF7 => map!(sysex_event, |x| Event::Sysex(dt, x))
    ) | map!(midi_event, |x| Event::Midi(dt, x))) >>
    (event)
  )
);


// MIDI Events /////////////////////////////////////////////////////////////////

#[derive(Debug)]
pub struct MidiEvent {
}

// TODO: THIS IS VERY WRONG, VERY BAD!
named!(midi_event<&[u8], MidiEvent>,
  preceded!(take!(2), value!(MidiEvent {}))
);


// Meta Events /////////////////////////////////////////////////////////////////

#[derive(Debug)]
pub struct MetaEvent<'a> {
    kind: u8,
    data: &'a [u8],
}

named!(meta_event<&[u8], MetaEvent>,
  do_parse!(
    tag!([0xFF]) >>
    kind: be_u8 >>
    len: var_length >>
    data: take!(len) >>
    (MetaEvent {
        kind: kind,
        data: data,
    })
  )
);


// System Exclusive Events /////////////////////////////////////////////////////

#[derive(Debug)]
pub struct SysexEvent<'a> {
    /// Set when parsing an F0 message, and unset on an F7 message
    start: bool,
    /// Set when the message ends with F7
    end: bool,
    data: &'a [u8],
}

named!(sysex_event<&[u8], SysexEvent>,
  do_parse!(
    kind: alt!(tag!([0xF0]) | tag!([0xF7])) >>
    len: var_length >>
    data: take!(len) >>
    (SysexEvent {
        start: kind == [0xF0],
        end: data[data.len()-1] == 0xF7,
        data: data,
    })
  )
);


// Utility Parsers /////////////////////////////////////////////////////////////

pub fn var_length(input: &[u8]) -> IResult<&[u8], u32> {
    for i in 0..4 {
        if input[i] & 0xF0 == 0 {
            let mut result = 0;
            for j in 0..i {
                result <<= 8;
                result |= input[j] as u32;
            }
            return IResult::Done(&input[i..], result);
        }
    }
    IResult::Error(ErrorKind::Custom(0))
}


// Tests ///////////////////////////////////////////////////////////////////////

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
    }
}
