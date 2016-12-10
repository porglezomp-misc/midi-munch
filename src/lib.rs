// Copyright (c) 2016 by Caleb Jones <code@calebjones.net>

#[macro_use]
extern crate nom;

use nom::{be_i8, be_u8, be_u16, be_u32, IResult, ErrorKind};


// Main Parser Entry Point /////////////////////////////////////////////////////

pub fn parse_midi(input: &[u8]) -> Result<Midi, ErrorKind> {
    match complete!(input, parse_file) {
        IResult::Done(_, midi) => Ok(midi),
        IResult::Error(e) => Err(e),
        IResult::Incomplete(_) => unreachable!(),
    }
}


// Midi Data Structures ////////////////////////////////////////////////////////

#[derive(Debug, PartialEq, Eq)]
pub struct Midi<'a> {
    header: Header,
    chunks: Vec<Chunk<'a>>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct Header {
    len: u32,
    format: u16,
    tracks: u16,
    division: u16,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Event<'a> {
    Midi(u32, MidiEvent),
    Meta(u32, MetaEvent<'a>),
    Sysex(u32, SysexEvent<'a>),
}

#[derive(Debug, PartialEq, Eq)]
pub struct TrackChunk<'a> {
    events: Vec<Event<'a>>,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Chunk<'a> {
    Track(TrackChunk<'a>),
}


// Midi Container Parsers //////////////////////////////////////////////////////

named!(parse_file<&[u8], Midi>,
  complete!(do_parse!(
    header: header >>
    chunks: many0!(chunk) >>
    eof!() >>
    (Midi {
        header: header,
        chunks: chunks.into_iter().filter_map(|x| x).collect(),
    })
  ))
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
    let mut events = Vec::new();
    let mut running_status = None;
    let mut input = data;
    loop {
        match event(input, &mut running_status) {
            IResult::Done(new_inp, item) => {
                events.push(item);
                input = new_inp;
            }
            IResult::Error(_) => break,
            IResult::Incomplete(i) => return IResult::Incomplete(i),
        }
    }
    try_parse!(input, eof!());
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

fn event<'a>(input: &'a [u8], running_status: &mut Option<u8>) -> IResult<&'a [u8], Event<'a>> {
    // Sysex events and meta events cancel any running status which was in
    // effect. Running status does not apply to and may not be used for these
    // messages.
    let (input, dt) = try_parse!(input, complete!(var_length));
    let (_, kind) = try_parse!(input, be_u8);
    match kind {
        0xFF => {
            *running_status = None;
            map!(input, meta_event, |x| Event::Meta(dt, x))
        }
        0xF0 => {
            *running_status = None;
            map!(input, sysex_event, |x| Event::Sysex(dt, x))
        }
        0xF7 => {
            *running_status = None;
            map!(input, sysex_event, |x| Event::Sysex(dt, x))
        }
        0xF1...0xF6 | 0xF8...0xFE => unimplemented!(),
        n@0x80...0xEF => {
            *running_status = Some(n);
            do_parse!(input,
              prefix: be_u8 >>
              event: call!(midi_event, prefix) >>
              (Event::Midi(dt, event))
            )
        }
        0x00...0x7F => match *running_status {
            Some(n@0x80...0xEF) =>
                do_parse!(input,
                event: call!(midi_event, n) >>
                (Event::Midi(dt, event))),
            Some(_) => IResult::Error(ErrorKind::Custom(2)),
            None => IResult::Error(ErrorKind::Custom(3)),
        },
        _ => unreachable!(),
    }
}


// MIDI Events /////////////////////////////////////////////////////////////////

#[derive(Debug, PartialEq, Eq)]
pub enum MidiEvent {
    NoteOn {
        channel: u8,
        number: u8,
        velocity: u8,
    },
    NoteOff {
        channel: u8,
        number: u8,
        velocity: u8,
    },
    PolyphonicAftertouch {
        channel: u8,
        number: u8,
        pressure: u8,
    },
    ChannelAftertouch {
        channel: u8,
        pressure: u8,
    },
    Control {
        channel: u8,
        change: ControlChange,
    },
    ProgramChange {
        channel: u8,
        program_number: u8,
    },
    PitchBend {
        channel: u8,
        pitch: u16,
    },
    /// Used to implement "running-status" messages. When a message is sent
    /// without a type, it's interpreted as being the same as the previous
    /// event.
    Previous(u8, u8),
}

#[derive(Debug, PartialEq, Eq)]
pub enum ControlChange {
    Raw(u8, u8),
}


// TODO: Fix parsing the running-status events.
fn midi_event(input: &[u8], prefix: u8) -> IResult<&[u8], MidiEvent> {
    match prefix {
        n@0x80...0x8F => do_parse!(input,
          num: u7 >>
          vel: u7 >>
          (MidiEvent::NoteOff {
              channel: n & 0x0F,
              number: num,
              velocity: vel,
          })),
        n@0x90...0x9F => do_parse!(input,
          num: u7 >>
          vel: u7 >>
          (MidiEvent::NoteOn {
              channel: n & 0x0F,
              number: num,
              velocity: vel,
          })),
        n@0xA0...0xAF => do_parse!(input,
          num: u7 >>
          pres: u7 >>
          (MidiEvent::PolyphonicAftertouch {
              channel: n & 0x0F,
              number: num,
              pressure: pres,
          })),
        n@0xB0...0xBF => do_parse!(input,
          controller: u7 >>
          value: u7 >>
          (MidiEvent::Control {
              channel: n & 0x0F,
              change: ControlChange::Raw(controller, value),
          })),
        n@0xC0...0xCF => do_parse!(input,
          patch: u7 >>
          (MidiEvent::ProgramChange {
              channel: n & 0x0F,
              program_number: patch,
          })),
        n@0xD0...0xDF => do_parse!(input,
          pres: u7 >>
          (MidiEvent::ChannelAftertouch {
              channel: n & 0x0F,
              pressure: pres,
          })),
        n@0xE0...0xEF => do_parse!(input,
          lsb: u7 >>
          msb: u7 >>
          (MidiEvent::PitchBend {
              channel: n & 0x0F,
              pitch: (msb as u16) << 7 | lsb as u16,
          })),
        _ => IResult::Error(ErrorKind::Custom(1))
  }
}


// Meta Events /////////////////////////////////////////////////////////////////

#[derive(Debug, PartialEq, Eq)]
pub enum MetaEvent<'a> {
    SequenceNumber(u16),
    Text {
        kind: TextType,
        text: &'a [u8],
    },
    ChannelPrefix(u8),
    EndOfTrack,
    SetTempo(u32),
    SmpteOffset {
        hours: u8,
        minutes: u8,
        seconds: u8,
        frames: u8,
        /// Represents hundredths of a frame
        fractional_frames: u8,
    },
    TimeSignature {
        numerator: u8,
        denominator: u8,
        clocks_per_metronome: u8,
        notated_divisions: u8,
    },
    KeySignature {
        key: Key,
        kind: KeyKind,
    },
    SequencerSpecific {
        data: &'a [u8],
    },
    Other {
        kind: u8,
        data: &'a [u8],
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum TextType {
    Text,
    Copyright,
    TrackName,
    InstrumentName,
    Lyric,
    Marker,
    CuePoint,
    Other(u8),
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Key {
    Flats(u8),
    OfC,
    Sharps(u8),
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum KeyKind {
    Major,
    Minor,
}

impl From<u8> for TextType {
    fn from(val: u8) -> TextType {
        use TextType::*;
        match val {
            0x01 => Text,
            0x02 => Copyright,
            0x03 => TrackName,
            0x04 => Lyric,
            0x05 => Marker,
            0x06 => CuePoint,
            x => Other(x),
        }
    }
}

named!(meta_event<&[u8], MetaEvent>,
  preceded!(tag!([0xFF]),
    switch!(be_u8,
      0x00 => do_parse!(
          tag!([0x02]) >>
          seq: be_u16 >>
          (MetaEvent::SequenceNumber(seq))) |
      kind@0x01...0x0F => do_parse!(
          len: var_length >>
          data: take!(len) >>
          (MetaEvent::Text {
              kind: TextType::from(kind),
              text: data,
          })) |
      0x20 => do_parse!(
          tag!([0x01]) >>
          prefix: be_u8 >>
          (MetaEvent::ChannelPrefix(prefix))) |
      0x2F => do_parse!(
          tag!([0x00]) >>
          (MetaEvent::EndOfTrack)) |
      0x51 => do_parse!(
          tag!([0x03]) >>
          b: take!(3) >>
          ({
              let t = (b[0] as u32) << 16 | (b[1] as u32) << 8 | b[0] as u32;
              MetaEvent::SetTempo(t)
          })) |
      0x54 => do_parse!(
          tag!([0x05]) >>
          hr: be_u8 >>
          min: be_u8 >>
          sec: be_u8 >>
          frame: be_u8 >>
          frac: be_u8 >>
          (MetaEvent::SmpteOffset {
              hours: hr,
              minutes: min,
              seconds: sec,
              frames: frame,
              fractional_frames: frac,
          })) |
      0x58 => do_parse!(
          tag!([0x04]) >>
          num: be_u8 >>
          denom: be_u8 >>
          clock: be_u8 >>
          div: be_u8 >>
          (MetaEvent::TimeSignature {
              numerator: num,
              denominator: denom,
              clocks_per_metronome: clock,
              notated_divisions: div,
          })) |
      0x59 => do_parse!(
          tag!([0x02]) >>
          sharp_flat: be_i8 >>
          major_minor: alt!(tag!([0]) | tag!([1])) >>
          (MetaEvent::KeySignature {
              key: match sharp_flat {
                  x if x > 0 => Key::Sharps(x as u8),
                  x if x < 0 => Key::Flats(-x as u8),
                  _ => Key::OfC,
              },
              kind: match major_minor[0] {
                  0 => KeyKind::Major,
                  1 => KeyKind::Minor,
                  _ => unreachable!(),
              }
          })) |
      0x7F => do_parse!(
          len: var_length >>
          data: take!(len) >>
          (MetaEvent::SequencerSpecific {
              data: data,
          })) |
      // We have to request the whole range because nom generates bad parsers
      // for exhaustive switch.
      kind@0x00...0xFF => do_parse!(
          len: var_length >>
          data: take!(len) >>
          (MetaEvent::Other {
              kind: kind,
              data: data,
          }))
    )
  )
);


// System Exclusive Events /////////////////////////////////////////////////////

#[derive(Debug, PartialEq, Eq)]
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
    let mut result = 0;
    for i in 0..4 {
        if i >= input.len() {
            return IResult::Incomplete(nom::Needed::Unknown);
        }
        result <<= 7;
        result |= (input[i] & 0x7F) as u32;
        if input[i] & 0x80 == 0 {
            return IResult::Done(&input[i+1..], result);
        }
    }
    IResult::Error(ErrorKind::Custom(0))
}

named!(u7<&[u8], u8>,
  switch!(be_u8,
    n@0x00...0x7F => value!(n)
  )
);


// Tests ///////////////////////////////////////////////////////////////////////

#[cfg(test)]
#[test]
fn test_var_length() {
    let cases = [
        (0x______00, vec![0x00]),
        (0x______40, vec![0x40]),
        (0x______7F, vec![0x7F]),
        (0x______80, vec![0x81, 0x00]),
        (0x____2000, vec![0xC0, 0x00]),
        (0x____3FFF, vec![0xFF, 0x7F]),
        (0x____4000, vec![0x81, 0x80, 0x00]),
        (0x__100000, vec![0xC0, 0x80, 0x00]),
        (0x__1FFFFF, vec![0xFF, 0xFF, 0x7F]),
        (0x__200000, vec![0x81, 0x80, 0x80, 0x00]),
        (0x08000000, vec![0xC0, 0x80, 0x80, 0x00]),
        (0x0FFFFFFF, vec![0xFF, 0xFF, 0xFF, 0x7F]),
    ];

    for &(number, ref bytes) in &cases {
        assert_eq!(var_length(&bytes[..]), IResult::Done(&b""[..], number));
    }
}

#[cfg(test)]
#[test]
fn test_event() {
    assert_eq!(event(&[0x00, 0xFF, 0x00, 0x02, 0x00, 0x01], &mut None),
               IResult::Done(&b""[..], Event::Meta(0, MetaEvent::SequenceNumber(1))));
    assert_eq!(event(&[0x00, 0xFF, 0x58, 0x04, 0x06, 0x03, 0x24, 0x08], &mut None),
               IResult::Done(&b""[..], Event::Meta(0, MetaEvent::TimeSignature {
                   numerator: 6,
                   denominator: 3,
                   clocks_per_metronome: 36,
                   notated_divisions: 8,
               })));
}

// let data = [
//     0x00,
//     0xF0, 0x03, 0x43, 0x12, 0x00,
//     0x81, 0x48,
//     0xF7, 0x06, 0x43, 0x12, 0x00, 0x43, 0x12, 0x00,
//     0x64,
//     0xF7, 0x04, 0x43, 0x12, 0x00, 0xF7,
// ];
// let res: IResult<&[u8], Event> = event(data);
// assert_eq!(res.output().unwrap(), )
