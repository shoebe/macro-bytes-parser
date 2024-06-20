use parsing::{Endianess, Parse, ReadBytes};
use std::marker::PhantomData;

pub type Byte = u8;
pub type Word = u16;
pub type Short = i16;
pub type Dword = u32;
pub type Long = i32;

#[derive(Clone, Copy, PartialEq, Eq, Debug, Parse)]
pub struct Fixed {
    num: u32,
}

impl Fixed {
    pub fn as_float(self) -> f32 {
        (self.num >> 16) as f32 + (self.num & (u16::MAX as u32)) as f32
    }
}

pub type Float = f32;
pub type Double = f64;
pub type Qword = u64;
pub type Long64 = i64;

#[derive(Clone, Copy, PartialEq, Eq, Debug, Parse)]
pub struct Point {
    pub x: Long,
    pub y: Long,
}

#[derive(Clone, Copy, PartialEq, Eq, Debug, Parse)]
pub struct Size {
    pub w: Long,
    pub h: Long,
}

#[derive(Clone, Copy, PartialEq, Eq, Debug, Parse)]
pub struct Rect {
    pub tl: Point,
    pub size: Size,
}

parsing::parsable_struct! {
    #[derive(Clone, PartialEq, Debug)]
    pub struct AsepriteFile<'a> {
        [[param: Dword = file_size]]
        [[limit_buffer = file_size as usize - std::mem::size_of::<Dword>()]]
        [[magic: Word = 0xA5E0]]
        pub num_frames: Word,
        pub width: Word,
        pub height: Word,
        /// 32=RGBA, 16=Grayscale, 8=Indexed
        pub color_depth: Word,
        pub flags: Dword,
        /// Deprecated, now on each frame
        pub frame_ms_dur: Word,
        [[ignore: Dword]]
        [[ignore: Dword]]
        pub invis_palette_ind: Byte,
        [[padding_bytes = 3]]
        /// 0 means 256 for old sprites
        pub color_num: Word,
        pub pix_width: Byte,
        pub pix_height: Byte,
        pub grid_x_pos: Short,
        pub grid_y_pos: Short,
        pub grid_width: Word,
        pub grid_heigth: Word,
        [[padding_bytes = 84]]
        #[parse(rest_of_buf)]
        pub frame_bytes: &'a [u8],
    }
}

parsing::parsable_struct! {
    #[derive(Clone, PartialEq, Debug)]
    pub struct Frame<'a> {
        [[param: Dword = frame_size]]
        [[limit_buffer = frame_size as usize - std::mem::size_of::<Dword>()]]
        [[magic: Word = 0xF1FA]]
        old_num_chunks: Word,
        pub frame_dur_ms: Word,
        [[padding_bytes = 2]]
        new_num_chunks: Dword,
        #[parse(rest_of_buf)]
        pub chunk_bytes: &'a [u8],
    }
}

impl Frame<'_> {
    pub fn num_chunks(&self) -> usize {
        if self.old_num_chunks == 0xFFFF {
            self.new_num_chunks as usize
        } else {
            self.old_num_chunks as usize
        }
    }
}

parsing::parsable_struct! {
    #[derive(Clone, PartialEq, Debug)]
    pub struct RestOfBytes<'a> {
        #[parse(rest_of_buf)]
        rest: &'a [u8]
    }
}

parsing::parsable_enum! {
    #[derive(Clone, PartialEq, Debug)]
    #[repr(u16)]
    pub enum Chunk<'a> {
        [[param: Dword = chunk_size]]
        [[limit_buffer = chunk_size as usize - std::mem::size_of::<Dword>()]]
        [[param: Word = chunk_type]]
        [[enum_type = chunk_type]]
        OldPalette1 ( OldPaletteChunk<'a> ) = 0x0004,
        OldPalette2 ( OldPaletteChunk<'a> ) = 0x0011,
        Layer(LayerChunk<'a>) = 0x2004,
        Cel(CelChunk<'a>) = 0x2005,
        CelExtra(RestOfBytes<'a>) = 0x2006,
        ColorProfile(ColorProfileChunk<'a>) = 0x2007,
        External(RestOfBytes<'a>) = 0x2008,
        /// deprecated
        Mask(RestOfBytes<'a>) = 0x2016,
        Path(RestOfBytes<'a>) = 0x2017,
        Tags(TagsChunk<'a>) = 0x2018,
        Palette(PaletteChunk<'a>) = 0x2019,
        UserData(UserDataChunk<'a>) = 0x2020,
    }
}

parsing::parsable_struct! {
    #[derive(Clone, PartialEq, Eq, Debug)]
    pub struct OldPaletteChunk<'a> {
        pub num_packets: Word,
        #[parse(rest_of_buf)]
        /// avoid parsing since not used
        pub bytes: &'a [u8],
    }
}

parsing::parsable_struct! {
    #[derive(Clone, PartialEq, Eq, Debug)]
    pub struct OldPalettePacket<'a> {
        pub num_skip: Byte,
        pub num_colors: Byte,
        #[parse(sized_buf = num_colors as usize * 3)]
        pub colors: &'a [u8],
    }
}

parsing::parsable_struct! {
    #[derive(Clone, PartialEq, Eq, Debug)]
    pub struct LayerChunk<'a> {
        pub flags: Word,
        pub layer_type: Word,
        pub child_level: Word,
        [[ignore: Word]]
        [[ignore: Word]]
        pub blend_mode: Word,
        pub opacity: Byte,
        [[padding_bytes = 3]]
        [[param: Word = string_size]]
        #[parse(sized_utf8_string = string_size)]
        pub layer_name: &'a str,
        #[parse(option_if: Dword = layer_type == 2)]
        pub tileset_index: Option<Dword>,
    }
}
parsing::parsable_struct! {
    #[derive(Clone, PartialEq, Eq, Debug)]
    pub struct CelChunk<'a> {
        pub layer_ind: Word,
        pub x_pos: Short,
        pub y_pos: Short,
        pub opacity: Byte,
        pub cel_type: Word,
        pub z_ind: Short,
        [[padding_bytes = 5]]
        #[parse(rest_of_buf)]
        pub rest: &'a [u8],
    }
}

impl CelChunk<'_> {
    pub fn parse_image(&self) -> parsing::Result<()> {
        let mut rest = self.rest;
        match self.cel_type {
            0x0 | 0x2 => {
                let width = rest.read_type_le::<Word>()?;
                let height = rest.read_type_le::<Word>()?;
                let img = rest;
                todo!()
            }
            0x1 | 0x3 => todo!(),
            _ => todo!(),
        }
    }
}

parsing::parsable_struct! {
    #[derive(Clone, PartialEq, Eq, Debug)]
    pub struct ColorProfileChunk<'a> {
        pub typ: Word,
        pub flags: Word,
        pub fixed_gamma: Fixed,
        [[padding_bytes = 8]]
        #[parse(rest_of_buf)]
        pub rest: &'a [u8],
    }
}

impl ColorProfileChunk<'_> {
    pub fn is_srgb(&self) -> bool {
        self.typ == 1 && self.flags == 0
            || self.flags == 1 && (self.fixed_gamma.as_float() - 2.2) < 0.0001
    }
}

parsing::parsable_struct! {
    #[derive(Clone, PartialEq, Eq, Debug)]
    pub struct TagsChunk<'a> {
        [[param: Word = num_tags]]
        #[parse(collection: Tag = num_tags)]
        pub tags: Vec<Tag<'a>>,
    }
}

parsing::parsable_struct! {
    #[derive(Clone, PartialEq, Eq, Debug)]
    pub struct Tag<'a> {
        pub start_ind: Word,
        pub end_ind: Word,
        pub direction: Byte,
        pub repeat: Word,
        [[padding_bytes = 10]]
        [[param: Word = string_size]]
        #[parse(sized_utf8_string = string_size)]
        pub tag_name: &'a str,
    }
}

parsing::parsable_struct! {
    #[derive(Clone, PartialEq, Eq, Debug)]
    pub struct PaletteChunk<'a> {
        pub new_palette_size: Dword,
        pub first_ind: Dword,
        pub last_ind: Dword,
        [[padding_bytes = 8]]
        #[parse(rest_of_buf)]
        pub palette_bytes: &'a [u8],
    }
}

parsing::parsable_struct! {
    #[derive(Clone, PartialEq, Eq, Debug)]
    pub struct UserDataChunk<'a> {
        pub new_palette_size: Dword,
        pub first_ind: Dword,
        pub last_ind: Dword,
        [[padding_bytes = 8]]
        #[parse(rest_of_buf)]
        pub palette_bytes: &'a [u8],
    }
}
