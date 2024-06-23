Attempt at making a macro-based parser / deserializer to be used to parse the aseprite data format

the format has a lot of ifs within sections though, which made the macros really messy and complicated

the nom-based parser I had before was working too

might come back to this someday though

little snippet

```rust
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

parsing::parsable_enum! {
    #[derive(Clone, PartialEq, Debug)]
    #[repr(u16)]
    pub enum Chunk<'a> {
        [[param: Dword = chunk_size]]
        [[limit_buffer = chunk_size as usize - std::mem::size_of::<Dword>()]]
        [[param: Word = chunk_type]]
        [[enum_type = chunk_type]]
        OldPalette1(OldPaletteChunk<'a>) = 0x0004,
        OldPalette2(OldPaletteChunk<'a>) = 0x0011,
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

let b = std::fs::read("test/animated.aseprite").unwrap();
let mut s = b.as_slice();
let h = s.read_type_le::<AsepriteFile>().unwrap();
let mut frame_bytes = h.frame_bytes;
for _ in 0..h.num_frames {
    let frame = frame_bytes.read_type_le::<Frame>().unwrap();
    let mut chunk_bytes = frame.chunk_bytes;
    for _ in 0..frame.num_chunks() {
        let chunk = chunk_bytes.read_type_le::<Chunk>().unwrap();
    }
}
```

