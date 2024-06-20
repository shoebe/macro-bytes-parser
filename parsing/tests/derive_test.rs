#![allow(dead_code)]

use parsing::ReadBytes;

#[test]
fn test_derive() {
    parsing::parsable_struct! {
        #[derive(Debug)]
        pub struct Header {
            item1: u64,
            [[magic: u8 = 0xFF]]
            [[padding_bytes = 3]]
            item2: u32,
        }
    }

    let mut bytes = [0_u8; std::mem::size_of::<Header>()];
    bytes[0] = 1;
    bytes[8] = u8::MAX;
    let mut b = bytes.as_slice();

    let h: Header = b.read_type_le().unwrap();
    assert_eq!(h.item1, 1);
    dbg!(h);

    let mut b = bytes.as_slice();
    let h: Header = b.read_type_be().unwrap();
    assert_eq!(h.item1, 72057594037927936);
    dbg!(h);

    let bytes = [0_u8; std::mem::size_of::<Header>()];
    let mut b = bytes.as_slice();
    let h = b.read_type_le::<Header>();
    assert_eq!(h.unwrap_err(), parsing::Error::MagicCheckFailed);
}

#[test]
fn test_derive_with_str() {
    parsing::parsable_struct! {
        #[derive(Debug)]
        pub struct Header<'a> {
            [[param: u8 = string_size]]
            something: u32,
            #[parse(sized_utf8_string = string_size)]
            s: &'a str,
            [[padding_bytes = 20]]
            something_else: u64,
        }
    }

    const MSG: &str = "hello my name is bob";
    const BYTES: &[u8] = MSG.as_bytes();
    let mut bytes = [0_u8;
        std::mem::size_of::<u8>()
            + std::mem::size_of::<u32>()
            + BYTES.len()
            + 20
            + std::mem::size_of::<u64>()];

    bytes[0] = MSG.len() as u8;
    bytes[1..5].copy_from_slice(&[5, 0, 0, 0]);
    bytes[5..(5 + BYTES.len())].copy_from_slice(BYTES);
    bytes[25 + BYTES.len()] = 1;

    let mut b = bytes.as_slice();
    let h: Header = b.read_type_le().unwrap();
    dbg!(h);

    let mut b = bytes.as_slice();
    let h: Header = b.read_type_be().unwrap();
    dbg!(h);
}

#[test]
fn test_derive_with_string() {
    parsing::parsable_struct! {
        #[derive(Debug)]
        pub struct Header {
            [[param: u8 = string_size]]
            something: u32,
            #[parse(sized_utf8_string = string_size)]
            s: String,
        }
    }

    const MSG: &str = "hello my name is bob";
    const BYTES: &[u8] = MSG.as_bytes();
    let mut bytes = [0_u8; std::mem::size_of::<u8>() + std::mem::size_of::<u32>() + BYTES.len()];

    bytes[0] = MSG.len() as u8;
    bytes[1..5].copy_from_slice(&[5, 0, 0, 0]);
    bytes[5..].copy_from_slice(BYTES);

    let mut b = bytes.as_slice();
    let h: Header = b.read_type_le().unwrap();
    dbg!(h);

    let mut b = bytes.as_slice();
    let h: Header = b.read_type_be().unwrap();
    dbg!(h);
}

#[test]
fn test_derive_with_buf() {
    parsing::parsable_struct! {
        #[derive(Debug)]
        pub struct Header<'a> {
            [[param: u8 = buf_size]]
            something: u32,
            #[parse(sized_buf = buf_size)]
            s: &'a [u8],
        }
    }

    const MSG: [u8; 10] = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10];
    const BYTES: &[u8] = MSG.as_slice();
    let mut bytes = [0_u8; std::mem::size_of::<u8>() + std::mem::size_of::<u32>() + BYTES.len()];

    bytes[0] = MSG.len() as u8;
    bytes[1..5].copy_from_slice(&[5, 0, 0, 0]);
    bytes[5..].copy_from_slice(BYTES);

    let mut b = bytes.as_slice();
    let h: Header = b.read_type_le().unwrap();
    dbg!(h);

    let mut b = bytes.as_slice();
    let h: Header = b.read_type_be().unwrap();
    dbg!(h);
}

#[test]
fn test_derive_with_buf2() {
    parsing::parsable_struct! {
        #[derive(Debug)]
        pub struct Header {
            [[param: u8 = buf_size]]
            something: u32,
            #[parse(sized_buf = buf_size)]
            s: Vec<u8>,
        }
    }

    const MSG: [u8; 10] = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10];
    const BYTES: &[u8] = MSG.as_slice();
    let mut bytes = [0_u8; std::mem::size_of::<u8>() + std::mem::size_of::<u32>() + BYTES.len()];

    bytes[0] = MSG.len() as u8;
    bytes[1..5].copy_from_slice(&[5, 0, 0, 0]);
    bytes[5..].copy_from_slice(BYTES);

    let mut b = bytes.as_slice();
    let h: Header = b.read_type_le().unwrap();
    dbg!(h);

    let mut b = bytes.as_slice();
    let h: Header = b.read_type_be().unwrap();
    dbg!(h);
}

#[test]
fn test_derive_with_collection() {
    parsing::parsable_struct! {
        #[derive(Debug)]
        pub struct Item<'a> {
            [[param: u8 = buf_size]]
            #[parse(sized_buf = buf_size)]
            buf: &'a [u8],
        }
    }

    parsing::parsable_struct! {
        #[derive(Debug)]
        pub struct Header<'a> {
            [[param: u8 = num_items]]
            something: u32,
            #[parse(collection: Item = num_items)]
            items: Vec<Item<'a>>,
        }
    }

    const MSG: [u8; 17] = [
        3, 0, 0, 0, 0, //header
        5, 1, 2, 3, 4, 5, //item1
        1, 6, //item2
        3, 7, 8, 9, //item3
    ];
    const BYTES: &[u8] = MSG.as_slice();

    let mut b = BYTES;
    let h: Header = b.read_type_le().unwrap();
    dbg!(h);
}

#[test]
fn test_enum_derive() {
    parsing::parsable_enum! {
        #[derive(Debug)]
        #[repr(u16)]
        pub enum Item {
            [[param: u8 = type_field]]
            [[enum_type = type_field]]
            Variant1(u32) = 0x1,
            Variant2(u16) = 0x2,
            //Variant3{hi: u8} = 0x3,
            //Variant4(u16, u16) = 0x4,
        }
    }

    {
        const MSG: [u8; 3] = [
            2, // variant 2
            1, 0,
        ];
        const BYTES: &[u8] = MSG.as_slice();

        let mut b = BYTES;
        let h: Item = b.read_type_le().unwrap();
        dbg!(h);
        let mut b = BYTES;
        let h: Item = b.read_type_be().unwrap();
        dbg!(h);
    }

    {
        const MSG: [u8; 5] = [
            1, // variant 2
            1, 0, 0, 0,
        ];
        const BYTES: &[u8] = MSG.as_slice();

        let mut b = BYTES;
        let h: Item = b.read_type_le().unwrap();
        dbg!(h);
        let mut b = BYTES;
        let h: Item = b.read_type_be().unwrap();
        dbg!(h);
    }
}
