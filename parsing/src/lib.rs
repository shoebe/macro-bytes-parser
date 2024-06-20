pub use parsing_macro::*;

pub trait Endianess {}

pub struct LittleEndian;
impl Endianess for LittleEndian {}

pub type LE = LittleEndian;

pub struct BigEndian;
impl Endianess for BigEndian {}

pub type BE = BigEndian;

#[derive(Debug, PartialEq)]
pub enum Error {
    SliceTooSmall,
    MagicCheckFailed,
    InterpretStrFailed(std::str::Utf8Error),
    EnumTypeValueMatchFailed,
}

pub type Result<T> = std::result::Result<T, Error>;

pub trait ReadBytes<'a> {
    fn read_bytes(&mut self, num: usize) -> Result<&'a [u8]>;
    fn read_rest(&mut self) -> &'a [u8];
    fn read_type<E: Endianess, T: Parse<'a, E>>(&mut self) -> Result<T>;
    fn read_type_be<T: Parse<'a, BigEndian>>(&mut self) -> Result<T> {
        self.read_type::<BigEndian, T>()
    }
    fn read_type_le<T: Parse<'a, LittleEndian>>(&mut self) -> Result<T> {
        self.read_type::<LittleEndian, T>()
    }
}

impl<'a> ReadBytes<'a> for &'a [u8] {
    fn read_bytes(&mut self, num: usize) -> Result<&'a [u8]> {
        // split_at_checked is unstable
        if num <= self.len() {
            let (front, back) = self.split_at(num);
            *self = back;
            Ok(front)
        } else {
            Err(Error::SliceTooSmall)
        }
    }

    fn read_rest(&mut self) -> &'a [u8] {
        self.read_bytes(self.len()).unwrap()
    }

    fn read_type<E: Endianess, T: Parse<'a, E>>(&mut self) -> Result<T> {
        T::parse(self)
    }
}

pub trait Parse<'a, E: Endianess>: Sized {
    fn parse(input: &mut impl ReadBytes<'a>) -> Result<Self>;
}

macro_rules! impl_primitive_parse {
    ($typ: ty) => {
        impl<'a> Parse<'a, LittleEndian> for $typ {
            fn parse(input: &mut impl ReadBytes<'a>) -> Result<Self> {
                let bytes = input.read_bytes(std::mem::size_of::<$typ>())?;
                Ok(<$typ>::from_le_bytes(bytes.try_into().unwrap()))
            }
        }

        impl<'a> Parse<'a, BigEndian> for $typ {
            fn parse(input: &mut impl ReadBytes<'a>) -> Result<Self> {
                let bytes = input.read_bytes(std::mem::size_of::<$typ>())?;
                Ok(<$typ>::from_be_bytes(bytes.try_into().unwrap()))
            }
        }

        impl<'a, E: Endianess, const N: usize> Parse<'a, E> for [$typ; N]
        where
            $typ: Parse<'a, E>,
        {
            fn parse(input: &mut impl ReadBytes<'a>) -> Result<Self> {
                let mut out = [Default::default(); N];
                for i in out.iter_mut() {
                    *i = input.read_type::<E, $typ>()?;
                }
                Ok(out)
            }
        }
    };
}

impl_primitive_parse!(u8);
impl_primitive_parse!(u16);
impl_primitive_parse!(u32);
impl_primitive_parse!(u64);
impl_primitive_parse!(u128);

impl_primitive_parse!(i8);
impl_primitive_parse!(i16);
impl_primitive_parse!(i32);
impl_primitive_parse!(i64);
impl_primitive_parse!(i128);

impl_primitive_parse!(f32);
impl_primitive_parse!(f64);

#[cfg(test)]
mod test {
    use crate::ReadBytes;
    use crate::LE;

    #[test]
    fn test() {
        let bytes = [0_u8; 8];
        let mut b = bytes.as_slice();
        let num: u64 = b.read_type_le().unwrap();
        assert_eq!(num, 0);
    }

    #[test]
    fn test2() {
        let bytes = [0_u8; 8];
        let mut b = bytes.as_slice();
        let num = b.read_type::<LE, u64>().unwrap();
        assert_eq!(num, 0);
    }
}
