//! The `Opr24` type, used for encoding instruction operands.

/// A 24-bit integer encoding an instruction operand.
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct Opr24 {
    pub(super) bytes: [u8; 3],
}

/// A number is too big to fit in an `Opr24`.
#[derive(Debug)]
pub struct Opr24OutOfRange(());

impl Opr24 {
    pub const MAX: u32 = (1 << 24);

    /// Tries to construct a new `Opr24`.
    pub fn new(x: u32) -> Result<Self, Opr24OutOfRange> {
        if x < Self::MAX {
            Ok(Self {
                bytes: [(x & 0xFF) as u8, ((x >> 8) & 0xFF) as u8, ((x >> 16) & 0xFF) as u8],
            })
        } else {
            Err(Opr24OutOfRange(()))
        }
    }

    /// Packs `T` into an `Opr24`.
    pub fn pack<T>(x: T) -> Self
    where
        T: PackableToOpr24,
    {
        x.pack_to_opr24()
    }

    /// Unpacks an `Opr24` into `T`.
    pub fn unpack<T>(self) -> T
    where
        T: PackableToOpr24,
    {
        T::unpack_from_opr24(self)
    }
}

impl From<u8> for Opr24 {
    fn from(value: u8) -> Self {
        Self { bytes: [value, 0, 0] }
    }
}

impl TryFrom<usize> for Opr24 {
    type Error = Opr24OutOfRange;

    fn try_from(value: usize) -> Result<Self, Self::Error> {
        Self::new(u32::try_from(value).map_err(|_| Opr24OutOfRange(()))?)
    }
}

impl From<Opr24> for u32 {
    fn from(opr: Opr24) -> u32 {
        (opr.bytes[0] as u32) | ((opr.bytes[1] as u32) << 8) | ((opr.bytes[2] as u32) << 16)
    }
}

impl From<Opr24> for usize {
    fn from(opr: Opr24) -> usize {
        (opr.bytes[0] as usize) | ((opr.bytes[1] as usize) << 8) | ((opr.bytes[2] as usize) << 16)
    }
}

impl std::fmt::Debug for Opr24 {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:x}", u32::from(*self))
    }
}

impl std::fmt::Display for Opr24 {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{self:?}")
    }
}

pub trait PackableToOpr24 {
    fn pack_to_opr24(self) -> Opr24;
    fn unpack_from_opr24(opr: Opr24) -> Self;
}

impl PackableToOpr24 for (u16, u8) {
    fn pack_to_opr24(self) -> Opr24 {
        // SAFETY: This packs the two numbers into 24 bits and will never exceed the range of an
        // Opr24.
        unsafe { Opr24::new((self.0 as u32) << 8 | self.1 as u32).unwrap_unchecked() }
    }

    fn unpack_from_opr24(opr: Opr24) -> Self {
        let x = u32::from(opr);
        let big = ((x & 0xFFFF00) >> 8) as u16;
        let small = (x & 0x0000FF) as u8;
        (big, small)
    }
}
