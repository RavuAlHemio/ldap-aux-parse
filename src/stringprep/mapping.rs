/// The target of a mapping of a character or range of characters.
#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub(crate) enum MappingTarget {
    /// Any character in the range is to be thrown away.
    ThrowAway,

    /// Any character in the range is to be mapped to this one. 
    One(char),

    /// Any character in the range is to be mapped to these two.
    Two(char, char),

    /// Any character in the range is to be mapped to these three.
    Three(char, char, char),

    /// Any character in the range is to be mapped to these four.
    Four(char, char, char, char),

    /// Any character in the range is to be mapped by adding the given value to its code point.
    BlockShift(i32),

    /// Any character in the range is to be mapped by adding the given value to its code point, then
    /// followed by a character with a constant value.
    BlockShiftAndConst(i32, char),

    /// Any character in the range is to be mapped by adding 1 to its code point, but only if its
    /// remainder after division by 2 is the given value.
    PairShift(u32),
}
impl MappingTarget {
    pub fn will_change(&self, source: char) -> bool {
        match self {
            Self::ThrowAway => true,
            Self::One(a) => *a != source,
            Self::Two(_, _) => true,
            Self::Three(_, _, _) => true,
            Self::Four(_, _, _, _) => true,
            Self::BlockShift(n) => *n != 0,
            Self::BlockShiftAndConst(_, _) => true,
            Self::PairShift(remainder_by_2) => (source as u32) % 2 == *remainder_by_2,
        }
    }

    pub fn map_write(&self, source: char, destination: &mut String) {
        match self {
            Self::ThrowAway => {},
            Self::One(a) => {
                destination.push(*a);
            },
            Self::Two(a, b) => {
                destination.push(*a);
                destination.push(*b);
            },
            Self::Three(a, b, c) => {
                destination.push(*a);
                destination.push(*b);
                destination.push(*c);
            },
            Self::Four(a, b, c, d) => {
                destination.push(*a);
                destination.push(*b);
                destination.push(*c);
                destination.push(*d);
            },
            Self::BlockShift(offset) => {
                let source_i32 = (source as u32) as i32;
                let target = source_i32 + *offset;
                destination.push(char::from_u32(target as u32).unwrap());
            },
            Self::BlockShiftAndConst(offset, a) => {
                let source_i32 = (source as u32) as i32;
                let target = source_i32 + *offset;
                destination.push(char::from_u32(target as u32).unwrap());
                destination.push(*a);
            },
            Self::PairShift(remainder_by_2) => {
                if (source as u32) % 2 == *remainder_by_2 {
                    // increase by 1
                    destination.push(char::from_u32((source as u32) + 1).unwrap());
                } else {
                    // take unchanged
                    destination.push(source);
                }
            },
        }
    }
}

/// The mapping of a character or range of characters to a different character or range of
/// characters.
#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub(crate) struct Mapping {
    pub first_char: char,
    pub last_char: char,
    pub target: MappingTarget,
}
impl Mapping {
    pub const fn new(
        first_char: char,
        last_char: char,
        target: MappingTarget,
    ) -> Self {
        Self {
            first_char,
            last_char,
            target,
        }
    }
}
