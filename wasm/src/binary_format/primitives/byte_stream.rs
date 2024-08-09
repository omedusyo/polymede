#[derive(Debug)]
pub struct Offset(u32);

// ===Bytes4===
pub fn bytes4(bytes: [u8; 4]) -> Bytes4 {
    Bytes4 { index: 0, bytes }
}

pub struct Bytes4 {
    index: usize,
    bytes: [u8; 4],
}

impl ByteStream for Bytes4 {
    fn next(&mut self) -> Response {
        use Response::*;
        if self.index == 4 {
            End
        } else {
            let b = self.bytes[self.index];
            self.index += 1;
            New(b)
        }
    }
}

// ===Byte===
pub fn byte(b: u8) -> Byte {
    Byte(Some(b))
}

pub struct Byte(Option<u8>);

impl ByteStream for Byte {
    fn next(&mut self) -> Response {
        use Response::*;
        match self.0 {
            Some(b) => {
                self.0 = None;
                New(b)
            },
            None => End,
        }
    }
}


impl Offset {
    // How to encode the Offset?
    // It should be encoded as LEB128 of u32.
    // Problem is that LEB128 is variable length encoding, and for offset we want fixed size.
    // Fortunately LEB128 allows for redundancies.
    //
    // How many bytes can that be at-most?
    // Note 35 = 5*7 is the least multiple of 7 greater than 32.
    // So we pad x : u32 with zeroes to get x : u35.
    // Then we have 5 groups of 7 bits. Each group will get additional bit as a flag.
    // Since we emit a byte for each group, this wil take 5 bytes.
    // So... u32 can always be encoded as u40 via LEB128.
    // WARNING: In WASM spec we have "As an additional constraint, the total number of bytes encoding a value of type uN must not exceed ceil(N/7) bytes.
    //
    // Fortunately for us, ceil(32/7) == 5, so it seems we're all good.

    const SIZE: usize = 5; // 5 bytes.

    const ZERO: [u8; Self::SIZE] = [0, 0, 0, 0, 0];

    fn new(n: u32) -> Offset {
        Self(n)
    }

    fn encode(&self) -> U32ToFixed40LEB128 {
        U32ToFixed40LEB128::new(self.0)
    }
}

pub enum Response {
    End,
    New(u8),
    // Tells the writer using the byte-stream to allocate space for a pointer of type Offset
    // that will be allocated later.
    // We assume that the writer will allocate `std::mem::size_of::<Offset>()`-many bytes.
    PrepareForForwardPointer,
    // Tells the writer that the pointer has now become known,
    // so he can go back and write the pointer into the file.
    Pointer(Offset),
}

pub trait ByteStream {
    fn next(&mut self) -> Response;

    fn to_vec(&mut self) -> Vec<u8> {
        use Response::*;

        let mut bytes: Vec<u8> = vec![];
        let mut pointer_stack: Vec<usize> = vec![];
        loop {
            match self.next() {
                New(byte) => {
                    bytes.push(byte)
                },
                PrepareForForwardPointer => {
                    pointer_stack.push(bytes.len());
                    bytes.extend_from_slice(&Offset::ZERO);
                },
                Pointer(pointer) => {
                    let offset = pointer_stack.pop().unwrap();
                    let encoded_pointer = pointer.encode().to_vec();

                    // TODO: This is really ugly. Find appropriate vector function.
                    bytes[offset] = encoded_pointer[0];
                    bytes[offset + 1] = encoded_pointer[1];
                    bytes[offset + 2] = encoded_pointer[2];
                    bytes[offset + 3] = encoded_pointer[3];
                    bytes[offset + 4] = encoded_pointer[4];
                }
                End => {
                    break
                }
            }
        }
        bytes
    }

    // fn seq(self, s1: impl ByteStream) -> impl ByteStream where Self: Sized {
    fn seq<S: ByteStream>(self, s1: S) -> Seq<Self, S> where Self: Sized {
        concat(self, s1)
    }

    // fn enclose(self) -> impl ByteStream where Self: Sized {
    fn enclose(self) -> Enclose<Self> where Self: Sized {
        Enclose::new(self)
    }
}

// ===Sequence===
pub enum Seq<S0, S1> {
    DoFirst(S0, Option<S1>), // Second one is optional because when the first is
                             // optional, we need to move the second one into a new
                             // branch. This is impossible without ownership. But with
                             // Option we can gain ownership by exchaning Nothing for
                             // the second stream.
    DoSecond(S1),
}

impl <S0, S1> Seq<S0, S1> {
    fn new(s0: S0, s1: S1) -> Self {
        Self::DoFirst(s0, Some(s1))
    }
}

impl <S0: ByteStream, S1: ByteStream> ByteStream for Seq<S0, S1> {
    fn next(&mut self) -> Response {
        use Response::*;
        use Seq::*;
        match self {
            DoFirst(s0, s1) => match s0.next() {
                New(byte) => New(byte),
                PrepareForForwardPointer => PrepareForForwardPointer,
                Pointer(pointer) => Pointer(pointer),
                End => {
                    // SAFETY: s1 in BeforeFirstStarted(s0, s1) is always Some(_) by construction.
                    *self = DoSecond(s1.take().unwrap());
                    self.next()
                }
            },
            DoSecond(s1) => s1.next(),
        }
    }
}

// pub fn concat(s0: impl ByteStream, s1: impl ByteStream) -> impl ByteStream {
//     Seq::new(s0, s1)
// }

pub fn concat<S0: ByteStream, S1: ByteStream>(s0: S0, s1: S1) -> Seq<S0, S1> {
    Seq::new(s0, s1)
}

// pub fn concat1(s0: &mut impl ByteStream, s1: impl ByteStream) -> impl ByteStream {
//     ByteStreamPair::new(s0, s1)
// }

// ===Enclose with Size===
pub enum Enclose<S> {
    NotYetStarted(Option<S>),
    Do(usize, S), // first component counts the number of bytes seen from the stream.
    Done,
}

impl <S> Enclose<S> {
    fn new(s: S) -> Self {
        Self::NotYetStarted(Some(s))
    }
}

impl <S: ByteStream> ByteStream for Enclose<S> {
    fn next(&mut self) -> Response {
        use Response::*;
        use Enclose::*;
        match self {
            NotYetStarted(s) => {
                // SAFETY: s in NotYetStarted(s) is always Some(_) by construction.
                *self = Do(0, s.take().unwrap());
                PrepareForForwardPointer
            },
            Do(count, s) => match s.next() {
                New(byte) => {
                    *count += 1;
                    New(byte)
                },
                PrepareForForwardPointer => {
                    *count += Offset::SIZE;
                    PrepareForForwardPointer
                },
                Pointer(pointer) => Pointer(pointer),
                End => {
                    let count = *count;
                    *self = Done;
                    // WARNING: May panic!
                    Pointer(Offset::new(count as u32))
                }
            },
            Done => End,
        }
    }
}

// ====Counted Vector===
// A vector s.t. we encode its length as a prefix
pub enum CVec<S> {
    NotYetStarted { ss: Option<Vec<S>> },
    Do { index: usize, ss: Vec<S> },
    Done,
}

// TODO: Can I change this into iterator? I don't think so. I need to have ownership of these
// things. They are living machines. Wait... the iterator will construct the machines, so it's all
// good. Should be possible. This is like calling .collect(), right?
pub fn cvector<S: ByteStream>(ss: Vec<S>) -> CVec<S> {
    CVec::NotYetStarted { ss: Some(ss) }
}

impl <S: ByteStream> ByteStream for CVec<S> {
    fn next(&mut self) -> Response {
        use Response::*;
        use CVec::*;
        match self {
            NotYetStarted { ss } => {
                // SAFETY: ss in NotYetStarted { ss } is always Some(_) by construction.
                *self = Do { index: 0, ss: ss.take().unwrap() };
                PrepareForForwardPointer
            },
            Do { index, ss } => {
                if *index < ss.len() {
                    match ss[*index].next() {
                        New(byte) => New(byte),
                        PrepareForForwardPointer => PrepareForForwardPointer,
                        Pointer(offset) => Pointer(offset),
                        End => {
                            *index += 1;
                            self.next()
                        },
                    }
                } else {
                    let count = ss.len();
                    *self = Done;
                    Pointer(Offset::new(count as u32))
                }
            },
            Done => End,
        }
    }
}

// ===Ending Vector===
// A vector s.t. we encode some ending delimiter at the end.
pub enum EVec<S, E> {
    Do { index: usize, ss: Vec<S>, end: Option<E> },
    DoEnding { end: E },
}

pub fn evector<S: ByteStream, E: ByteStream>(ss: Vec<S>, e: E) -> EVec<S, E> {
    EVec::Do { index: 0, ss, end: Some(e) }
}

impl <S: ByteStream, E: ByteStream> ByteStream for EVec<S, E> {
    fn next(&mut self) -> Response {
        use Response::*;
        use EVec::*;
        match self {
            Do { index, ss, end } => {
                if *index < ss.len() {
                    match ss[*index].next() {
                        New(byte) => New(byte),
                        PrepareForForwardPointer => PrepareForForwardPointer,
                        Pointer(offset) => Pointer(offset),
                        End => {
                            *index += 1;
                            self.next()
                        },
                    }
                } else {
                    // SAFETY: end in Do { end } is always Some(_) by construction.
                    *self = DoEnding { end: end.take().unwrap() };
                    self.next()
                }
            },
            DoEnding { end } => {
                end.next()
            }
        }
    }
}


// ====Or====
pub enum Either<A, B> {
    Left(A),
    Right(B),
}

impl <S0: ByteStream, S1: ByteStream> ByteStream for Either<S0, S1> {
    fn next(&mut self) -> Response {
        use Either::*;
        match self {
            Left(s0) => s0.next(),
            Right(s1) => s1.next(),
        }
    }
}

// ====Option===
impl <S: ByteStream> ByteStream for Option<S> {
    fn next(&mut self) -> Response {
        use Response::*;
        match self {
            Some(s) => {
                s.next()
            },
            None => End,
        }
    }
}

// ===Vector===
// This just sequences encoders one after the other (no prefixesa or postfixes follow).
pub enum Vector<S> {
    Do { index: usize, ss: Vec<S> },
    Done,
}

pub fn vector<S: ByteStream>(ss: Vec<S>) -> Vector<S> {
    Vector::Do { index:0, ss }
}

impl <S: ByteStream> ByteStream for Vector<S> {
    fn next(&mut self) -> Response {
        use Response::*;
        use Vector::*;
        match self {
            Do { index, ss } => {
                if *index < ss.len() {
                    match ss[*index].next() {
                        New(byte) => New(byte),
                        PrepareForForwardPointer => PrepareForForwardPointer,
                        Pointer(offset) => Pointer(offset),
                        End => {
                            *index += 1;
                            self.next()
                        },
                    }
                } else {
                    *self = Done;
                    End
                }
            },
            Done => End,
        }
    }
}


// ====constant bytes===
pub struct Bytes {
    bytes: Vec<u8>,
    index: usize,
}

pub fn bytes(bytes: Vec<u8>) -> Bytes {
    Bytes::new(bytes)
}

impl Bytes {
    pub fn new(bytes: Vec<u8>) -> Self {
        Self { bytes, index: 0 }
    }
}

impl ByteStream for Bytes {
    fn next(&mut self) -> Response {
        use Response::*;
        if self.index < self.bytes.len() {
            let byte = self.bytes[self.index];
            self.index += 1;
            New(byte)
        } else {
            End
        }
    }
}


// ====0, 1, 2, ..., 255===
pub struct EnumerateByteValues {
    state: Option<u8>,
}

impl EnumerateByteValues {
    pub fn new() -> Self {
        Self { state: Some(0) }
    }
}

impl ByteStream for EnumerateByteValues {
    fn next(&mut self) -> Response {
        use Response::*;
        match self.state {
            Some(byte) => {
                if byte == 255 {
                    self.state = None
                } else {
                    self.state = Some(byte + 1)
                }
                New(byte)
            },
            None => End,
        }
    }
}


// ====LEB128====

// Such a stream is at-most 5 bytes.
pub struct U32ToVariableLEB128 {
    state: Option<u32>, // None means stream is done.
}

impl U32ToVariableLEB128 {
    pub fn new(x: u32) -> Self {
        Self { state: Some(x) }
    }
}

impl ByteStream for U32ToVariableLEB128 {
    fn next(&mut self) -> Response {
        use Response::*;
        match self.state {
            Some(x) if x < 128 => {
                self.state = None;
                New(x as u8)
            },
            Some(x) => {
                // Suppose x == ..._ _ _ _   _ b6 b5 b4 b3 b2 b1 b0
                // Then    y == 1 b6 b5 b4 b3 b2 b1 b0
                let y: u8 = ((x as u8) & 127) + 128;
                self.state = Some(x >> 7); // Forget about the first 7 bits by shifting.
                New(y)
            },
            None => End,
        }
    }
}

// Fixed stream is always 5 bytes (40 bits).
pub enum U32ToFixed40LEB128 {
    Continue { bytes_emmited: usize, x: u32 },
    Pad { bytes_emmited: usize },
    Done,
}

impl U32ToFixed40LEB128 {
    pub fn new(x: u32) -> Self {
        Self::Continue { bytes_emmited: 0, x }
    }
}

impl ByteStream for U32ToFixed40LEB128 {
    fn next(&mut self) -> Response {
        use Response::*;
        use U32ToFixed40LEB128::*;
        match self {
            Continue { bytes_emmited, x } if *x < 128 => {
                let bytes_emmited = *bytes_emmited + 1;
                let x = *x;
                if bytes_emmited == 5 {
                    *self = Done;
                    New(x as u8)
                } else {
                    *self = Pad { bytes_emmited };
                    New((x + 128) as u8)
                }
            },
            Continue { bytes_emmited, x } => {
                let y: u8 = ((*x as u8) & 127) + 128;
                *self = Continue { bytes_emmited: *bytes_emmited + 1, x: *x >> 7 };
                New(y)
            },
            Pad { bytes_emmited } => {
                let bytes_emmited = *bytes_emmited + 1;
                if bytes_emmited == 5 {
                    *self = Done;
                    New(0)
                } else {
                    *self = Pad { bytes_emmited };
                    New(128)
                }
            },
            Done => End,
        }
    }
}


// ===Signed LEB128===
// Fixed stream is always 5 bytes (40 bits).
// TODO: This should be more polymorphic.
pub enum I32ToSignedLEB128 {
    Continue { x: i32 },
    Done,
}

pub enum I64ToSignedLEB128 {
    Continue { x: i64 },
    Done,
}

impl I32ToSignedLEB128 {
    pub fn new(x: i32) -> Self { Self::Continue { x } }
}

impl I64ToSignedLEB128 {
    pub fn new(x: i64) -> Self { Self::Continue { x } }
}

impl ByteStream for I32ToSignedLEB128 {
    fn next(&mut self) -> Response {
        use Response::*;
        use I32ToSignedLEB128::*;
        // This is adapted from the js code in https://en.wikipedia.org/wiki/LEB128
        match self {
            Continue { x } => {
                let mut val = *x;
                let byte = (val & 0x7f) as u8; // Last 7 bits.
                val = val >> 7; // Get rid of the last 7 bits.
                *x = val; 
                // Note that byte & 0x40 is basically the 7th bit of byte.
                if (val == 0 && (byte & 0x40) == 0) || (val == -1 && (byte &0x40) != 0) {
                    *self = Done;
                    New(byte)
                } else {
                    New(byte | 0x80)
                }
            },
            Done => End,
        }
    }
}

impl ByteStream for I64ToSignedLEB128 {
    fn next(&mut self) -> Response {
        use Response::*;
        use I64ToSignedLEB128::*;
        // This is adapted from the js code in https://en.wikipedia.org/wiki/LEB128
        match self {
            Continue { x } => {
                let mut val = *x;
                let byte = (val & 0x7f) as u8; // Last 7 bits.
                val = val >> 7; // Get rid of the last 7 bits.
                *x = val; 
                // Note that byte & 0x40 is basically the 7th bit of byte.
                if (val == 0 && (byte & 0x40) == 0) || (val == -1 && (byte &0x40) != 0) {
                    *self = Done;
                    New(byte)
                } else {
                    New(byte | 0x80)
                }
            },
            Done => End,
        }
    }
}

// ===Floats===
pub struct F32ToIEEE754LittleEndian(Bytes4);

impl F32ToIEEE754LittleEndian {
    pub fn new(x: f32) -> Self {
        let bytes = x.to_le_bytes();
        Self(bytes4(bytes))
    }
}

impl ByteStream for F32ToIEEE754LittleEndian {
    fn next(&mut self) -> Response { self.0.next() }
}

// ===Strings===
pub struct UTF8(CVec<Byte>);

pub fn string(s: &str) -> UTF8 {
    let bs: Vec<Byte> = s.bytes().map(byte).collect();
    UTF8(cvector(bs))
}

impl ByteStream for UTF8 {
    fn next(&mut self) -> Response {
        self.0.next()
    }
}


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn bytes0() {
        let mut s = EnumerateByteValues::new();

        let v = s.to_vec();
        assert!(v.len() == 256);
        assert!(v[0] == 0);
        assert!(v[1] == 1);
        assert!(v[2] == 2);

        assert!(v[255] == 255);

    }

    #[test]
    fn seq0() {
        let mut s = EnumerateByteValues::new().seq(EnumerateByteValues::new()).seq(EnumerateByteValues::new());
        let v = s.to_vec();

        assert!(v.len() == 256 + 256 + 256);
    }

    #[test]
    fn variable_leb128_0() {
        let x: u32 = 0b10100100101001001010010000111001;
        let mut s = U32ToVariableLEB128::new(x);

        let v = s.to_vec();

        // 10100100101001001010010000111001
        //
        //    1010
        // 0100101
        // 0010010
        // 1001000
        // 0111001
        assert!(v == [0b10111001, 0b11001000, 0b10010010, 0b10100101, 0b1010].to_vec());
    }

    #[test]
    fn variable_leb128_1() {
        let x: u32 = 125;
        let mut byte_values = U32ToVariableLEB128::new(x);
        let v = byte_values.to_vec();

        assert!(v[0] == 125);
        assert!(v.len() == 1);
        assert!(v == [125]);
    }

    #[test]
    fn variable_leb128_2() {
        let x: u32 = 128;
        let mut s = U32ToVariableLEB128::new(x);
        let v = s.to_vec();

        assert!(v == [128, 1]);
    }

    #[test]
    fn fixed_leb128_0() {
        let x: u32 = 4294967295;
        let mut s = U32ToFixed40LEB128::new(x);
        let v = s.to_vec();

        assert!([255, 255, 255, 255, 15].to_vec() == v);
    }

    #[test]
    fn fixed_leb128_1() {
        let x: u32 = 0;
        let mut s = U32ToFixed40LEB128::new(x);
        let v = s.to_vec();

        assert!([128, 128, 128, 128, 0].to_vec() == v);
    }

    #[test]
    fn signed_leb128_0() {
        let x: i32 = -123456;
        let mut s = I32ToSignedLEB128::new(x);
        let v = s.to_vec();

        let v0 = vec![0xc0, 0xbb, 0x78];
        assert!([0xc0, 0xbb, 0x78].to_vec() == v);
    }


    #[test]
    fn enclose0() {
        let mut s = bytes(vec![1,2,3,4,5,6,7,8,9]).enclose();
        let v = s.to_vec();

        assert!(v == [ 137,128,128,128,0,  1,2,3,4,5,6,7,8,9 ].to_vec());
    }

    #[test]
    fn enclose1()  {
        let mut s = bytes(vec![]).enclose();
        let v = s.to_vec();

        assert!(v == [ 128,128,128,128,0, ].to_vec());
    }

    #[test]
    fn enclosure_of_single_byte() {
        let bytes = byte(9).enclose().to_vec();

        assert!(bytes == vec![128 + 1, 128, 128, 128, 0,    9]);
    }

    #[test]
    fn empty_vector() {
        let vs: Vec<Byte> = vec![];
        let bytes = cvector(vs).to_vec();

        assert!(bytes == vec![128 + 0, 128 + 0, 128 + 0, 128 + 0, 0]);
    }

    #[test]
    fn enclosure_of_multiple_bytes0() {
        let bytes = bytes(vec![0, 0, 0, 0, 0]).enclose().to_vec();

        assert!(bytes == vec![128 + 4 + 1, 128 + 0, 128 + 0, 128 + 0, 0,     0, 0, 0, 0, 0]);
    }

    #[test]
    fn enclosure_of_multiple_bytes1() {
        let bytes = bytes(vec![128, 128, 128, 128, 0]).enclose().to_vec();

        assert!(bytes == vec![128 + 4 + 1, 128 + 0, 128 + 0, 128 + 0, 0,     128, 128, 128, 128, 0]);
    }

    #[test]
    fn enclosure_of_enclosure() {
        let mut s = byte(9).enclose().enclose();
        let bytes = s.to_vec();

        assert!(bytes == vec![128 + 6, 128 + 0, 128 + 0, 128 + 0, 0,     128 + 1, 128, 128, 128, 0,      9]);

    }

    #[test]
    fn enclosure_of_empty_vector() {
        let vs: Vec<Byte> = vec![];
        let bytes = cvector(vs).enclose().to_vec();

        assert!(bytes == vec![128 + 4 + 1, 128 + 0, 128 + 0, 128 + 0, 0,     128 + 0, 128 + 0, 128 + 0, 128 + 0, 0]);
    }

    #[test]
    fn string0() {
        let mut s = string("f");
        let bytes = s.to_vec();

        assert!(bytes == vec![128 + 1, 128, 128, 128, 0,    102]);
    }
}
