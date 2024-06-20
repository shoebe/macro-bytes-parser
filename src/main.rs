#[allow(unused)]
mod ase_file;
pub use ase_file::*;
use parsing::ReadBytes;

fn main() {
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
}
