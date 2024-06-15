extern crate dds_bridge as dds;
use dds::SmallSet as _;
use std::process::ExitCode;

/// Generate a hand with one card of each rank
fn get_random_average_hand(rng: &mut (impl rand::Rng + ?Sized)) -> dds::Hand {
    let bits: u64 = rng.gen();

    (0..13).fold(dds::Hand::EMPTY, |mut hand, i| {
        // SAFETY: we are slicing valid consecutive 2 bits from `bits`
        let suit = unsafe { core::mem::transmute((bits >> (2 * i) & 3) as u8) };
        hand.insert(dds::Card::new(suit, i + 2));
        hand
    })
}

fn get_random_symmetric_deal(rng: &mut (impl rand::Rng + ?Sized)) -> dds::Deal {
    let hand = get_random_average_hand(rng);
    let sequence = [
        hand.0[1], hand.0[2], hand.0[3], hand.0[0], hand.0[1], hand.0[2],
    ];
    let north = hand;
    let east = dds::Hand(sequence[0..4].try_into().expect("Invalid hand"));
    let south = dds::Hand(sequence[1..5].try_into().expect("Invalid hand"));
    let west = dds::Hand(sequence[2..6].try_into().expect("Invalid hand"));
    dds::Deal([north, east, south, west])
}

fn compute_deal(
    rng: &mut (impl rand::Rng + ?Sized),
) -> Result<(dds::Deal, dds::TricksTable), dds::Error> {
    const N: usize = dds_bridge_sys::MAXNOOFTABLES as usize;
    loop {
        let deals: [_; N] = core::array::from_fn(|_| get_random_symmetric_deal(rng));
        // SAFETY: `N` is exactly the maximum length of a deal segment.
        let tables = unsafe { dds::solve_deal_segment(&deals, dds::StrainFlags::all())? };

        for (i, &table) in tables.results[..N].iter().enumerate() {
            let tricks = dds::TricksTable::from(table);
            let pars = dds::calculate_pars(tricks, dds::Vulnerability::all())?;

            if pars[0].score + pars[1].score > 0 {
                return Ok((deals[i], tricks));
            }
        }
    }
}

#[doc = include_str!("README.md")]
fn main() -> Result<ExitCode, dds::Error> {
    let deals = match std::env::args().nth(1) {
        Some(string) => {
            if let Ok(n) = string.parse::<usize>() {
                n
            } else {
                eprintln!("{}", include_str!("README.md"));
                return Ok(ExitCode::FAILURE);
            }
        }
        None => 1,
    };

    for _ in 0..deals {
        let (deal, tricks) = compute_deal(&mut rand::thread_rng())?;
        println!(
            "{} {:X}",
            deal.display(dds::Seat::North),
            tricks.hex(dds::Seat::North, dds::Strain::SYS)
        );
    }

    Ok(ExitCode::SUCCESS)
}
