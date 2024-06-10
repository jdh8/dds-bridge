use dds_bridge::deal::{Card, Deal, Hand, Seat, SmallSet as _};
use dds_bridge::solver;
use std::process::ExitCode;

/// Generate a hand with one card of each rank
fn get_random_average_hand(rng: &mut (impl rand::Rng + ?Sized)) -> Hand {
    let bits: u64 = rng.gen();

    (0..13).fold(Hand::EMPTY, |mut hand, i| {
        // SAFETY: we are slicing valid consecutive 2 bits from `bits`
        let suit = unsafe { core::mem::transmute((bits >> (2 * i) & 3) as u8) };
        hand.insert(Card::new(suit, i + 2));
        hand
    })
}

fn get_random_symmetric_deal(rng: &mut (impl rand::Rng + ?Sized)) -> Deal {
    let hand = get_random_average_hand(rng);
    let sequence = [
        hand.0[1], hand.0[2], hand.0[3], hand.0[0], hand.0[1], hand.0[2],
    ];
    let north = hand;
    let east = Hand(sequence[0..4].try_into().expect("Invalid hand"));
    let south = Hand(sequence[1..5].try_into().expect("Invalid hand"));
    let west = Hand(sequence[2..6].try_into().expect("Invalid hand"));
    Deal([north, east, south, west])
}

fn compute_deal() -> Result<Deal, solver::Error> {
    const N: usize = dds_bridge_sys::MAXNOOFTABLES as usize;

    loop {
        let deals: [_; N] =
            core::array::from_fn(|_| get_random_symmetric_deal(&mut rand::thread_rng()));
        // SAFETY: `N` is exactly the maximum length of a deal segment.
        let tables = unsafe { solver::solve_deal_segment(&deals, solver::StrainFlags::all())? };

        for (i, &table) in tables.results[..N].iter().enumerate() {
            let tricks = solver::TricksTable::from(table);
            let pars = solver::calculate_pars(tricks, solver::Vulnerability::all())?;

            if pars[0].score > 0 && pars[1].score > 0 {
                return Ok(deals[i]);
            }
        }
    }
}

#[doc = include_str!("README.md")]
fn main() -> Result<ExitCode, solver::Error> {
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
        let deal = compute_deal()?;
        println!("{}", deal.display(Seat::North));
    }

    Ok(ExitCode::SUCCESS)
}
