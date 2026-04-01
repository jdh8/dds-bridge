use dds_bridge::deal::{Hand, Seat};
use dds_bridge::solver::{Vulnerability, emulate_par};

fn main() -> anyhow::Result<()> {
    let north: Hand = "T9762.AT54.JT75.".parse()?;
    let south: Hand = "A.KQ962.A86.Q642".parse()?;
    let (score, contract) = emulate_par(north, south, Vulnerability::NONE, Seat::East, 1000)?;

    print!("NS par: {score}, ");

    match contract {
        Some((contract, seat)) => {
            println!(
                "{}{}{}{}",
                contract.bid.level,
                contract.bid.strain.unicode(),
                contract.penalty,
                char::from(seat)
            );
        }
        None => println!("P"),
    }

    Ok(())
}
