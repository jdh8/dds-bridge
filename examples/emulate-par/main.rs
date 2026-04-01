use clap::Parser;
use dds_bridge::deal::{Hand, Seat};
use dds_bridge::solver::{Vulnerability, emulate_par};

#[derive(clap::ValueEnum, Clone, Copy)]
enum Vul {
    None,
    Ns,
    Ew,
    Both,
}

impl From<Vul> for Vulnerability {
    fn from(v: Vul) -> Self {
        match v {
            Vul::None => Self::NONE,
            Vul::Ns => Self::NS,
            Vul::Ew => Self::EW,
            Vul::Both => Self::ALL,
        }
    }
}

#[derive(clap::ValueEnum, Clone, Copy)]
enum Dealer {
    N,
    E,
    S,
    W,
}

impl From<Dealer> for Seat {
    fn from(d: Dealer) -> Self {
        match d {
            Dealer::N => Self::North,
            Dealer::E => Self::East,
            Dealer::S => Self::South,
            Dealer::W => Self::West,
        }
    }
}

/// Emulate par score for North-South by simulating random deals
#[derive(Parser)]
struct Args {
    /// North's hand in dot-separated suit notation (e.g. T9762.AT54.JT75.)
    #[arg(short = 'N', long)]
    north: Hand,

    /// South's hand in dot-separated suit notation (e.g. A.KQ962.A86.Q642)
    #[arg(short = 'S', long)]
    south: Hand,

    /// Vulnerability
    #[arg(short, long, default_value = "none")]
    vulnerability: Vul,

    /// Dealer seat
    #[arg(short, long, default_value = "n")]
    dealer: Dealer,

    /// Number of simulated deals
    #[arg(short = 'n', long, default_value = "1000")]
    count: usize,
}

fn main() -> anyhow::Result<()> {
    let args = Args::parse();
    let (score, contract) = emulate_par(
        args.north,
        args.south,
        args.vulnerability.into(),
        args.dealer.into(),
        args.count,
    )?;

    print!("NS par: {score:.2}, ");

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
