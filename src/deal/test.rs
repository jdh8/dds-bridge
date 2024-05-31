use super::*;

const _: () = {
    let mut bits = 0;
    while bits < 0xFFFF {
        assert!(Holding::from_bits(bits).to_bits() == bits & Holding::ALL.to_bits());
        bits += 1;
    }
    assert!(Holding::from_bits(bits).to_bits() == bits & Holding::ALL.to_bits());
};

fn all_holdings() -> impl Iterator<Item = Holding> {
    (0..1 << 13).map(|i| Holding::from_bits(i << 2))
}

#[test]
fn test_eq() {
    all_holdings().zip(all_holdings()).for_each(|(u, v)| {
        assert_eq!(u == v, u.to_bits() == v.to_bits());
    });
}

#[test]
fn test_bitand() {
    all_holdings().zip(all_holdings()).for_each(|(u, v)| {
        assert_eq!((u & v).to_bits(), u.to_bits() & v.to_bits());
    });
}

#[test]
fn test_bitor() {
    all_holdings().zip(all_holdings()).for_each(|(u, v)| {
        assert_eq!((u | v).to_bits(), u.to_bits() | v.to_bits());
    });
}

#[test]
fn test_bitxor() {
    all_holdings().zip(all_holdings()).for_each(|(u, v)| {
        assert_eq!((u ^ v).to_bits(), u.to_bits() ^ v.to_bits());
    });
}

#[test]
fn test_sub() {
    all_holdings().zip(all_holdings()).for_each(|(u, v)| {
        assert_eq!((u - v).to_bits(), u.to_bits() & !v.to_bits());
    });
}

#[test]
fn test_not() {
    all_holdings().for_each(|v| {
        assert_eq!(!v, Holding::ALL - v);
        assert_eq!(!v, Holding::ALL ^ v);
    });
}

#[test]
fn test_random_deals() {
    (0..1_000_000).for_each(|_| {
        let hands = Deal::new(&mut rand::thread_rng()).0;
        assert_eq!(hands[0] | hands[1] | hands[2] | hands[3], Hand::ALL);
        assert_eq!(hands[0] & hands[1], Hand::EMPTY);
        assert_eq!(hands[0] & hands[2], Hand::EMPTY);
        assert_eq!(hands[0] & hands[3], Hand::EMPTY);
        assert_eq!(hands[1] & hands[2], Hand::EMPTY);
        assert_eq!(hands[1] & hands[3], Hand::EMPTY);
        assert_eq!(hands[2] & hands[3], Hand::EMPTY);
        assert_eq!(hands[0].len(), 13);
        assert_eq!(hands[1].len(), 13);
        assert_eq!(hands[2].len(), 13);
        assert_eq!(hands[3].len(), 13);
    });
}
